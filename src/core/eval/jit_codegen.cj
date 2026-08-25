package ystyle::xisp.core.eval
import ystyle::xisp.types.*
import std.collection.ArrayList
import std.collection.HashMap
import std.env.getVariable

/**
 * JIT 翻译器（J1：Int 快路径 + 直接调用）
 *
 * 值表示：16B (tag: Int64, payload: Int64)；tag 编码与 LispValue 无布局依赖：
 *   JIT_TAG_INT=1 FLOAT=2 BOOL=3 NIL=4 STR=5 SYM=6 REF=7
 *
 * 函数 ABI（所有 JIT 函数入口统一）：
 *   rdi = 参数区基址（16B × k：[arg0][arg1]...；调用者须在 rdi-16 预留结果槽）
 *   rsi = 参数个数 k
 *   rdx = ctx（CPointer<Unit>，慢路径/deopt 用）
 *   结束把结果写到 [rdi-16]（tag,payload）后 ret；调用者从 [rdi-16] 读取
 *
 * 帧布局（prologue：push rbp; mov rbp,rsp; sub rsp, frameSize）：
 *   槽 i（0..localSlots-1）@ [rbp - 16*(i+1)]
 *   T_CTX     @ [rbp - 16*(localSlots+1)]
 *   T_ARGBASE @ [rbp - 16*(localSlots+2)]
 *   T_ARGC    @ [rbp - 16*(localSlots+3)]
 *
 * JIT 栈：16B 值往下增长；调用布局 [func][arg0]...[argN-1]（N=argCount, func 最低地址）
 */
public class JitCodegen {
    // J2：跨函数调用/全局解析（由 JitRuntime.init 注入）
    public static var entryTablePtr: Int64 = 0
    public static var resolveFnAbs: Int64 = 0

    static let R_RAX: Int64 = 0
    static let R_RCX: Int64 = 1
    static let R_RDX: Int64 = 2
    static let R_RSP: Int64 = 4
    static let R_RBP: Int64 = 5
    static let R_RSI: Int64 = 6
    static let R_RDI: Int64 = 7
    static let R_R10: Int64 = 10
    static let R_R9: Int64 = 9
    // rsi 高位置位 = 调用方已填入 callee 全局槽（自/同函数调用；跨函数未填 → callee 走 helper）
    static let FLAG_GLOBALS_FILLED: Int64 = 1 << 31   // 需 imm32 可表示（and rax, imm32 符号扩展）
    static let R_R11: Int64 = 11

    let em = JitEmitter()
    // 跳转回填：(rel32 位置, 目标字节码 ip｜-1=函数入口 0)
    var patches: ArrayList<(Int64, Int64)> = ArrayList<(Int64, Int64)>()
    var labelAt: HashMap<Int64, Int64> = HashMap<Int64, Int64>()
    // deopt 慢分支：(jcc 位置, site id)（回填到 deopt stub；r11 携带 id）
    var deoptJumps: ArrayList<(Int64, Int64)> = ArrayList<(Int64, Int64)>()
    // 后调用空结果级联跳转（回填到 deopt stub；rax 已为 0）
    var bailJumps: ArrayList<Int64> = ArrayList<Int64>()
    var deoptTargetHint: Int64 = 0
    var fn: CompiledFunction = CompiledFunction(ArrayList<Int64>(), ArrayList<LispValue>(), ArrayList<CompiledFunction>(), "", Nil, ArrayList<String>(), ArrayList<String>(), ArrayList<Int64>(), 0, ArrayList<ParamInfo>())
    var selfClosureId: Int64 = -1

    public func compile(f: CompiledFunction, selfClosureId: Int64): Option<ArrayList<UInt8>> {
        this.fn = f
        this.selfClosureId = selfClosureId
        this.patches = ArrayList<(Int64, Int64)>()
        this.labelAt = HashMap<Int64, Int64>()
        this.deoptJumps = ArrayList<(Int64, Int64)>()
        this.bailJumps = ArrayList<Int64>()

        if (!this.checkSupported()) { return None }

        let localSlots = f.localSlots
        let frameSize = (localSlots + 3) * 16

        // ---- prologue ----
        em.pushRbp()
        em.movRbpRsp()
        em.subRspImm32(frameSize)
        let k = f.params.size
        for (i in 0..k) {
            em.movRegRmDisp(R_RAX, R_RDI, 16 * i)
            em.movRmDispReg(R_RBP, -16 * (i + 1), R_RAX)
            em.movRegRmDisp(R_RAX, R_RDI, 16 * i + 8)
            em.movRmDispReg(R_RBP, -16 * (i + 1) + 8, R_RAX)
        }
        em.movRmDispReg(R_RBP, -16 * (localSlots + 1), R_RDX)
        em.movRmDispReg(R_RBP, -16 * (localSlots + 2), R_RDI)
        em.movRmDispReg(R_RBP, -16 * (localSlots + 3), R_RSI)
        // 全局槽：调用方已填（flag）→ 从缓冲复制；否则（跨函数）→ @C helper 实时解析
        if (f.globalRefs.size > 0) {
            em.movRegRm(R_RAX, R_RSI)
            em.b(0x48); em.b(0x25)                 // and rax, imm32
            em.w32(FLAG_GLOBALS_FILLED)
            let jHelper = em.jccRel32(0x84)        // je → helper
            for (gi in 0..f.globalSlots.size) {
                let gs = f.globalSlots[gi]
                if (gs >= k && gs < localSlots) {
                    em.movRegRmDisp(R_RAX, R_RDI, 16 * gs)
                    em.movRmDispReg(R_RBP, -16 * (gs + 1), R_RAX)
                    em.movRegRmDisp(R_RAX, R_RDI, 16 * gs + 8)
                    em.movRmDispReg(R_RBP, -16 * (gs + 1) + 8, R_RAX)
                }
            }
            let jAfterG = em.jmpRel32()
            em.patchRel32(jHelper, em.here())
            if (JitCodegen.resolveFnAbs != 0) {
                em.movRmImm32(R_RDI, this.selfClosureId)
                em.movRegRm(R_RSI, R_RBP)
                em.movRmImm32(R_RDX, k)
                em.callAbs(JitCodegen.resolveFnAbs)
            }
            em.patchRel32(jAfterG, em.here())
        }

        // ---- body ----
        let code = f.code
        var ip: Int64 = 0
        while (ip < code.size) {
            this.labelAt[ip] = em.here()
            let op = code[ip]; ip++
            let r: Option<Option<Int64>> = match (op) {
                case 0 => this.emitPushConst(this.argAt(code, ip), ip + 1)
                case 1 => this.emitPushTagPayload(JIT_TAG_NIL, 0, ip)
                case 2 => this.emitPushTagPayload(JIT_TAG_BOOL, 1, ip)
                case 3 => this.emitPushTagPayload(JIT_TAG_BOOL, 0, ip)
                case 4 => this.emitPop(ip)
                case 7 => this.emitCall(this.argAt(code, ip), ip + 1)
                case 8 => this.emitBinStack(0, ip)
                case 9 => this.emitBinStack(1, ip)
                case 12 => this.emitBinStack(2, ip)
                case 13 => this.emitBinStack(3, ip)
                case 14 => this.emitBinStack(4, ip)
                case 15 => this.emitBinStack(5, ip)
                case 16 => this.emitBinStack(6, ip)
                case 10 => this.emitJump(this.argAt(code, ip), ip + 1)
                case 11 => this.emitJmpIfFalse(this.argAt(code, ip), ip + 1)
                case 17 => this.emitNot(ip)
                case 23 => this.emitDup(ip)
                case 24 => this.emitLoadLocal(this.argAt(code, ip), ip + 1)
                case 25 => this.emitStoreLocal(this.argAt(code, ip), ip + 1)
                case 43 => this.emitReturn(ip)
                case 52 => this.emitBinLC(this.argAt(code, ip), this.argAt(code, ip + 1), this.argAt(code, ip + 2), ip + 3)
                case 56 => this.emitCmpLCJF(this.argAt(code, ip), this.argAt(code, ip + 1), this.argAt(code, ip + 2), this.argAt(code, ip + 3), ip + 4)
                case 99 => this.emitReturn(ip)
                case _ => None
            }
            match (r) {
                case Some(Some(nextIp)) => ip = nextIp
                case _ => return None
            }
        }
        this.labelAt[ip] = em.here()

        // ---- 函数体结束 epilogue（VM 语义：无 RETURN 时栈顶即结果；与 op43/99 相同）----
        this.emitReturn(ip)

        // ---- 回填跳转 ----
        if (getVariable("XISP_JIT_DEBUG") != None) {
            var keys = StringBuilder("")
            for ((k, v) in labelAt) {
                keys.append(k.toString())
                keys.append("=")
                keys.append(v.toString())
                keys.append(" ")
            }
            eprintln("[JIT] labels: ${keys}")
            eprintln("[JIT] patches=${this.patches.size} jumps=${this.deoptJumps.size} ip=${ip} codsize=${this.fn.code.size}")
            for ((spos, sid) in deoptJumps) {
                eprintln("[JIT] site id=${sid} pos=${spos} bcIp=${this.findIpForOffset(spos)}")
            }
            var rawc = StringBuilder("")
            for (w in this.fn.code) {
                rawc.append(w.toString())
                rawc.append(",")
            }
            eprintln("[JIT] raw: ${rawc}")
            for ((at, targetIp) in patches) {
                let t = if (targetIp == -1) { 0 } else if (labelAt.contains(targetIp)) { labelAt[targetIp] } else { -999 }
                eprintln("[JIT]   patch at=${at} targetIp=${targetIp} resolved=${t}")
            }
        }
        for ((at, targetIp) in patches) {
            let t = if (targetIp == -1) { 0 } else if (labelAt.contains(targetIp)) { labelAt[targetIp] } else { 0 }
            em.patchRel32(at, t)
        }

        // ---- deopt stub ----
        let deoptTarget = em.here()
        for ((p, _) in deoptJumps) {
            em.patchRel32(p, deoptTarget)
        }
        for (p in bailJumps) {
            em.patchRel32(p, deoptTarget)
        }
        if (deoptJumps.size > 0) {
            // deopt：恢复帧后返回 siteId（rax=r11，小整数）→ 桥感知 → VM 回退解释器重跑该闭包调用
            em.movRegRm(R_RAX, R_R11)
            em.movRspRbp()
            em.popRbp()
            em.ret()
        }

        Some(em.bytes())
    }

    func argAt(code: ArrayList<Int64>, i: Int64): Int64 {
        if (i < code.size) { code[i] } else { -1 }
    }

    func checkSupported(): Bool {
        let f = this.fn
        // 编译器只产出纯位置参数（paramInfos 恒为空或与 params 等长）；带默认值等不编译
        if (f.paramInfos.size != 0 && f.paramInfos.size != f.params.size) { return false }
        for (info in f.paramInfos) {
            if (info.isRest || info.isKey) { return false }
            if (let Some(_) <- info.defaultValue) { return false }
        }
        for (c in f.constants) {
            match (c) {
                case Int(_) | Boolean(_) => ()
                case _ => return false
            }
        }
        true
    }

    // ============ 指令模板（全部返回 Some(nextIp) 或 None=不支持） ============

    func emitPushConst(idx: Int64, nextIp: Int64): Option<Option<Int64>> {
        let c = this.fn.constants[idx]
        match (c) {
            case Int(v) => Some(this.emitPushTagPayload(JIT_TAG_INT, v, nextIp))
            case Boolean(v) => Some(this.emitPushTagPayload(JIT_TAG_BOOL, if (v) { 1 } else { 0 }, nextIp))
            case _ => None
        }
    }

    func emitPushTagPayload(tag: Int64, payload: Int64, nextIp: Int64): Option<Int64> {
        em.movRmImm32(R_RAX, payload)
        em.movRmImm32(R_RCX, tag)
        this.emitPushRcxRax()
        Some(nextIp)
    }

    func emitPushRcxRax(): Unit {
        em.subRspImm32(16)
        em.movRmDispReg(R_RSP, 0, R_RCX)
        em.movRmDispReg(R_RSP, 8, R_RAX)
    }

    func emitPop(nextIp: Int64): Option<Int64> {
        em.addRspImm32(16)
        Some(nextIp)
    }

    func emitJump(target: Int64, nextIp: Int64): Option<Int64> {
        let at = em.jmpRel32()
        patches.add((at, target))
        Some(nextIp)
    }

    func emitJmpIfFalse(target: Int64, nextIp: Int64): Option<Int64> {
        // 栈顶 truthy：INT payload!=0 → 真；=0 → 假跳；NIL → 假；BOOL payload==1；其他 deopt
        em.movRegRmDisp(R_RAX, R_RSP, 0)
        em.movRmImm32(R_RCX, JIT_TAG_INT)
        em.cmpRmReg(R_RAX, R_RCX)
        let jNotInt = em.jccRel32(0x85)
        em.movRegRmDisp(R_RAX, R_RSP, 8)
        em.movRmImm32(R_RCX, 0)
        em.cmpRmReg(R_RAX, R_RCX)
        let jFalse = em.jccRel32(0x84)      // je → 假
        // 真：弹栈继续
        em.addRspImm32(16)
        let jContinue = em.jmpRel32()
        // 假路径：弹栈 + 跳 target
        em.patchRel32(jFalse, em.here())
        em.addRspImm32(16)
        let jTarget = em.jmpRel32()
        patches.add((jTarget, target))
        em.patchRel32(jContinue, em.here())
        this.fixSlow([jNotInt])
        Some(nextIp)
    }

    func emitLoadLocal(idx: Int64, nextIp: Int64): Option<Int64> {
        em.subRspImm32(16)
        em.movRegRmDisp(R_RAX, R_RBP, -16 * (idx + 1))
        em.movRmDispReg(R_RSP, 0, R_RAX)
        em.movRegRmDisp(R_RAX, R_RBP, -16 * (idx + 1) + 8)
        em.movRmDispReg(R_RSP, 8, R_RAX)
        Some(nextIp)
    }

    func emitStoreLocal(idx: Int64, nextIp: Int64): Option<Int64> {
        em.movRegRmDisp(R_RAX, R_RSP, 0)
        em.movRmDispReg(R_RBP, -16 * (idx + 1), R_RAX)
        em.movRegRmDisp(R_RAX, R_RSP, 8)
        em.movRmDispReg(R_RBP, -16 * (idx + 1) + 8, R_RAX)
        em.addRspImm32(16)
        Some(nextIp)
    }

    func emitDup(nextIp: Int64): Option<Int64> {
        em.subRspImm32(16)
        em.movRegRmDisp(R_RAX, R_RSP, 16)
        em.movRmDispReg(R_RSP, 0, R_RAX)
        em.movRegRmDisp(R_RAX, R_RSP, 24)
        em.movRmDispReg(R_RSP, 8, R_RAX)
        Some(nextIp)
    }

    func emitNot(nextIp: Int64): Option<Int64> {
        em.movRegRmDisp(R_RAX, R_RSP, 0)
        em.movRmImm32(R_RCX, JIT_TAG_INT)
        em.cmpRmReg(R_RAX, R_RCX)
        let slow = this.slowJcc()
        em.movRegRmDisp(R_RAX, R_RSP, 8)
        em.movRmImm32(R_RCX, 0)
        em.cmpRmReg(R_RAX, R_RCX)
        let jne0 = em.jccRel32(0x85)
        em.movRmImm32(R_RAX, 1)                      // 0 → not(true)
        let jdone = em.jmpRel32()
        em.patchRel32(jne0, em.here())
        em.movRmImm32(R_RAX, 0)                      // ≠0 → not(false)
        em.patchRel32(jdone, em.here())
        em.movRmImm32(R_RCX, JIT_TAG_BOOL)
        em.addRspImm32(16)
        this.emitPushRcxRax()
        this.fixSlow([slow])
        Some(nextIp)
    }

    func emitBinStack(kind: Int64, nextIp: Int64): Option<Int64> {
        // 栈顶 [b @ rsp+0][a @ rsp+16]
        em.movRegRmDisp(R_RAX, R_RSP, 0)
        em.movRmImm32(R_RCX, JIT_TAG_INT)
        em.cmpRmReg(R_RAX, R_RCX)
        let slowB = this.slowJcc()
        em.movRegRmDisp(R_RAX, R_RSP, 16)
        em.cmpRmReg(R_RAX, R_RCX)
        let slowA = this.slowJcc()
        em.movRegRmDisp(R_RAX, R_RSP, 24)   // a payload
        em.movRegRmDisp(R_RCX, R_RSP, 8)    // b payload
        match (kind) {
            case 0 | 1 | 2 | 3 =>
                let extra = this.emitArith(kind)
                for (p in extra) { this.fixSlow([p]) }
                em.movRmImm32(R_RCX, JIT_TAG_INT)
                em.addRspImm32(32)
                this.emitPushRcxRax()
            case 4 =>   // a < b
                em.cmpRmReg(R_RAX, R_RCX)
                this.emitCmpBoolPush(0x8C, 32, nextIp)   // jl → true
                this.fixSlow([slowB, slowA])
                return Some(nextIp)
            case 5 =>
                em.cmpRmReg(R_RAX, R_RCX)
                this.emitCmpBoolPush(0x8F, 32, nextIp)
                this.fixSlow([slowB, slowA])
                return Some(nextIp)
            case 6 =>
                em.cmpRmReg(R_RAX, R_RCX)
                this.emitCmpBoolPush(0x84, 32, nextIp)
                this.fixSlow([slowB, slowA])
                return Some(nextIp)
            case _ => return None
        }
        this.fixSlow([slowB, slowA])
        Some(nextIp)
    }

    // 发射：cmp 已做；jccSuffix → push BOOL(1) else BOOL(0)；popN=弹出字节数
    func emitCmpBoolPush(jccSuffix: Int64, popBytes: Int64, nextIp: Int64): Unit {
        let jtrue = em.jccRel32(jccSuffix)
        em.movRmImm32(R_RAX, 0)
        let jdone = em.jmpRel32()
        em.patchRel32(jtrue, em.here())
        em.movRmImm32(R_RAX, 1)
        em.patchRel32(jdone, em.here())
        em.movRmImm32(R_RCX, JIT_TAG_BOOL)
        em.addRspImm32(popBytes)
        this.emitPushRcxRax()
    }

    func emitArith(kind: Int64): ArrayList<Int64> {
        match (kind) {
            case 0 =>
                em.addRmReg(R_RAX, R_RCX)
                ArrayList<Int64>()
            case 1 =>
                em.subRmReg(R_RAX, R_RCX)
                ArrayList<Int64>()
            case 2 =>
                em.b(0x48); em.b(0x0F); em.b(0xAF); em.b(0xC1)
                ArrayList<Int64>()
            case _ =>
                // 3: a / b；b==0 → 慢
                em.movRmImm32(R_RDX, 0)
                em.cmpRmReg(R_RCX, R_RDX)
                let slowDiv = this.slowJe()
                em.b(0x48); em.b(0x99)
                em.b(0x48); em.b(0xF7); em.b(0xF9)
                let arr = ArrayList<Int64>()
                arr.add(slowDiv)
                arr
        }
    }

    func emitBinLC(kind: Int64, localIdx: Int64, constIdx: Int64, nextIp: Int64): Option<Int64> {
        let c = this.fn.constants[constIdx]
        match (c) {
            case Int(bv) =>
                if (kind == 7) {
                    // not(truthy(槽))
                    em.movRegRmDisp(R_RAX, R_RBP, -16 * (localIdx + 1))
                    em.movRmImm32(R_RCX, JIT_TAG_INT)
                    em.cmpRmReg(R_RAX, R_RCX)
                    let slow = this.slowJcc()
                    em.movRegRmDisp(R_RAX, R_RBP, -16 * (localIdx + 1) + 8)
                    em.movRmImm32(R_RCX, 0)
                    em.cmpRmReg(R_RAX, R_RCX)
                    let jne0 = em.jccRel32(0x85)
                    em.movRmImm32(R_RAX, 1)
                    let jdone = em.jmpRel32()
                    em.patchRel32(jne0, em.here())
                    em.movRmImm32(R_RAX, 0)
                    em.patchRel32(jdone, em.here())
                    em.movRmImm32(R_RCX, JIT_TAG_BOOL)
                    this.emitPushRcxRax()
                    this.fixSlow([slow])
                    return Some(nextIp)
                }
                em.movRegRmDisp(R_RAX, R_RBP, -16 * (localIdx + 1))
                em.movRmImm32(R_RCX, JIT_TAG_INT)
                em.cmpRmReg(R_RAX, R_RCX)
                let slow = this.slowJcc()
                em.movRegRmDisp(R_RAX, R_RBP, -16 * (localIdx + 1) + 8)
                em.movRmImm32(R_RCX, bv)
                match (kind) {
                    case 0 => em.addRmReg(R_RAX, R_RCX)
                    case 1 => em.subRmReg(R_RAX, R_RCX)
                    case 2 => em.b(0x48); em.b(0x0F); em.b(0xAF); em.b(0xC1)
                    case 3 =>
                        em.movRmImm32(R_RDX, 0)
                        em.cmpRmReg(R_RCX, R_RDX)
                        let slowDiv = this.slowJe()
                        em.b(0x48); em.b(0x99)
                        em.b(0x48); em.b(0xF7); em.b(0xF9)
                        this.fixSlow([slowDiv])
                    case 4 =>
                        em.cmpRmReg(R_RAX, R_RCX)
                        this.emitCmpBoolPush(0x8C, 0, nextIp)
                        this.fixSlow([slow])
                        return Some(nextIp)
                    case 5 =>
                        em.cmpRmReg(R_RAX, R_RCX)
                        this.emitCmpBoolPush(0x8F, 0, nextIp)
                        this.fixSlow([slow])
                        return Some(nextIp)
                    case 6 =>
                        em.cmpRmReg(R_RAX, R_RCX)
                        this.emitCmpBoolPush(0x84, 0, nextIp)
                        this.fixSlow([slow])
                        return Some(nextIp)
                    case _ => return None
                }
                em.movRmImm32(R_RCX, JIT_TAG_INT)
                this.emitPushRcxRax()
                this.fixSlow([slow])
                return Some(nextIp)
            case _ => return None
        }
    }

    func emitCmpLCJF(cmpKind: Int64, localIdx: Int64, constIdx: Int64, target: Int64, nextIp: Int64): Option<Int64> {
        let c = this.fn.constants[constIdx]
        match (c) {
            case Int(bv) =>
                em.movRegRmDisp(R_RAX, R_RBP, -16 * (localIdx + 1))
                em.movRmImm32(R_RCX, JIT_TAG_INT)
                em.cmpRmReg(R_RAX, R_RCX)
                let slow = this.slowJcc()
                em.movRegRmDisp(R_RAX, R_RBP, -16 * (localIdx + 1) + 8)
                em.movRmImm32(R_RCX, bv)
                em.cmpRmReg(R_RAX, R_RCX)
                let at = match (cmpKind) {
                    case 0 => em.jccRel32(0x8D)   // jnl = !(a<b)
                    case 1 => em.jccRel32(0x8E)   // jng = !(a>b)
                    case _ => em.jccRel32(0x85)   // jne = !(a==b)
                }
                patches.add((at, target))
                this.fixSlow([slow])
                return Some(nextIp)
            case _ => None
        }
    }

    // CALL：JIT 栈（低→高）= [argN-1][argN-2]...[arg0][func]（func 最高地址，共 N=argCount 个 16B 槽）
    // 快路径（func 为 REF 且 payload==selfClosureId）：
    //   1. sub rsp, 16*(calleeLocalSlots+1)，新区布局 [result@0][slot0@16]...[slotN-1@16N]
    //   2. 复制 arg_j → slot_j；复制自己的全局槽 → slot_gs（与桥布局一致）
    //   3. call callee；callee 结束写结果到 [rdi-16]=[newRsp+0]
    func emitCall(argCount: Int64, nextIp: Int64): Option<Int64> {
        if (argCount < 2) { return None }
        let n = argCount
        let k = n - 1
        let localSlots = this.fn.localSlots
        em.movRegRmDisp(R_RAX, R_RSP, 16 * k)             // func tag @ [rsp+16k]
        em.movRmImm32(R_RCX, JIT_TAG_REF)
        em.cmpRmReg(R_RAX, R_RCX)
        let slowTag = this.slowJcc()
        em.movRegRmDisp(R_RAX, R_RSP, 16 * k + 8)         // func payload
        em.movRmImm32(R_RCX, this.selfClosureId)
        em.cmpRmReg(R_RAX, R_RCX)
        // J2（本次仅自调用）：非自 → deopt 回退（跨函数 JIT 基建 r9/入口表就绪；互递归时序 bug 下轮修）
        let slowId = this.slowJcc()
        this.emitFrameAndCall(k, localSlots, false)
        // 3b. 结果为空（下层 deopt）→ 本帧级联退栈
        em.b(0x48); em.b(0x85); em.b(0xC0)   // test rax, rax
        let bail = em.jccRel32(0x84)         // je → deopt stub（延迟回填）
        bailJumps.add(bail)
        // 4. 结果 @ [newRsp+0]
        em.movRegRmDisp(R_RAX, R_RSP, 0)
        em.movRegRmDisp(R_RCX, R_RSP, 8)
        em.addRspImm32(16 * (k + 1 + n))
        em.subRspImm32(16)
        em.movRmDispReg(R_RSP, 0, R_RAX)
        em.movRmDispReg(R_RSP, 8, R_RCX)
        Some(nextIp)
    }

    // 公共调用段：帧缓冲 = result + k 参数槽；rdi=&slot0；r11Mode=true → call r11（跨函数），
    // false → call rel32（self，回填入口 0）
    func emitFrameAndCall(k: Int64, localSlots: Int64, r11Mode: Bool): Unit {
        em.subRspImm32(16 * (k + 1))
        for (j in 0..k) {
            let srcOff = 16 * (k + 1) + 16 * (k - 1 - j)
            em.movRegRmDisp(R_RAX, R_RSP, srcOff)
            em.movRmDispReg(R_RSP, 16 * (j + 1), R_RAX)
            em.movRegRmDisp(R_RAX, R_RSP, srcOff + 8)
            em.movRmDispReg(R_RSP, 16 * (j + 1) + 8, R_RAX)
        }
        // self（同函数）：调用方复制 callee 全局槽 + FLAG；跨函数：无全局复制（callee helper 解析）
        if (!r11Mode) {
            for (gi in 0..this.fn.globalSlots.size) {
                let gs = this.fn.globalSlots[gi]
                if (gs >= k && gs < localSlots) {
                    em.movRegRmDisp(R_RAX, R_RBP, -16 * (gs + 1))
                    em.movRmDispReg(R_RSP, 16 * (gs + 1), R_RAX)
                    em.movRegRmDisp(R_RAX, R_RBP, -16 * (gs + 1) + 8)
                    em.movRmDispReg(R_RSP, 16 * (gs + 1) + 8, R_RAX)
                }
            }
        }
        em.leaRegRmDisp(R_RDI, R_RSP, 16)
        em.movRmImm32(R_RSI, if (r11Mode) { k } else { k | FLAG_GLOBALS_FILLED })
        em.movRegRmDisp(R_RDX, R_RBP, -16 * (localSlots + 1))
        if (r11Mode) {
            em.movR11R9()                                  // r11 = 入口
            em.b(0x41); em.b(0xFF); em.b(0xD3)             // call r11
        } else {
            let callAt = em.callRel32()
            patches.add((callAt, -1))                      // 函数入口 0
        }
    }

    func emitReturn(nextIp: Int64): Option<Int64> {
        let localSlots = this.fn.localSlots
        em.movRegRmDisp(R_RAX, R_RSP, 0)
        em.movRegRmDisp(R_RCX, R_RSP, 8)
        em.movRegRmDisp(R_RDX, R_RBP, -16 * (localSlots + 2))   // argBase
        em.movRmDispReg(R_RDX, -16, R_RAX)
        em.movRmDispReg(R_RDX, -8, R_RCX)
        em.leaRegRmDisp(R_RAX, R_RDX, -16)
        em.movRspRbp()
        em.popRbp()
        em.ret()
        Some(nextIp)
    }

    var siteSeq: Int64 = 0

    func findIpForOffset(off: Int64): Int64 {
        var best: Int64 = -1
        var bestOff: Int64 = -1
        for ((k, v) in labelAt) {
            if (v <= off && v > bestOff) {
                bestOff = v
                best = k
            }
        }
        best
    }

    // 发射慢分支：mov r11, <siteId> + jne rel32
    func slowJcc(): Int64 {
        let id = this.siteSeq
        this.siteSeq = this.siteSeq + 1
        em.movRmImm32(R_R11, id)
        let p = em.jccRel32(0x85)
        deoptJumps.add((p, id))
        p
    }

    func slowJe(): Int64 {
        let id = this.siteSeq
        this.siteSeq = this.siteSeq + 1
        em.movRmImm32(R_R11, id)
        let p = em.jccRel32(0x84)
        deoptJumps.add((p, id))
        p
    }

    func fixSlow(ps: Array<Int64>): Unit {
        ()   // 已由 slowJcc/slowJe 登记
    }
}
