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

        // ---- deopt stub（慢分支：rax=r11 site id <1024 → 桥/上层识别）----
        let deoptTarget = em.here()
        for ((p, _) in deoptJumps) {
            em.patchRel32(p, deoptTarget)
        }
        if (deoptJumps.size > 0) {
            // deopt：恢复帧后返回 siteId（rax=r11，小整数）→ 桥感知 → VM 回退解释器重跑该闭包调用
            em.movRegRm(R_RAX, R_R11)
            em.movRspRbp()
            em.popRbp()
            em.ret()
        }
        // ---- bail stub（级联：下层 <1024 → 本帧 rax=0 返回 → 上层/桥继续识别）----
        let bailTarget = em.here()
        for (p in bailJumps) {
            em.patchRel32(p, bailTarget)
        }
        if (bailJumps.size > 0) {
            em.movRmImm32(R_RAX, 0)
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
        // 大帧函数（大量 let 局部）机器栈帧过大 → 深递归可能撞原生栈守卫 → 回退 VM
        if (f.localSlots > 64) { return false }
        // 编译器只产出纯位置参数（paramInfos 恒为空或与 params 等长）；带默认值等不编译
        if (f.paramInfos.size != 0 && f.paramInfos.size != f.params.size) { return false }
        for (info in f.paramInfos) {
            if (info.isRest || info.isKey) { return false }
            if (let Some(_) <- info.defaultValue) { return false }
        }
        for (c in f.constants) {
            match (c) {
                case Int(v) =>
                    // imm32 符号扩展：仅 [-2^31, 2^31-1] 常量可正确装载（mov r64, imm32 符号扩展 bit31）
                    // 超出 → 回退解释器（曾静默错值：3000000000 变 -1294967296）
                    if (v < -2147483648 || v > 2147483647) { return false }
                case Boolean(_) => ()
                case _ => return false
            }
        }
        // 自递归函数若含 arity != 自身参数数的调用：缺参自调用会让 callee 读垃圾参数槽
        // （R/v1 已资格拒缺参；generic 补齐——即便该调用是调不同 arity 的函数，保守拒绝仅失优化不失正确）
        var isSelfRec: Bool = false
        for (name in f.globalRefs) {
            if (name == f.name) { isSelfRec = true }
        }
        if (isSelfRec) {
            var ip: Int64 = 0
            while (ip < f.code.size) {
                let op = f.code[ip]; ip++
                if (op == 7) {
                    let argCount = if (ip < f.code.size) { f.code[ip] } else { 0 }
                    if (argCount - 1 != f.params.size) { return false }
                }
                let n: Int64 = match (op) {
                    case 0 | 7 | 10 | 11 | 24 | 25 => 1
                    case 52 => 3
                    case 56 => 4
                    case _ => 0
                }
                ip = ip + n
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
    // 跨函数路径（J2b：互递归/单程 cross，payload != selfClosureId）：
    //   r9 = [entryTablePtr + payload*8]（仅 generic ABI 登记）；r9==0（未编译/非 generic）→ deopt 冷启动
    //   → callee 自解析全局（rsi 无 FLAG）→ call r11
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
        let jCross = em.jccRel32(0x85)                    // payload != self → 跨函数路径
        // ---- 自调用路径 ----
        if (k != this.fn.params.size) {
            // 自调用 arity 缺参：callee 会读垃圾参数槽（R/v1 资格已拒；generic 需运行时守卫——
            //  曾静默产出 tag=0 垃圾结果，被 unbox 绑定模式掩盖成 Int(0)）
            let id = this.siteSeq
            this.siteSeq = this.siteSeq + 1
            em.movRmImm32(R_R11, id)
            let jArity = em.jmpRel32()
            deoptJumps.add((jArity, id))
        } else {
            this.emitFrameAndCall(k, localSlots, false)
        }
        let jAfter = em.jmpRel32()
        // ---- 跨函数路径（J2b）----
        em.patchRel32(jCross, em.here())
        // rax = callee payload（closureId）；越界（≥入口表容量）→ deopt 防 OOB 读
        em.cmpRmImm32(R_RAX, JIT_ENTRY_CAP)
        let oobPayload = this.slowJae()
        // 入口表仅 generic ABI 函数登记（mmap 固定页，地址稳定）
        em.movRmImm64(R_R10, JitCodegen.entryTablePtr)    // r10 = 表基址
        em.movR9PtrR10X()                                 // r9 = [r10 + rax*8]（callee 入口，0=未编译/非 generic）
        em.cmpRmImm32(R_R9, 0)
        let notCompiled = this.slowJe()                   // r9==0 → deopt（VM 编译 callee 后重入 → 下次直连）
        this.emitFrameAndCall(k, localSlots, true)        // r11 模式：不复制全局槽（callee 自解析）+ call r11
        em.patchRel32(jAfter, em.here())
        // 3b. 下层返回 <1024（deopt/site id 或 0）→ 本帧级联退栈（机器码交叉调用后嵌套 deopt 必检）
        em.cmpRmImm32(R_RAX, 1024)
        let bail = em.jccRel32(0x82)         // jb → bail stub（延迟回填）
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

    // 慢分支：jae（无符号 ≥）→ deopt（OOB 防护等）
    func slowJae(): Int64 {
        let id = this.siteSeq
        this.siteSeq = this.siteSeq + 1
        em.movRmImm32(R_R11, id)
        let p = em.jccRel32(0x83)
        deoptJumps.add((p, id))
        p
    }

    func fixSlow(ps: Array<Int64>): Unit {
        ()   // 已由 slowJcc/slowJe 登记
    }

    // ==================== R/v1 资格补充：返回类型检查 ====================
    // R/v1 是 INT 特化（纯 rax/INT 标签返回）：返回 Boolean/Nil/函数值会被错解为 Int
    // （三模式不一致，例：R 形式 (b n) 返回 (> n 0) 得到 Int(1)，BC/AST 为 Boolean(true)）。
    // 类型跟踪镜像资格扫描的 CFG worklist：任一 RETURN 栈顶为非 Int → true（拒绝，回退 generic）。
    static let T_INT: Int64 = 0
    static let T_BOOL: Int64 = 1
    static let T_NIL: Int64 = 2
    static let T_PHANTOM: Int64 = 3
    static let T_CALL: Int64 = 4
    static let T_MIXED: Int64 = 5

    public static func returnsNonInt(f: CompiledFunction): Bool {
        let k = f.params.size
        let code = f.code
        let labelTypes = HashMap<Int64, ArrayList<Int64>>()
        let pending = ArrayList<(Int64, ArrayList<Int64>)>()
        pending.add((0, ArrayList<Int64>()))
        while (pending.size > 0) {
            let (startIp, startTypes) = pending[pending.size - 1]
            pending.remove((pending.size - 1)..pending.size)
            var ip: Int64 = startIp
            var types = JitCodegen.copyTypes(startTypes)
            var advancing = true
            while (advancing && ip < code.size) {
                let op = code[ip]; ip++
                let n: Int64 = match (op) {
                    case 0 | 7 | 10 | 11 | 24 | 25 => 1
                    case 52 => 3
                    case 56 => 4
                    case _ => 0
                }
                match (op) {
                    case 0 =>
                        let idx = if (ip < code.size) { code[ip] } else { -1 }
                        if (idx >= 0 && idx < f.constants.size) {
                            match (f.constants[idx]) {
                                case Boolean(_) => types.add(T_BOOL)
                                case _ => types.add(T_INT)
                            }
                        } else { types.add(T_INT) }
                    case 1 =>
                        types.add(T_NIL)
                    case 2 | 3 =>
                        types.add(T_BOOL)
                    case 4 =>
                        if (types.size > 0) { types.remove((types.size - 1)..types.size) }
                    case 7 =>
                        let argCount = if (ip < code.size) { code[ip] } else { 0 }
                        for (i in 0..argCount) {
                            if (types.size > 0) { types.remove((types.size - 1)..types.size) }
                        }
                        types.add(T_CALL)
                    case 8 | 9 | 12 =>
                        if (types.size >= 2) {
                            types.remove((types.size - 1)..types.size)
                            types.remove((types.size - 1)..types.size)
                        }
                        types.add(T_INT)
                    case 14 | 15 | 16 =>
                        if (types.size >= 2) {
                            types.remove((types.size - 1)..types.size)
                            types.remove((types.size - 1)..types.size)
                        }
                        types.add(T_BOOL)
                    case 17 =>
                        if (types.size > 0) { types[types.size - 1] = T_BOOL }
                    case 23 =>
                        if (types.size > 0) { types.add(types[types.size - 1]) }
                    case 24 =>
                        let idx = if (ip < code.size) { code[ip] } else { -1 }
                        if (idx < k) { types.add(T_INT) } else { types.add(T_PHANTOM) }
                    case 25 =>
                        if (types.size > 0) { types.remove((types.size - 1)..types.size) }
                    case 10 =>
                        let target = if (ip < code.size) { code[ip] } else { -1 }
                        if (target >= 0) { JitCodegen.mergeTypes(labelTypes, pending, target, types) }
                        advancing = false
                    case 11 =>
                        let target = if (ip < code.size) { code[ip] } else { -1 }
                        if (types.size > 0) { types.remove((types.size - 1)..types.size) }
                        if (target >= 0) { JitCodegen.mergeTypes(labelTypes, pending, target, types) }
                    case 52 =>
                        let kind = if (ip < code.size) { code[ip] } else { 0 }
                        let constIdx = if (ip + 2 < code.size) { code[ip + 2] } else { -1 }
                        var isBool = kind >= 4
                        if (!isBool && constIdx >= 0 && constIdx < f.constants.size) {
                            match (f.constants[constIdx]) {
                                case Boolean(_) => isBool = true
                                case _ => ()
                            }
                        }
                        types.add(if (isBool) { T_BOOL } else { T_INT })
                    case 56 => ()
                    case 43 | 99 =>
                        if (types.size > 0) {
                            let top = types[types.size - 1]
                            if (top == T_BOOL || top == T_NIL || top == T_PHANTOM || top == T_MIXED) { return true }
                        }
                        advancing = false
                    case _ => ()
                }
                ip = ip + n
            }
            if (advancing && types.size > 0) {
                // 代码自然结束 = 隐式 RETURN（VM 语义：栈顶即结果）
                let top = types[types.size - 1]
                if (top == T_BOOL || top == T_NIL || top == T_PHANTOM || top == T_MIXED) { return true }
            }
        }
        false
    }

    static func copyTypes(ts: ArrayList<Int64>): ArrayList<Int64> {
        let c = ArrayList<Int64>()
        for (t in ts) { c.add(t) }
        c
    }

    static func mergeTypes(labelTypes: HashMap<Int64, ArrayList<Int64>>, pending: ArrayList<(Int64, ArrayList<Int64>)>, target: Int64, incoming: ArrayList<Int64>): Unit {
        if (!labelTypes.contains(target)) {
            labelTypes[target] = JitCodegen.copyTypes(incoming)
            pending.add((target, JitCodegen.copyTypes(incoming)))
        } else {
            let existing = labelTypes[target]
            if (existing.size != incoming.size) {
                // 深度冲突（资格扫描已拒；防御性全部置 MIXED → 保守拒绝）
                var mixed = ArrayList<Int64>()
                for (i in 0..incoming.size) { mixed.add(JitCodegen.T_MIXED) }
                labelTypes[target] = mixed
                pending.add((target, JitCodegen.copyTypes(mixed)))
            } else {
                var changed = false
                var merged = ArrayList<Int64>()
                for (i in 0..existing.size) {
                    // CALL = 函数自身返回类型（兼容任意：与对方合并解析为对方）
                    if (existing[i] == incoming[i]) { merged.add(existing[i]) }
                    else if (existing[i] == JitCodegen.T_CALL) { merged.add(incoming[i]) }
                    else if (incoming[i] == JitCodegen.T_CALL) { merged.add(existing[i]) }
                    else { merged.add(JitCodegen.T_MIXED); changed = true }
                }
                if (changed) {
                    labelTypes[target] = merged
                    pending.add((target, JitCodegen.copyTypes(merged)))
                }
            }
        }
    }
}
