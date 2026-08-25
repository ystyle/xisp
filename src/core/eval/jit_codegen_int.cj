package ystyle::xisp.core.eval
import ystyle::xisp.types.*
import std.collection.ArrayList
import std.collection.HashMap

/**
 * JIT 整数特化（J4）：全 INT/BOOL 函数的 8B 槽 + 零 tag 检查形式
 *
 * 与通用 JitCodegen 的差异：
 * - 槽 i 8B @ [rbp - 8*(i+1)]（payload only；tag 静态恒定 INT/BOOL，桥写 16B 槽、此处读 payload）
 * - JIT 栈项 8B（sub rsp,8 / add rsp,8）
 * - 无 tag 检查（类型经资格判定保证）；仅自递归调用（非自 → 尾桩 rax=0 级联回退，同通用 deopt 语义）
 * - 调用帧缓冲仍 16B（与桥/通用 ABI 一致）：callee 读 [rdi+16i+8]（payload）；
 *   结果仍写 16B boxed [argBase-16]（tag=INT），调用方读 payload [..+8]
 * - 全局槽：递归 frame 由调用方复制 payload（REF id）；首帧由桥填充（写 16B）+ 复制 payload
 */
public class JitCodegenInt {
    static let R_RAX: Int64 = 0
    static let R_RCX: Int64 = 1
    static let R_RDX: Int64 = 2
    static let R_RSP: Int64 = 4
    static let R_RBP: Int64 = 5
    static let R_RSI: Int64 = 6
    static let R_RDI: Int64 = 7
    static let R_R10: Int64 = 10

    let em = JitEmitter()
    var patches: ArrayList<(Int64, Int64)> = ArrayList<(Int64, Int64)>()
    var labelAt: HashMap<Int64, Int64> = HashMap<Int64, Int64>()
    var fn: CompiledFunction = CompiledFunction(ArrayList<Int64>(), ArrayList<LispValue>(), ArrayList<CompiledFunction>(), "", Nil, ArrayList<String>(), ArrayList<String>(), ArrayList<Int64>(), 0, ArrayList<ParamInfo>())
    var selfClosureId: Int64 = -1
    // 慢分支（非自调用）→ 尾桩（恢复帧 + rax=0 + ret；级联回退）
    var slowJumps: ArrayList<Int64> = ArrayList<Int64>()
    // 调用后结果为空检查 → 尾桩
    var bailJumps: ArrayList<Int64> = ArrayList<Int64>()

    public static func isEligible(f: CompiledFunction): Bool {
        if (f.localSlots > 64) { return false }   // 大帧栈溢出防护
        // 仅自递归：所有全局引用必须是本函数自身（INT 特化不能处理非自调用——慢桩会返回 0 破坏语义）
        for (name in f.globalRefs) {
            if (name != f.name) { return false }
        }
        for (c in f.constants) {
            match (c) {
                case Int(_) | Boolean(_) => ()
                case _ => return false
            }
        }
        let code = f.code
        var ip: Int64 = 0
        while (ip < code.size) {
            let op = code[ip]; ip++
            let n: Int64 = match (op) {
                case 0 | 5 | 6 | 7 | 10 | 11 | 24 | 25 | 30 | 42 | 48 | 51 => 1
                case 38 | 59 => 2
                case 52 => 3
                case 56 => 4
                case _ => 0
            }
            let supported = match (op) {
                case 0 | 1 | 2 | 3 | 4 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 23 | 24 | 25 | 43 | 52 | 56 | 99 => true
                case _ => false
            }
            if (!supported) { return false }
            // 除法（kind 3）排除：除零语义无法在无 deopt 的 INT 形式表达
            if (op == 52 && code[ip + 2] == 3) { return false }
            if (op == 13) { return false }
            // 自调用必须传递全部参数（缺参槽 → callee 读垃圾寄存器/槽）
            if (op == 7 && code[ip] - 1 != f.params.size) { return false }
            ip = ip + n
        }
        true
    }

    public func compile(f: CompiledFunction, selfClosureId: Int64): Option<ArrayList<UInt8>> {
        this.fn = f
        this.selfClosureId = selfClosureId
        this.patches = ArrayList<(Int64, Int64)>()
        this.labelAt = HashMap<Int64, Int64>()
        this.slowJumps = ArrayList<Int64>()
        this.bailJumps = ArrayList<Int64>()

        let localSlots = f.localSlots
        // 16B 对齐（调用点要求）：8B 槽帧向上取整到 16 倍数
        let frameSize = ((localSlots + 3) * 8 + 15) / 16 * 16

        // ---- prologue ----
        em.pushRbp()
        em.movRbpRsp()
        em.subRspImm32(frameSize)
        let k = f.params.size
        for (i in 0..k) {
            em.movRegRmDisp(R_RAX, R_RDI, 16 * i + 8)     // payload @ [rdi + 16i + 8]
            em.movRmDispReg(R_RBP, -8 * (i + 1), R_RAX)
        }
        for (gi in 0..f.globalSlots.size) {
            let gs = f.globalSlots[gi]
            if (gs >= k && gs < localSlots) {
                em.movRegRmDisp(R_RAX, R_RDI, 16 * gs + 8)
                em.movRmDispReg(R_RBP, -8 * (gs + 1), R_RAX)
            }
        }
        em.movRmDispReg(R_RBP, -8 * (localSlots + 1), R_RDX)
        em.movRmDispReg(R_RBP, -8 * (localSlots + 2), R_RDI)
        em.movRmDispReg(R_RBP, -8 * (localSlots + 3), R_RSI)

        // ---- body ----
        let code = f.code
        var ip: Int64 = 0
        while (ip < code.size) {
            this.labelAt[ip] = em.here()
            let op = code[ip]; ip++
            let r: Option<Int64> = match (op) {
                case 0 => this.emitPushConst(this.argAt(code, ip), ip + 1)
                case 1 => this.emitPushImmVal(0, ip)
                case 2 => this.emitPushImmVal(1, ip)
                case 3 => this.emitPushImmVal(0, ip)
                case 4 => this.emitPop(ip)
                case 7 => this.emitCall(this.argAt(code, ip), ip + 1)
                case 8 => this.emitBinStack(0, ip)
                case 9 => this.emitBinStack(1, ip)
                case 12 => this.emitBinStack(2, ip)
                case 13 => return None   // 排除
                case 14 => this.emitCmpStack(0x8C, ip)
                case 15 => this.emitCmpStack(0x8F, ip)
                case 16 => this.emitCmpStack(0x84, ip)
                case 17 => this.emitNot(ip)
                case 10 => this.emitJump(this.argAt(code, ip), ip + 1)
                case 11 => this.emitJmpIfFalse(this.argAt(code, ip), ip + 1)
                case 23 => this.emitDup(ip)
                case 24 => this.emitLoadLocal(this.argAt(code, ip), ip + 1)
                case 25 => this.emitStoreLocal(this.argAt(code, ip), ip + 1)
                case 43 => this.emitReturn(ip)
                case 52 => this.emitBinLC(this.argAt(code, ip), this.argAt(code, ip + 1), this.argAt(code, ip + 2), ip + 3)
                case 56 => this.emitCmpLCJF(this.argAt(code, ip), this.argAt(code, ip + 1), this.argAt(code, ip + 2), this.argAt(code, ip + 3), ip + 4)
                case 99 => this.emitReturn(ip)
                case _ => return None
            }
            match (r) {
                case Some(nextIp) => ip = nextIp
                case None => return None
            }
        }
        this.labelAt[ip] = em.here()

        // ---- 函数体结束 epilogue（VM 语义：无 RETURN 时栈顶即结果）----
        match (this.emitReturn(ip)) {
            case Some(_) => ()
            case None => return None
        }

        // ---- 回填 ----
        for ((at, targetIp) in patches) {
            let t = if (targetIp == -1) { 0 } else if (labelAt.contains(targetIp)) { labelAt[targetIp] } else { 0 }
            em.patchRel32(at, t)
        }

        // ---- 尾桩：慢/回退 ----
        // 慢分发（非自）：rax 保留（= func id，供诊断）→ 恢复帧 → ret
        if (slowJumps.size > 0) {
            let stub = em.here()
            for (p in slowJumps) { em.patchRel32(p, stub) }
            em.movRspRbp()
            em.popRbp()
            em.ret()
        }
        // 级联（下层回退）：rax=0 → 恢复帧 → ret
        if (bailJumps.size > 0) {
            let stub2 = em.here()
            for (p in bailJumps) { em.patchRel32(p, stub2) }
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

    // ============ 指令模板 ============

    func emitPushImmVal(v: Int64, nextIp: Int64): Option<Int64> {
        em.subRspImm32(8)
        em.movRmImm32(R_RAX, v)
        em.movRmDispReg(R_RSP, 0, R_RAX)
        Some(nextIp)
    }

    func emitPushConst(idx: Int64, nextIp: Int64): Option<Int64> {
        let c = this.fn.constants[idx]
        match (c) {
            case Int(v) => this.emitPushImmVal(v, nextIp)
            case Boolean(v) => this.emitPushImmVal(if (v) { 1 } else { 0 }, nextIp)
            case _ => None
        }
    }

    func emitPop(nextIp: Int64): Option<Int64> {
        em.addRspImm32(8)
        Some(nextIp)
    }

    func emitJump(target: Int64, nextIp: Int64): Option<Int64> {
        let at = em.jmpRel32()
        patches.add((at, target))
        Some(nextIp)
    }

    func emitJmpIfFalse(target: Int64, nextIp: Int64): Option<Int64> {
        em.movRegRmDisp(R_RAX, R_RSP, 0)
        em.movRmImm32(R_RCX, 0)
        em.cmpRmReg(R_RAX, R_RCX)
        let jFalse = em.jccRel32(0x84)
        em.addRspImm32(8)
        let jCont = em.jmpRel32()
        em.patchRel32(jFalse, em.here())
        em.addRspImm32(8)
        let jTarget = em.jmpRel32()
        patches.add((jTarget, target))
        em.patchRel32(jCont, em.here())
        Some(nextIp)
    }

    func emitLoadLocal(idx: Int64, nextIp: Int64): Option<Int64> {
        em.subRspImm32(8)
        em.movRegRmDisp(R_RAX, R_RBP, -8 * (idx + 1))
        em.movRmDispReg(R_RSP, 0, R_RAX)
        Some(nextIp)
    }

    func emitStoreLocal(idx: Int64, nextIp: Int64): Option<Int64> {
        em.movRegRmDisp(R_RAX, R_RSP, 0)
        em.movRmDispReg(R_RBP, -8 * (idx + 1), R_RAX)
        em.addRspImm32(8)
        Some(nextIp)
    }

    func emitDup(nextIp: Int64): Option<Int64> {
        em.subRspImm32(8)
        em.movRegRmDisp(R_RAX, R_RSP, 8)
        em.movRmDispReg(R_RSP, 0, R_RAX)
        Some(nextIp)
    }

    func emitNot(nextIp: Int64): Option<Int64> {
        em.movRegRmDisp(R_RAX, R_RSP, 0)
        em.movRmImm32(R_RCX, 0)
        em.cmpRmReg(R_RAX, R_RCX)
        let jne = em.jccRel32(0x85)
        em.movRmImm32(R_RAX, 1)
        let jd = em.jmpRel32()
        em.patchRel32(jne, em.here())
        em.movRmImm32(R_RAX, 0)
        em.patchRel32(jd, em.here())
        em.movRmDispReg(R_RSP, 0, R_RAX)
        Some(nextIp)
    }

    func emitBinStack(kind: Int64, nextIp: Int64): Option<Int64> {
        // 栈顶 8B：b @ [rsp+0]，a @ [rsp+8]
        em.movRegRmDisp(R_RAX, R_RSP, 8)
        em.movRegRmDisp(R_RCX, R_RSP, 0)
        match (kind) {
            case 0 => em.addRmReg(R_RAX, R_RCX)
            case 1 => em.subRmReg(R_RAX, R_RCX)
            case 2 => em.b(0x48); em.b(0x0F); em.b(0xAF); em.b(0xC1)
            case _ => return None
        }
        em.movRmDispReg(R_RSP, 8, R_RAX)
        em.addRspImm32(8)
        Some(nextIp)
    }

    func emitCmpStack(jccSuffix: Int64, nextIp: Int64): Option<Int64> {
        // b @ [rsp+0]，a @ [rsp+8]：cmp a, b；jcc → 1 else 0（结果写 [rsp+8]，弹 b）
        em.movRegRmDisp(R_RAX, R_RSP, 8)
        em.movRegRmDisp(R_RCX, R_RSP, 0)
        em.cmpRmReg(R_RAX, R_RCX)
        this.emitCmpTail(jccSuffix)
        em.movRmDispReg(R_RSP, 8, R_RAX)
        em.addRspImm32(8)
        Some(nextIp)
    }

    // cmp 后：jcc → rax=1 else 0
    func emitCmpTail(jccSuffix: Int64): Unit {
        let jtrue = em.jccRel32(jccSuffix)
        em.movRmImm32(R_RAX, 0)
        let jd = em.jmpRel32()
        em.patchRel32(jtrue, em.here())
        em.movRmImm32(R_RAX, 1)
        em.patchRel32(jd, em.here())
    }

    func emitBinLC(kind: Int64, localIdx: Int64, constIdx: Int64, nextIp: Int64): Option<Int64> {
        let c = this.fn.constants[constIdx]
        match (c) {
            case Int(bv) =>
                if (kind == 7) {
                    em.movRegRmDisp(R_RAX, R_RBP, -8 * (localIdx + 1))
                    em.movRmImm32(R_RCX, 0)
                    em.cmpRmReg(R_RAX, R_RCX)
                    let jne = em.jccRel32(0x85)
                    em.movRmImm32(R_RAX, 1)
                    let jd = em.jmpRel32()
                    em.patchRel32(jne, em.here())
                    em.movRmImm32(R_RAX, 0)
                    em.patchRel32(jd, em.here())
                    this.pushRax()
                    return Some(nextIp)
                }
                em.movRegRmDisp(R_RAX, R_RBP, -8 * (localIdx + 1))
                em.movRmImm32(R_RCX, bv)
                match (kind) {
                    case 0 => em.addRmReg(R_RAX, R_RCX)
                    case 1 => em.subRmReg(R_RAX, R_RCX)
                    case 2 => em.b(0x48); em.b(0x0F); em.b(0xAF); em.b(0xC1)
                    case 3 => return None
                    case 4 =>
                        em.cmpRmReg(R_RAX, R_RCX)
                        this.emitCmpTail(0x8C)
                        this.pushRax()
                        return Some(nextIp)
                    case 5 =>
                        em.cmpRmReg(R_RAX, R_RCX)
                        this.emitCmpTail(0x8F)
                        this.pushRax()
                        return Some(nextIp)
                    case 6 =>
                        em.cmpRmReg(R_RAX, R_RCX)
                        this.emitCmpTail(0x84)
                        this.pushRax()
                        return Some(nextIp)
                    case _ => return None
                }
                this.pushRax()
                return Some(nextIp)
            case Boolean(bv) =>
                em.movRmImm32(R_RCX, if (bv) { 1 } else { 0 })
                em.movRegRmDisp(R_RAX, R_RBP, -8 * (localIdx + 1))
                em.cmpRmReg(R_RAX, R_RCX)
                this.emitCmpTail(0x84)
                this.pushRax()
                return Some(nextIp)
            case _ => None
        }
    }

    func pushRax(): Unit {
        em.subRspImm32(8)
        em.movRmDispReg(R_RSP, 0, R_RAX)
    }

    func emitCmpLCJF(cmpKind: Int64, localIdx: Int64, constIdx: Int64, target: Int64, nextIp: Int64): Option<Int64> {
        let c = this.fn.constants[constIdx]
        match (c) {
            case Int(bv) =>
                em.movRegRmDisp(R_RAX, R_RBP, -8 * (localIdx + 1))
                em.movRmImm32(R_RCX, bv)
                em.cmpRmReg(R_RAX, R_RCX)
                let at = match (cmpKind) {
                    case 0 => em.jccRel32(0x8D)
                    case 1 => em.jccRel32(0x8E)
                    case _ => em.jccRel32(0x85)
                }
                patches.add((at, target))
                return Some(nextIp)
            case Boolean(bv) =>
                em.movRegRmDisp(R_RAX, R_RBP, -8 * (localIdx + 1))
                em.movRmImm32(R_RCX, if (bv) { 1 } else { 0 })
                em.cmpRmReg(R_RAX, R_RCX)
                let at2 = em.jccRel32(0x85)
                patches.add((at2, target))
                return Some(nextIp)
            case _ => None
        }
    }

    func emitCall(argCount: Int64, nextIp: Int64): Option<Int64> {
        if (argCount < 2) { return None }
        let n = argCount
        let k = n - 1
        let localSlots = this.fn.localSlots
        // JIT 栈 8B：func id @ [rsp + 8*(n-1)]；arg_j @ [rsp + 8*(k-1-j)]
        em.movRegRmDisp(R_RAX, R_RSP, 8 * (n - 1))
        em.movRmImm32(R_RCX, this.selfClosureId)
        em.cmpRmReg(R_RAX, R_RCX)
        let slowId = em.jccRel32(0x85)
        slowJumps.add(slowId)
        // 帧缓冲（16B stride）：result@[newRsp+0]；slot_j payload@[newRsp+16(j+1)+8]
        // FB = round16(16*(k+1) + 8n)：覆盖帧 + 栈项，保持调用点 16B 对齐
        let fb = ((16 * (k + 1) + 8 * n + 15) / 16) * 16
        em.subRspImm32(fb)
        for (j in 0..k) {
            let srcOff = fb + 8 * (k - 1 - j)
            em.movRegRmDisp(R_RAX, R_RSP, srcOff)
            em.movRmDispReg(R_RSP, 16 * (j + 1) + 8, R_RAX)
        }
        for (gi in 0..this.fn.globalSlots.size) {
            let gs = this.fn.globalSlots[gi]
            if (gs >= k && gs < localSlots) {
                em.movRegRmDisp(R_RAX, R_RBP, -8 * (gs + 1))
                em.movRmDispReg(R_RSP, 16 * (gs + 1) + 8, R_RAX)
            }
        }
        em.leaRegRmDisp(R_RDI, R_RSP, 16)
        em.movRmImm32(R_RSI, k)
        em.movRegRmDisp(R_RDX, R_RBP, -8 * (localSlots + 1))
        let callAt = em.callRel32()
        patches.add((callAt, -1))
        // 下层回退 → 级联
        em.b(0x48); em.b(0x85); em.b(0xC0)   // test rax, rax
        let bail = em.jccRel32(0x84)
        bailJumps.add(bail)
        // 结果（16B boxed @ [newRsp+0]）：payload @ [newRsp+8]
        em.movRegRmDisp(R_RAX, R_RSP, 8)
        em.addRspImm32(fb + 8 * n)
        em.subRspImm32(8)
        em.movRmDispReg(R_RSP, 0, R_RAX)
        Some(nextIp)
    }

    func emitReturn(nextIp: Int64): Option<Int64> {
        let localSlots = this.fn.localSlots
        em.movRegRmDisp(R_RAX, R_RSP, 0)
        em.movRegRmDisp(R_RDX, R_RBP, -8 * (localSlots + 2))
        em.movRmImm32(R_RCX, JIT_TAG_INT)
        em.movRmDispReg(R_RDX, -16, R_RCX)
        em.movRmDispReg(R_RDX, -8, R_RAX)
        em.leaRegRmDisp(R_RAX, R_RDX, -16)
        em.movRspRbp()
        em.popRbp()
        em.ret()
        Some(nextIp)
    }
}
