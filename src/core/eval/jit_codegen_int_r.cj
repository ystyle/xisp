package ystyle::xisp.core.eval
import ystyle::xisp.types.*
import std.collection.ArrayList
import std.collection.HashMap
import std.env.getVariable

/**
 * JIT 寄存器级整数特化（J4b）：fib 类纯自递归 INT 函数
 *
 * 寄存器布局（全部 callee-saved，递归帧间自动保持）：
 *   r12+[0..k-1]  参数 0..k-1（prologue 从 rdi/rsi/rdx/rcx 复制）
 *   r13, r14, r15 值栈寄存器池（编译器静态分配；深度 ≤3）
 *   自身闭包 id：需要处直接发射 imm32（selfClosureId）
 *   r10           &buf（结果写 [r10+0]=tag, [r10+8]=payload）
 * 入口：rdi = &buf；参数经 rdi/rsi/rdx/rcx 传入（调用点约定）
 * 帧：prologue push r12-r15 + 对齐 pad；epilogue 逆序 pop；16B 对齐
 * 调用：rel32（自递归）；参数从值池/寄存器 mov 到 rdi/rsi...；结果 rax → 值池
 *
 * 资格：常量 Int/Boolean；指令子集；参数 ≤3；全局 ≤1；无 let 槽；值栈深度 ≤3；无除法
 */
public class JitCodegenIntR {
    static let R_RAX: Int64 = 0
    static let R_RCX: Int64 = 1
    static let R_RDX: Int64 = 2
    static let R_RSP: Int64 = 4
    static let R_RDI: Int64 = 7
    static let R_R8: Int64 = 8
    static let R_R9: Int64 = 9
    static let R_R10: Int64 = 10
    static let R_R11: Int64 = 11
    static let R_R12: Int64 = 12
    static let R_R13: Int64 = 13
    static let R_R14: Int64 = 14
    static let R_R15: Int64 = 15

    let em = JitEmitter()
    var patches: ArrayList<(Int64, Int64)> = ArrayList<(Int64, Int64)>()
    var labelAt: HashMap<Int64, Int64> = HashMap<Int64, Int64>()
    var fn: CompiledFunction = CompiledFunction(ArrayList<Int64>(), ArrayList<LispValue>(), ArrayList<CompiledFunction>(), "", Nil, ArrayList<String>(), ArrayList<String>(), ArrayList<Int64>(), 0, ArrayList<ParamInfo>())
    var selfClosureId: Int64 = -1
    var bailJumps: ArrayList<Int64> = ArrayList<Int64>()

    public static func isEligible(f: CompiledFunction): Bool {
        if (f.params.size < 1 || f.params.size > 3) { return false }
        if (f.globalSlots.size > 1 || f.globalRefs.size != f.globalSlots.size) { return false }
        for (name in f.globalRefs) {
            if (name != f.name) { return false }
        }
        for (c in f.constants) {
            match (c) {
                case Int(_) | Boolean(_) => ()
                case _ => return false
            }
        }
        let k = f.params.size
        let code = f.code
        var ip: Int64 = 0
        var depth: Int64 = 0
        var maxDepth: Int64 = 0
        if (getVariable("XISP_JIT_DEBUG") != None) {
            eprintln("[R] scan params=${f.params.size} globals=${f.globalSlots} name=${f.name} refs=${f.globalRefs}")
        }
        while (ip < code.size) {
            let op = code[ip]; ip++
            // 推进指令：n = 参数个数（op 已由 ip++ 消费）
            let n: Int64 = match (op) {
                case 0 | 7 | 10 | 11 | 24 | 25 => 1
                case 52 => 3
                case 56 => 4
                case _ => 0
            }
            // 真实深度模拟
            match (op) {
                case 0 | 1 | 2 | 3 | 14 | 15 | 16 | 17 | 23 | 24 | 52 =>
                    depth = depth + 1
                case 4 | 11 =>
                    depth = depth - 1
                    if (depth < 0) { if (getVariable("XISP_JIT_DEBUG") != None) { eprintln("[R] fail depth op=${op}") }; return false }
                case 8 | 9 | 12 =>
                    depth = depth - 1
                    if (depth < 0) { if (getVariable("XISP_JIT_DEBUG") != None) { eprintln("[R] fail depth op=${op}") }; return false }
                case 7 =>
                    let argCount = if (ip < code.size) { code[ip] } else { 0 }
                    depth = depth - argCount
                    if (depth < 0) { if (getVariable("XISP_JIT_DEBUG") != None) { eprintln("[R] fail call depth=${depth}") }; return false }
                case 10 | 43 | 56 | 99 => ()
                case _ =>
                    if (getVariable("XISP_JIT_DEBUG") != None) { eprintln("[R] fail op ${op}") }
                    return false
            }
            if (getVariable("XISP_JIT_DEBUG") != None) { eprintln("[R] step op=${op} ip=${ip} depth=${depth}") }
            if (depth > maxDepth) { maxDepth = depth }
            if (maxDepth > 3) { if (getVariable("XISP_JIT_DEBUG") != None) { eprintln("[R] fail maxdepth ${maxDepth}") }; return false }
            // 槽检查
            if (op == 24 || op == 25) {
                let idx = code[ip]
                var isGlobal = false
                for (gi in 0..f.globalSlots.size) {
                    if (f.globalSlots[gi] == idx) { isGlobal = true }
                }
                if (idx >= k && !isGlobal) { if (getVariable("XISP_JIT_DEBUG") != None) { eprintln("[R] fail slot idx=${idx}") }; return false }
            }
            if (op == 52 || op == 56) {
                let localIdx = code[ip + 1]
                if (localIdx >= k) { if (getVariable("XISP_JIT_DEBUG") != None) { eprintln("[R] fail localIdx=${localIdx} op=${op}") }; return false }
            }
            if (op == 52 && code[ip] == 3) { if (getVariable("XISP_JIT_DEBUG") != None) { eprintln("[R] fail div") }; return false }
            ip = ip + n
        }
        true
    }

    public func compile(f: CompiledFunction, selfClosureId: Int64): Option<ArrayList<UInt8>> {
        this.fn = f
        this.selfClosureId = selfClosureId
        this.patches = ArrayList<(Int64, Int64)>()
        this.labelAt = HashMap<Int64, Int64>()
        this.bailJumps = ArrayList<Int64>()

        let k = f.params.size

        // ---- prologue ----
        // entry: rdi = &buf；args rdi/rsi/rdx（caller 约定）
        em.pushReg(R_R12); em.pushReg(R_R13); em.pushReg(R_R14); em.pushReg(R_R15)
        em.subRspImm32(8)                          // 对齐：entry rsp%16==8 + 32(push) → 8 → sub 8 → 0
        em.movRegRm(R_R10, R_RDI)                  // &buf
        // 参数 rdi/rsi/rdx → r12/r13/r14 —— 先存 rax 中转（r12 对 rdi 的覆盖安全）
        let srcRegs: Array<Int64> = [R_RDI, 6, R_RDX]   // rdi, rsi, rdx
        for (i in 0..k) {
            em.movRegRm(R_RAX, srcRegs[i])
            em.movRegRm(R_R12 + i, R_RAX)
        }

        // ---- body ----
        let code = f.code
        var ip: Int64 = 0
        var valStack = ArrayList<Int64>()
        while (ip < code.size) {
            this.labelAt[ip] = em.here()
            let op = code[ip]; ip++
            if (getVariable("XISP_JIT_DEBUG") != None) { eprintln("[C] op=${op} ip=${ip}") }
            let r: Option<Int64> = match (op) {
                case 0 => this.emitPushConst(this.argAt(code, ip), ip + 1, valStack)
                case 1 => this.pushVal(0, ip, valStack)
                case 2 => this.pushVal(1, ip, valStack)
                case 3 => this.pushVal(0, ip, valStack)
                case 4 =>
                    this.popV(valStack)
                    Some(ip)
                case 7 => this.emitCall(this.argAt(code, ip), ip + 1, valStack)
                case 8 => this.emitBinStack(0, ip, valStack)
                case 9 => this.emitBinStack(1, ip, valStack)
                case 12 => this.emitBinStack(2, ip, valStack)
                case 14 => this.emitCmpStack(0x8C, ip, valStack)
                case 15 => this.emitCmpStack(0x8F, ip, valStack)
                case 16 => this.emitCmpStack(0x84, ip, valStack)
                case 17 => this.emitNot(ip, valStack)
                case 10 => this.emitJump(this.argAt(code, ip), ip + 1)
                case 11 => this.emitJmpIfFalse(this.argAt(code, ip), ip + 1, valStack)
                case 23 => this.emitDup(ip, valStack)
                case 24 => this.emitLoadLocal(this.argAt(code, ip), ip + 1, valStack)
                case 25 => this.emitStoreLocal(this.argAt(code, ip), ip + 1, valStack)
                case 43 => this.emitReturn(ip, valStack)
                case 52 => this.emitBinLC(this.argAt(code, ip), this.argAt(code, ip + 1), this.argAt(code, ip + 2), ip + 3, valStack)
                case 56 => this.emitCmpLCJF(this.argAt(code, ip), this.argAt(code, ip + 1), this.argAt(code, ip + 2), this.argAt(code, ip + 3), ip + 4)
                case 99 => this.emitReturn(ip, valStack)
                case _ => return None
            }
            match (r) {
                case Some(nextIp) => ip = nextIp
                case None => return None
            }
        }
        this.labelAt[ip] = em.here()

        match (this.emitReturn(ip, valStack)) {
            case Some(_) => ()
            case None => return None
        }

        // ---- 回填 ----
        for ((at, targetIp) in patches) {
            let t = if (targetIp == -1) { 0 } else if (labelAt.contains(targetIp)) { labelAt[targetIp] } else { 0 }
            em.patchRel32(at, t)
        }

        // ---- bail 桩（非自调用 → rax=0 级联）----
        if (bailJumps.size > 0) {
            let stub = em.here()
            for (p in bailJumps) { em.patchRel32(p, stub) }
            em.movRmImm32(R_RAX, 0)
            em.addRspImm32(8)
            em.popReg(R_R15); em.popReg(R_R14); em.popReg(R_R13); em.popReg(R_R12)
            em.ret()
        }

        Some(em.bytes())
    }

    func popV(valStack: ArrayList<Int64>): Unit {
        let _ = this.popV(valStack)
        ()
    }

    func argAt(code: ArrayList<Int64>, i: Int64): Int64 {
        if (i < code.size) { code[i] } else { -1 }
    }

    // ============ 模板 ============

    func pushVal(v: Int64, nextIp: Int64, valStack: ArrayList<Int64>): Option<Int64> {
        let reg = this.allocVal(valStack)
        if (reg < 0) { return None }
        em.movRmImm32(reg, v)
        valStack.add(reg)
        Some(nextIp)
    }

    func allocVal(valStack: ArrayList<Int64>): Int64 {
        for (p in [R_R13, R_R14, R_R15]) {
            if (!valStack.contains(p)) {
                return p
            }
        }
        -1
    }

    func emitPushConst(idx: Int64, nextIp: Int64, valStack: ArrayList<Int64>): Option<Int64> {
        let c = this.fn.constants[idx]
        match (c) {
            case Int(v) => this.pushVal(v, nextIp, valStack)
            case Boolean(v) => this.pushVal(if (v) { 1 } else { 0 }, nextIp, valStack)
            case _ => None
        }
    }

    func emitJump(target: Int64, nextIp: Int64): Option<Int64> {
        let at = em.jmpRel32()
        patches.add((at, target))
        Some(nextIp)
    }

    func emitJmpIfFalse(target: Int64, nextIp: Int64, valStack: ArrayList<Int64>): Option<Int64> {
        let top = valStack[valStack.size - 1]
        em.movRmImm32(R_RCX, 0)
        em.cmpRmReg(top, R_RCX)
        let jFalse = em.jccRel32(0x84)
        this.popV(valStack)
        let jCont = em.jmpRel32()
        em.patchRel32(jFalse, em.here())
        let jTarget = em.jmpRel32()
        patches.add((jTarget, target))
        em.patchRel32(jCont, em.here())
        Some(nextIp)
    }

    func emitLoadLocal(idx: Int64, nextIp: Int64, valStack: ArrayList<Int64>): Option<Int64> {
        if (idx < this.fn.params.size) {
            valStack.add(R_R12 + idx)
            return Some(nextIp)
        }
        // 全局（自身闭包 id）→ 直接发射 imm
        if (idx < this.fn.params.size + this.fn.globalSlots.size) {
            let reg = this.allocVal(valStack)
            if (reg < 0) { return None }
            em.movRmImm32(reg, this.selfClosureId)
            valStack.add(reg)
            return Some(nextIp)
        }
        None
    }

    func emitStoreLocal(idx: Int64, nextIp: Int64, valStack: ArrayList<Int64>): Option<Int64> {
        let top = valStack[valStack.size - 1]
        if (idx < this.fn.params.size) {
            em.movRegRm(R_R12 + idx, top)
            this.popV(valStack)
            return Some(nextIp)
        }
        None
    }

    func emitDup(nextIp: Int64, valStack: ArrayList<Int64>): Option<Int64> {
        let reg = this.allocVal(valStack)
        if (reg < 0) { return None }
        em.movRegRm(reg, valStack[valStack.size - 1])
        valStack.add(reg)
        Some(nextIp)
    }

    func emitNot(nextIp: Int64, valStack: ArrayList<Int64>): Option<Int64> {
        let top = valStack[valStack.size - 1]
        em.movRmImm32(R_RCX, 0)
        em.cmpRmReg(top, R_RCX)
        let jne = em.jccRel32(0x85)
        em.movRmImm32(top, 1)
        let jd = em.jmpRel32()
        em.patchRel32(jne, em.here())
        em.movRmImm32(top, 0)
        em.patchRel32(jd, em.here())
        Some(nextIp)
    }

    func emitBinStack(kind: Int64, nextIp: Int64, valStack: ArrayList<Int64>): Option<Int64> {
        if (valStack.size < 2) { return None }
        let a = valStack[valStack.size - 2]
        let b = valStack[valStack.size - 1]
        em.movRegRm(R_RAX, a)
        em.movRegRm(R_RCX, b)
        match (kind) {
            case 0 => em.addRmReg(R_RAX, R_RCX)
            case 1 => em.subRmReg(R_RAX, R_RCX)
            case 2 => em.b(0x48); em.b(0x0F); em.b(0xAF); em.b(0xC1)
            case _ => return None
        }
        this.popV(valStack)
        em.movRegRm(a, R_RAX)
        Some(nextIp)
    }

    func emitCmpStack(jccSuffix: Int64, nextIp: Int64, valStack: ArrayList<Int64>): Option<Int64> {
        if (valStack.size < 2) { return None }
        let a = valStack[valStack.size - 2]
        let b = valStack[valStack.size - 1]
        em.movRegRm(R_RAX, a)
        em.movRegRm(R_RCX, b)
        em.cmpRmReg(R_RAX, R_RCX)
        let jtrue = em.jccRel32(jccSuffix)
        em.movRmImm32(R_RAX, 0)
        let jd = em.jmpRel32()
        em.patchRel32(jtrue, em.here())
        em.movRmImm32(R_RAX, 1)
        em.patchRel32(jd, em.here())
        this.popV(valStack)
        this.popV(valStack)
        em.movRegRm(a, R_RAX)
        valStack.add(a)
        Some(nextIp)
    }

    func emitBinLC(kind: Int64, localIdx: Int64, constIdx: Int64, nextIp: Int64, valStack: ArrayList<Int64>): Option<Int64> {
        let c = this.fn.constants[constIdx]
        let lreg = this.localReg(localIdx)
        if (lreg < 0) { return None }
        match (c) {
            case Int(bv) =>
                if (kind == 7) {
                    em.movRmImm32(R_RCX, 0)
                    em.cmpRmReg(lreg, R_RCX)
                    let jne = em.jccRel32(0x85)
                    let reg = this.allocVal(valStack)
                    if (reg < 0) { return None }
                    em.movRmImm32(reg, 1)
                    let jd = em.jmpRel32()
                    em.patchRel32(jne, em.here())
                    em.movRmImm32(reg, 0)
                    em.patchRel32(jd, em.here())
                    valStack.add(reg)
                    return Some(nextIp)
                }
                em.movRmImm32(R_RCX, bv)
                let reg = this.allocVal(valStack)
                if (reg < 0) { return None }
                em.movRegRm(reg, lreg)          // 复制参数到值寄存器（不得就地修改参数寄存器！）
                match (kind) {
                    case 0 => em.addRmReg(reg, R_RCX)
                    case 1 => em.subRmReg(reg, R_RCX)
                    case 2 => em.b(0x48); em.b(0x0F); em.b(0xAF); em.b(0xC1)
                    case 3 => return None
                    case 4 => return this.cmpLocalJcc(0x8C, reg, nextIp, valStack)
                    case 5 => return this.cmpLocalJcc(0x8F, reg, nextIp, valStack)
                    case 6 => return this.cmpLocalJcc(0x84, reg, nextIp, valStack)
                    case _ => return None
                }
                valStack.add(reg)
                return Some(nextIp)
            case Boolean(bv) =>
                em.movRmImm32(R_RCX, if (bv) { 1 } else { 0 })
                em.cmpRmReg(lreg, R_RCX)
                return this.cmpLocalJcc(0x84, lreg, nextIp, valStack)
            case _ => None
        }
    }

    func cmpLocalJcc(jccSuffix: Int64, lreg: Int64, nextIp: Int64, valStack: ArrayList<Int64>): Option<Int64> {
        let jtrue = em.jccRel32(jccSuffix)
        let reg = this.allocVal(valStack)
        if (reg < 0) { return None }
        em.movRmImm32(reg, 0)
        let jd = em.jmpRel32()
        em.patchRel32(jtrue, em.here())
        em.movRmImm32(reg, 1)
        em.patchRel32(jd, em.here())
        valStack.add(reg)
        Some(nextIp)
    }

    func localReg(idx: Int64): Int64 {
        if (idx < this.fn.params.size) {
            return R_R12 + idx
        }
        -1
    }

    func emitCmpLCJF(cmpKind: Int64, localIdx: Int64, constIdx: Int64, target: Int64, nextIp: Int64): Option<Int64> {
        let c = this.fn.constants[constIdx]
        let lreg = this.localReg(localIdx)
        if (lreg < 0) { return None }
        match (c) {
            case Int(bv) =>
                em.movRmImm32(R_RCX, bv)
                em.cmpRmReg(lreg, R_RCX)
                let at = match (cmpKind) {
                    case 0 => em.jccRel32(0x8D)
                    case 1 => em.jccRel32(0x8E)
                    case _ => em.jccRel32(0x85)
                }
                patches.add((at, target))
                return Some(nextIp)
            case Boolean(bv) =>
                em.movRmImm32(R_RCX, if (bv) { 1 } else { 0 })
                em.cmpRmReg(lreg, R_RCX)
                let at2 = em.jccRel32(0x85)
                patches.add((at2, target))
                return Some(nextIp)
            case _ => None
        }
    }

    func emitCall(argCount: Int64, nextIp: Int64, valStack: ArrayList<Int64>): Option<Int64> {
        if (getVariable("XISP_JIT_DEBUG") != None) { eprintln("[CALL] n=${argCount} vs=${valStack.size}") }
        if (argCount < 2) { return None }
        let n = argCount
        let k = n - 1
        if (k > 3 || valStack.size < n) { return None }
        let funcReg = valStack[valStack.size - n]
        em.movRmImm32(R_RCX, this.selfClosureId)
        em.cmpRmReg(funcReg, R_RCX)
        let bail = em.jccRel32(0x85)
        bailJumps.add(bail)
        // 参数 → rdi/rsi/rdx（arg0 → rdi）
        let argRegs: Array<Int64> = [R_RDI, 6, R_RDX]
        for (j in 0..k) {
            let reg = valStack[valStack.size - n + 1 + j]
            em.movRegRm(argRegs[j], reg)
        }
        // 弹 n 项；结果 rax → 值池
        for (i in 0..n) {
            this.popV(valStack)
        }
        let callAt = em.callRel32()
        patches.add((callAt, -1))
        if (getVariable("XISP_JIT_DEBUG") != None) { eprintln("[CALL] after call ${callAt} vs=${valStack.size}") }
        let resReg = this.allocVal(valStack)
        if (getVariable("XISP_JIT_DEBUG") != None) { eprintln("[CALL] resReg=${resReg}") }
        if (resReg < 0) { return None }
        em.movRegRm(resReg, R_RAX)
        valStack.add(resReg)
        Some(nextIp)
    }

    func emitReturn(nextIp: Int64, valStack: ArrayList<Int64>): Option<Int64> {
        if (valStack.size > 0) {
            em.movRegRm(R_RAX, valStack[valStack.size - 1])
        }
        // 写 [r10+0]=tag(INT), [r10+8]=payload
        em.movRmDispReg(R_R10, 0, R_RAX)
        em.movRmImm32(R_RCX, JIT_TAG_INT)
        em.movRmDispReg(R_R10, 0, R_RCX)
        em.movRmDispReg(R_R10, 8, R_RAX)
        em.addRspImm32(8)
        em.popReg(R_R15); em.popReg(R_R14); em.popReg(R_R13); em.popReg(R_R12)
        em.ret()
        Some(nextIp)
    }
}
