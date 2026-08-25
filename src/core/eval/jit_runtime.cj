package ystyle::xisp.core.eval
import ystyle::xisp.types.*
import std.collection.ArrayList
import std.env.getVariable
import std.collection.HashMap

// JIT 值 tag（16B 槽：tag @ [0]，payload @ [8]）
let JIT_TAG_INT: Int64 = 1
let JIT_TAG_FLOAT: Int64 = 2
let JIT_TAG_BOOL: Int64 = 3
let JIT_TAG_NIL: Int64 = 4
let JIT_TAG_STR: Int64 = 5
let JIT_TAG_SYM: Int64 = 6
let JIT_TAG_REF: Int64 = 7

foreign func mmap(addr: CPointer<Unit>, length: UIntNative, prot: Int32, flags: Int32, fd: Int32, offset: Int64): CPointer<Unit>
foreign func munmap(addr: CPointer<Unit>, length: UIntNative): Int32

// J2：JIT 调用上下文（单线程解释器）
var jitCurEnv: Environment = Environment()
// 入口表：closureId → 机器码入口地址（0=未编译）；固定容量保证 raw 指针稳定
let JIT_ENTRY_CAP: Int64 = 1024
var jitEntryTable = Array<Int64>(JIT_ENTRY_CAP) { _ => 0 }
// JIT 调试标志（一次读取；热路径避免重复 getVariable）
var jitDebug: Bool = false

/**
 * J2：callee 全局引用解析（@C，被 callee prologue 绝对地址调用）
 * 参数：rdi=closureId, rsi=framePtr(callee rbp), rdx=paramCount
 * 语义与 switchFrame 一致：实时 env.lookup；宏保留名字字符串。
 */
@C
func jitResolveGlobals(closureId: Int64, framePtr: CPointer<Unit>, paramCount: Int64): Unit {
    if (closureId >= 0 && closureId < BytecodeVM.functionTable.size) {
        let f = BytecodeVM.functionTable[closureId]
        let p8 = CPointer<UInt8>(framePtr)
        for (i in 0..f.globalRefs.size) {
            let slot = if (i < f.globalSlots.size) { f.globalSlots[i] } else { f.params.size + i }
            let val = jitCurEnv.lookup(f.globalRefs[i])
            let slotVal = match (val) {
                case Macro(_, _, _) | MacroFromParams(_, _, _) => Str(f.globalRefs[i])
                case _ => val
            }
            let (tag, payload) = jitBox(slotVal)
            let off = -16 * (slot + 1)
            unsafe {
                CPointer<Int64>(p8 + off).write(tag)
                CPointer<Int64>(p8 + off + 8).write(payload)
            }
        }
    }
}

func jitBox(v: LispValue): (Int64, Int64) {
    match (v) {
        case Int(x) => (JIT_TAG_INT, x)
        case Float(x) => (JIT_TAG_FLOAT, floatBits(x))
        case Boolean(x) => (JIT_TAG_BOOL, if (x) { 1 } else { 0 })
        case Nil => (JIT_TAG_NIL, 0)
        case Str(s) => (JIT_TAG_STR, 0)
        case Symbol(s) => (JIT_TAG_SYM, 0)
        case Closure(id, _) => (JIT_TAG_REF, id)
        case _ => (JIT_TAG_REF, -1)
    }
}

/**
 * JIT 运行时：页管理、函数登记、解释器↔JIT 桥
 */
public class JitRuntime {
    // globalClosureId(functionTable id) → 机器码入口
    var entries: HashMap<Int64, CPointer<Unit>> = HashMap<Int64, CPointer<Unit>>()
    // 寄存器级形式（R-ABI：rdi/rsi/rdx=裸参数，rcx=结果槽）
    var rForms: HashMap<Int64, Bool> = HashMap<Int64, Bool>()
    static var shared: Option<JitRuntime> = None

    public init() {
        JitRuntime.shared = Some(this)
        let dbg = getVariable("XISP_JIT_DEBUG")
        jitDebug = dbg != None
        // 入口表 raw 指针（固定容量，地址稳定）
        let handle = unsafe { acquireArrayRawData<Int64>(jitEntryTable) }
        JitCodegen.entryTablePtr = Int64(unsafe { handle.pointer.toUIntNative() })
        // 全局解析 helper 地址
        let hf: CFunc<(Int64, CPointer<Unit>, Int64) -> Unit> = jitResolveGlobals
        JitCodegen.resolveFnAbs = Int64(unsafe { CPointer<Int8>(hf).toUIntNative() })

    }

    public func hasEntry(globalId: Int64): Bool {
        entries.contains(globalId)
    }

    // 尝试编译；成功返回入口；失败 None（回退解释器）
    public func getEntry(globalId: Int64, f: CompiledFunction): Option<CPointer<Unit>> {
        if (entries.contains(globalId)) {
            return Some(entries[globalId])
        }
        if (jitDebug) { eprintln("[JIT] eligible-int=${JitCodegenInt.isEligible(f)} eligible-reg=${JitCodegenIntR.isEligible(f)}") }
        // J4b R 形式（寄存器级）：已修 分支快照/prologue/BIN_LC 复制/弹栈顺序/桥接 rcx 槽 等，
        // 但递归调用链路仍有最终不一致（值池跨调用复用/返回路径）——暂禁用，保 v1 INT（正确性优先）
        let intCode: Option<ArrayList<UInt8>> = if (JitCodegenInt.isEligible(f)) {
            let cg2 = JitCodegenInt()
            cg2.compile(f, globalId)
        } else {
            None
        }
        if (let Some(bytes) <- intCode) {
            if (jitDebug) {
                var hx = StringBuilder("")
                for (i in 0..bytes.size) {
                    let h = bytes[i]
                    let hi = h / 16
                    let lo = h % 16
                    hx.append(if (hi < 10) { Rune(0x30 + Int32(hi)) } else { Rune(0x61 + Int32(hi) - 10) })
                    hx.append(if (lo < 10) { Rune(0x30 + Int32(lo)) } else { Rune(0x61 + Int32(lo) - 10) })
                    hx.append(" ")
                }
                eprintln("[JIT] hex: ${hx}")
            }
            let entry = this.allocPage(bytes)
            if (jitDebug) { eprintln("[JIT] entryaddr=${entry.toUIntNative()}") }
            if (globalId >= 0 && globalId < JIT_ENTRY_CAP) {
                jitEntryTable[globalId] = Int64(entry.toUIntNative())
            }
            entries[globalId] = entry
            rForms[globalId] = false
            return Some(entry)
        }
        let codegen = JitCodegen()
        if (jitDebug) {
            var gs = StringBuilder("")
            for (i in 0..f.globalRefs.size) {
                gs.append("${f.globalRefs[i]}=${if (i < f.globalSlots.size) { f.globalSlots[i] } else { -1 }} ")
            }
            eprintln("[JIT] compiling fn ${f.name} id=${globalId} locals=${f.localSlots} globals: ${gs} params=${f.params.size}")
        }
        if (let Some(code) <- codegen.compile(f, globalId)) {
            if (jitDebug) {
                var hx = StringBuilder("")
                for (i in 0..code.size) {
                    let h = code[i]
                    let hi = h / 16
                    let lo = h % 16
                    hx.append(if (hi < 10) { Rune(0x30 + Int32(hi)) } else { Rune(0x61 + Int32(hi) - 10) })
                    hx.append(if (lo < 10) { Rune(0x30 + Int32(lo)) } else { Rune(0x61 + Int32(lo) - 10) })
                    hx.append(" ")
                }
                eprintln("[JIT] ${code.size} bytes: ${hx}")
            }
            let entry = this.allocPage(code)
            if (globalId >= 0 && globalId < JIT_ENTRY_CAP) {
                jitEntryTable[globalId] = Int64(entry.toUIntNative())
            }
            if (jitDebug) { eprintln("[JIT] entry=${entry.toUIntNative()}") }
            entries[globalId] = entry
            return Some(entry)
        }
        None
    }

    // 分配 RWX 页并拷贝代码（按代码长度扩展容量，防止越界写）
    func allocPage(code: ArrayList<UInt8>): CPointer<Unit> {
        let total = Int64(code.size) + 4096
        var cap = 4096
        while (cap < total) {
            cap = cap + 4096
        }
        var page = unsafe { mmap(CPointer<Unit>(), UIntNative(cap), 7, 0x22, -1, 0) }
        if (page.isNull()) {
            throw Exception("JIT mmap failed")
        }
        let p8 = CPointer<UInt8>(page)
        for (i in 0..code.size) {
            unsafe { (p8 + i).write(code[i]) }
        }
        page
    }

    /**
     * 解释器→JIT 桥：args 打包为 16B 槽 → 调入口 → 结果解包
     * 全局引用每次调用实时解析（与 switchFrame 语义一致）
     */
    // 返回 None：编译失败或 deopt（调用方回退解释器）
    public func invoke(globalId: Int64, f: CompiledFunction, args: ArrayList<LispValue>, env: Environment): Option<LispValue> {
        if (jitDebug) { eprintln("[JIT] invoke id=${globalId} fn=${f.name} args=${args.size}") }
        let entryOpt = this.getEntry(globalId, f)
        match (entryOpt) {
            case None =>
                return None
            case Some(entryPtr) =>
                jitCurEnv = env
                let n = f.localSlots
                // 缓冲：16B×(n+1)：buf[0]=结果槽；buf[16+i*16]=槽 i
                let buf = Array<UInt8>(16 * (n + 1)) { _ => 0 }
                // 全局引用解析入槽
                for (i in 0..f.globalRefs.size) {
                    let name = f.globalRefs[i]
                    let slotIdx = if (i < f.globalSlots.size) { f.globalSlots[i] } else { f.params.size + i }
                    if (slotIdx >= 0 && slotIdx < n) {
                        let val = env.lookup(name)
                        let slotVal = match (val) {
                            case Macro(_, _, _) | MacroFromParams(_, _, _) => Str(name)
                            case _ => val
                        }
                        this.writeSlot(buf, slotIdx, slotVal)
                    }
                }
                // 参数入槽
                for (j in 0..args.size) {
                    if (j < f.params.size) {
                        this.writeSlot(buf, j, args[j])
                    }
                }
                // raw 指针 → 调用入口
                let handle = unsafe { acquireArrayRawData<UInt8>(buf) }
                let argBase = unsafe { handle.pointer + 16 }
                let fptr: CFunc<(CPointer<Unit>, Int64, CPointer<Unit>) -> CPointer<Unit>> = CFunc<(CPointer<Unit>, Int64, CPointer<Unit>) -> CPointer<Unit>>(entryPtr)
                let retPtr = unsafe { fptr(CPointer<Unit>(argBase), Int64(args.size), CPointer<Unit>()) }
                unsafe { releaseArrayRawData<UInt8>(handle) }
                let retVal = retPtr.toUIntNative()
                if (retVal < 1024) {
                    if (jitDebug) { eprintln("[JIT] deopt id=${globalId} site=${retVal}") }
                    return None   // deopt → 回退解释器
                }
                Some(this.readSlot(buf, -1))
        }
    }

    // 写入 16B 槽（盒装 box）
    func writeSlot(buf: Array<UInt8>, slotIdx: Int64, v: LispValue): Unit {
        let (tag, payload) = this.box(v)
        let base = 16 + slotIdx * 16
        writeU64(buf, base, tag)
        writeU64(buf, base + 8, payload)
    }

    func readSlot(buf: Array<UInt8>, slotIdx: Int64): LispValue {
        let base = 16 + slotIdx * 16
        let tag = readU64(buf, base)
        let payload = readU64(buf, base + 8)
        this.unbox(tag, payload)
    }

    // LispValue → (tag, payload)
    func box(v: LispValue): (Int64, Int64) {
        jitBox(v)
    }


    // (tag, payload) → LispValue
    func unbox(tag: Int64, payload: Int64): LispValue {
        match (tag) {
            case JIT_TAG_INT => LispValue.Int(payload)
            case JIT_TAG_FLOAT => LispValue.Float(floatFromBits(payload))
            case JIT_TAG_BOOL => LispValue.Boolean(payload != 0)
            case JIT_TAG_NIL => Nil
            case _ =>
                LispValue.Error(XispError(ErrorType.RuntimeError, "JIT: unbox unsupported tag ${tag}"))
        }
    }
}

func floatBits(v: Float64): Int64 {
    let arr = Array<Float64>(1) { _ => v }
    let handle = unsafe { acquireArrayRawData<Float64>(arr) }
    let r = unsafe { CPointer<Int64>(handle.pointer).read() }
    unsafe { releaseArrayRawData<Float64>(handle) }
    r
}

func floatFromBits(bits: Int64): Float64 {
    let arr = Array<Int64>(1) { _ => bits }
    let handle = unsafe { acquireArrayRawData<Int64>(arr) }
    let r = unsafe { CPointer<Float64>(handle.pointer).read() }
    unsafe { releaseArrayRawData<Int64>(handle) }
    r
}

// 16B 槽缓冲读写（手工小端）
func writeU64(buf: Array<UInt8>, base: Int64, v: Int64): Unit {
    for (i in 0..8) {
        buf[base + i] = UInt8((v >> (8 * i)) & 0xFF)
    }
}

func readU64(buf: Array<UInt8>, base: Int64): Int64 {
    var v: Int64 = 0
    for (i in 0..8) {
        v = v | (Int64(buf[base + i]) << (8 * i))
    }
    v
}
