package ystyle::xisp.core.eval
import ystyle::xisp.types.*
import std.collection.ArrayList



// 栈机指令
let OP_PUSH_CONSTANT: Int64 = 0
let OP_PUSH_NIL: Int64 = 1
let OP_PUSH_TRUE: Int64 = 2
let OP_PUSH_FALSE: Int64 = 3
let OP_POP: Int64 = 4
let OP_STORE_GLOBAL: Int64 = 5
let OP_LOAD_GLOBAL: Int64 = 6
let OP_CALL: Int64 = 7
let OP_ADD: Int64 = 8
let OP_SUB: Int64 = 9
let OP_JUMP: Int64 = 10
let OP_JUMP_IF_FALSE: Int64 = 11
let OP_MUL: Int64 = 12
let OP_DIV: Int64 = 13
let OP_LT: Int64 = 14
let OP_GT: Int64 = 15
let OP_EQ: Int64 = 16
let OP_DUP: Int64 = 23
let OP_NOT: Int64 = 17
let OP_LOAD_LOCAL: Int64 = 24
let OP_STORE_LOCAL: Int64 = 25
let OP_LOAD_CONST: Int64 = 30
let OP_ADD_R: Int64 = 31
let OP_SUB_R: Int64 = 32
let OP_MUL_R: Int64 = 33
let OP_DIV_R: Int64 = 34
let OP_NEG_R: Int64 = 35
let OP_MOV_R: Int64 = 36
let OP_PUSH_R: Int64 = 37
let OP_MATCH_LIT: Int64 = 38
let OP_LOAD_LOCAL_R: Int64 = 39
let OP_LOAD_GLOBAL_R: Int64 = 40
let OP_NOT_R: Int64 = 41
let OP_MAKE_CLOSURE: Int64 = 42
let OP_RETURN: Int64 = 43
let OP_LT_R: Int64 = 44
let OP_GT_R: Int64 = 45
let OP_EQ_R: Int64 = 46
let OP_POP_R: Int64 = 47
let OP_SET_GLOBAL: Int64 = 48
let OP_CONS: Int64 = 49
let OP_CONCAT: Int64 = 50
let OP_APPLY: Int64 = 51
// 超级指令：局部槽 + 常量二元运算（省 LOAD_LOCAL/PUSH_CONSTANT 分派与栈操作）
let OP_ADD_LC: Int64 = 52    // localIdx, constIdx — push(local[idx] + const)
let OP_SUB_LC: Int64 = 53
let OP_MUL_LC: Int64 = 54
let OP_DIV_LC: Int64 = 55
// 比较 + 跳转合一：cond 为 (</>/= local const) 的 if 分支
// cmpKind, localIdx, constIdx, target — 比较为假则跳
let OP_CMP_LC_JF: Int64 = 56

let OP_HALT: Int64 = 99

public class CompiledFunction {
    // 编译函数唯一标识（跨闭包创建稳定；globalRefs 缓存 key）
    static var nextIdentity: Int64 = 0
    public let identity: Int64
    public let code: ArrayList<Int64>
    public let constants: ArrayList<LispValue>
    public let functions: ArrayList<CompiledFunction>
    public let name: String
    public let body: LispValue
    public let params: ArrayList<String>
    public let globalRefs: ArrayList<String>
    // 参数属性（支持的参数模式；未支持模式（&rest/&key/默认值）编译器直接回退，
    // 不会产出语义错误的字节码）
    public let paramInfos: ArrayList<ParamInfo>
    // 全局引用对应的帧槽号（与 globalRefs 平行；帧切换时解析并写入该槽）
    public let globalSlots: ArrayList<Int64>
    // 本函数帧的局部槽总数（参数 + 全局引用 + let 绑定），帧栈区从该槽之上开始
    public let localSlots: Int64

    public init(code: ArrayList<Int64>, constants: ArrayList<LispValue>, functions: ArrayList<CompiledFunction>, name: String, body: LispValue, params: ArrayList<String>, globalRefs: ArrayList<String>, globalSlots: ArrayList<Int64>, localSlots: Int64, paramInfos: ArrayList<ParamInfo>) {
        this.identity = CompiledFunction.nextIdentity
        CompiledFunction.nextIdentity = CompiledFunction.nextIdentity + 1
        this.code = code
        this.constants = constants
        this.functions = functions
        this.name = name
        this.body = body
        this.params = params
        this.paramInfos = paramInfos
        this.globalRefs = globalRefs
        this.globalSlots = globalSlots
        this.localSlots = localSlots
    }
}

public enum CompiledCode {
    | Bytecode(CompiledFunction)
}
