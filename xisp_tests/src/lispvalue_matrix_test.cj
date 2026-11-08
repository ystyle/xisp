package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp.types.*
import ystyle::xisp.core.builtin.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import std.collection.HashMap

/**
 * `LispValue` 变体矩阵（C6：新增变体时的**编译期**闸门）
 *
 * ## 它防的是什么
 *
 * `LispValue` 是封闭枚举。给它加一个变体时：
 * - **非穷尽 `match`**（无 `case _`）→ 编译器报 `non-exhaustive patterns`，逐处指出，不会漏；
 * - **带 `case _` 的 `match`** → **静默**落到兜底分支，编译器一声不响。
 *
 * 后者才是真正的风险：加 `External` 时实测就只有 2 处是非穷尽的，其余十几处
 * 都靠 `case _` 兜着——`type-of` 返回 `"unknown"`、`=` 恒 `false`、`length` 恒 `0`
 * 这类"不报错但语义错"的问题，全靠人肉发现（本轮就是这样补的）。
 *
 * ## 机制：让守卫自己变成编译错误
 *
 * 本文件的 `variantTag` **刻意不写 `case _`**。于是**再加变体时，这个文件编译不过**，
 * 作者必须先在这里登记新变体、并回答"它在下面这些横切语义上应该表现成什么"，
 * 才有机会去检查那些 `case _` 兜底处。这不是运行时断言能替代的。
 *
 * > 加变体时的检查清单见 `docs/plans/2026-10-11-capability-domains-design.md`
 * > §7.2 / §12.3：`type-of` / `isTruthy` / `toString` / `cloneValue` / `=` / `length` /
 * > `nth` / 调用位置 / `str` / 谓词 / 桥接序列化。
 */
/** 测试用外部值：只为在矩阵里占一个 `External` 样本 */
class MatrixExternal <: LispExternal {
    init() {}

    public prop typeName: String {
        get() { "matrix-sample" }
    }

    public func display(): String {
        "#<matrix-sample>"
    }
}

/**
 * 变体 → 稳定标签。**故意穷尽、无 `case _`** —— 新增变体时这里编译失败。
 */
func variantTag(v: LispValue): String {
    match (v) {
        case Nil => "Nil"
        case Boolean(_) => "Boolean"
        case Int(_) => "Int"
        case Float(_) => "Float"
        case Str(_) => "Str"
        case Symbol(_) => "Symbol"
        case Cons(_) => "Cons"
        case Procedure(_, _, _) => "Procedure"
        case ProcedureFromParams(_, _, _) => "ProcedureFromParams"
        case Macro(_, _, _) => "Macro"
        case MacroFromParams(_, _, _) => "MacroFromParams"
        case NativeFunc(_) => "NativeFunc"
        case Closure(_, _) => "Closure"
        case External(_) => "External"
        case HashMap(_) => "HashMap"
        case Error(_) => "Error"
    }
}

@Test
class LispValueMatrixTest {
    /** 每种变体一个样本，以及它在该变体下的**期望标签** */
    func samples(): Array<(LispValue, String)> {
        let emptyParams = ArrayList<String>()
        let emptyInfos = ArrayList<ParamInfo>()
        let env = Environment()
        [
            (Nil, "Nil"),
            // 取 false 作样本：顺带覆盖"假值"这一侧
            (LispValue.Boolean(false), "Boolean"),
            (LispValue.Int(0), "Int"),
            (LispValue.Float(0.0), "Float"),
            (LispValue.Str("s"), "Str"),
            (LispValue.Symbol("sym"), "Symbol"),
            (LispValue.Cons(ConsCell(LispValue.Int(1), Nil)), "Cons"),
            (LispValue.Procedure(emptyParams, Nil, env), "Procedure"),
            (LispValue.ProcedureFromParams(emptyInfos, Nil, env), "ProcedureFromParams"),
            (LispValue.Macro(emptyParams, Nil, env), "Macro"),
            (LispValue.MacroFromParams(emptyInfos, Nil, env), "MacroFromParams"),
            (LispValue.NativeFunc({ _ => Nil }), "NativeFunc"),
            (LispValue.Closure(0, env), "Closure"),
            (LispValue.External(MatrixExternal()), "External"),
            (LispValue.HashMap(HashMap<String, LispValue>()), "HashMap"),
            (LispValue.Error(XispError(ErrorType.RuntimeError, "boom")), "Error"),
        ]
    }

    /**
     * 每个变体都必须有：稳定标签、非空显示、可克隆且克隆后显示一致、可求 `type-of`。
     *
     * 这几条是**所有变体共享的横切契约**。任何一条对某个变体不成立，说明该变体
     * 在某个通用内建里没被处理（典型现象就是 `type-of` 给 `"unknown"`）。
     */
    @TestCase
    func testEveryVariantHasDefinedBehaviour() {
        println("\n=== LispValue 变体矩阵：横切契约 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let typeOf = env.lookup("type-of")

        for ((v, expectTag) in this.samples()) {
            let tag = variantTag(v)
            if (tag != expectTag) {
                @Fail("variantTag 与样本不符：期望 ${expectTag}，实得 ${tag}")
            }

            // 1. 显示非空
            let shown = v.toString()
            if (shown.isEmpty()) {
                @Fail("${tag}: toString() 为空")
            }

            // 2. 克隆不改变显示（值语义一致的观察口径）
            if (v.cloneValue().toString() != shown) {
                @Fail("${tag}: cloneValue() 后显示变了：${shown} -> ${v.cloneValue().toString()}")
            }

            // 3. type-of 必须有明确类型名，不能是 "unknown"
            let typeName = match (typeOf) {
                case NativeFunc(f) =>
                    let args = ArrayList<LispValue>()
                    args.add(v)
                    f(args).toString()
                case _ => ""
            }
            if (typeName.isEmpty()) {
                @Fail("${tag}: type-of 取不到结果（type-of 未注册？）")
            }
            if (typeName == "\"unknown\"") {
                @Fail("${tag}: type-of 返回 \"unknown\" —— 该变体在 type-of 里没被处理")
            }
        }
        println("✓ 16 个变体：标签稳定、显示非空、克隆保形、type-of 有明确名称")
    }

    /**
     * 真值语义：只有 `nil` / `false` / `0` / `0.0` 为假，**其余（含 `External`）恒真**。
     *
     * `External` 恒真是设计决策（对象存在即真，判空用显式谓词），不是默认兜底。
     */
    @TestCase
    func testTruthinessMatrix() {
        println("\n=== 真值语义矩阵 ===")
        for ((v, tag) in this.samples()) {
            let expect = match (tag) {
                case "Nil" => false
                case "Boolean" => false   // 样本取的是 false
                case "Int" => false       // 样本取的是 0
                case "Float" => false     // 样本取的是 0.0
                // External 恒真：对象存在即真（设计决策，见能力域文档 §7.4 #2）
                case _ => true
            }
            if (v.isTruthy() != expect) {
                @Fail("${tag}: isTruthy() 期望 ${expect}，实得 ${v.isTruthy()}")
            }
        }
        println("✓ 只有 nil/false/0/0.0 为假，External 与其余变体恒真")
    }

    /** `External` 的三个"近乎一等公民"的能力由 core 通用内建提供（不靠专用变体） */
    @TestCase
    func testExternalGetsCoreTreatment() {
        println("\n=== External 的通用内建待遇 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let ext = LispValue.External(MatrixExternal())

        let call = { name: String, args: ArrayList<LispValue> =>
            match (env.lookup(name)) {
                case NativeFunc(f) => f(args).toString()
                case _ => "<未注册>"
            }
        }
        let one = ArrayList<LispValue>()
        one.add(ext)

        @Assert(call("type-of", one) == "\"matrix-sample\"")
        @Assert(call("external?", one) == "true")
        @Assert(call("external-type", one) == "\"matrix-sample\"")
        @Assert(call("str", one) == "\"#<matrix-sample>\"")
        // 未实现 LispExternalIndexable → length 按既有约定返回 0（不是崩溃/未定义）
        @Assert(call("length", one) == "0")
        println("✓ type-of / external? / external-type / str / length 均按能力接口分派")
    }
}
