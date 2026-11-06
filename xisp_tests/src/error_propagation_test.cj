package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp_bc.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*
import ystyle::xisp.bridge.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList

/**
 * 错误传播语义：**三个后端必须一致**
 *
 * ## 为什么需要这一组用例
 *
 * `LispValue.Error` 是**值**而不是异常，因此"错误发生在哪里、要不要中止"
 * 完全由各处的分发代码决定。历史上 AST 与字节码在这一点上分道扬镳：
 *
 * | 写法 | AST（旧） | 字节码（旧） |
 * |---|---|---|
 * | `(begin (error "b") 42)` | `42` —— 错误被丢弃 | `RuntimeError` |
 * | `(define (f) (assert 假) 42) (f)` | `42` —— **assert 形同虚设** | `RuntimeError` |
 * | `(let ((a (error "b"))) 42)` | `42` | `RuntimeError` |
 * | `(if 未定义变量 1 2)` | `[UnboundVariable]` | `1` —— 错误被当真值 |
 * | `(+ 1 未定义变量)` | `[UnboundVariable]` | `[TypeError]` |
 * | `(< 1 未定义变量)` | `[UnboundVariable]` | `false` |
 * | `(not 未定义变量)` | `[UnboundVariable]` | `false` |
 * | `(match 未定义变量 (1 "one") (else "other"))` | `"other"` | `[UnboundVariable]` |
 * | `(foo 1)`（foo 未绑定） | `[UndefinedFunction]` | `[UnboundVariable]` |
 * | `(apply 42 '(1 2))` | `nil` 静默 | `RuntimeError` |
 *
 * 最严重的一条是第三行：`assert` 是**给用户做参数校验的 API**，在 AST 下
 * 完全没有保护作用——校验失败后函数继续算下去并返回正常结果。
 *
 * ## 统一的语义（本文件钉住它）
 *
 * **错误一旦产生就中止当前求值链**，一路返回到最外层——与异常语义相同，
 * 只是用值传递。绑定初值出错时**不建立绑定**（不把错误塞进变量）。
 *
 * 错误**种类**必须一致（`UnboundVariable` / `UndefinedFunction` / `TypeError` …），
 * 文案可以不同：两侧的错误分别由不同的代码路径产出，措辞差异不是语义差异。
 */
func backendNames(): Array<String> {
    ["AST", "BC", "JIT"]
}

/** 按后端名求值（三种装配只差后端，其余完全一致） */
func evalWithBackend(code: String, backend: String): LispValue {
    let opts = ArrayList<InterpreterOption>()
    opts.add(withQuietMode())
    opts.add(withStdLib())
    if (backend != "AST") {
        opts.add(withBytecodeCompiler())
    }
    if (backend == "JIT") {
        opts.add(withJit())
    }
    let it = LispInterpreter(opts.toArray())
    var result: LispValue = Nil
    for (expr in Parser.parseFromString(code)) {
        result = it.evalExpr(expr)
    }
    result
}

/** 取错误种类名；非错误返回空串 */
func kindOf(v: LispValue): String {
    match (v) {
        case Error(err) => err.kind.name()
        case _ => ""
    }
}

/**
 * 断言：三个后端都得到**错误**，且错误种类相同。
 *
 * @return 该错误种类（便于用例继续检查细节）
 */
func assertErrorAllBackends(code: String, expectKind: String): Unit {
    var firstKind = ""
    for (b in backendNames()) {
        let r = evalWithBackend(code, b)
        let k = kindOf(r)
        if (k.isEmpty()) {
            @Fail("[${b}] 期望错误 `${expectKind}`，实际得到正常值：${r.toString()}（代码：${code}）")
        }
        if (firstKind.isEmpty()) {
            firstKind = k
        } else if (k != firstKind) {
            @Fail("后端错误种类不一致：${code} → ${firstKind} vs ${b}:${k}")
        }
        if (k != expectKind) {
            @Fail("[${b}] 期望 `${expectKind}`，实际 `${k}`（代码：${code}）")
        }
    }
}

/** 断言：三个后端结果均为正常值且字符串表示相同 */
func assertSameResultAllBackends(code: String, expect: String): Unit {
    for (b in backendNames()) {
        let r = evalWithBackend(code, b)
        if (kindOf(r).size > 0) {
            @Fail("[${b}] 期望结果 `${expect}`，实际报错：${r.toString()}（代码：${code}）")
        }
        if (r.toString() != expect) {
            @Fail("[${b}] 期望 `${expect}`，实际 `${r.toString()}`（代码：${code}）")
        }
    }
}

@Test
class ErrorPropagationTest {
    // ==================== 中止语义 ====================

    /**
     * `assert` 必须真的拦住：这是最有用户价值的一条。
     *
     * 旧 AST 下 `assert` 失败后函数继续算完并返回正常结果——参数校验形同虚设。
     */
    @TestCase
    func testAssertAbortsFunctionBody() {
        let code = "(define (validate age) (assert (>= age 0) \"age 必须非负\") (* age 2)) (validate -5)"
        assertErrorAllBackends(code, "RuntimeError")
    }

    /** 校验通过时不受影响（确认不是把所有 assert 都当失败） */
    @TestCase
    func testAssertPassesWhenConditionHolds() {
        assertSameResultAllBackends(
            "(define (validate age) (assert (>= age 0) \"bad\") (* age 2)) (validate 5)", "10")
    }

    /** begin 中某步出错 → 中止，后续表达式不执行 */
    @TestCase
    func testBeginAbortsOnError() {
        assertErrorAllBackends("(begin (error \"boom\") 42)", "RuntimeError")
    }

    /** 错误中止后副作用不再发生（用 define 观测：被中止则变量不存在） */
    @TestCase
    func testBeginAbortSkipsRemainingSideEffects() {
        let code = "(begin (error \"boom\") (define never-defined 1)) (type-of never-defined)"
        assertErrorAllBackends(code, "UnboundVariable")
    }

    // ==================== 不把错误塞进绑定 ====================

    /** define 初值出错 → 不建立绑定，且错误本身照原样传播 */
    @TestCase
    func testDefineDoesNotBindErrorValue() {
        // 单条表达式：初值出错即中止，define 不执行
        assertErrorAllBackends("(begin (define r (foo 1)) r)", "UndefinedFunction")
    }

    /**
     * 顶层逐表达式执行时，`define` 失败**不留下绑定**。
     *
     * 若错误值被塞进变量，第二条 `r` 会读到它；正确行为是读到"未绑定"。
     */
    @TestCase
    func testFailedDefineLeavesNoBinding() {
        for (b in backendNames()) {
            let opts = ArrayList<InterpreterOption>()
            opts.add(withQuietMode())
            opts.add(withStdLib())
            if (b != "AST") {
                opts.add(withBytecodeCompiler())
            }
            if (b == "JIT") {
                opts.add(withJit())
            }
            let it = LispInterpreter(opts.toArray())
            let first = it.eval("(define r (foo 1))")
            if (kindOf(first) != "UndefinedFunction") {
                @Fail("[${b}] define 初值出错应传播 UndefinedFunction，实际 ${kindOf(first)}")
            }
            let second = it.eval("r")
            if (kindOf(second) != "UnboundVariable") {
                @Fail("[${b}] define 失败后不应留下绑定，实际得到 ${second.toString()}")
            }
        }
    }

    /** 结构相同的对照组：确认成功路径照常绑定 */
    @TestCase
    func testDefineBindsNormalValue() {
        assertSameResultAllBackends("(define r (+ 1 2)) r", "3")
    }

    /** let 初值出错 → 不执行 body */
    @TestCase
    func testLetAbortsOnBindingError() {
        assertErrorAllBackends("(let ((a (error \"x\"))) 42)", "RuntimeError")
    }

    /** let 内多个绑定：只要有一个出错就中止 */
    @TestCase
    func testLetAbortsOnAnyBindingError() {
        assertErrorAllBackends("(let ((a 1) (b (error \"x\")) (c 3)) (+ a c))", "RuntimeError")
    }

    /** let 绑定正常时照旧 */
    @TestCase
    func testLetBindsNormalValues() {
        assertSameResultAllBackends("(let ((a 1) (b 2)) (+ a b))", "3")
    }

    // ==================== 控制流不得把错误当真值 ====================

    /** if 的条件出错 → 传播，不能走 then */
    @TestCase
    func testIfPropagatesConditionError() {
        assertErrorAllBackends("(if undefined-xyz 1 2)", "UnboundVariable")
    }

    /** and 的首项出错 → 传播（错误值 isTruthy() 为真，不查就会继续） */
    @TestCase
    func testAndPropagatesError() {
        assertErrorAllBackends("(and undefined-xyz 1)", "UnboundVariable")
    }

    /** or 的首项出错 → 传播（错误值会被当成"真"短路掉） */
    @TestCase
    func testOrPropagatesError() {
        assertErrorAllBackends("(or undefined-xyz 1)", "UnboundVariable")
    }

    /** match 的被匹配表达式出错 → 传播，不能落到 else */
    @TestCase
    func testMatchPropagatesScrutineeError() {
        assertErrorAllBackends("(match undefined-xyz (1 \"one\") (else \"other\"))", "UnboundVariable")
    }

    /** 真·短路仍然生效：0 为假，后面的未定义符号不求值 */
    @TestCase
    func testAndStillShortCircuits() {
        assertSameResultAllBackends("(and 0 undefined-xyz)", "false")
    }

    // ==================== 运算不得把错误变成类型错误/布尔 ====================

    /** 算术：AST 曾报 UnboundVariable、字节码报 TypeError */
    @TestCase
    func testArithmeticPropagatesError() {
        assertErrorAllBackends("(+ 1 undefined-xyz)", "UnboundVariable")
    }

    /** 比较：字节码曾静默得 false */
    @TestCase
    func testComparisonPropagatesError() {
        assertErrorAllBackends("(< 1 undefined-xyz)", "UnboundVariable")
    }

    /** 等值：同上 */
    @TestCase
    func testEqualityPropagatesError() {
        assertErrorAllBackends("(= 1 undefined-xyz)", "UnboundVariable")
    }

    /** not：字节码曾静默得 false */
    @TestCase
    func testNotPropagatesError() {
        assertErrorAllBackends("(not undefined-xyz)", "UnboundVariable")
    }

    // ==================== 错误种类一致 ====================

    /**
     * 头位置未绑定 → `UndefinedFunction`（与 AST 对齐）。
     *
     * 字节码曾报 `UnboundVariable`。宿主按种类分发时，两后端不同意味着
     * "同一段脚本换后端就换了一种错误"。
     */
    @TestCase
    func testUndefinedFunctionKindMatches() {
        assertErrorAllBackends("(no-such-fn 1)", "UndefinedFunction")
    }

    /** 局部变量形式的未绑定（非头位置）→ UnboundVariable */
    @TestCase
    func testUnboundVariableKindMatches() {
        assertErrorAllBackends("(println undefined-xyz)", "UnboundVariable")
    }

    /** apply 非可调用值 → 两后端同为 TypeError（AST 曾静默返回 nil） */
    @TestCase
    func testApplyNonCallableKindMatches() {
        assertErrorAllBackends("(apply 42 '(1 2))", "TypeError")
    }

    /** apply 的函数位置未绑定 → 传播该错误本身，不包装成 TypeError */
    @TestCase
    func testApplyPropagatesFunctionError() {
        assertErrorAllBackends("(apply undefined-xyz '(1))", "UnboundVariable")
    }

    // ==================== 回归：曾被静默吞错掩盖的缺陷 ====================

    /**
     * 点对绑定必须吃掉**两个**元素（模式 + 值）。
     *
     * 此前只推进一个元素，值表达式会被当成下一个绑定的模式再处理一遍，
     * 报 "Attempt to call a non-function value: 1"。该错误一直被 begin 吞掉，
     * 所以从外部完全看不出来——直到错误改成中止才暴露。
     */
    @TestCase
    func testDottedPairBinding() {
        assertSameResultAllBackends("(let ((x . y) (quote (1 2 3))) x)", "1")
    }

    /** 嵌套点对 */
    @TestCase
    func testNestedDottedPairBinding() {
        assertSameResultAllBackends("(let (((x . y) . z) (quote ((1 2) 3 4))) (list x y z))", "(1 (2) (3 4))")
    }

    // ==================== 顶层脚本仍继续执行 ====================

    /**
     * 顶层**逐表达式**执行：某条表达式出错会打印错误、后续表达式照常执行。
     *
     * 这是脚本宿主的行为（`evalMultiple`），与"单条表达式内部中止"不矛盾：
     * 中止的是出错的那条表达式，不是整个文件。
     */
    @TestCase
    func testTopLevelContinuesAfterError() {
        let opts = ArrayList<InterpreterOption>()
        opts.add(withQuietMode())
        opts.add(withStdLib())
        let it = LispInterpreter(opts.toArray())
        let r1 = it.eval("(error \"boom\")")
        @Assert(kindOf(r1) == "RuntimeError")
        let r2 = it.eval("(+ 1 2)")
        @Assert(kindOf(r2).isEmpty())
        @Assert(r2.toString() == "3")
    }
}
