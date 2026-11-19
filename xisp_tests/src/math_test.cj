package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp_bc.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import std.math.*

/**
 * 标量数学函数的契约测试
 *
 * 这些函数是**语言能力**（0.9.0 起进入 `arithmetic` 域），所以必须三后端一致、
 * 错误种类清楚。数组的逐元素版本由 `LispExternalElementwise` 分派，
 * 分派协议本身在 `external_dispatch_test.cj` 里测。
 */

func mathBackendNames(): Array<String> {
    ["AST", "BC", "JIT"]
}

func evalMathWithBackend(code: String, backend: String): LispValue {
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

func asFloat(v: LispValue): Option<Float64> {
    match (v) {
        case Float(x) => Some(x)
        case Int(i) => Some(Float64(i))
        case _ => None
    }
}

/** 三后端都得到同一个数值（带容差） */
func expectMathAllBackends(code: String, want: Float64, tol: Float64, what: String): Unit {
    for (b in mathBackendNames()) {
        let r = evalMathWithBackend(code, b)
        match (asFloat(r)) {
            case Some(x) =>
                if (abs(x - want) > tol) {
                    @Fail("[${b}] ${what}：期望 ${want}，实际 ${x}（代码：${code}）")
                }
            case None =>
                @Fail("[${b}] ${what}：期望数值 ${want}，实际 ${r.toString()}（代码：${code}）")
        }
    }
}

/** 三后端都得到同一个整数 */
func expectMathIntAllBackends(code: String, want: Int64, what: String): Unit {
    for (b in mathBackendNames()) {
        let r = evalMathWithBackend(code, b)
        match (r) {
            case Int(i) =>
                if (i != want) {
                    @Fail("[${b}] ${what}：期望 ${want}，实际 ${i}（代码：${code}）")
                }
            case _ =>
                @Fail("[${b}] ${what}：期望整数 ${want}，实际 ${r.toString()}（代码：${code}）")
        }
    }
}

func expectMathErrorAllBackends(code: String, what: String): Unit {
    for (b in mathBackendNames()) {
        let r = evalMathWithBackend(code, b)
        match (r) {
            case Error(_) => ()
            case _ => @Fail("[${b}] ${what}：期望错误，实际 ${r.toString()}（代码：${code}）")
        }
    }
}

@Test
class ScalarMathTest {
    /**
     * 三角与指数：结果恒为浮点
     *
     * 整型输入也要升成浮点 —— `(sqrt 4)` 是 `2.0` 而不是 `2`，
     * 因为开方结果不保证落在整数上，让类型随输入变化会让下游代码难以预测。
     */
    @TestCase
    func testTranscendental() {
        println("\n=== 三角 / 指数 / 对数 ===")
        expectMathAllBackends("(sin 0)", 0.0, 0.0, "sin 0")
        expectMathAllBackends("(cos 0)", 1.0, 0.0, "cos 0")
        expectMathAllBackends("(tan 0)", 0.0, 0.0, "tan 0")
        expectMathAllBackends("(asin 0)", 0.0, 0.0, "asin 0")
        expectMathAllBackends("(acos 1)", 0.0, 0.0, "acos 1")
        expectMathAllBackends("(atan 0)", 0.0, 0.0, "atan 0")
        expectMathAllBackends("(exp 0)", 1.0, 0.0, "exp 0")
        expectMathAllBackends("(exp 1)", 2.718281828459045, 1e-9, "exp 1")
        expectMathAllBackends("(log 1)", 0.0, 0.0, "log 1（自然对数）")
        expectMathAllBackends("(log10 1000)", 3.0, 1e-12, "log10 1000")
        println("✓ 三角 / exp / log / log10 正确，整型输入升为浮点")
    }

    @TestCase
    func testRoots() {
        println("\n=== 开方 ===")
        expectMathAllBackends("(sqrt 4)", 2.0, 0.0, "sqrt 4（整型输入）")
        expectMathAllBackends("(sqrt 2)", 1.4142135623730951, 1e-12, "sqrt 2")
        expectMathAllBackends("(cbrt 27)", 3.0, 1e-12, "cbrt 27")
        expectMathAllBackends("(cbrt -8)", -2.0, 1e-12, "cbrt 负数")
        println("✓ sqrt / cbrt")
    }

    @TestCase
    func testPowAndHypot() {
        println("\n=== pow / hypot ===")
        expectMathAllBackends("(pow 2 10)", 1024.0, 0.0, "pow 2 10")
        expectMathAllBackends("(pow 2 -1)", 0.5, 0.0, "pow 2 -1")
        expectMathAllBackends("(hypot 3 4)", 5.0, 0.0, "hypot 3 4")
        // 这条钉的是"不中间溢出"：直接 sqrt(a²+b²) 在 1e200 上会得到 inf。
        // 用 (pow 10 200) 而不是 1e200 —— xisp 的数字字面量目前不支持科学计数法。
        expectMathAllBackends("(hypot (pow 10 200) (pow 10 200))",
            1.4142135623730951e200, 1e188, "hypot 1e200 1e200 不溢出")
        println("✓ pow / hypot（含大数不溢出）")
    }

    /**
     * `abs` / `floor` / `ceil` / `sign` **保留整数性**
     *
     * 它们与上面那批"恒浮点"的函数故意不同：取绝对值、取整、取符号都不改变量纲，
     * 返回浮点会让 `(nth a (floor 3.7))` 直接类型错。
     */
    @TestCase
    func testIntegerPreserving() {
        println("\n=== 保留整数性的函数 ===")
        expectMathIntAllBackends("(abs -3)", 3, "abs -3")
        expectMathAllBackends("(abs -3.5)", 3.5, 0.0, "abs 浮点版")
        expectMathIntAllBackends("(floor 3.7)", 3, "floor 3.7")
        expectMathIntAllBackends("(floor -3.2)", -4, "floor -3.2 向负无穷")
        expectMathIntAllBackends("(ceil 3.2)", 4, "ceil 3.2")
        expectMathIntAllBackends("(ceil -3.7)", -3, "ceil -3.7 向正无穷")
        expectMathIntAllBackends("(floor 5)", 5, "floor 整数恒等")
        expectMathIntAllBackends("(sign -5)", -1, "sign -5")
        expectMathIntAllBackends("(sign 0)", 0, "sign 0")
        expectMathIntAllBackends("(sign 7)", 1, "sign 7")
        expectMathAllBackends("(sign -2.5)", -1.0, 0.0, "sign 浮点版")
        println("✓ abs / floor / ceil / sign 保留入参量纲")
    }

    /**
     * 特殊值：`log 0` 是 -inf，`sqrt -1` 是 nan
     *
     * 不报错是**有意的**：数值计算里 nan 会沿算式传播，用户能在结果里看到它；
     * 报错则会让"一批数据里有一个非法值"整个中断。判空由调用方用 `nan?` 之类处理。
     */
    @TestCase
    func testSpecialValues() {
        println("\n=== 特殊值 ===")
        let negInf = evalMathWithBackend("(log 0)", "AST")
        match (asFloat(negInf)) {
            case Some(x) =>
                if (!x.isInf() || x > 0.0) {
                    @Fail("(log 0) 应为 -inf，实际 ${x}")
                }
            case None => @Fail("(log 0) 应为数值，实际 ${negInf.toString()}")
        }
        let nanVal = evalMathWithBackend("(sqrt -1)", "AST")
        match (asFloat(nanVal)) {
            case Some(x) =>
                if (!x.isNaN()) {
                    @Fail("(sqrt -1) 应为 nan，实际 ${x}")
                }
            case None => @Fail("(sqrt -1) 应为数值，实际 ${nanVal.toString()}")
        }
        println("✓ log 0 = -inf，sqrt -1 = nan（不抛错，让 nan 沿算式传播）")
    }

    // ===== 错误路径 =====

    @TestCase
    func testArityErrors() {
        println("\n=== 参数个数 ===")
        expectMathErrorAllBackends("(sin)", "sin 无参")
        expectMathErrorAllBackends("(sin 1 2)", "sin 两参")
        expectMathErrorAllBackends("(pow 2)", "pow 一参")
        println("✓ 参数个数不对报错")
    }

    @TestCase
    func testTypeErrors() {
        println("\n=== 类型错误 ===")
        expectMathErrorAllBackends("(sin \"x\")", "sin 字符串")
        expectMathErrorAllBackends("(pow 2 \"x\")", "pow 字符串")
        expectMathErrorAllBackends("(abs '(1 2))", "abs 列表")
        println("✓ 非数值参数报 TypeError")
    }

    /**
     * `min` / `max` / `sum` / `product` 原有的列表与变参形式不能被破坏
     *
     * 0.9.0 给它们加了外部值分派，这条钉住旧行为没变。
     */
    @TestCase
    func testAggregatesStillWorkOnLists() {
        println("\n=== 归约对列表与变参仍可用 ===")
        expectMathAllBackends("(sum '(1 2 3))", 6.0, 0.0, "sum 列表")
        expectMathAllBackends("(product '(2 3 4))", 24.0, 0.0, "product 列表")
        expectMathAllBackends("(max '(1 5 3))", 5.0, 0.0, "max 列表")
        expectMathAllBackends("(min '(4 2 8))", 2.0, 0.0, "min 列表")
        expectMathAllBackends("(max 1 5 3)", 5.0, 0.0, "max 变参")
        println("✓ 加入外部值分派后，列表与变参形式行为不变")
    }
}
