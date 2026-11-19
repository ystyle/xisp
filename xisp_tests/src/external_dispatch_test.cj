package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp_bc.*
import ystyle::xisp.core.builtin.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import std.math.*

/**
 * 外部值分派协议（`LispExternalArithmetic` / `Reducible` / `Elementwise`）的契约测试
 *
 * core 不能依赖 xisp-sci（依赖方向是反的），所以这里的 `VectorBox` 是**测试替身**：
 * 实现协议要求的最小语义，用来验证 core 侧的调用约定是否正确。
 * 真正的数组在 xisp-sci 仓，那一边有自己的测试。
 *
 * 这些用例钉的是三个容易写错的地方：
 * 1. 减法/除法**不可交换** —— `(- 10 box)` 必须是 `10 - box`；
 * 2. `None` 的语义是"这个组合我不支持"，core 要报 `TypeError` 而不是静默算错；
 * 3. N 元运算是**从左往右折叠**，中间结果仍须是可算外部值。
 */

/** 二元标量运算；不认识的 op 或除零返回 None */
func boxApply2(op: String, a: Float64, b: Float64): Option<Float64> {
    if (op == "+") {
        return Some(a + b)
    }
    if (op == "-") {
        return Some(a - b)
    }
    if (op == "*") {
        return Some(a * b)
    }
    if (op == "/") {
        if (b == 0.0) {
            return None
        }
        return Some(a / b)
    }
    None
}

func boxNumOf(v: LispValue): Option<Float64> {
    match (v) {
        case Int(i) => Some(Float64(i))
        case Float(x) => Some(x)
        case _ => None
    }
}

/**
 * 测试替身：最小的逐元素容器
 *
 * 只实现协议要求的部分，长度不同的两个 box 相加返回 `None`（不做广播 ——
 * 广播是真数组的事，不该在 core 里）。
 */
class VectorBox <: LispExternalArithmetic & LispExternalReducible & LispExternalElementwise {
    let data: Array<Float64>

    init(data: Array<Float64>) {
        this.data = data
    }

    public prop typeName: String {
        get() { "vector-box" }
    }

    public func display(): String {
        var s = "box["
        var i: Int64 = 0
        while (i < data.size) {
            if (i > 0) {
                s = s + " "
            }
            s = s + data[i].toString()
            i++
        }
        s + "]"
    }

    /** 与另一个数组逐元素运算；`selfOnRight` 为真表示 `other op self` */
    func zipWith(op: String, other: Array<Float64>, selfOnRight: Bool): Option<LispValue> {
        if (other.size != data.size) {
            return None
        }
        let out = Array<Float64>(data.size, repeat: 0.0)
        var i: Int64 = 0
        while (i < data.size) {
            let a = if (selfOnRight) { other[i] } else { data[i] }
            let b = if (selfOnRight) { data[i] } else { other[i] }
            match (boxApply2(op, a, b)) {
                case Some(v) => out[i] = v
                case None => return None
            }
            i++
        }
        Some(LispValue.External(VectorBox(out)))
    }

    /** 与标量逐元素运算；`selfOnRight` 为真表示 `scalar op self` */
    func scalarWith(op: String, x: Float64, selfOnRight: Bool): Option<LispValue> {
        let out = Array<Float64>(data.size, repeat: 0.0)
        var i: Int64 = 0
        while (i < data.size) {
            let a = if (selfOnRight) { x } else { data[i] }
            let b = if (selfOnRight) { data[i] } else { x }
            match (boxApply2(op, a, b)) {
                case Some(v) => out[i] = v
                case None => return None
            }
            i++
        }
        Some(LispValue.External(VectorBox(out)))
    }

    public func arithLeft(op: String, other: LispValue): Option<LispValue> {
        match (other) {
            case External(e) =>
                match (e) {
                    case o: VectorBox => zipWith(op, o.data, false)
                    case _ => None
                }
            case _ =>
                match (boxNumOf(other)) {
                    case Some(x) => scalarWith(op, x, false)
                    case None => None
                }
        }
    }

    public func arithRight(op: String, other: LispValue): Option<LispValue> {
        match (other) {
            case External(e) =>
                match (e) {
                    case o: VectorBox => zipWith(op, o.data, true)
                    case _ => None
                }
            case _ =>
                match (boxNumOf(other)) {
                    case Some(x) => scalarWith(op, x, true)
                    case None => None
                }
        }
    }

    public func reduceAll(op: String): Option<LispValue> {
        if (data.size == 0) {
            return None
        }
        if (op == "sum") {
            var t = 0.0
            for (x in data) {
                t = t + x
            }
            return Some(LispValue.Float(t))
        }
        if (op == "product") {
            var t = 1.0
            for (x in data) {
                t = t * x
            }
            return Some(LispValue.Float(t))
        }
        if (op == "max") {
            var m = data[0]
            for (x in data) {
                if (x > m) {
                    m = x
                }
            }
            return Some(LispValue.Float(m))
        }
        if (op == "min") {
            var m = data[0]
            for (x in data) {
                if (x < m) {
                    m = x
                }
            }
            return Some(LispValue.Float(m))
        }
        None
    }

    public func applyElementwise(fn: String): Option<LispValue> {
        let out = Array<Float64>(data.size, repeat: 0.0)
        var i: Int64 = 0
        while (i < data.size) {
            let x = data[i]
            let y = if (fn == "sqrt") {
                sqrt(x)
            } else if (fn == "abs") {
                abs(x)
            } else if (fn == "exp") {
                exp(x)
            } else {
                return None
            }
            out[i] = y
            i++
        }
        Some(LispValue.External(VectorBox(out)))
    }
}

/**
 * 在指定后端上求值，把 box 放进环境
 *
 * **必须能切换后端**：外部值分派横跨三条执行路径 —— AST 的 `NativeFunc`、
 * 字节码的 `OP_ADD` 等特化指令、JIT 的同一批指令。只测 AST 会漏掉后两条，
 * 而 0.9.0 发布时正是这么漏的（BC 后端对数据数组报 "expected a number"）。
 */
func evalWithBoxesOn(code: String, boxes: Array<(String, Array<Float64>)>,
                     backend: String): LispValue {
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
    for (b in boxes) {
        it.getEnvironment().define(b[0], LispValue.External(VectorBox(b[1])))
    }
    var result: LispValue = Nil
    for (expr in Parser.parseFromString(code)) {
        result = it.evalExpr(expr)
    }
    result
}

func evalWithBoxes(code: String, boxes: Array<(String, Array<Float64>)>): LispValue {
    evalWithBoxesOn(code, boxes, "AST")
}

func boxValue(v: LispValue): Array<Float64> {
    match (v) {
        case External(e) =>
            match (e) {
                case b: VectorBox => b.data
                case _ => Array<Float64>()
            }
        case _ => Array<Float64>()
    }
}

func expectBox(v: LispValue, want: Array<Float64>, what: String): Unit {
    let got = boxValue(v)
    if (got.size != want.size) {
        @Fail("${what}：期望 ${want.size} 个元素，实际 ${got.size} 个（${v.toString()}）")
    }
    var i: Int64 = 0
    while (i < want.size) {
        if (got[i] != want[i]) {
            @Fail("${what}：第 ${i} 个元素期望 ${want[i]}，实际 ${got[i]}")
        }
        i++
    }
}

func expectFloat(v: LispValue, want: Float64, what: String): Unit {
    let got = match (v) {
        case Float(x) => x
        case Int(i) => Float64(i)
        case _ => Float64.getNaN()
    }
    if (got != want) {
        @Fail("${what}：期望 ${want}，实际 ${got}（${v.toString()}）")
    }
}

func expectTypeError(v: LispValue, what: String): Unit {
    match (v) {
        case Error(e) =>
            if (e.kind.name() != "TypeError") {
                @Fail("${what}：期望 TypeError，实际 ${e.kind.name()}（${e.message}）")
            }
        case _ => @Fail("${what}：期望 TypeError，实际正常值 ${v.toString()}")
    }
}

@Test
class ExternalArithmeticTest {
    /**
     * 外部值与外部值：逐元素
     */
    @TestCase
    func testBoxPlusBox() {
        println("\n=== box + box ===")
        let r = evalWithBoxes("(+ a b)", [("a", [1.0, 2.0, 3.0]), ("b", [10.0, 20.0, 30.0])])
        expectBox(r, [11.0, 22.0, 33.0], "(+ a b)")
        println("✓ 逐元素相加")
    }

    /**
     * 外部值在右：`(+ 10 a)` 走 `arithRight`
     */
    @TestCase
    func testScalarOnLeft() {
        println("\n=== 标量在左 ===")
        let r = evalWithBoxes("(+ 10 a)", [("a", [1.0, 2.0])])
        expectBox(r, [11.0, 12.0], "(+ 10 a)")
        println("✓ 标量在左也能算")
    }

    /**
     * **不可交换的运算符方向必须正确**
     *
     * `(- 10 a)` 是 `10 - a` 而不是 `a - 10`。这一条如果错了，结果看着"像数字"
     * 所以特别难发现 —— 断言里两个方向的值故意不对称。
     */
    @TestCase
    func testNonCommutativeDirection() {
        println("\n=== 减法 / 除法的方向 ===")
        let sub = evalWithBoxes("(- 10 a)", [("a", [1.0, 2.0, 4.0])])
        expectBox(sub, [9.0, 8.0, 6.0], "(- 10 a)")
        let div = evalWithBoxes("(/ 12 a)", [("a", [2.0, 3.0, 4.0])])
        expectBox(div, [6.0, 4.0, 3.0], "(/ 12 a)")
        // 反过来也要对
        let sub2 = evalWithBoxes("(- a 10)", [("a", [1.0, 2.0])])
        expectBox(sub2, [-9.0, -8.0], "(- a 10)")
        println("✓ 10 - a = 9/8/6，不是 -9/-8/-6")
    }

    /**
     * 一元 `(- a)` 是取负
     */
    @TestCase
    func testUnaryMinus() {
        println("\n=== 一元取负 ===")
        let r = evalWithBoxes("(- a)", [("a", [1.0, -2.0, 3.0])])
        expectBox(r, [-1.0, 2.0, -3.0], "(- a)")
        println("✓ (- a) 取负")
    }

    /**
     * N 元从左往右折叠，中间结果仍须是可算外部值
     */
    @TestCase
    func testFoldNary() {
        println("\n=== N 元折叠 ===")
        let r = evalWithBoxes("(+ a b c)",
            [("a", [1.0]), ("b", [2.0]), ("c", [3.0])])
        expectBox(r, [6.0], "(+ a b c)")
        // 减法的折叠顺序：(a - b) - c，不是 a - (b - c)
        let s = evalWithBoxes("(- a b c)",
            [("a", [10.0]), ("b", [1.0]), ("c", [2.0])])
        expectBox(s, [7.0], "(- a b c) = (10-1)-2 = 7")
        println("✓ 折叠，减法顺序 (a-b)-c")
    }

    /**
     * 长度不一致 → `None` → `TypeError`
     *
     * 静默算第一个元素或补零都是错的；宁可报错。
     */
    @TestCase
    func testMismatchedLengthIsError() {
        println("\n=== 长度不一致报错 ===")
        let r = evalWithBoxes("(+ a b)", [("a", [1.0, 2.0]), ("b", [1.0])])
        expectTypeError(r, "长度 2 + 长度 1")
        println("✓ 长度不一致返回 TypeError")
    }

    @TestCase
    func testUnsupportedOperandIsError() {
        println("\n=== 不支持的组合报错 ===")
        expectTypeError(evalWithBoxes("(+ a \"x\")", [("a", [1.0])]), "box + 字符串")
        expectTypeError(evalWithBoxes("(+ a '(1 2))", [("a", [1.0])]), "box + 列表")
        println("✓ box + 非数值 返回 TypeError")
    }

    @TestCase
    func testDivisionByZeroIsError() {
        println("\n=== 除零 ===")
        let r = evalWithBoxes("(/ a 0)", [("a", [1.0, 2.0])])
        expectTypeError(r, "box / 0")
        println("✓ 除零返回 TypeError（由实现方判定为不支持）")
    }
}

@Test
class ExternalReduceTest {
    /**
     * 四个归约都走 `LispExternalReducible`
     *
     * 关键在于 `sum` / `product` / `max` / `min` 此前**只认 Cons 链**，
     * 数组传进去会静默得到 0 / 1 —— 这条钉住分派已经接通。
     */
    @TestCase
    func testReductions() {
        println("\n=== sum / product / max / min ===")
        let data: Array<Float64> = [1.0, 2.0, 3.0, 4.0]
        expectFloat(evalWithBoxes("(sum a)", [("a", data)]), 10.0, "(sum a)")
        expectFloat(evalWithBoxes("(product a)", [("a", data)]), 24.0, "(product a)")
        expectFloat(evalWithBoxes("(max a)", [("a", data)]), 4.0, "(max a)")
        expectFloat(evalWithBoxes("(min a)", [("a", data)]), 1.0, "(min a)")
        println("✓ 四个归约都由外部值实现，不是 core 逐元素遍历")
    }

    /** 空数组的归约由实现方决定（返回 None → TypeError），core 不猜 */
    @TestCase
    func testEmptyReductionIsError() {
        println("\n=== 空 box 的归约 ===")
        expectTypeError(evalWithBoxes("(max a)", [("a", Array<Float64>())]), "空 box 求 max")
        println("✓ 空 box 的 max 报 TypeError（由实现方拒绝）")
    }
}

@Test
class ExternalElementwiseTest {
    /**
     * `(sin box)` 与 `(sin 1.0)` 是同一个符号
     *
     * 这正是「不搞两套名字」的落地验证。
     */
    @TestCase
    func testElementwiseMath() {
        println("\n=== 逐元素数学 ===")
        let r = evalWithBoxes("(sqrt a)", [("a", [4.0, 9.0, 16.0])])
        expectBox(r, [2.0, 3.0, 4.0], "(sqrt a)")
        let e = evalWithBoxes("(exp a)", [("a", [0.0])])
        expectBox(e, [1.0], "(exp a)")
        let ab = evalWithBoxes("(abs a)", [("a", [-3.0, 4.0])])
        expectBox(ab, [3.0, 4.0], "(abs a)")
        println("✓ sqrt / exp / abs 都走 LispExternalElementwise")
    }

    /** 实现方不认识的函数 → None → TypeError，不能悄悄返回原值 */
    @TestCase
    func testUnknownElementwiseIsError() {
        println("\n=== 不认识的逐元素函数 ===")
        expectTypeError(evalWithBoxes("(log a)", [("a", [1.0])]), "(log box)")
        println("✓ log 返回 TypeError（替身没实现 log）")
    }

    /** 标量路径不受影响：同一个 sin 对数字仍正常 */
    @TestCase
    func testScalarStillWorks() {
        println("\n=== 标量路径不受影响 ===")
        expectFloat(evalWithBoxes("(sqrt 4)", []), 2.0, "(sqrt 4)")
        expectFloat(evalWithBoxes("(sin 0)", []), 0.0, "(sin 0)")
        println("✓ 同一个符号对数字走数值路径")
    }
}


/**
 * 三个后端的分派行为必须一致
 *
 * 0.9.0 发布时漏了这一组：AST 走 `NativeFunc` 能算，字节码把 `+ - * /` 编成
 * `OP_ADD` 等指令、VM 的 `binNum` 只认数字，于是同一段代码在 BC 下报
 * `expected a number`。修复见 `xisp_bc/src/vm.cj` 的 `binNumFallback`。
 *
 * 教训：**外部值分派横跨 AST / BC / JIT 三条路径**，只测 AST 等于没测。
 */
@Test
class ExternalDispatchAllBackendsTest {
    @TestCase
    func testArithAllBackends() {
        println("\n=== 算术分派三后端一致 ===")
        let boxes: Array<(String, Array<Float64>)> =
            [("a", [1.0, 2.0, 3.0]), ("b", [10.0, 20.0, 30.0])]
        for (backend in ["AST", "BC", "JIT"]) {
            let sum = evalWithBoxesOn("(+ a b)", boxes, backend)
            expectBox(sum, [11.0, 22.0, 33.0], "[${backend}] (+ a b)")
            // 非交换运算的方向
            let sub = evalWithBoxesOn("(- 10 a)", boxes, backend)
            expectBox(sub, [9.0, 8.0, 7.0], "[${backend}] (- 10 a)")
            // 一元取负
            let neg = evalWithBoxesOn("(- a)", boxes, backend)
            expectBox(neg, [-1.0, -2.0, -3.0], "[${backend}] (- a)")
            // N 元折叠
            let fold = evalWithBoxesOn("(+ a b b)", boxes, backend)
            expectBox(fold, [21.0, 42.0, 63.0], "[${backend}] (+ a b b)")
        }
        println("✓ AST / BC / JIT 三个后端的算术分派一致")
    }

    @TestCase
    func testReduceAllBackends() {
        println("\n=== 归约分派三后端一致 ===")
        let boxes: Array<(String, Array<Float64>)> = [("a", [1.0, 2.0, 3.0, 4.0])]
        for (backend in ["AST", "BC", "JIT"]) {
            expectFloat(evalWithBoxesOn("(sum a)", boxes, backend), 10.0, "[${backend}] sum")
            expectFloat(evalWithBoxesOn("(max a)", boxes, backend), 4.0, "[${backend}] max")
        }
        println("✓ AST / BC / JIT 的归约分派一致")
    }

    /**
     * 逐元素分派三后端一致
     *
     * `sin` / `sqrt` 没有编译期特化（走普通函数调用），理论上一致，但
     * `withBytecodeCompiler()` 下函数调用路径不同，仍要验。
     */
    @TestCase
    func testElementwiseAllBackends() {
        println("\n=== 逐元素分派三后端一致 ===")
        let boxes: Array<(String, Array<Float64>)> = [("a", [4.0, 9.0])]
        for (backend in ["AST", "BC", "JIT"]) {
            expectBox(evalWithBoxesOn("(sqrt a)", boxes, backend), [2.0, 3.0], "[${backend}] sqrt")
        }
        println("✓ AST / BC / JIT 的逐元素分派一致")
    }
}
