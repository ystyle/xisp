package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp_bc.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*

import std.unittest.*
import std.unittest.testmacro.*

/**
 * 派生形式降级为宏：`condb` / `if-let` / `let*` / `when-let*`
 *
 * 这四个原本是求值器里的特殊形式（`trySpecialForm` 的 case），但它们的实现
 * 本身就是"展开成更基本的形状"（`if-let` 展开成 `let`+`if`；`let*` 展开成嵌套 `let`）
 * —— 属**纯派生语法**，归到④层（宏）比留在①层更准确。
 *
 * 降级的前提是宏在**编译前**展开（见 macro_compile_time_test.cj）：否则含这些形式的
 * 函数都会让字节码整树回退 AST。
 *
 * 本测试锁定：语义与降级前一致（两个后端逐例相同）+ 确为宏类型。
 */
@Test
class DerivedFormsAsMacroTest {
    func evalAll(code: String, useBc: Bool): String {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let ev = Evaluator(env)
        if (useBc) {
            ev.setExecutionBackend(BytecodeExecutionBackend())
        }
        ev.setMaxStackDepth(100000)
        var result: LispValue = Nil
        for (expr in Parser.parseFromString(code)) {
            result = ev.eval(expr)
        }
        result.toString()
    }

    /** 每个用例都要求 AST 与字节码结果一致 */
    func assertBothBackends(code: String, expected: String): Unit {
        let ast = this.evalAll(code, false)
        let bc = this.evalAll(code, true)
        @Assert(ast == expected)
        @Assert(bc == expected)
    }

    @TestCase
    func testLetStar() {
        println("\n=== let* 顺序绑定 ===")
        this.assertBothBackends("(let* ((a 1) (b (+ a 1))) (+ a b))", "3")
        println("✓ (let* ((a 1) (b (+ a 1))) (+ a b)) = 3")
        this.assertBothBackends("(let* ((a 1) (b (+ a 2)) (c (* b 3))) c)", "9")
        println("✓ 三绑定 = 9")
        this.assertBothBackends("(let* ((a 5)) (* a a))", "25")
        println("✓ 单绑定 = 25")
        this.assertBothBackends("(let* () (+ 1 2))", "3")
        println("✓ 空绑定 = 3")
        // 顺序绑定的关键：后一个能用前一个；而 let 是并行的
        this.assertBothBackends("(let ((a 1)) (let* ((b a) (c b)) c))", "1")
        println("✓ 能引用外层变量 = 1")
    }

    @TestCase
    func testIfLet() {
        println("\n=== if-let ===")
        this.assertBothBackends("(if-let (x 7) (+ x 1) 0)", "8")
        println("✓ 真值分支 = 8")
        this.assertBothBackends("(if-let (x 0) x 100)", "100")
        println("✓ 假值走 else = 100")
        // 无 else 分支
        this.assertBothBackends("(if-let (x 0) x)", "nil")
        println("✓ 无 else = nil")
    }

    @TestCase
    func testWhenLetStar() {
        println("\n=== when-let* ===")
        this.assertBothBackends("(when-let* ((a 1) (b (+ a 1))) (+ a b))", "3")
        println("✓ 两绑定全真 = 3")
        // 语义要点：全部绑定都建立，只判**最后一个**值的真值
        this.assertBothBackends("(when-let* ((y 0)) (+ y 1))", "nil")
        println("✓ 末值为假 → nil")
        this.assertBothBackends("(when-let* () 42)", "nil")
        println("✓ 空绑定 → nil")
        // 词法作用域：不得污染外层
        this.assertBothBackends("(define a 99) (when-let* ((a 1)) a)", "1")
        println("✓ 词法作用域隔离 = 1")
        this.assertBothBackends("(define a 99) (begin (when-let* ((a 1)) a) a)", "99")
        println("✓ 外层 a 未被污染 = 99")
        // 多 body
        this.assertBothBackends("(when-let* ((a 2)) a (* a 10))", "20")
        println("✓ 多 body 取末个 = 20")
    }

    @TestCase
    func testCondb() {
        println("\n=== condb ===")
        // 基础：条件 / 结果 对
        this.assertBothBackends("(condb #t 1 else 2)", "1")
        println("✓ (condb #t 1 else 2) = 1")
        this.assertBothBackends("(condb #f 1 else 2)", "2")
        println("✓ 走 else = 2")
        // 多分支，顺序匹配
        this.assertBothBackends("(condb #f 1 #t 2 else 3)", "2")
        println("✓ 第二个条件命中 = 2")
        // :let 绑定参与后续条件
        this.assertBothBackends("(condb (:let x 5) (> x 3) \"large\" else \"small\")", "\"large\"")
        println("✓ (:let x 5) + (> x 3) = large")
        this.assertBothBackends("(condb (:let x 1) (> x 3) \"large\" else \"small\")", "\"small\"")
        println("✓ 条件不成立 → small")
        // 多个 :let，顺序绑定
        this.assertBothBackends("(condb (:let a 1) (:let b (+ a 1)) (= b 2) \"ok\" else \"no\")", "\"ok\"")
        println("✓ 多个 :let 顺序绑定 = ok")
        // 全假且无 else → nil
        this.assertBothBackends("(condb #f 1 #f 2)", "nil")
        println("✓ 全假无 else = nil")
    }

    /**
     * 归属断言：这四个不再是特殊形式，而是宏。
     *
     * 直接查环境里的绑定类型 —— 特殊形式根本不在环境里，宏则可见。
     */
    @TestCase
    func testAreMacrosNow() {
        println("\n=== 四个派生形式已归入④层（宏） ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        for (name in ["condb", "if-let", "let*", "when-let*", "and", "or"]) {
            match (env.lookup(name)) {
                case LispValue.Macro(_, _, _) => println("✓ " + name + " 是 Macro")
                case LispValue.MacroFromParams(_, _, _) => println("✓ " + name + " 是 MacroFromParams")
                case other => @Fail(name + " 应为宏，实际 " + other.toString())
            }
        }
        // 对照：①层核心原语不该在环境里（未被注册）
        for (name in ["if", "quote", "lambda", "define"]) {
            match (env.lookup(name)) {
                case LispValue.Error(_) => println("✓ " + name + " 仍是①层原语（不在环境中）")
                case other => @Fail(name + " 不应被注册为值，实际 " + other.toString())
            }
        }
    }
}
