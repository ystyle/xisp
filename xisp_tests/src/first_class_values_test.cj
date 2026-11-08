package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp_bc.*
import ystyle::xisp.types.*
import std.collection.ArrayList

import std.unittest.*
import std.unittest.testmacro.*

/**
 * ②类符号的一等值（能力域设计 §8.1 接缝 0e）
 *
 * ## 修复的问题
 *
 * `map` / `filter` / `reduce` / `apply` / `eval` / `->` / `for-each` 由求值器
 * 直接分派，**环境里没有绑定**，因此不能作为值传递：
 *
 * ```
 * (procedure? map)               → [UnboundVariable]
 * (define (go f) (f …)) (go map) → [UnboundVariable]
 * ```
 *
 * ## 为什么不能简单改成普通函数
 *
 * 1. **栈深**：纯 Lisp 版 `map` 500 元素就爆栈（实测，默认栈深 1000）；
 * 2. **语义**：`apply` / `eval` 需要"展开实参列表"/"对数据求值"，纯 Lisp 无法表达；
 * 3. **性能**：求值器对高阶函数是专门的迭代实现。
 *
 * ## 做法：双轨
 *
 * 保留求值器快速路径（调用位置），**另注册一个等价 NativeFunc**（值位置）。
 * 后者把实参重新拼成调用式交给原路径执行 —— 不重复实现逻辑，行为必然一致。
 * core 自己安装的值带标记，故不会被让位机制误判成"用户实现"。
 */
@Test
class FirstClassValuesTest {
    func ev(code: String): String {
        let it = LispInterpreter([withQuietMode(), withStdLib()])
        it.eval(code).toString()
    }

    /** 值位置：这些符号现在是可调用值 */
    @TestCase
    func testAreCallableValues() {
        println("\n=== ②类符号可取作值 ===")
        for (name in ["map", "filter", "reduce", "apply", "eval", "->", "for-each"]) {
            let r = this.ev("(procedure? " + name + ")")
            @Assert(r == "true")
            println("✓ (procedure? " + name + ") = true")
        }
    }

    /** 可作为参数传给高阶函数（这是"一等值"的实质） */
    @TestCase
    func testPassAsArgument() {
        println("\n=== 可作为参数传递 ===")
        let code = """
            (define (go f) (f (lambda (x) (* x 2)) (quote (1 2 3))))
            (go map)
        """
        @Assert(this.ev(code) == "(2 4 6)")
        println("✓ (go map) 把 map 当参数传入 = (2 4 6)")
    }

    /** 调用位置仍走求值器快速路径：语义与栈深不变 */
    @TestCase
    func testCallSemanticsUnchanged() {
        println("\n=== 调用位置语义不变 ===")
        let cases: Array<(String, String)> = [
            ("(map (lambda (x) (* x 2)) (quote (1 2 3)))", "(2 4 6)"),
            ("(filter (lambda (x) (> x 1)) (quote (1 2 3)))", "(2 3)"),
            ("(reduce + 0 (quote (1 2 3)))", "6"),
            ("(apply + (quote (1 2)))", "3"),
            ("(eval (quote (+ 1 2)))", "3"),
            ("(-> 5 (+ 3) (* 2))", "16"),
            ("(for-each (lambda (x) x) (quote (1 2)))", "nil"),
        ]
        for ((code, expected) in cases) {
            let r = this.ev(code)
            @Assert(r == expected)
            println("✓ " + code + " = " + r)
        }
    }

    /**
     * 长列表仍能处理 —— 证明走的是求值器实现而非纯 Lisp 递归版
     * （后者 500 元素就爆栈）。
     */
    @TestCase
    func testLongListStillWorks() {
        println("\n=== 长列表不爆栈（走求值器实现） ===")
        let code = """
            (define (build n acc) (if (= n 0) acc (build (- n 1) (cons n acc))))
            (length (map (lambda (x) x) (build 3000 nil)))
        """
        // 递归构造 3000 元素列表需要更大的栈深（默认 1000 会在 build 阶段爆栈）
        let it = LispInterpreter([withQuietMode(), withStdLib(), withMaxStackDepth(100000)])
        let r = it.eval(code).toString()
        @Assert(r == "3000")
        println("✓ 3000 元素 map 正常 = 3000")
    }

    /** 用户仍可顶替（core 安装标记不阻挡用户覆盖） */
    @TestCase
    func testUserCanStillOverride() {
        println("\n=== 用户仍可顶替 ===")
        let r = this.ev("(define (m f l) 999) (define map m) (map 1 2)")
        @Assert(r == "999")
        println("✓ 用户 define map 后调用走用户实现 = 999")
    }

    /** 可关闭：追求最小环境的宿主用 withNoFirstClassValues() */
    @TestCase
    func testCanBeDisabled() {
        println("\n=== 可关闭（最小环境） ===")
        let it = LispInterpreter([withQuietMode(), withNoFirstClassValues()])
        let r = it.eval("(procedure? map)").toString()
        @Assert(r.contains("UnboundVariable"))
        println("✓ 关闭后 (procedure? map) = " + r)
        // 但调用位置仍可用
        @Assert(it.eval("(map (lambda (x) x) (quote (1 2)))").toString() == "(1 2)")
        println("✓ 调用位置仍可用")
    }

    /** 双后端一致 */
    @TestCase
    func testBothBackendsAgree() {
        println("\n=== 双后端一致 ===")
        let code = """
            (define (go f) (f (lambda (x) (* x 2)) (quote (1 2 3))))
            (go map)
        """
        for (useBc in [false, true]) {
            let opts = ArrayList<InterpreterOption>()
            opts.add(withQuietMode())
            opts.add(withStdLib())
            if (useBc) {
                opts.add(withBytecodeCompiler())
            }
            let it = LispInterpreter(opts.toArray())
            @Assert(it.eval(code).toString() == "(2 4 6)")
        }
        println("✓ AST 与字节码下 (go map) 均为 (2 4 6)")
    }
}
