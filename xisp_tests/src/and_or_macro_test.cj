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
 * `and` / `or` 作为**宏**的语义一致性
 *
 * 二者曾是注册函数（`NativeFunc`），实参先全部求值 —— 既不短路、也不容忍后续实参出错。
 * 字节码后端却按"编译期展开成跳转"处理，于是两个后端语义分歧（实测 4 处不一致）：
 *
 *   (and 0 (undefined-fn))   AST → [UndefinedFunction]    BC → false
 *   (and 1 0)                AST → false（丢掉了 0 本身）  BC → 0
 *   (or)                     AST → false                  BC → nil
 *
 * 改为宏后两后端走同一份展开产物。本测试同时锁定：
 * 1. AST 与字节码**逐例一致**；
 * 2. 短路真的生效（后续实参不被求值、不出错）；
 * 3. 首项只求值一次（`or` 用 gensym 暂存，而非重复展开）。
 */
@Test
class AndOrMacroTest {
    func evalAll(code: String, useBc: Bool): String {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let ev = Evaluator(env)
        if (useBc) {
            ev.setExecutionBackend(BytecodeExecutionBackend())
        }
        var result: LispValue = Nil
        for (expr in Parser.parseFromString(code)) {
            result = ev.eval(expr)
        }
        result.toString()
    }

    /**
     * 核心不变量：同一表达式在两个后端下结果一致。
     *
     * 修复前这里会有 4 处不一致；表里保留了当初分歧的那几例。
     */
    @TestCase
    func testAstAndBytecodeAgree() {
        println("\n=== AST 与字节码逐例一致 ===")
        let cases = [
            "(and)",
            "(or)",
            "(and 1 2)",
            "(or 1 2)",
            "(and 1 0)",
            "(or 0 1)",
            "(and 0 1)",
            "(or 0 0)",
            "(and #f #t)",
            "(or #f #t)",
            "(and 1 2 3)",
            "(or 0 0 3)",
            "(and nil 1)",
            "(or nil 2)",
        ]
        for (code in cases) {
            let ast = this.evalAll(code, false)
            let bc = this.evalAll(code, true)
            @Assert(ast == bc)
            println("✓ " + code + "  =>  " + ast)
        }
    }

    /**
     * 短路：后续实参**不得被求值**。用未定义函数作探针 —— 修复前 AST 会报错。
     */
    @TestCase
    func testShortCircuitSkipsLaterArgs() {
        println("\n=== 短路：后续实参不被求值 ===")
        let cases = [
            "(and 0 (undefined-fn-xyz))",
            "(and #f (undefined-fn-xyz))",
            "(and nil (undefined-fn-xyz))",
            "(or 1 (undefined-fn-xyz))",
            "(or #t (undefined-fn-xyz))",
        ]
        for (code in cases) {
            for (useBc in [false, true]) {
                let r = this.evalAll(code, useBc)
                @Assert(!r.contains("UndefinedFunction"))
                @Assert(!r.contains("Error"))
            }
            let ast = this.evalAll(code, false)
            let bc = this.evalAll(code, true)
            @Assert(ast == bc)
            println("✓ " + code + "  =>  " + ast + "（两后端一致，未求值后续实参）")
        }
    }

    /**
     * 副作用只发生一次：`or` 的首项用 gensym 暂存，不重复展开。
     */
    @TestCase
    func testFirstOperandEvaluatedOnce() {
        println("\n=== 首项只求值一次 ===")
        let orCode = """
            (define n 0)
            (define (bump) (set! n (+ n 1)) 5)
            (or (bump) 9)
            n
        """
        let andCode = """
            (define m 0)
            (define (bump2) (set! m (+ m 1)) 5)
            (and (bump2) 9)
            m
        """
        for (useBc in [false, true]) {
            let r = this.evalAll(orCode, useBc)
            @Assert(r == "1")
            let a = this.evalAll(andCode, useBc)
            @Assert(a == "1")
        }
        println("✓ or / and 首项副作用各 1 次")

        // 非卫生陷阱：用户变量不得被宏内部符号捕获（or 用 gensym）
        let capture = """
            (let ((g 5)) (or #f g))
        """
        let r = this.evalAll(capture, false)
        @Assert(r == "5")
        println("✓ (let ((g 5)) (or #f g)) = " + r + "（未被宏内部符号捕获）")
    }

    /**
     * 返回值语义：`and` 返回最后一个真值，`or` 返回第一个真值；全假归 `false`。
     */
    @TestCase
    func testReturnValues() {
        println("\n=== 返回值语义 ===")
        let cases: Array<(String, String)> = [
            ("(and)", "true"),
            ("(or)", "false"),
            ("(and 1 2 3)", "3"),
            ("(or 0 0 3)", "3"),
            ("(and 1 0)", "false"),
            ("(or 0 0)", "false"),
            ("(and nil 1)", "false"),
            ("(or nil 2)", "2"),
        ]
        for ((code, expected) in cases) {
            let r = this.evalAll(code, false)
            @Assert(r == expected)
            println("✓ " + code + " = " + r)
        }
    }

    /**
     * `and`/`or` 现在是宏，不再是 `procedure?` 为真的函数值 —— 这是**有意的**归属变化：
     * 它们需要控制求值（短路），属语法而非函数。
     */
    @TestCase
    func testIsNowMacroNotFunction() {
        println("\n=== and/or 现在是宏 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        for (name in ["and", "or"]) {
            match (env.lookup(name)) {
                case LispValue.Macro(_, _, _) => println("✓ " + name + " 是 Macro")
                case LispValue.MacroFromParams(_, _, _) => println("✓ " + name + " 是 MacroFromParams")
                case other => @Fail(name + " 应为宏，实际 " + other.toString())
            }
        }
    }
}
