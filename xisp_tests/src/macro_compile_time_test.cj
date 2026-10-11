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
 * 编译前宏展开
 *
 * 宏是语法层的：必须在编译前展开，否则字节码后端会把它编译成"运行时调宏"
 * —— 每次调用重新展开，且参数已被求值为值（与 AST 语义可能不等价）。
 *
 * 本测试锁定三条不变量：
 * 1. **展开时机**：含宏的函数在**定义时**展开一次，调用不再展开；
 * 2. **语义等价**：同一段代码在 AST 与字节码下结果一致；
 * 3. **边界正确**：`quote` / `quasiquote` 内是数据，不得展开。
 */
@Test
class MacroCompileTimeExpansionTest {
    /** 在指定后端下求值整段代码，返回最后一个表达式的结果 */
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

    /**
     * 核心不变量：宏在**编译期**展开，而非每次调用重新展开。
     *
     * 手法：宏体对计数器自增。修复前字节码路径下调用 N 次就展开 N 次
     * （实测 50 次调用 → 计数 50）；修复后定义时展开一次，此后恒定。
     */
    @TestCase
    func testMacroExpandsAtDefinitionNotPerCall() {
        println("\n=== 宏展开时机：定义时一次，调用不再展开 ===")
        let program = """
            (define expansion-count 0)
            (defmacro probe-macro (x)
              (begin (set! expansion-count (+ expansion-count 1)) x))
            (define (f) (probe-macro 42))
        """
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let ev = Evaluator(env)
        ev.setExecutionBackend(BytecodeExecutionBackend())
        ev.setMaxStackDepth(100000)
        for (expr in Parser.parseFromString(program)) {
            ev.eval(expr)
        }
        let afterDefine = ev.eval(Parser.parseSingleFromString("expansion-count")).toString()
        @Assert(afterDefine == "1")
        println("✓ 定义后展开次数 = " + afterDefine)

        let callExpr = Parser.parseSingleFromString("(f)")
        for (_ in 0..50) {
            ev.eval(callExpr)
        }
        let after = ev.eval(Parser.parseSingleFromString("expansion-count")).toString()
        @Assert(after == "1")
        println("✓ 调用 50 次后展开次数仍 = " + after + "（未随调用增长）")
    }

    /**
     * 语义等价：宏出现在各种位置时，AST 与字节码结果必须一致。
     *
     * 这条针对的是修复前 VM `callMacro` 的隐患：编译路径下宏参数**已被求值**，
     * VM 只能把值 quote 回去冒充语法 —— 对"把参数当语法分析"的宏就会不等价。
     */
    @TestCase
    func testMacroSemanticsMatchAst() {
        println("\n=== AST 与字节码语义一致 ===")
        let cases = [
            "(define (f) (when #t 1)) (f)",
            "(define (f x) (if x (when #t 10) (unless #f 20))) (f #t)",
            "(define (f x) (if x (when #t 10) (unless #f 20))) (f #f)",
            "(when #t (define (g) 7)) (g)",
            "(define (f) (+ 1 (when #t 2))) (f)",
            "(define (f) (let ((x (when #t 5))) x)) (f)",
            "(defmacro m2 (a b) (list (quote +) a b)) (define (f) (m2 3 4)) (f)",
            "(define (f) (when #t (unless #f 9))) (f)",
        ]
        for (code in cases) {
            let ast = this.evalAll(code, false)
            let bc = this.evalAll(code, true)
            @Assert(ast == bc)
            println("✓ " + code + "  =>  " + ast)
        }
    }

    /**
     * 边界：`quote` / `quasiquote` 内是**数据**，不得被展开。
     *
     * 这是不复用 `macroExpandAll` 的原因 —— 它会把 `(quote (when …))` 展平。
     */
    @TestCase
    func testQuoteIsNotExpanded() {
        println("\n=== quote 内是数据，不展开 ===")
        // quote 内的宏调用应保持原样（符号不被求值、也不展开宏）
        let quoted = this.evalAll("(quote (when a b))", true)
        @Assert(quoted == "(when a b)")
        println("✓ (quote (when a b)) => " + quoted)

        // 嵌套 quote 同样保持
        let nested = this.evalAll("(define x (quote (when a b))) x", true)
        @Assert(nested == "(when a b)")
        println("✓ 经 define 的 quote 保持 => " + nested)

        // 宏在数据位置不能被"抢先展开"，否则拼接语法会错
        let build = this.evalAll("""
            (defmacro mk (x) (list (quote quote) x))
            (mk (when a b))
        """, true)
        @Assert(build == "(when a b)")
        println("✓ 宏产出的 quote 数据未被二次展开 => " + build)
    }

    /**
     * 展开产物可编译：含宏的函数应仍被编译成闭包（不退化为 AST 过程）。
     */
    @TestCase
    func testMacroFunctionStillCompiled() {
        println("\n=== 含宏的函数仍然被编译 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let ev = Evaluator(env)
        ev.setExecutionBackend(BytecodeExecutionBackend())
        ev.eval(Parser.parseSingleFromString("(define (f) (when #t 42))"))
        let f = ev.eval(Parser.parseSingleFromString("f"))
        match (f) {
            case LispValue.Closure(_, _) =>
                println("✓ f 是 Closure（已编译，未退化为 AST 过程）")
            case other =>
                @Fail("期望 Closure，实际 ${other}")
        }
        let r = ev.eval(Parser.parseSingleFromString("(f)"))
        @Assert(r.toString() == "42")
        println("✓ (f) = " + r.toString())
    }
}
