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
 * 内建宏包（④层，能力域 `macros.core`）
 *
 * 这一层的意义是"**lisp 实现 lisp 所有**"：`when` / `unless` / `do` / `defun`
 * 这些"语法"不是仓颉硬编码的，而是十几行 **Lisp 源码**，启动时装进环境。
 *
 * 本测试锁定两件事：
 * 1. 16 个内建宏的语义（含 `gensym` 修掉 `swap` 的卫生性隐患）；
 * 2. 宏包**可被整体替换** —— 用户用自己的同名宏顶掉 core 的实现。
 */
@Test
class MacroPackTest {
    func evalOne(code: String): String {
        let it = LispInterpreter([withStdLib(), withQuietMode()])
        it.eval(code).toString()
    }

    /** 宏包声明的符号清单应与实际注册的一致（防清单漂移） */
    @TestCase
    func testSymbolsMatchRegistrations() {
        println("\n=== 宏包符号清单与实现一致 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        for (name in BuiltinMacros.allSymbols()) {
            match (env.lookup(name)) {
                case LispValue.Macro(_, _, _) => ()
                case LispValue.MacroFromParams(_, _, _) => ()
                case other => @Fail(name + " 应为宏，实际 " + other.toString())
            }
        }
        println("✓ 全部 " + BuiltinMacros.allSymbols().size.toString() + " 个符号均已注册为宏")
    }

    /** 条件类宏 */
    @TestCase
    func testConditionalMacros() {
        println("\n=== 条件类 ===")
        @Assert(this.evalOne("(when #t 42)") == "42")
        @Assert(this.evalOne("(when #f 42)") == "nil")
        @Assert(this.evalOne("(unless #f 43)") == "43")
        @Assert(this.evalOne("(unless #t 43)") == "nil")
        @Assert(this.evalOne("(condb #t 1 else 2)") == "1")
        @Assert(this.evalOne("(condb #f 1 else 2)") == "2")
        @Assert(this.evalOne("(if-let (x 7) (+ x 1) 0)") == "8")
        @Assert(this.evalOne("(if-let (x 0) x 100)") == "100")
        @Assert(this.evalOne("(when-let* ((a 1) (b 2)) (+ a b))") == "3")
        println("✓ when / unless / condb / if-let / when-let* 语义正确")
    }

    /** 赋值糖与列表糖 */
    @TestCase
    func testSugarMacros() {
        println("\n=== 赋值糖 / 列表糖 ===")
        @Assert(this.evalOne("(define x 5) (incf x) x") == "6")
        @Assert(this.evalOne("(define y 5) (decf y) y") == "4")
        @Assert(this.evalOne("(negate 5)") == "-5")
        @Assert(this.evalOne("(define a 1) (define b 2) (swap a b) (list a b)") == "(2 1)")
        @Assert(this.evalOne("(define l (quote (2 3))) (push 1 l)") == "(1 2 3)")
        @Assert(this.evalOne("(pop (quote (1 2 3)))") == "(2 3)")
        @Assert(this.evalOne("(do 1 2 3)") == "3")
        @Assert(this.evalOne("(defun f (x) (* x 2)) (f 21)") == "42")
        println("✓ incf / decf / negate / swap / push / pop / do / defun 语义正确")
    }

    /**
     * `swap` 用 `gensym` 取临时符号。
     *
     * 原实现用固定符号 `temp` —— 非卫生宏下会被调用点的同名变量捕获。
     * 本测试用"用户自己有 temp"的场景锁定修复。
     */
    @TestCase
    func testSwapIsHygienic() {
        println("\n=== swap 的卫生性（用 gensym） ===")
        // 用户作用域里已有 temp；swap 内部的临时符号不得与之冲突
        let r = this.evalOne("""
            (let ((temp 99) (a 1) (b 2))
              (begin (swap a b) (list a b temp)))
        """)
        @Assert(r == "(2 1 99)")
        println("✓ 用户 temp 未被污染: " + r)
    }

    /**
     * 宏包可被整体替换：用户定义同名宏即顶掉 core 的实现。
     *
     * 这是"可替换"在④层（纯 Lisp 源码层）的直接体现 —— 无需任何求值器改动。
     */
    @TestCase
    func testMacroPackIsReplaceable() {
        println("\n=== 宏包可被用户替换 ===")
        let it = LispInterpreter([withStdLib(), withQuietMode()])
        it.eval("(defmacro when (t . b) 999)")
        @Assert(it.eval("(when #t 1 2)").toString() == "999")
        println("✓ 用户 defmacro when 顶掉内建实现 = 999")

        // 只替换其中一个，其余不受影响
        let it2 = LispInterpreter([withStdLib(), withQuietMode()])
        it2.eval("(defmacro unless (t . b) 888)")
        @Assert(it2.eval("(unless #f 1)").toString() == "888")
        @Assert(it2.eval("(when #t 42)").toString() == "42")
        println("✓ 只换 unless，when 不受影响")
    }

    /**
     * `and` / `or` 由宏实现后，短路在两个后端一致（此前的 4 处分歧已归零）。
     */
    @TestCase
    func testAndOrShortCircuitBothBackends() {
        println("\n=== and/or 短路（双后端一致） ===")
        let code = "(and 0 (undefined-fn-xyz))"
        let code2 = "(or 1 (undefined-fn-xyz))"
        for (c in [code, code2]) {
            var ast = ""
            var bc = ""
            for (useBc in [false, true]) {
                let it = LispInterpreter([
                    withStdLib(), withQuietMode(), withPreset(presetFull())
                ])
                if (useBc) {
                    it.setExecutionBackend(BytecodeExecutionBackend())
                }
                let r = it.eval(c).toString()
                if (useBc) { bc = r } else { ast = r }
            }
            @Assert(ast == bc)
            @Assert(!ast.contains("UndefinedFunction"))
            println("✓ " + c + " => " + ast + "（两后端一致，未求值后续实参）")
        }
    }
}
