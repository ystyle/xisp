package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp.types.*
import ystyle::xisp.core.builtin.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.parser.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.HashSet

/**
 * `gensym` —— 非卫生宏的临时符号来源
 *
 * `defmacro` 与调用点共享符号空间，宏体里引入的临时变量会捕获用户的同名变量。
 * 卫生宏由语言机制解决；非卫生宏的通行解法是由宏在展开期向运行时索要唯一符号。
 */
@Test
class GensymTest {
    func evalCode(code: String): LispValue {
        let it = LispInterpreter([withStdLib(), withQuietMode()])
        it.eval(code)
    }

    @TestCase
    func testGensymIsDefined() {
        println("\n=== gensym 已注册 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        var bound = false
        match (env.lookup("gensym")) {
            case LispValue.NativeFunc(_) => bound = true
            case _ => ()
        }
        @Assert(bound)
        println("✓ registerAll 后 gensym 有绑定")
    }

    @TestCase
    func testGensymIsDefinedInPure() {
        println("\n=== gensym 在纯函数子集里同样可用 ===")
        // 宏展开属于"表达式能力"，pure 环境必须能造宏，否则宏包不可用
        let env = Environment()
        BuiltinFunctions.registerPure(env)
        var bound = false
        match (env.lookup("gensym")) {
            case LispValue.NativeFunc(_) => bound = true
            case _ => ()
        }
        @Assert(bound)
        println("✓ registerPure 后 gensym 有绑定")
    }

    @TestCase
    func testGensymReturnsSymbol() {
        println("\n=== gensym 返回符号 ===")
        let r = this.evalCode("(gensym)")
        match (r) {
            case LispValue.Symbol(s) =>
                @Assert(s.startsWith("#:g"))
                println("✓ (gensym) = " + s)
            case _ => @Fail("期望 Symbol，实际 ${r}")
        }
    }

    @TestCase
    func testGensymIsUnique() {
        println("\n=== gensym 每次都不同 ===")
        let it = LispInterpreter([withStdLib(), withQuietMode()])
        let seen = HashSet<String>()
        for (_ in 0..200) {
            match (it.eval("(gensym)")) {
                case LispValue.Symbol(s) =>
                    @Assert(!seen.contains(s))
                    seen.add(s)
                case other => @Fail("期望 Symbol，实际 ${other}")
            }
        }
        @Assert(seen.size == 200)
        println("✓ 200 次调用无重复")
    }

    @TestCase
    func testGensymWithPrefix() {
        println("\n=== gensym 可带前缀（仅用于可读性） ===")
        let r = this.evalCode("(gensym \"tmp\")")
        match (r) {
            case LispValue.Symbol(s) =>
                @Assert(s.startsWith("#:g"))
                @Assert(s.endsWith("tmp"))
                println("✓ (gensym \"tmp\") = " + s)
            case _ => @Fail("期望 Symbol，实际 ${r}")
        }
        // 前缀不参与唯一性：同样前缀两次仍不同
        let a = this.evalCode("(gensym \"x\")")
        let b = this.evalCode("(gensym \"x\")")
        @Assert(a.toString() != b.toString())
        println("✓ 同前缀两次不同: " + a.toString() + " / " + b.toString())
    }

    /**
     * 核心用例：非卫生宏的变量捕获，以及 gensym 如何修掉它。
     */
    @TestCase
    func testUnhygienicMacroCapturesUserVariable() {
        println("\n=== 复现：非卫生宏捕获用户变量 ===")
        // my-or 用固定符号 tmp 暂存，用户恰好也叫 tmp 时被捕获
        let code = """
            (defmacro my-or (a b)
              (list (quote let) (list (list (quote tmp) a))
                    (list (quote if) (quote tmp) (quote tmp) b)))
            (let ((tmp 5)) (my-or #f tmp))
        """
        let r = this.evalCode(code)
        // 期望 5（用户的 tmp）；实际被宏内部的 tmp 覆盖 => false
        @Assert(r.toString() == "false")
        println("✓ 复现捕获: 期望 5，实际 ${r}")
    }

    @TestCase
    func testGensymFixesCapture() {
        println("\n=== gensym 修掉捕获 ===")
        // 同样的宏，临时符号改用 gensym
        let code = """
            (defmacro my-or (a b)
              (let ((t (gensym)))
                (list (quote let) (list (list t a))
                      (list (quote if) t t b))))
            (let ((tmp 5)) (my-or #f tmp))
        """
        let r = this.evalCode(code)
        @Assert(r.toString() == "5")
        println("✓ 用 gensym 后正确返回用户的 tmp = ${r}")
    }

    @TestCase
    func testGensymAvoidsDoubleEvaluation() {
        println("\n=== gensym 支撑「避免二次求值」的经典展开 ===")
        // (my-or a b) 若展开成 (if a a b)，a 有副作用时会被求值两次
        // 用 gensym 暂存即可只求值一次
        let code = """
            (define counter 0)
            (define (bump) (set! counter (+ counter 1)) counter)
            (defmacro my-or (a b)
              (let ((t (gensym)))
                (list (quote let) (list (list t a))
                      (list (quote if) t t b))))
            (my-or (bump) 99)
            counter
        """
        let it = LispInterpreter([withStdLib(), withQuietMode()])
        it.eval(code)
        let counter = it.eval("counter")
        @Assert(counter.toString() == "1")
        println("✓ 副作用只发生一次: counter = ${counter}")
    }

    @TestCase
    func testGensymInPureEnvMacro() {
        println("\n=== 纯函数子集里也能用 gensym 写宏 ===")
        let env = Environment()
        BuiltinFunctions.registerPure(env)
        let ev = Evaluator2(env)
        ev.run("""
            (defmacro my-when (test . body)
              (let ((t (gensym)))
                (list (quote if) test (cons (quote begin) body) (quote nil))))
        """)
        let r = ev.run("(my-when #t 42)")
        @Assert(r.toString() == "42")
        println("✓ pure 环境下宏可用: (my-when #t 42) = ${r}")
    }
}

/** 小助手：在给定环境上用源码求值（供纯函数子集用例使用） */
class Evaluator2 {
    let ev: Evaluator

    init(env: Environment) {
        this.ev = Evaluator(env)
    }

    func run(code: String): LispValue {
        let lexer = Lexer(code)
        let parser = Parser(lexer.tokenize())
        var result: LispValue = Nil
        for (e in parser.parse()) {
            result = this.ev.eval(e)
        }
        result
    }
}
