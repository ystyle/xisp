package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp_bc.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList

/**
 * 特殊形式的「让位」与函数值调用入口（能力域设计 §3.1 / §3.2）
 *
 * 分层目标不是"可裁剪"而是**可替换**：core 带了同能力实现，用户应当能
 * 不用它、直接用自己的实现顶上。这两条接缝是实现该目标的核心。
 */
@Test
class DeferAndApplyTest {
    func evalAll(code: String): String {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let ev = Evaluator(env)
        var result: LispValue = Nil
        for (expr in Parser.parseFromString(code)) {
            result = ev.eval(expr)
        }
        result.toString()
    }

    /**
     * 让位：用户定义同名实现后，调用走用户的。
     *
     * 修复前这 6 个符号由求值器硬编码拦截，`(define map …)` 只能改"值"、
     * 改不了"行为"（`(map f lst)` 仍走 core）。
     */
    @TestCase
    func testUserCanReplaceSpecialForms() {
        println("\n=== 用户实现可顶掉 core 的同能力符号 ===")
        let cases: Array<(String, String)> = [
            ("(define (m f l) 999) (define map m) (map (lambda (x) x) (quote (1 2)))", "999"),
            ("(define (f2 p l) 888) (define filter f2) (filter (lambda (x) #t) (quote (1 2)))", "888"),
            ("(define (r f i l) 777) (define reduce r) (reduce + 0 (quote (1 2)))", "777"),
            ("(define (a f l) 666) (define apply a) (apply + (quote (1 2)))", "666"),
            ("(define (e x) 555) (define eval e) (eval (quote 1))", "555"),
            ("(defmacro m2 (. a) 444) (define -> m2) (-> 1 (+ 1))", "444"),
        ]
        for ((code, expected) in cases) {
            let r = this.evalAll(code)
            @Assert(r == expected)
            println("✓ " + r + "  ←  " + code)
        }
    }

    /**
     * 未定义时保持 core 语义（让位机制不能误伤默认行为）。
     */
    @TestCase
    func testDefaultSemanticsPreserved() {
        println("\n=== 未重定义时保持 core 语义 ===")
        let cases: Array<(String, String)> = [
            ("(map (lambda (x) (* x 2)) (quote (1 2 3)))", "(2 4 6)"),
            ("(filter (lambda (x) (> x 1)) (quote (1 2 3)))", "(2 3)"),
            ("(reduce + 0 (quote (1 2 3)))", "6"),
            ("(apply + (quote (1 2)))", "3"),
            ("(eval (quote (+ 1 2)))", "3"),
            ("(-> 5 (+ 3) (* 2))", "16"),
        ]
        for ((code, expected) in cases) {
            let r = this.evalAll(code)
            @Assert(r == expected)
            println("✓ " + code + " = " + r)
        }
    }

    /**
     * 让位后**定义在被顶掉之后**的调用同样走用户实现（用 `define` 顺序验证
     * 守卫缓存随 `Environment.defineVersion` 失效）。
     */
    @TestCase
    func testCacheInvalidationOnRedefine() {
        println("\n=== 守卫缓存随 define 失效 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let ev = Evaluator(env)

        // 先正常调用一次（把"无用户绑定"写进缓存）
        let before = ev.eval(Parser.parseSingleFromString("(reduce + 0 (quote (1 2 3)))"))
        @Assert(before.toString() == "6")

        // 再定义用户实现 —— 缓存必须失效
        for (expr in Parser.parseFromString("(define (myr f i l) 42) (define reduce myr)")) {
            ev.eval(expr)
        }
        let after = ev.eval(Parser.parseSingleFromString("(reduce + 0 (quote (1 2 3)))"))
        @Assert(after.toString() == "42")
        println("✓ 定义前 6 → 定义后 42（缓存已失效）")
    }

    /**
     * 函数值调用入口：可调用值判定 + 调用。
     *
     * 这是"用户用仓颉复刻 map"的前提 —— 纯 Lisp 实现不需要它（由求值器执行，
     * 天然能调闭包），门槛只卡在仓颉注册的函数上。
     */
    @TestCase
    func testApplyFunction() {
        println("\n=== applyFunction：调用 Lisp 函数值 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let ev = Evaluator(env)

        // 造一个 Lisp 闭包值
        ev.eval(Parser.parseSingleFromString("(define (double x) (* x 2))"))
        let fn = ev.eval(Parser.parseSingleFromString("double"))

        // 经新入口调用（这就是注册函数此前做不到的事）
        let args = ArrayList<LispValue>()
        args.add(LispValue.Int(21))
        let r = ev.applyFunction(fn, args)
        @Assert(r.toString() == "42")
        println("✓ applyFunction(double, [21]) = " + r.toString())

        // 仓颉注册的函数（NativeFunc）也能经同一入口调用
        let carFn = ev.eval(Parser.parseSingleFromString("car"))
        @Assert(ev.isCallableValue(carFn))
        // 实参是**已求值**的值：先求值 (quote (7 8)) 得到列表，再传给 car
        let listArg = ev.eval(Parser.parseSingleFromString("(quote (7 8))"))
        let carArgs = ArrayList<LispValue>()
        carArgs.add(listArg)
        let r2 = ev.applyFunction(carFn, carArgs)
        @Assert(r2.toString() == "7")
        println("✓ applyFunction(car, [(7 8)]) = " + r2.toString())

        // 非函数值 → 明确报错（不静默返回 nil）
        let notFn = ev.eval(Parser.parseSingleFromString("42"))
        @Assert(!ev.isCallableValue(notFn))
        let r3 = ev.applyFunction(notFn, ArrayList<LispValue>())
        match (r3) {
            case LispValue.Error(e) =>
                @Assert(e.kind == ErrorType.TypeError)
                println("✓ 非函数值明确报 TypeError（非静默 nil）")
            case other => @Fail("期望 Error，实际 " + other.toString())
        }
    }

    /**
     * `for-each` 现在能接收 Lisp 闭包。
     *
     * 旧实现是注册函数（`NativeFunc`），handler 拿不到求值器，只能
     * `if (let NativeFunc(proc) <- args[0])` —— 传 `lambda` 时**静默什么都不做**。
     * 移到求值器后走 `applyFunction`，与 map/filter/reduce 一致。
     */
    @TestCase
    func testForEachAcceptsClosure() {
        println("\n=== for-each 能调 Lisp 闭包（旧实现静默失败） ===")
        let code = """
            (define s 0)
            (for-each (lambda (x) (set! s (+ s x))) (quote (1 2 3)))
            s
        """
        for (useBc in [false, true]) {
            let env = Environment()
            BuiltinFunctions.registerAll(env)
            let ev = Evaluator(env)
            if (useBc) {
                ev.setExecutionBackend(BytecodeExecutionBackend())
            }
            var r: LispValue = Nil
            for (e in Parser.parseFromString(code)) {
                r = ev.eval(e)
            }
            @Assert(r.toString() == "6")
        }
        println("✓ 双后端 (for-each (lambda (x) (set! s (+ s x))) '(1 2 3)) 后 s = 6")

        // 原生函数同样可用
        let r = this.evalAll("(for-each println (quote (10 20)))")
        @Assert(r == "nil")
        println("✓ 原生函数可用，返回 nil")

        // 多列表：取最短
        let r2 = this.evalAll("""
            (define t 0)
            (for-each (lambda (a b) (set! t (+ t (+ a b)))) (quote (1 2)) (quote (10 20)))
            t
        """)
        @Assert(r2 == "33")
        println("✓ 多列表同位取参 = 33")

        // 非可调用值：明确报错（不静默）
        let r3 = this.evalAll("(for-each 42 (quote (1 2)))")
        @Assert(r3.contains("TypeError"))
        println("✓ 非可调用值报 TypeError，不静默跳过")

        // 可被用户顶替
        let r4 = this.evalAll("""
            (define (my-fe p l) 777)
            (define for-each my-fe)
            (for-each (lambda (x) x) (quote (1 2)))
        """)
        @Assert(r4 == "777")
        println("✓ 可被用户实现顶替 = 777")
    }

    /**
     * 让位必须**先于编译**生效。
     *
     * 修复前：用户 `define apply` 后，AST 走用户实现（666）而字节码仍按 core 编译
     * （返回 3）—— 两个后端不一致。让位判断现在置于编译分派之前。
     */
    @TestCase
    func testDeferBeatsCompilation() {
        println("\n=== 让位优先于编译（双后端一致） ===")
        let cases: Array<(String, String)> = [
            ("(define (m f l) 999) (define map m) (map (lambda (x) x) (quote (1 2)))", "999"),
            ("(define (a f l) 666) (define apply a) (apply + (quote (1 2)))", "666"),
            ("(define (r f i l) 777) (define reduce r) (reduce + 0 (quote (1 2)))", "777"),
            ("(define (e x) 555) (define eval e) (eval (quote 1))", "555"),
            ("(defmacro m2 (. a) 444) (define -> m2) (-> 1 (+ 1))", "444"),
            ("(define (fe p l) 333) (define for-each fe) (for-each (lambda (x) x) (quote (1 2)))", "333"),
        ]
        for ((code, expected) in cases) {
            var ast = ""
            var bc = ""
            for (useBc in [false, true]) {
                let env = Environment()
                BuiltinFunctions.registerAll(env)
                let ev = Evaluator(env)
                if (useBc) {
                    ev.setExecutionBackend(BytecodeExecutionBackend())
                }
                var r: LispValue = Nil
                for (e in Parser.parseFromString(code)) {
                    r = ev.eval(e)
                }
                if (useBc) { bc = r.toString() } else { ast = r.toString() }
            }
            @Assert(ast == expected)
            @Assert(bc == expected)
            println("✓ AST=" + ast + " BC=" + bc + "  ←  " + code)
        }
    }

    /**
     * 宏不是"可调用值"：它展开、不被调用。防止误把宏传给 applyFunction。
     */
    @TestCase
    func testMacroIsNotCallable() {
        println("\n=== 宏不是可调用值 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let ev = Evaluator(env)
        let mac = ev.eval(Parser.parseSingleFromString("when"))
        @Assert(!ev.isCallableValue(mac))
        let r = ev.applyFunction(mac, ArrayList<LispValue>())
        match (r) {
            case LispValue.Error(_) => println("✓ 宏经 applyFunction 报错（不予调用）")
            case other => @Fail("期望 Error，实际 " + other.toString())
        }
    }
}
