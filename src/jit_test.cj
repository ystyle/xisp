package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import std.time.MonoTime
import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*

/**
 * JIT 翻译器 / 运行时测试（J1：Int 快路径 + 直接调用 + deopt 回退）
 *
 * 覆盖：
 * 1. 递归直接调用（fib）全机器码执行 + 正确性
 * 2. 未支持指令/常量（Str 常量）→ 函数级回退解释器（语义保真）
 * 3. 运行时类型 guard 失败 → deopt 回 VM 解释器重跑（语义保真）
 * 4. 互递归（非自调用）→ CALL guard 失败 → 回退（语义保真）
 */
@Test
class JitTest {
    /**
     * JIT 模式求值（字节码 + JIT 叠层）
     */
    func evalWithJit(code: String): LispValue {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)
        let compiler = BytecodeCompiler()
        let vm = BytecodeVM()
        vm.setCompiler(compiler)
        evaluator.setCompiler(compiler)
        evaluator.setVM(vm)
        JitRuntime()
        var result: LispValue = Nil
        for (expr in Parser.parseFromString(code)) {
            result = evaluator.eval(expr)
        }
        result
    }

    @TestCase
    func testJitFibRecursion() {
        // 自递归全机器码：fib(10) = 55
        let result = this.evalWithJit(
            "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 55)
            case _ => @Fail("expected 55, got ${result}")
        }
    }

    @TestCase
    func testJitRecursionBase() {
        // 自递归（结果为常量的分支）：(f 3) = 0
        let result = this.evalWithJit(
            "(define (f n) (if (< n 2) 0 (f (- n 1)))) (f 3)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 0)
            case _ => @Fail("expected 0, got ${result}")
        }
    }

    @TestCase
    func testJitFallbackUnsupportedConstant() {
        // Str 常量 → JIT 不编译（函数级）→ 解释器回退 → 语义保真
        let result = this.evalWithJit(
            "(define (g n) (if (< n 2) \"x\" (g (- n 1)))) (g 3)")
        match (result) {
            case Str(s) => @Assert(s == "x")
            case _ => @Fail("expected \"x\", got ${result}")
        }
    }

    @TestCase
    func testJitGuardDeoptFallback() {
        // 运行时类型 guard 失败（Float 参数）→ deopt → VM 解释器重跑 → 语义保真
        let result = this.evalWithJit(
            "(define (h x) (+ x 1)) (h 1.5)")
        match (result) {
            case LispValue.Float(f) => @Assert(f == 2.5)
            case _ => @Fail("expected 2.5, got ${result}")
        }
    }

    @TestCase
    func testJitMutualRecursionFallback() {
        // 互递归（g→f 非自调用）→ CALL guard 失败 → 回退 → 语义保真
        let result = this.evalWithJit(
            "(define (f n) (if (< n 2) n (g (- n 1)))) (define (g n) (if (< n 2) n (+ (f (- n 1)) 1))) (f 10)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 5)
            case _ => @Fail("expected 5, got ${result}")
        }
    }

    @TestCase
    func testJitMutualRecursion() {
        // 互递归（非自调用 → CALL guard deopt → VM 回退级联；语义保真）
        let result = this.evalWithJit(
            "(define (f n) (if (< n 2) n (g (- n 1)))) (define (g n) (if (< n 2) n (+ (f (- n 1)) 1))) (f 10)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 5)
            case _ => @Fail("expected 5, got ${result}")
        }
    }

    @TestCase
    func testJitRFormFibSeries() {
        // R 形式（寄存器级）回归：fib 全系列（含叶路径 n<2 与深递归路径）
        let cases = [(0, 0), (1, 1), (2, 1), (3, 2), (10, 55), (20, 6765), (30, 832040)]
        for ((n, expect) in cases) {
            let result = this.evalWithJit(
                "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib ${n})")
            match (result) {
                case LispValue.Int(i) => @Assert(i == expect)
                case _ => @Fail("fib(${n}) expected ${expect}, got ${result}")
            }
        }
    }

    @TestCase
    func testJitRFormArityMismatchFallback() {
        // 自调用缺参（2 参函数 1 参自调用）→ JIT 不编译 → 回退 VM 结果一致（回归：曾 v1 编译后读垃圾槽崩溃）
        let jit = this.evalWithJit(
            "(define (ev x a) (if (< x 1) a (ev (- x a)))) (ev 20 3)")
        let bc = Environment()
        BuiltinFunctions.registerAll(bc)
        let evaluator = Evaluator(bc)
        let compiler = BytecodeCompiler()
        let vm = BytecodeVM()
        vm.setCompiler(compiler)
        evaluator.setCompiler(compiler)
        evaluator.setVM(vm)
        var r2: LispValue = Nil
        for (expr in Parser.parseFromString("(define (ev x a) (if (< x 1) a (ev (- x a)))) (ev 20 3)")) {
            r2 = evaluator.eval(expr)
        }
        match (jit) {
            case LispValue.Int(i) =>
                match (r2) {
                    case LispValue.Int(j) => @Assert(i == j)
                    case _ => @Fail("bc result mismatch")
                }
            case _ => @Fail("expected Int, got ${jit}")
        }
    }

    @TestCase
    func testJitRFormBoolResult() {
        // 布尔谓词结果（BIN_LC kind 4-6 → 0/1 池值）贯穿返回
        let result = this.evalWithJit(
            "(define (b n) (if (< n 2) (> n 0) (b (- n 1)))) (b 5)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 1)
            case _ => @Fail("expected 1, got ${result}")
        }
    }

    @TestCase
    func testJitRFormJifCond() {
        // IF 条件走 JUMP_IF_FALSE（op11）路径：条件为参数本身
        let result = this.evalWithJit(
            "(define (f n) (if n (f (- n 1)) 99)) (f 7)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 99)
            case _ => @Fail("expected 99, got ${result}")
        }
    }

    @TestCase
    func testJitFib30Perf() {
        // 进程内 fib(30) 计时（R 形式；宽松上界防灾难性回归，真实值远低于此）
        let t0 = MonoTime.now()
        let result = this.evalWithJit(
            "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 30)")
        let ms = (MonoTime.now() - t0).toNanoseconds() / 1000000
        match (result) {
            case LispValue.Int(i) => @Assert(i == 832040)
            case _ => @Fail("expected 832040, got ${result}")
        }
        println("  [JIT] fib(30) in-process: ${ms} ms")
        @Assert(ms < 100)
    }

    @TestCase
    func testJitCrossCallWithGlobal() {
        // 跨函数调用（被调函数 JIT 化 + callee 自解析全局）：(p 4) = 503
        let result = this.evalWithJit(
            "(define (p n) (if (< n 2) n (q (- n 1)))) (define (q n) (+ n 500)) (p 4)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 503)
            case _ => @Fail("expected 503, got ${result}")
        }
    }

    @TestCase
    func testJitConsistentWithBytecode() {
        // 多表达式：JIT 与纯字节码结果一致（含全局访问 + let 局部槽）
        let jit = this.evalWithJit(
            "(define (f x y) (let ((s (+ x y))) (* s 2))) (f 3 4)")
        let bc = Environment()
        BuiltinFunctions.registerAll(bc)
        let evaluator = Evaluator(bc)
        let compiler = BytecodeCompiler()
        let vm = BytecodeVM()
        vm.setCompiler(compiler)
        evaluator.setCompiler(compiler)
        evaluator.setVM(vm)
        var r2: LispValue = Nil
        for (expr in Parser.parseFromString("(define (f x y) (let ((s (+ x y))) (* s 2))) (f 3 4)")) {
            r2 = evaluator.eval(expr)
        }
        match (jit) {
            case LispValue.Int(i) =>
                match (r2) {
                    case LispValue.Int(j) => @Assert(i == j)
                    case _ => @Fail("bc result mismatch")
                }
            case _ => @Fail("jit expected Int, got ${jit}")
        }
    }
}
