package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.*
import std.sync.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*

/**
 * 求值中断检查点单元测试。
 *
 * 中断以「错误值」表达（`Error(RuntimeError, INTERRUPTED_MESSAGE)`），
 * 而不是抛异常——与 xisp 既有的错误即值模型一致，宿主无需 try/catch
 * 即可区分「被中断」与「脚本自身运行时错误」。
 *
 * 覆盖四处落位：表达式入口、高阶迭代体、字节码派发循环、宿主函数边界。
 */
@Test
class InterruptTest {

    func isInterruptedError(v: LispValue): Bool {
        match (v) {
            case Error(e) => e.message == INTERRUPTED_MESSAGE
            case _ => false
        }
    }

    /** 第 limit 次检查之后返回 true */
    func countingChecker(counter: AtomicInt64, limit: Int64): () -> Bool {
        { => counter.fetchAdd(1) >= limit }
    }

    func makeEvaluator(): (Evaluator, AtomicInt64) {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let ev = Evaluator(env)
        let counter = AtomicInt64(0)
        ev.setInterruptChecker(countingChecker(counter, 50))
        (ev, counter)
    }

    // ==================== 未设置检查器：行为不变 ====================

    @TestCase
    func testNoCheckerMeansNoInterrupt() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let ev = Evaluator(env)
        // 未设置检查回调时，长列表必须完整跑完
        match (ev.eval(Parser.parseSingleFromString("(length (map (lambda (x) x) (range 1 1000)))"))) {
            case Int(n) => @Assert(n == 999)
            case _ => @Fail("evaluation without checker should complete normally")
        }
    }

    // ==================== AST 路径：高阶迭代体 ====================

    @TestCase
    func testInterruptDuringMap() {
        let (ev, counter) = makeEvaluator()
        let v = ev.eval(Parser.parseSingleFromString("(map (lambda (x) (* x 2)) (range 1 1000))"))
        @Assert(isInterruptedError(v))
        // 阈值为 50 而入口只检查 1 次：计数器达到阈值说明中断确实发生在迭代体中
        @Assert(counter.load() >= 50)
    }

    @TestCase
    func testInterruptDuringFilter() {
        let (ev, counter) = makeEvaluator()
        let v = ev.eval(Parser.parseSingleFromString("(filter (lambda (x) (> x 0)) (range 1 1000))"))
        @Assert(isInterruptedError(v))
        @Assert(counter.load() >= 50)
    }

    @TestCase
    func testInterruptDuringReduce() {
        let (ev, counter) = makeEvaluator()
        let v = ev.eval(Parser.parseSingleFromString("(reduce + 0 (range 1 1000))"))
        @Assert(isInterruptedError(v))
        @Assert(counter.load() >= 50)
    }

    // ==================== AST 路径：表达式入口 ====================

    @TestCase
    func testImmediateInterrupt() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let ev = Evaluator(env)
        ev.setInterruptChecker({ => true })
        let v = ev.eval(Parser.parseSingleFromString("(+ 1 2)"))
        @Assert(isInterruptedError(v))
    }

    @TestCase
    func testInterruptMessageIsStable() {
        // 宿主靠这条固定消息识别中断，不能随措辞调整而漂移
        @Assert(INTERRUPTED_MESSAGE == "evaluation interrupted")
    }

    // ==================== 清除后恢复 ====================

    @TestCase
    func testClearRestoresEvaluation() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let ev = Evaluator(env)
        ev.setInterruptChecker({ => true })
        @Assert(isInterruptedError(ev.eval(Parser.parseSingleFromString("(+ 1 2)"))))
        ev.clearInterruptChecker()
        match (ev.eval(Parser.parseSingleFromString("(+ 1 2)"))) {
            case Int(n) => @Assert(n == 3)
            case _ => @Fail("clearing the checker should restore evaluation")
        }
    }

    // ==================== 字节码路径 ====================

    @TestCase
    func testBytecodePathInterruptible() {
        // 开字节码后不再经过 AST 的 eval 入口，VM 派发循环必须自己检查。
        // 深递归在 VM 里走寄存器栈与帧切换（不消耗仓颉栈），因此规模可放大。
        let interp = LispInterpreter([withBytecodeCompiler()])
        let counter = AtomicInt64(0)
        interp.setInterruptChecker(countingChecker(counter, 100))
        interp.eval("(define (sum n acc) (if (= n 0) acc (sum (- n 1) (+ acc n))))")
        let v = interp.eval("(sum 200000 0)")
        @Assert(isInterruptedError(v))
    }

    @TestCase
    func testBytecodePathWithoutCheckerCompletes() {
        // 同一段程序在不设检查器时必须正常算出结果（确认上面的中断不是「本来就算不出」）
        let interp = LispInterpreter([withBytecodeCompiler()])
        interp.eval("(define (sum n acc) (if (= n 0) acc (sum (- n 1) (+ acc n))))")
        match (interp.eval("(sum 1000 0)")) {
            case Int(n) => @Assert(n == 500500)
            case _ => @Fail("bytecode evaluation should complete without a checker")
        }
    }

    @TestCase
    func testLispInterpreterInterruptForwards() {
        // LispInterpreter 是主要嵌入入口，转发必须生效（默认 AST 路径）
        let interp = LispInterpreter()
        let counter = AtomicInt64(0)
        interp.setInterruptChecker(countingChecker(counter, 50))
        let v = interp.eval("(map (lambda (x) x) (range 1 1000))")
        @Assert(isInterruptedError(v))
    }
}
