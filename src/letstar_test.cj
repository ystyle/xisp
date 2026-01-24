package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*

/**
 * let* 测试
 */
@Test
class LetStarTest {
    func evalCode(code: String, env: Environment): LispValue {
        let exprs = Parser.parseFromString(code)
        var result: LispValue = Nil
        let evaluator = Evaluator(env)
        for (i in 0..exprs.size) {
            result = evaluator.eval(exprs[i])
        }
        result
    }

    @TestCase
    func testLetStarBasic() {
        // 测试基本的顺序绑定
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (let* ((a 1) (b (+ a 10))) (+ a b))
        let result = evalCode("(let* ((a 1) (b (+ a 10))) (+ a b))", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 11 && i < 13)  // 应该是 12
            case LispValue.Float(f) => @Assert(f > 11.0 && f < 13.0)
            case _ => @Fail("let* should return number")
        }
    }

    @TestCase
    func testLetStarMultipleBindings() {
        // 测试多个顺序绑定
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (let* ((a 1) (b (+ a 2)) (c (* b 3))) c)
        let result = evalCode("(let* ((a 1) (b (+ a 2)) (c (* b 3))) c)", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 8 && i < 10)  // 应该是 9
            case LispValue.Float(f) => @Assert(f > 8.0 && f < 10.0)
            case _ => @Fail("let* with multiple bindings should work")
        }
    }

    @TestCase
    func testLetStarSingleBinding() {
        // 测试单个绑定（应该退化为普通 let）
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (let* ((a 5)) (* a a))
        let result = evalCode("(let* ((a 5)) (* a a))", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 24 && i < 26)  // 应该是 25
            case LispValue.Float(f) => @Assert(f > 24.0 && f < 26.0)
            case _ => @Fail("let* with single binding should work")
        }
    }

    @TestCase
    func testLetStarNoBindings() {
        // 测试没有绑定的情况（应该直接执行 body）
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (let* () (+ 1 2))
        let result = evalCode("(let* () (+ 1 2))", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 2 && i < 4)  // 应该是 3
            case LispValue.Float(f) => @Assert(f > 2.0 && f < 4.0)
            case _ => @Fail("let* with no bindings should execute body")
        }
    }

    @TestCase
    func testLetStarWithBegin() {
        // 测试在 body 中使用 begin
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (let* ((a 1) (b 2)) (begin (println a) (println b) (+ a b)))
        let result = evalCode("(let* ((a 1) (b 2)) (begin (+ a b)))", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 2 && i < 4)  // 应该是 3
            case LispValue.Float(f) => @Assert(f > 2.0 && f < 4.0)
            case _ => @Fail("let* with begin should work")
        }
    }

    @TestCase
    func testLetStarLexicalScope() {
        // 测试词法作用域
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // 在外层定义 x
        evalCode("(define x 100)", env)

        // let* 中的 x 应该遮蔽外层的 x
        let result = evalCode("(let* ((x 1) (y (+ x 10))) y)", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 10 && i < 12)  // 应该是 11
            case LispValue.Float(f) => @Assert(f > 10.0 && f < 12.0)
            case _ => @Fail("let* should create lexical scope")
        }

        // 外层的 x 不应该被修改
        let outerX = evalCode("x", env)
        match (outerX) {
            case LispValue.Int(i) => @Assert(i > 99 && i < 101)  // 应该还是 100
            case LispValue.Float(f) => @Assert(f > 99.0 && f < 101.0)
            case _ => @Fail("outer x should not be modified")
        }
    }

    @TestCase
    func testWhenLetStarBasic() {
        // 测试 when-let* 基本功能
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (when-let* ((x 5) (y (+ x 10))) y)
        // 绑定 x=5, y=15，检查 y 是否为真，返回 y
        let result = evalCode("(when-let* ((x 5) (y (+ x 10))) y)", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 14 && i < 16)  // 应该是 15
            case LispValue.Float(f) => @Assert(f > 14.0 && f < 16.0)
            case _ => @Fail("when-let* should return number when condition is true")
        }
    }

    @TestCase
    func testWhenLetStarFalseCondition() {
        // 测试 when-let* 条件为假的情况
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (when-let* ((x 1) (y 0)) y)
        // y 为假（0），返回 nil
        let result = evalCode("(when-let* ((x 1) (y 0)) y)", env)

        match (result) {
            case Nil => ()  // 期望返回 nil
            case _ => @Fail("when-let* should return nil when condition is false")
        }
    }

    @TestCase
    func testWhenLetStarMultipleBindings() {
        // 测试 when-let* 多个绑定
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (when-let* ((a 10) (b (* a 2)) (c (+ b 5))) c)
        let result = evalCode("(when-let* ((a 10) (b (* a 2)) (c (+ b 5))) c)", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 24 && i < 26)  // 应该是 25 (10*2+5)
            case LispValue.Float(f) => @Assert(f > 24.0 && f < 26.0)
            case _ => @Fail("when-let* with multiple bindings should work")
        }
    }

    @TestCase
    func testWhenLetStarWithBegin() {
        // 测试 when-let* 中使用 begin
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (when-let* ((x 5) (y (* x 2))) (begin (* x y)))
        // x=5, y=10, (* x y) = 50
        let result = evalCode("(when-let* ((x 5) (y (* x 2))) (begin (* x y)))", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 49 && i < 51)  // 应该是 50 (5 * 10)
            case LispValue.Float(f) => @Assert(f > 49.0 && f < 51.0)
            case _ => @Fail("when-let* with begin should work")
        }
    }
}
