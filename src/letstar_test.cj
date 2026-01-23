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
            case Number(n) => @Assert(n > 11.0 && n < 13.0)  // 应该是 12
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
            case Number(n) => @Assert(n > 8.0 && n < 10.0)  // 应该是 9
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
            case Number(n) => @Assert(n > 24.0 && n < 26.0)  // 应该是 25
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
            case Number(n) => @Assert(n > 2.0 && n < 4.0)  // 应该是 3
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
            case Number(n) => @Assert(n > 2.0 && n < 4.0)  // 应该是 3
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
            case Number(n) => @Assert(n > 10.0 && n < 12.0)  // 应该是 11
            case _ => @Fail("let* should create lexical scope")
        }

        // 外层的 x 不应该被修改
        let outerX = evalCode("x", env)
        match (outerX) {
            case Number(n) => @Assert(n > 99.0 && n < 101.0)  // 应该还是 100
            case _ => @Fail("outer x should not be modified")
        }
    }

    @TestCase
    func testWhenLetStarBasic() {
        // 测试 when-let* 基本功能
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (when-let* ((x 5) (y (> x 3))) (+ x y))
        // 如果 y 为真（即 x > 3），则执行 (+ x y)
        let result = evalCode("(when-let* ((x 5) (y (> x 3))) (+ x y))", env)

        match (result) {
            case Number(n) => @Assert(n > 7.0 && n < 9.0)  // 应该是 8 (5 + 3)
            case _ => @Fail("when-let* should return number when condition is true")
        }
    }

    @TestCase
    func testWhenLetStarFalseCondition() {
        // 测试 when-let* 条件为假的情况
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (when-let* ((x 1) (y (> x 3))) (+ x y))
        // 如果 y 为假（即 x <= 3），则返回 nil
        let result = evalCode("(when-let* ((x 1) (y (> x 3))) (+ x y))", env)

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

        // (when-let* ((a 10) (b (> a 5)) (c (* b 2))) c)
        let result = evalCode("(when-let* ((a 10) (b (> a 5)) (c (* b 2))) c)", env)

        match (result) {
            case Number(n) => @Assert(n > 1.0 && n < 3.0)  // 应该是 2 (1 * 2)
            case _ => @Fail("when-let* with multiple bindings should work")
        }
    }

    @TestCase
    func testWhenLetStarWithBegin() {
        // 测试 when-let* 中使用 begin
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (when-let* ((x 5) (y (> x 3))) (begin (+ x y) (* x y)))
        let result = evalCode("(when-let* ((x 5) (y (> x 3))) (begin (* x y)))", env)

        match (result) {
            case Number(n) => @Assert(n > 14.0 && n < 16.0)  // 应该是 15 (5 * 3)
            case _ => @Fail("when-let* with begin should work")
        }
    }
}
