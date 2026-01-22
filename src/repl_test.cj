package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import ystyle::xisp.core.*
import ystyle::xisp.parser.*

/**
 * 交互式环境功能单元测试
 * 测试类似于 REPL 的环境持久化和多次求值
 */
@Test
class ReplTest {
    @TestCase
    func testMultiEval() {
        println("\n=== 测试多次求值 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 定义函数
        let defExpr = Parser.parseSingleFromString("(define square (lambda (x) (* x x)))")
        evaluator.eval(defExpr)

        // 多次调用
        let expr1 = Parser.parseSingleFromString("(square 5)")
        let result1 = evaluator.eval(expr1)
        match (result1) {
            case Number(n) => @Assert(n == 25.0)
            case _ => @Fail("Function should work across multiple evals")
        }

        let expr2 = Parser.parseSingleFromString("(square 10)")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case Number(n) => @Assert(n == 100.0)
            case _ => @Fail("Function should persist across evals")
        }
    }

    @TestCase
    func testEnvironmentIsolation() {
        println("\n=== 测试环境隔离 ===")
        let env1 = Environment()
        let env2 = Environment()
        let evaluator1 = Evaluator(env1)
        let evaluator2 = Evaluator(env2)

        BuiltinFunctions.registerAll(env1)
        BuiltinFunctions.registerAll(env2)

        // 在 env1 中定义变量
        let defExpr = Parser.parseSingleFromString("(define x 100)")
        evaluator1.eval(defExpr)

        // env1 应该能访问
        let expr1 = Parser.parseSingleFromString("x")
        let result1 = evaluator1.eval(expr1)
        match (result1) {
            case Number(n) => @Assert(n == 100.0)
            case _ => @Fail("env1 should have x defined")
        }

        // env2 不应该有这个变量
        let result2 = evaluator2.eval(expr1)
        match (result2) {
            case Symbol(_) => ()  // 正常：返回未定义的符号
            case _ => @Fail("env2 should not have x from env1")
        }
    }

    @TestCase
    func testStatePersistence() {
        println("\n=== 测试状态持久化 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 定义一系列变量
        evaluator.eval(Parser.parseSingleFromString("(define a 1)"))
        evaluator.eval(Parser.parseSingleFromString("(define b 2)"))
        evaluator.eval(Parser.parseSingleFromString("(define c 3)"))

        // 验证所有变量都存在
        let expr = Parser.parseSingleFromString("(+ a b c)")
        let result = evaluator.eval(expr)
        match (result) {
            case Number(n) => @Assert(n == 6.0)
            case _ => @Fail("All variables should persist")
        }
    }

    @TestCase
    func testWithListOperations() {
        println("\n=== 测试列表操作 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 构建列表
        evaluator.eval(Parser.parseSingleFromString("(define lst (list 1 2 3 4 5))"))

        // 测试各种列表操作
        let expr1 = Parser.parseSingleFromString("(length lst)")
        let result1 = evaluator.eval(expr1)
        match (result1) {
            case Number(n) => @Assert(n == 5.0)
            case _ => @Fail("length should work")
        }

        let expr2 = Parser.parseSingleFromString("(first lst)")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case Number(n) => @Assert(n == 1.0)
            case _ => @Fail("first should work")
        }

        let expr3 = Parser.parseSingleFromString("(rest lst)")
        let result3 = evaluator.eval(expr3)
        match (result3) {
            case Cons(cell) => @Assert(cell.car.toString() == "2.000000")
            case _ => @Fail("rest should work")
        }
    }

    @TestCase
    func testWithHigherOrder() {
        println("\n=== 测试高阶函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 使用 map
        let expr1 = Parser.parseSingleFromString("(map (lambda (x) (* x 2)) (quote (1 2 3)))")
        let result1 = evaluator.eval(expr1)
        @Assert(result1.isList())

        // 使用 filter
        let expr2 = Parser.parseSingleFromString("(filter (lambda (n) (> n 2)) (quote (1 2 3 4 5)))")
        let result2 = evaluator.eval(expr2)
        @Assert(result2.isList())

        // 使用 reduce
        let expr3 = Parser.parseSingleFromString("(reduce (lambda (a b) (+ a b)) 0 (quote (1 2 3 4 5)))")
        let result3 = evaluator.eval(expr3)
        match (result3) {
            case Number(n) => @Assert(n == 15.0)
            case _ => @Fail("reduce should sum correctly")
        }
    }

    @TestCase
    func testWithStringOperations() {
        println("\n=== 测试字符串操作 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // str 函数
        let expr1 = Parser.parseSingleFromString("(str \"Hello\" \" \" \"World\")")
        let result1 = evaluator.eval(expr1)
        match (result1) {
            case Str(s) => @Assert(s == "Hello World")
            case _ => @Fail("str should work")
        }

        // symbol?
        let expr2 = Parser.parseSingleFromString("(symbol? 'test)")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case Boolean(b) => @Assert(b == true)
            case _ => @Fail("symbol? should return true")
        }

        // string?
        let expr3 = Parser.parseSingleFromString("(string? \"test\")")
        let result3 = evaluator.eval(expr3)
        match (result3) {
            case Boolean(b) => @Assert(b == true)
            case _ => @Fail("string? should return true")
        }
    }

    @TestCase
    func testComplexExpression() {
        println("\n=== 测试复杂表达式 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 定义嵌套函数
        let code = """
        (define (square x) (* x x))
        (define (sumOfSquares a b) (+ (square a) (square b)))
        """

        let exprs = Parser.parseFromString(code)
        for (expr in exprs) {
            evaluator.eval(expr)
        }

        // 调用
        let expr = Parser.parseSingleFromString("(sumOfSquares 3 4)")
        let result = evaluator.eval(expr)
        match (result) {
            case Number(n) => @Assert(n == 25.0)  // 9 + 16
            case _ => @Fail("Nested functions should work")
        }
    }
}
