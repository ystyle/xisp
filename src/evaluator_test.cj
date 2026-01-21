package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import ystyle::xisp.core.*
import ystyle::xisp.parser.*

/**
 * Evaluator 测试项
 */
class EvalTestItem {
    EvalTestItem(let input: String, let expectedStr: String) {}
}

var evalTests = ArrayList<EvalTestItem>([
    // 字面量
    EvalTestItem("42", "42"),
    EvalTestItem("3.14", "3.14"),
    EvalTestItem("\"hello\"", "\"hello\""),
    EvalTestItem("#t", "true"),
    EvalTestItem("#f", "false"),

    // quote
    EvalTestItem("'x", "x"),
    EvalTestItem("'(a b c)", "(a b c)"),

    // 变量定义和引用
    EvalTestItem("(define x 42) x", "42"),
    EvalTestItem("(define y 100) (+ y 10)", "110"),

    // set!
    EvalTestItem("(define x 10) (set! x 20) x", "20"),

    // if
    EvalTestItem("(if #t 1 2)", "1"),
    EvalTestItem("(if #f 1 2)", "2"),
    EvalTestItem("(if #t 1)", "1"),

    // lambda 和函数调用
    EvalTestItem("((lambda (x) (* x x)) 5)", "25"),
    EvalTestItem("(define square (lambda (x) (* x x))) (square 6)", "36"),

    // begin
    EvalTestItem("(begin 1 2 3)", "3"),
    EvalTestItem("(begin (define x 10) (define y 20) (+ x y))", "30"),

    // let
    EvalTestItem("(let ((x 1) (y 2)) (+ x y))", "3"),
    EvalTestItem("(let ((x 10)) (let ((y (+ x 5))) y))", "15"),

    // 嵌套函数调用
    EvalTestItem("(+ (+ 1 2) (+ 3 4))", "10"),
])

/**
 * Evaluator 测试
 */
@Test
class EvaluatorTest {
    @TestCase
    func testEval() {
        for (i in 0..evalTests.size) {
            let test = evalTests[i]
            println("[${i}] Testing: ${test.input}")

            let env = Environment()
            BuiltinFunctions.registerAll(env)  // 注册基础函数
            let evaluator = Evaluator(env)

            // 解析表达式
            let exprs = Parser.parseFromString(test.input)
            var result: LispValue = Nil

            for (j in 0..exprs.size) {
                result = evaluator.eval(exprs[j])
            }

            let resultStr = result.toString()
            println("[${i}] result: ${resultStr}, expected contains: ${test.expectedStr}")

            if (!resultStr.contains(test.expectedStr)) {
                @Fail("[${i}] Expected '${resultStr}' to contain '${test.expectedStr}'")
            }
        }
    }

    @TestCase
    func testClosures() {
        // 测试闭包和词法作用域
        let env = Environment()
        BuiltinFunctions.registerAll(env)  // 注册基础函数
        let evaluator = Evaluator(env)

        // (define make-adder (lambda (x) (lambda (y) (+ x y))))
        let makeAdderDef = Parser.parseSingleFromString("(define make-adder (lambda (x) (lambda (y) (+ x y))))")
        evaluator.eval(makeAdderDef)

        // (define add5 (make-adder 5))
        let add5Def = Parser.parseSingleFromString("(define add5 (make-adder 5))")
        evaluator.eval(add5Def)

        // (add5 10) => 15
        let callExpr = Parser.parseSingleFromString("(add5 10)")
        let result = evaluator.eval(callExpr)

        println("Closure test result: ${result.toString()}")
        @Assert(result.isNumber())

        match (result) {
            case Number(n) => @Assert(n > 14.0 && n < 16.0)
            case _ => @Fail("Expected Number")
        }
    }

    @TestCase
    func testRecursiveFunction() {
        // 测试递归函数
        let env = Environment()
        BuiltinFunctions.registerAll(env)  // 注册基础函数
        let evaluator = Evaluator(env)

        // 定义阶乘函数
        // (define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
        let factDef = Parser.parseSingleFromString(
            "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))"
        )
        evaluator.eval(factDef)

        // (fact 5) => 120
        let callExpr = Parser.parseSingleFromString("(fact 5)")
        let result = evaluator.eval(callExpr)

        println("Factorial test result: ${result.toString()}")

        match (result) {
            case Number(n) => @Assert(n > 119.0 && n < 121.0)
            case _ => @Fail("Expected Number")
        }
    }

    @TestCase
    func testVariableShadowing() {
        // 测试变量遮蔽
        let env = Environment()
        BuiltinFunctions.registerAll(env)  // 注册基础函数
        let evaluator = Evaluator(env)

        env.define("x", Number(10.0))

        // (let ((x 20)) x)
        let letExpr = Parser.parseSingleFromString("(let ((x 20)) x)")
        let result = evaluator.eval(letExpr)

        match (result) {
            case Number(n) => @Assert(n > 19.0 && n < 21.0)
            case _ => @Fail("Expected Number")
        }

        // 外层的 x 应该还是 10
        let outerX = env.lookup("x")
        match (outerX) {
            case Number(n) => @Assert(n > 9.0 && n < 11.0)
            case _ => @Fail("Expected Number")
        }
    }
}
