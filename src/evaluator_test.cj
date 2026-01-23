package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import ystyle::xisp.core.*
import ystyle::xisp.types.*
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

    @TestCase
    func testMatchListPattern() {
        // 测试match表达式的列表模式匹配（复现match_demo test 3的问题）
        let env = Environment()
        BuiltinFunctions.registerAll(env)  // 注册基础函数
        let evaluator = Evaluator(env)

        let code3 = "(match (quote (1 2 3)) (x y z) (str \"x=\" x \", y=\" y \", z=\" z) _ \"not a list of 3\")"

        println("[MATCH TEST] Testing: ${code3}")

        // 先测试lexer
        let lexer = Lexer(code3)
        let tokens = lexer.tokenize()
        println("[MATCH TEST] Tokenized ${tokens.size} tokens")

        // 解析表达式
        let exprs = Parser.parseFromString(code3)

        println("[MATCH TEST] Parsed ${exprs.size} expressions")

        // 应该只解析出1个表达式
        if (exprs.size != 1) {
            @Fail("Expected 1 expression, got ${exprs.size}")
        }

        // 第一个表达式不应该是nil
        match (exprs[0]) {
            case Nil => @Fail("First expression should not be Nil")
            case _ => ()
        }

        // 求值
        var result: LispValue = Nil
        for (j in 0..exprs.size) {
            result = evaluator.eval(exprs[j])
        }

        let resultStr = result.toString()
        println("[MATCH TEST] result: ${resultStr}")

        // 结果不应该包含"nil"错误
        if (resultStr == "nil") {
            @Fail("Match expression should not return nil")
        }

        // 结果应该包含期望的字符串
        if (!resultStr.contains("x=") && !resultStr.contains("1.000000")) {
            @Fail("Expected result to contain pattern match results, got: ${resultStr}")
        }
    }
}

/**
 * 宏系统测试
 */
@Test
class MacroTest {
    // 辅助函数：执行字符串代码并返回最后结果
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
    func testDefmacroBasic() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let result = evalCode("(defmacro identity (x) x)", env)
        match (result) {
            case Symbol(name) => @Assert(name == "identity")
            case _ => @Fail("defmacro should return symbol")
        }
        let macroValue = env.lookup("identity")
        match (macroValue) {
            case Macro(_, _, _) => ()
            case _ => @Fail("identity should be a macro")
        }
    }

    @TestCase
    func testIdentityMacro() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        evalCode("(defmacro identity (x) x)", env)
        let result = evalCode("(identity 123)", env)
        match (result) {
            case Number(n) => @Assert(n == 123.0)
            case _ => @Fail("identity macro should return number")
        }
    }

    @TestCase
    func testWhenMacro() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        evalCode("(defmacro when (test then) (list (quote if) test then (quote 0)))", env)
        let result1 = evalCode("(when 1 100)", env)
        match (result1) {
            case Number(n) => @Assert(n == 100.0)
            case _ => @Fail("when with true condition should return 100")
        }
        let result2 = evalCode("(when 0 100)", env)
        match (result2) {
            case Number(n) => @Assert(n == 0.0)
            case _ => @Fail("when with false condition should return 0")
        }
    }

    @TestCase
    func testIncfMacro() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        evalCode("(defmacro incf (var) (list (quote setq) var (list (quote +) var 1)))", env)
        evalCode("(define counter 0)", env)
        let result1 = evalCode("(incf counter)", env)
        match (result1) {
            case Number(n) => @Assert(n == 1.0)
            case _ => @Fail("first incf should return 1")
        }
        let counter1 = evalCode("counter", env)
        match (counter1) {
            case Number(n) => @Assert(n == 1.0)
            case _ => @Fail("counter should be 1 after first incf")
        }
    }

    @TestCase
    func testSwapMacro() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        evalCode("(defmacro swap (a b) (list (quote let) (list (list (quote temp) a)) (list (quote setq) a b) (list (quote setq) b (quote temp))))", env)
        evalCode("(define x 1)", env)
        evalCode("(define y 2)", env)
        evalCode("(swap x y)", env)
        let x = evalCode("x", env)
        match (x) {
            case Number(n) => @Assert(n == 2.0)
            case _ => @Fail("x should be 2 after swap")
        }
        let y = evalCode("y", env)
        match (y) {
            case Number(n) => @Assert(n == 1.0)
            case _ => @Fail("y should be 1 after swap")
        }
    }

    @TestCase
    func testMacroNestedCall() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        evalCode("(defmacro add-one (x) (list (quote +) x 1))", env)
        evalCode("(defmacro add-two (x) (list (quote +) (list (quote add-one) x) 1))", env)
        let result = evalCode("(add-two 10)", env)
        match (result) {
            case Number(n) => @Assert(n == 12.0)
            case _ => @Fail("nested macro call should return 12")
        }
    }

    @TestCase
    func testBuiltinMacrosRegistration() {
        // 测试内置宏自动注册功能
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // 测试 when 宏（已自动注册，无需手动定义）
        let result1 = evalCode("(when 1 100)", env)
        match (result1) {
            case Number(n) => @Assert(n == 100.0)
            case _ => @Fail("builtin when macro should return 100")
        }

        let result2 = evalCode("(when 0 200)", env)
        match (result2) {
            case Number(n) => @Assert(n == 0.0)
            case _ => @Fail("builtin when macro should return 0")
        }

        // 测试 incf 宏
        evalCode("(define counter 0)", env)
        let result3 = evalCode("(incf counter)", env)
        match (result3) {
            case Number(n) => @Assert(n == 1.0)
            case _ => @Fail("builtin incf should return 1")
        }

        let counterVal = evalCode("counter", env)
        match (counterVal) {
            case Number(n) => @Assert(n == 1.0)
            case _ => @Fail("counter should be 1 after incf")
        }

        // 测试 push 宏
        evalCode("(define lst '(2 3))", env)
        let result4 = evalCode("(push 1 lst)", env)
        match (result4) {
            case Cons(cell) => @Assert(cell.car.toString() == "1.000000")
            case _ => @Fail("push should return new list")
        }

        // 测试 negate 宏
        let result5 = evalCode("(negate 5)", env)
        match (result5) {
            case Number(n) => @Assert(n < 0.0)
            case _ => @Fail("negate should return negative number")
        }
    }
}

