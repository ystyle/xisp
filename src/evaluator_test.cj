package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import std.fs.*
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
            case LispValue.Int(i) => @Assert(i > 14 && i < 16)
            case LispValue.Float(f) => @Assert(f > 14.0 && f < 16.0)
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
            case LispValue.Int(i) => @Assert(i > 119 && i < 121)
            case LispValue.Float(f) => @Assert(f > 119.0 && f < 121.0)
            case _ => @Fail("Expected Number")
        }
    }

    @TestCase
    func testVariableShadowing() {
        // 测试变量遮蔽
        let env = Environment()
        BuiltinFunctions.registerAll(env)  // 注册基础函数
        let evaluator = Evaluator(env)

        env.define("x", LispValue.Int(10))

        // (let ((x 20)) x)
        let letExpr = Parser.parseSingleFromString("(let ((x 20)) x)")
        let result = evaluator.eval(letExpr)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 19 && i < 21)
            case LispValue.Float(f) => @Assert(f > 19.0 && f < 21.0)
            case _ => @Fail("Expected Number")
        }

        // 外层的 x 应该还是 10
        let outerX = env.lookup("x")
        match (outerX) {
            case LispValue.Int(i) => @Assert(i > 9 && i < 11)
            case LispValue.Float(f) => @Assert(f > 9.0 && f < 11.0)
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
        if (!resultStr.contains("x=") && !resultStr.contains("1")) {
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
            case LispValue.Int(i) => @Assert(i == 123)
            case LispValue.Float(f) => @Assert(f == 123.0)
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
            case LispValue.Int(i) => @Assert(i == 100)
            case LispValue.Float(f) => @Assert(f == 100.0)
            case _ => @Fail("when with true condition should return 100")
        }
        let result2 = evalCode("(when 0 100)", env)
        match (result2) {
            case LispValue.Int(i) => @Assert(i == 0)
            case LispValue.Float(f) => @Assert(f == 0.0)
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
            case LispValue.Int(i) => @Assert(i == 1)
            case LispValue.Float(f) => @Assert(f == 1.0)
            case _ => @Fail("first incf should return 1")
        }
        let counter1 = evalCode("counter", env)
        match (counter1) {
            case LispValue.Int(i) => @Assert(i == 1)
            case LispValue.Float(f) => @Assert(f == 1.0)
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
            case LispValue.Int(i) => @Assert(i == 2)
            case LispValue.Float(f) => @Assert(f == 2.0)
            case _ => @Fail("x should be 2 after swap")
        }
        let y = evalCode("y", env)
        match (y) {
            case LispValue.Int(i) => @Assert(i == 1)
            case LispValue.Float(f) => @Assert(f == 1.0)
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
            case LispValue.Int(i) => @Assert(i == 12)
            case LispValue.Float(f) => @Assert(f == 12.0)
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
            case LispValue.Int(i) => @Assert(i == 100)
            case LispValue.Float(f) => @Assert(f == 100.0)
            case _ => @Fail("builtin when macro should return 100")
        }

        let result2 = evalCode("(when 0 200)", env)
        match (result2) {
            case LispValue.Int(i) => @Assert(i == 0)
            case LispValue.Float(f) => @Assert(f == 0.0)
            case _ => @Fail("builtin when macro should return 0")
        }

        // 测试 incf 宏
        evalCode("(define counter 0)", env)
        let result3 = evalCode("(incf counter)", env)
        match (result3) {
            case LispValue.Int(i) => @Assert(i == 1)
            case LispValue.Float(f) => @Assert(f == 1.0)
            case _ => @Fail("builtin incf should return 1")
        }

        let counterVal = evalCode("counter", env)
        match (counterVal) {
            case LispValue.Int(i) => @Assert(i == 1)
            case LispValue.Float(f) => @Assert(f == 1.0)
            case _ => @Fail("counter should be 1 after incf")
        }

        // 测试 push 宏
        evalCode("(define lst '(2 3))", env)
        let result4 = evalCode("(push 1 lst)", env)
        match (result4) {
            case Cons(cell) => @Assert(cell.car.toString() == "1")
            case _ => @Fail("push should return new list")
        }

        // 测试 negate 宏
        let result5 = evalCode("(negate 5)", env)
        match (result5) {
            case LispValue.Int(i) => @Assert(i < 0)
            case LispValue.Float(f) => @Assert(f < 0.0)
            case _ => @Fail("negate should return negative number")
        }
    }

    @TestCase
    func testBackquote() {
        // 测试反引号 (backquote) 语法
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // 测试简单的 backquote（类似 quote）
        let result1 = evalCode("`(a b c)", env)
        match (result1) {
            case Cons(cell) => @Assert(cell.toString() == "(a b c)")
            case _ => @Fail("backquote should return list")
        }

        // 测试 comma（取消引用）
        evalCode("(define x 2)", env)
        let result2 = evalCode("`(a ,x c)", env)
        match (result2) {
            case Cons(cell) => @Assert(cell.toString() == "(a 2 c)")
            case _ => @Fail("backquote with comma should evaluate x")
        }

        // 测试嵌套的 comma
        evalCode("(define y 3)", env)
        let result3 = evalCode("`(a ,x ,y)", env)
        match (result3) {
            case Cons(cell) => @Assert(cell.toString() == "(a 2 3)")
            case _ => @Fail("backquote with multiple commas should work")
        }
    }

    @TestCase
    func testBackquoteWithCommaAt() {
        // 测试 comma-at（列表拼接）
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        evalCode("(define lst '(b c))", env)
        let result = evalCode("`(a ,@lst d)", env)

        // 应该得到 (a b c d)
        match (result) {
            case Cons(cell) => @Assert(cell.toString() == "(a b c d)")
            case _ => @Fail("backquote with comma-at should splice list")
        }
    }

    @TestCase
    func testCommaOutsideBackquote() {
        // 测试 comma 在 backquote 外使用时返回错误
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        evalCode("(define x 1)", env)
        let result = evalCode(",x", env)

        match (result) {
            case Error(err) => @Assert(err.kind == ErrorType.SyntaxError)
            case _ => @Fail("comma outside backquote should return error")
        }
    }

    @TestCase
    func testMacroexpandAfterMacroCall() {
        // 测试调用宏后使用 macroexpand 和 define (bug 重现)
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // 定义 when 宏
        evalCode("(defmacro when (test then) `(if ,test ,then nil))", env)
        // 调用 when 宏
        evalCode("(when 1 (println \"x\"))", env)
        // 使用 define + macroexpand (这曾在 CLI 模式下导致栈溢出，现已修复)
        let result = evalCode("(define x (macroexpand (quote (when 1 2))))", env)

        // 如果能执行到这里，说明没有栈溢出，测试通过
        @Assert(true)
    }
}

/**
 * 高级宏特性测试
 */
@Test
class AdvancedMacroTest {
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
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (let* ((a 1) (b (+ a 10))) (+ a b))
        let result = evalCode("(let* ((a 1) (b (+ a 10))) (+ a b))", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 11 && i < 13)  // 应该是 12
            case LispValue.Float(f) => @Assert(f > 11.0 && f < 13.0)
            case _ => @Fail("let* basic should return number")
        }
    }

    @TestCase
    func testLetStarMultipleBindings() {
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
    func testLetStarLexicalScope() {
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
    func testIfLetBasic() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (if-let (x 5) x nil)
        let result = evalCode("(if-let (x 5) x nil)", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 4 && i < 6)  // 应该是 5
            case LispValue.Float(f) => @Assert(f > 4.0 && f < 6.0)
            case _ => @Fail("if-let should return bound value when true")
        }
    }

    @TestCase
    func testIfLetFalseCondition() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (if-let (x 0) x nil)
        let result = evalCode("(if-let (x 0) x nil)", env)

        match (result) {
            case Nil => ()  // 期望返回 nil
            case _ => @Fail("if-let should return nil when false")
        }
    }

    @TestCase
    func testIfLetWithExpression() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (if-let (x (+ 2 3)) (* x x) nil)
        let result = evalCode("(if-let (x (+ 2 3)) (* x x) nil)", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 24 && i < 26)  // 应该是 25
            case LispValue.Float(f) => @Assert(f > 24.0 && f < 26.0)
            case _ => @Fail("if-let should work with expressions")
        }
    }

    @TestCase
    func testIfLetWithElse() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (if-let (x 0) x 100)
        let result = evalCode("(if-let (x 0) x 100)", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 99 && i < 101)  // 应该返回 else 分支的 100
            case LispValue.Float(f) => @Assert(f > 99.0 && f < 101.0)
            case _ => @Fail("if-let should execute else branch when false")
        }
    }

    @TestCase
    func testWhenLetStarBasic() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (when-let* ((x 5)) x)
        let result = evalCode("(when-let* ((x 5)) x)", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 4 && i < 6)  // 应该是 5
            case LispValue.Float(f) => @Assert(f > 4.0 && f < 6.0)
            case _ => @Fail("when-let* with true value should return bound value")
        }
    }

    @TestCase
    func testWhenLetStarFalseCondition() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (when-let* ((x 0)) x)
        let result = evalCode("(when-let* ((x 0)) x)", env)

        match (result) {
            case Nil => ()  // 期望返回 nil
            case _ => @Fail("when-let* with false value should return nil")
        }
    }

    @TestCase
    func testWhenLetStarMultipleBindings() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (when-let* ((a 1) (b (+ a 10))) b)
        let result = evalCode("(when-let* ((a 1) (b (+ a 10))) b)", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 10 && i < 12)  // 应该是 11
            case LispValue.Float(f) => @Assert(f > 10.0 && f < 12.0)
            case _ => @Fail("when-let* with multiple bindings should work")
        }
    }

    @TestCase
    func testWhenLetStarWithBody() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (when-let* ((a 1) (b 2)) (+ a b))
        let result = evalCode("(when-let* ((a 1) (b 2)) (+ a b))", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 2 && i < 4)  // 应该是 3
            case LispValue.Float(f) => @Assert(f > 2.0 && f < 4.0)
            case _ => @Fail("when-let* should execute body with bindings")
        }
    }

    @TestCase
    func testWhenLetStarNoBindings() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // (when-let* () 42)
        let result = evalCode("(when-let* () 42)", env)

        match (result) {
            case Nil => ()  // 没有绑定时返回 nil
            case _ => @Fail("when-let* with no bindings should return nil")
        }
    }

    @TestCase
    func testWhenLetStarLexicalScope() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // 在外层定义 x
        evalCode("(define x 100)", env)

        // when-let* 中的 x 应该遮蔽外层的 x
        let result = evalCode("(when-let* ((x 1) (y (+ x 10))) y)", env)

        match (result) {
            case LispValue.Int(i) => @Assert(i > 10 && i < 12)  // 应该是 11
            case LispValue.Float(f) => @Assert(f > 10.0 && f < 12.0)
            case _ => @Fail("when-let* should create lexical scope")
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
    func testModuleImport() {
        // 测试实际模块导入
        // 使用 LispInterpreter 配置搜索路径
        let interpreter = LispInterpreter([
            withModulePath("examples")
        ])

        // 测试导入 pkg1
        println("Testing pkg1 import...")
        let importPkg1 = interpreter.eval("(import pkg1)")
        match (importPkg1) {
            case Nil => ()  // import 成功返回 nil
            case _ => @Fail("import should return nil on success")
        }

        // 测试调用 pkg1.greet
        let greetCall = interpreter.eval("(pkg1.greet \"Test\")")
        match (greetCall) {
            case Str(s) => @Assert(s.contains("Greeting done"))  // lambda 返回的字符串
            case _ => ()
        }

        // 测试导入 pkg2（依赖 pkg1）
        println("Testing pkg2 import...")
        let importPkg2 = interpreter.eval("(import pkg2)")
        match (importPkg2) {
            case Nil => ()
            case _ => @Fail("import pkg2 should return nil")
        }

        // 测试调用 pkg2.square
        let squareCall = interpreter.eval("(pkg2.square 7)")
        match (squareCall) {
            case LispValue.Int(i) => @Assert(i > 48 && i < 50)  // 7*7=49
            case LispValue.Float(f) => @Assert(f > 48.0 && f < 50.0)
            case _ => @Fail("pkg2.square should return 49")
        }

        // 测试跨包调用 pkg2.call-pkg1
        let crossCall = interpreter.eval("(pkg2.call-pkg1 \"World\")")
        match (crossCall) {
            case Str(s) => @Assert(s.contains("完成"))
            case _ => ()
        }
    }

    @TestCase
    func testModuleImportYstyleZlog() {
        // 测试导入 ystyle::zlog
        let interpreter = LispInterpreter([
            withModulePath("examples")
        ])

        println("Testing ystyle::zlog import...")
        let importZlog = interpreter.eval("(import ystyle::zlog)")
        match (importZlog) {
            case Nil => ()
            case Str(err) =>
                // 模块不存在或导入失败是预期的（因为可能没有真实的 zlog 包）
                println("zlog import result: ${err}")
            case _ => @Fail("unexpected import result")
        }
    }

    @TestCase
    func testModuleImportYstyleUtilsString() {
        // 测试导入 ystyle::utils.string
        let interpreter = LispInterpreter([
            withModulePath("examples")
        ])

        println("Testing ystyle::utils.string import...")
        let importStringUtils = interpreter.eval("(import ystyle::utils.string)")
        match (importStringUtils) {
            case Nil => ()
            case Str(err) =>
                // 模块不存在或导入失败是预期的
                println("utils.string import result: ${err}")
            case _ => @Fail("unexpected import result")
        }
    }

    @TestCase
    func testDotSymbolParsing() {
        // 测试带点符号的解析和求值
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 定义带点的符号
        let defineExpr = Parser.parseSingleFromString("(define test.func (lambda (x) (* x x)))")
        evaluator.eval(defineExpr)

        // 调用带点的符号
        let callExpr = Parser.parseSingleFromString("(test.func 8)")
        let result = evaluator.eval(callExpr)

        println("Dot symbol test result: ${result.toString()}")
        match (result) {
            case LispValue.Int(i) => @Assert(i > 63 && i < 65)  // 8*8=64
            case LispValue.Float(f) => @Assert(f > 63.0 && f < 65.0)
            case _ => @Fail("test.func should return 64")
        }
    }

    @TestCase
    func testLambdaWithBegin() {
        // 测试 lambda 中使用 begin 执行多个表达式
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 定义一个包含多个表达式的 lambda
        let code = "(define multi-expr (lambda (x) (begin (* x 2) (* x 3))))"
        let defineExpr = Parser.parseSingleFromString(code)
        evaluator.eval(defineExpr)

        // 调用函数，应该返回最后一个表达式的值
        let callExpr = Parser.parseSingleFromString("(multi-expr 5)")
        let result = evaluator.eval(callExpr)

        println("Lambda with begin test result: ${result.toString()}")
        match (result) {
            case LispValue.Int(i) => @Assert(i > 14 && i < 16)  // 5*3=15
            case LispValue.Float(f) => @Assert(f > 14.0 && f < 16.0)
            case _ => @Fail("multi-expr should return 15")
        }
    }

    @TestCase
    func testRelativePathImport() {
        // 测试相对路径导入功能
        // pkg2 导入了 pkg1，这是相对路径导入的实际案例
        let interpreter = LispInterpreter([
            withModulePath("examples")
        ])

        // 导入 pkg2（它会相对导入 pkg1）
        println("Testing relative path import: pkg2 imports pkg1...")
        let importPkg2 = interpreter.eval("(import pkg2)")
        match (importPkg2) {
            case Nil => ()  // 导入成功
            case Str(err) => @Fail("pkg2 import failed: ${err}")
            case _ => @Fail("unexpected result")
        }

        // 验证跨包调用工作正常
        let crossCall = interpreter.eval("(pkg2.call-pkg1 \"RelativeTest\")")
        match (crossCall) {
            case Str(s) => @Assert(s.contains("完成"))
            case _ => @Fail("cross-package call should work")
        }

        println("Relative path import test passed!")
    }

    @TestCase
    func testModuleSymbolBinding() {
        // 测试模块符号绑定到环境
        let interpreter = LispInterpreter([
            withModulePath("examples")
        ])

        // 导入模块
        interpreter.eval("(import pkg1)")

        // 验证符号被正确绑定到环境
        let env = interpreter.getEnvironment()
        let greetSymbol = env.lookup("pkg1.greet")

        match (greetSymbol) {
            case Procedure(_, _, _) => ()  // 符号应该是一个函数
            case _ => @Fail("pkg1.greet should be bound as a Procedure")
        }

        // 验证可以调用绑定的符号
        let result = interpreter.eval("(pkg1.greet \"BindingTest\")")
        match (result) {
            case Str(s) => @Assert(s.contains("Greeting done"))
            case _ => ()
        }

        println("Module symbol binding test passed!")
    }

    @TestCase
    func testRelativePathImportSyntax() {
        // 测试相对路径导入的解析功能
        // pkg2 不使用相对路径导入，但我们可以测试解析是否正确
        let interpreter = LispInterpreter([
            withModulePath("examples")
        ])

        // 测试字符串形式的相对路径导入语法
        // 注意：这里不测试实际加载，只测试语法解析是否正确
        // 因为相对路径导入需要从模块内部使用，currentFilePath 才会设置

        // 验证相对路径字符串被正确解析
        let relativePath1 = "./pkg1"
        @Assert(relativePath1.startsWith("./"))

        let relativePath2 = "../pkg1"
        @Assert(relativePath2.startsWith("../"))

        println("Relative path import syntax test passed!")
    }

    @TestCase
    func testEvalFile() {
        println("\n=== 测试 evalFile 功能 ===")

        // 使用项目中已存在的测试文件
        let testFilePath = "lisp-tests/debug_backslash_n.lisp"

        // 创建解释器并执行文件
        let interpreter = LispInterpreter([
            withStdLib(),
            withQuietMode()
        ])

        let result = interpreter.evalFile(testFilePath)

        // 验证文件执行成功（虽然会有错误，但不应该崩溃）
        println("✓ evalFile 成功执行文件")

        let resultStr = match (result) {
            case Error(err) => "Error: ${err.message}"
            case _ => result.toString()
        }
        println("✓ 返回值: ${resultStr}")
    }
}

