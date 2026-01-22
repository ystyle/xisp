package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import ystyle::xisp.core.*
import ystyle::xisp.parser.*
import ystyle::xisp.bridge.*

/**
 * 全面功能单元测试 - 用于发现 bug
 */
@Test
class ComprehensiveTest {

    @TestCase
    func testBugMaxMinFunctions() {
        println("\n=== 测试 max/min 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试 max
        let maxExpr = Parser.parseSingleFromString("(max (quote (1 5 3 9 2)))")
        let maxResult = evaluator.eval(maxExpr)
        println("max result: ${maxResult.toString()}")

        match (maxResult) {
            case Number(n) => @Assert(n == 9.0)
            case _ => @Fail("max should return Number")
        }

        // 测试 min
        let minExpr = Parser.parseSingleFromString("(min (quote (5 1 9 3 2)))")
        let minResult = evaluator.eval(minExpr)
        println("min result: ${minResult.toString()}")

        match (minResult) {
            case Number(n) => @Assert(n == 1.0)
            case _ => @Fail("min should return Number")
        }
    }

    @TestCase
    func testGuardClauses() {
        println("\n=== 测试守卫条件 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试守卫条件
        let expr = Parser.parseSingleFromString("(match 15 (x when (> x 10) (str \"large: \" x)) _ \"medium\")")
        let result = evaluator.eval(expr)
        println("Guard clause result: ${result.toString()}")

        // 检查不是 nil
        match (result) {
            case Nil => @Fail("Guard clause should not return nil")
            case _ => ()
        }
    }

    @TestCase
    func testStringInterpolation() {
        println("\n=== 测试字符串插值 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试字符串插值
        let expr = Parser.parseSingleFromString("#\"Value: {+ 1 2}\"")
        let result = evaluator.eval(expr)
        println("String interpolation result: ${result.toString()}")

        match (result) {
            case Str(_) => @Assert(true)
            case _ => @Fail("String interpolation should return Str")
        }
    }

    @TestCase
    func testThreadFirstMacro() {
        println("\n=== 测试管道操作符 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试管道操作符
        let expr = Parser.parseSingleFromString("(-> 10 (+ 5) (* 2))")
        let result = evaluator.eval(expr)
        println("Thread-first result: ${result.toString()}")

        match (result) {
            case Number(n) => @Assert(n == 30.0)
            case NativeFunc(_) => @Fail("Thread-first should not return NativeFunc")
            case _ => @Fail("Unexpected result type")
        }
    }

    @TestCase
    func testBugDestructuringLet() {
        println("\n=== 测试解构绑定 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试解构绑定
        let expr = Parser.parseSingleFromString("(let [(x y z) (quote (1 2 3))] (list x y z))")
        let result = evaluator.eval(expr)
        println("Destructuring let result: ${result.toString()}")

        match (result) {
            case Nil => @Fail("Destructuring should not return nil")
            case _ => ()
        }
    }

    @TestCase
    func testRestParameters() {
        println("\n=== 测试 rest 参数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试 rest 参数
        let expr = Parser.parseSingleFromString("(match (quote (1 2 3 4 5)) (x & rest) (str \"first: \" x))")
        let result = evaluator.eval(expr)
        println("Rest parameter result: ${result.toString()}")

        match (result) {
            case Nil => @Fail("Rest parameter should not return nil")
            case _ => ()
        }
    }

    @TestCase
    func testApplyFunction() {
        println("\n=== 测试 apply 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试 apply
        let expr = Parser.parseSingleFromString("(apply + (quote (1 2 3)))")
        let result = evaluator.eval(expr)
        println("Apply result: ${result.toString()}")

        match (result) {
            case Number(n) => @Assert(n == 6.0)
            case _ => @Fail("apply should return Number")
        }
    }

    @TestCase
    func testComplexPatternMatching() {
        println("\n=== 测试复杂模式匹配 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试复杂模式匹配
        let expr = Parser.parseSingleFromString("(match (quote ((1 2) (3 4))) ((a b) (c d)) (list a b c d))")
        let result = evaluator.eval(expr)
        println("Complex pattern matching result: ${result.toString()}")

        match (result) {
            case Nil => @Fail("Complex pattern matching should not return nil")
            case _ => ()
        }
    }

    @TestCase
    func testVectorLiteral() {
        println("\n=== 测试向量字面量 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试向量字面量
        let expr = Parser.parseSingleFromString("[1 2 3]")
        let result = evaluator.eval(expr)
        println("Vector literal result: ${result.toString()}")

        match (result) {
            case Nil => @Fail("Vector literal should not return nil")
            case _ => ()
        }
    }

    @TestCase
    func testHashMapLiteral() {
        println("\n=== 测试哈希映射字面量 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试哈希映射字面量
        let expr = Parser.parseSingleFromString("{:name \"Alice\" :age 30}")
        let result = evaluator.eval(expr)
        println("Hash map literal result: ${result.toString()}")

        match (result) {
            case Nil => @Fail("Hash map literal should not return nil")
            case _ => ()
        }
    }

    @TestCase
    func testHashSetLiteral() {
        println("\n=== 测试哈希集合字面量 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试哈希集合字面量
        let expr = Parser.parseSingleFromString("#{1 2 3}")
        let result = evaluator.eval(expr)
        println("Hash set literal result: ${result.toString()}")

        match (result) {
            case Nil => @Fail("Hash set literal should not return nil")
            case _ => ()
        }
    }

    @TestCase
    func testMapWithLambda() {
        println("\n=== 测试 map 与 lambda ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 定义平方函数
        let defExpr = Parser.parseSingleFromString("(define square (lambda (x) (* x x)))")
        evaluator.eval(defExpr)

        // 测试 map
        let mapExpr = Parser.parseSingleFromString("(map square (quote (1 2 3)))")
        let result = evaluator.eval(mapExpr)
        println("Map with lambda result: ${result.toString()}")

        @Assert(result.isList())
    }

    @TestCase
    func testFilterWithLambda() {
        println("\n=== 测试 filter 与 lambda ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试 filter
        let expr = Parser.parseSingleFromString("(filter (lambda (n) (> n 2)) (quote (1 2 3 4 5)))")
        let result = evaluator.eval(expr)
        println("Filter with lambda result: ${result.toString()}")

        @Assert(result.isList())
    }

    @TestCase
    func testReduceWithLambda() {
        println("\n=== 测试 reduce 与 lambda ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试 reduce
        let expr = Parser.parseSingleFromString("(reduce (lambda (a b) (+ a b)) 0 (quote (1 2 3 4 5)))")
        let result = evaluator.eval(expr)
        println("Reduce with lambda result: ${result.toString()}")

        match (result) {
            case Number(n) => @Assert(n == 15.0)
            case _ => @Fail("reduce should return Number")
        }
    }

    @TestCase
    func testAppendFunction() {
        println("\n=== 测试 append 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试基本 append
        let expr1 = Parser.parseSingleFromString("(append (quote (1 2)) (quote (3 4)))")
        let result1 = evaluator.eval(expr1)
        println("Append (1 2) (3 4) result: ${result1.toString()}")
        match (result1) {
            case Cons(_) => @Assert(result1.isList())
            case _ => @Fail("append should return a list")
        }

        // 测试 append with apply
        let expr2 = Parser.parseSingleFromString("(apply append (quote ((1 2) (3 4) (5 6))))")
        let result2 = evaluator.eval(expr2)
        println("Append with apply result: ${result2.toString()}")
        match (result2) {
            case Cons(_) => @Assert(result2.isList())
            case _ => @Fail("append with apply should return a list")
        }
    }

    @TestCase
    func testModernVectorDestructuring() {
        println("\n=== 测试现代向量解构 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试基础向量解构 [x y]
        let expr1 = Parser.parseSingleFromString("(let [[x y] (quote (1 2 3))] x)")
        let result1 = evaluator.eval(expr1)
        println("Vector destruct [x y] x result: ${result1.toString()}")
        match (result1) {
            case Number(n) => @Assert(n == 1.0)
            case _ => @Fail("Vector destruct should return Number 1.0")
        }

        // 测试 & rest 语法
        let expr2 = Parser.parseSingleFromString("(let [[x y & rest] (quote (1 2 3 4 5))] rest)")
        let result2 = evaluator.eval(expr2)
        println("Vector destruct [x y & rest] rest result: ${result2.toString()}")
        match (result2) {
            case Cons(_) => @Assert(result2.isList())
            case _ => @Fail("& rest should return a list")
        }

        // 测试嵌套向量解构
        let expr3 = Parser.parseSingleFromString("(let [[[a b] c] (quote ((1 2) 3))] (list a b c))")
        let result3 = evaluator.eval(expr3)
        println("Nested vector destruct result: ${result3.toString()}")
        match (result3) {
            case Cons(_) => @Assert(result3.isList())
            case _ => @Fail("Nested destruct should return a list")
        }
    }

    @TestCase
    func testTraditionalDotPairDestructuring() {
        println("\n=== 测试传统点对解构 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试基础点对解构 (x . y)
        let expr1 = Parser.parseSingleFromString("(let ((x . y) (quote (1 2 3))) x)")
        let result1 = evaluator.eval(expr1)
        println("Dot-pair (x . y) x result: ${result1.toString()}")
        match (result1) {
            case Number(n) => @Assert(n == 1.0)
            case _ => @Fail("Dot-pair destruct should return Number 1.0")
        }

        // 测试嵌套点对解构 ((x . y) . z)
        let expr2 = Parser.parseSingleFromString("(let (((x . y) . z) (quote ((1 2) 3 4))) (list x y z))")
        let result2 = evaluator.eval(expr2)
        println("Nested dot-pair ((x . y) . z) result: ${result2.toString()}")
        match (result2) {
            case Cons(_) => @Assert(result2.isList())
            case _ => @Fail("Nested dot-pair destruct should return a list")
        }
    }
}
