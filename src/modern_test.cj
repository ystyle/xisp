package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*

/**
 * 现代化函数测试项
 */
class ModernTestItem {
    ModernTestItem(let input: String, let expectedStr: String) {}
}

var modernTests = ArrayList<ModernTestItem>([
    // 现代别名测试
    ModernTestItem("(first '(1 2 3))", "1"),
    ModernTestItem("(rest '(1 2 3))", "(2"),  // 只检查开头
    ModernTestItem("(second '(a b c))", "b"),
    ModernTestItem("(third '(a b c))", "c"),
    ModernTestItem("(prepend 1 '(2 3))", "1"),  // 检查包含 1

    // 传统组合函数测试
    ModernTestItem("(cadr '(1 2 3))", "2"),     // cadr = second
    ModernTestItem("(caddr '(1 2 3))", "3"),    // caddr = third
    ModernTestItem("(cddr '(1 2 3 4 5))", "(3"), // cddr = (rest (rest lst))
    ModernTestItem("(caar '((a b) (c d)))", "a"), // caar = (car (car lst))
    ModernTestItem("(cdar '((a b) (c d)))", "(b"), // cdar = (cdr (car lst))

    // 高阶函数测试（使用原生函数）
    ModernTestItem("(sum '(1 2 3 4 5))", "15"),
    ModernTestItem("(product '(2 3 4))", "24"),
    ModernTestItem("(reverse '(1 2 3))", "3"),  // 检查包含 3
    ModernTestItem("(range 0 5)", "0"),  // 检查包含 0

    // reduce 测试（使用原生函数）
    ModernTestItem("(reduce + 0 '(1 2 3))", "6"),
])

/**
 * 现代化函数测试
 */
@Test
class ModernTest {
    @TestCase
    func testModernFunctions() {
        for (i in 0..modernTests.size) {
            let test = modernTests[i]
            println("[${i}] Testing: ${test.input}")

            let env = Environment()
            BuiltinFunctions.registerAll(env)
            let evaluator = Evaluator(env)

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
    func testMapWithLambda() {
        // 测试 map 与 lambda 结合
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 定义一个平方函数
        let def = Parser.parseSingleFromString("(define square (lambda (x) (* x x)))")
        evaluator.eval(def)

        // 使用 map
        let mapExpr = Parser.parseSingleFromString("(map square '(1 2 3 4 5))")
        let result = evaluator.eval(mapExpr)

        println("Map result: ${result.toString()}")
        @Assert(result.isList())
    }

    @TestCase
    func testChainedOperations() {
        // 测试链式操作（使用原生函数）
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // (reverse (range 1 6)) => (5 4 3 2 1)
        let expr = Parser.parseSingleFromString("(reverse (range 1 6))")
        let result = evaluator.eval(expr)

        println("Chained result: ${result.toString()}")
        @Assert(result.isList())

        // 验证结果包含 5, 4, 3
        let resultStr = result.toString()
        @Assert(resultStr.contains("5"))
        @Assert(resultStr.contains("4"))
        @Assert(resultStr.contains("3"))
    }

    @TestCase
    func testTypeOf() {
        // 测试 type-of 函数
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 测试整数
        let intExpr = Parser.parseSingleFromString("(type-of 42)")
        let intResult = evaluator.eval(intExpr)
        println("type-of 42: ${intResult.toString()}")
        @Assert(intResult.toString() == "\"integer\"")

        // 测试浮点数
        let floatExpr = Parser.parseSingleFromString("(type-of 3.14)")
        let floatResult = evaluator.eval(floatExpr)
        println("type-of 3.14: ${floatResult.toString()}")
        @Assert(floatResult.toString() == "\"float\"")

        // 测试字符串
        let strExpr = Parser.parseSingleFromString("(type-of \"hello\")")
        let strResult = evaluator.eval(strExpr)
        println("type-of \"hello\": ${strResult.toString()}")
        @Assert(strResult.toString() == "\"string\"")

        // 测试符号
        let symExpr = Parser.parseSingleFromString("(type-of 'foo)")
        let symResult = evaluator.eval(symExpr)
        println("type-of 'foo: ${symResult.toString()}")
        @Assert(symResult.toString() == "\"symbol\"")

        // 测试列表
        let listExpr = Parser.parseSingleFromString("(type-of '(1 2 3))")
        let listResult = evaluator.eval(listExpr)
        println("type-of '(1 2 3): ${listResult.toString()}")
        @Assert(listResult.toString() == "\"list\"")

        // 测试空列表（在 Lisp 中，空列表和 nil 是同一个东西）
        let emptyListExpr = Parser.parseSingleFromString("(type-of '())")
        let emptyListResult = evaluator.eval(emptyListExpr)
        println("type-of '(): ${emptyListResult.toString()}")
        @Assert(emptyListResult.toString() == "\"nil\"")

        // 测试 nil
        let nilExpr = Parser.parseSingleFromString("(type-of nil)")
        let nilResult = evaluator.eval(nilExpr)
        println("type-of nil: ${nilResult.toString()}")
        @Assert(nilResult.toString() == "\"nil\"")

        // 测试布尔值
        let boolExpr = Parser.parseSingleFromString("(type-of #t)")
        let boolResult = evaluator.eval(boolExpr)
        println("type-of #t: ${boolResult.toString()}")
        @Assert(boolResult.toString() == "\"boolean\"")

        // 测试过程
        let procExpr = Parser.parseSingleFromString("(type-of +)")
        let procResult = evaluator.eval(procExpr)
        println("type-of +: ${procResult.toString()}")
        @Assert(procResult.toString() == "\"procedure\"")
    }
}
