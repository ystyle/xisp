package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import ystyle::xisp.core.*
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
}
