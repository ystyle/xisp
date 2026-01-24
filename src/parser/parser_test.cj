package ystyle::xisp.parser

import ystyle::xisp.types.*
import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList

/**
 * Parser 测试项
 */
class ParserTestItem {
    ParserTestItem(let input: String, let expectedStr: String) {}
}

var parserTests = ArrayList<ParserTestItem>([
    // 数字
    ParserTestItem("42", "42"),
    // 字符串
    ParserTestItem("\"hello\"", "\"hello\""),
    // 符号
    ParserTestItem("foo", "foo"),
    // 布尔值
    ParserTestItem("#t", "true"),
    ParserTestItem("#f", "false"),
    // 空列表
    ParserTestItem("()", "nil"),
    // 简单列表
    ParserTestItem("(a b c)", "(a b c)"),
    // 嵌套列表
    ParserTestItem("(a (b c) d)", "(a (b c) d)"),
    // 点对
    ParserTestItem("(a . b)", "(a . b)"),
    // 引用
    ParserTestItem("'foo", "(quote foo)"),
    // 复杂表达式
    ParserTestItem("(define square (lambda (x) (* x x)))", "(define square (lambda (x) (* x x)))"),
    // match 表达式 - 验证嵌套列表解析正确
    ParserTestItem("(match 5 1 \"one\" 5 \"five\")", "(match 5 1 \"one\" 5 \"five\")"),
    // match 嵌套列表
    ParserTestItem("(match (quote (1 2 3)) (x y z) \"ok\")", "(match (quote (1 2 3)) (x y z) \"ok\")"),
])

/**
 * Parser 测试
 */
@Test
class ParserTest {
    @TestCase
    func testParseSingle() {
        for (i in 0..parserTests.size) {
            let test = parserTests[i]
            println("[${i}] Testing: ${test.input}")

            let expr = Parser.parseSingleFromString(test.input)
            let result = expr.toString()

            println("[${i}] result: ${result}, expected contains: ${test.expectedStr}")

            if (!result.contains(test.expectedStr)) {
                @Fail("[${i}] Expected '${result}' to contain '${test.expectedStr}'")
            }
        }
    }

    @TestCase
    func testParseMultiple() {
        let exprs = Parser.parseFromString("42 100 \"hello\"")

        println("Parsed ${exprs.size} expressions")
        @Assert(exprs.size, 3)

        match (exprs[0]) {
            case LispValue.Int(i) =>
                println("[0] Int: ${i}")
                @Assert(i > 40)
            case LispValue.Float(f) =>
                println("[0] Float: ${f}")
                @Assert(f > 40.0)
            case _ => @Fail("[0] Expected Number")
        }

        match (exprs[1]) {
            case LispValue.Int(i) =>
                println("[1] Int: ${i}")
                @Assert(i > 99)
            case LispValue.Float(f) =>
                println("[1] Float: ${f}")
                @Assert(f > 99.0)
            case _ => @Fail("[1] Expected Number")
        }

        match (exprs[2]) {
            case Str(s) =>
                println("[2] String: ${s}")
                @Assert(s, "hello")
            case _ => @Fail("[2] Expected String")
        }
    }

    @TestCase
    func testListProperties() {
        let expr = Parser.parseSingleFromString("(a b c)")

        @Assert(expr.isList())
        @Assert(expr.toString(), "(a b c)")

        match (expr) {
            case Cons(cell) =>
                println("List length: ${cell.length()}")
                @Assert(cell.length(), 3)
            case _ => @Fail("Expected Cons cell")
        }
    }

    @TestCase
    func testDottedList() {
        let expr = Parser.parseSingleFromString("(a . b)")

        println("Dotted list: ${expr.toString()}")
        @Assert(expr.toString(), "(a . b)")

        match (expr) {
            case Cons(cell) =>
                println("car: ${cell.car.toString()}, cdr: ${cell.cdr.toString()}")
                @Assert(cell.car.toString(), "a")
                @Assert(cell.cdr.toString(), "b")
            case _ => @Fail("Expected Cons cell")
        }
    }

    @TestCase
    func testMatchExpressionNoExtraTokens() {
        // 测试match表达式解析后没有多余的nil token
        let input = "(match (quote (1 2 3)) (x y z) (str \"x=\" x) _ \"default\")"
        let exprs = Parser.parseFromString(input)

        println("Parsed ${exprs.size} expressions from: ${input}")

        // 应该只解析出1个表达式
        if (exprs.size != 1) {
            @Fail("Expected 1 expression, got ${exprs.size}")
        }

        // 第一个表达式不应该是nil
        match (exprs[0]) {
            case Nil => @Fail("First expression should not be Nil")
            case _ => ()
        }

        println("Successfully parsed match expression without extra tokens")
    }

    @TestCase
    func testParseRestParameter() {
        println("=== Test: Parse & (rest parameter) ===")

        // 最简单的测试：只解析 & 符号
        let input = "&"
        println("Parsing: ${input}")

        let exprs = Parser.parseFromString(input)

        println("Parsed ${exprs.size} expression(s)")
        println("SUCCESS")
    }
}
