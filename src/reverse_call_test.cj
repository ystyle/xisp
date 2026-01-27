package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import ystyle::xisp.bridge.*
import ystyle::xisp.core.*
import ystyle::xisp.types.*

/**
 * 反向调用（仓颉调用 Lisp）测试
 * 测试 LispDeserializable 接口和 call 方法
 */
@Test
class ReverseCallTest {
    @TestCase
    func testCallWithInt() {
        println("\n=== 测试 call 方法 - 整数类型 ===")

        let interpreter = LispInterpreter([withStdLib()])

        // 定义 Lisp 函数
        interpreter.eval("(define (square x) (* x x))")

        // 从仓颉调用
        let result = interpreter.call("square", [5])

        // 在 match 内部完成所有测试
        var asIntSuccess = false
        var asCjValueSuccess = false

        match (result) {
            case LispValue.Int(i) =>
                @Assert(i == 25)
                println("✓ result 是 Int(25)")
                asIntSuccess = true
            case _ =>
                @Fail("期望 Int(25)")
        }

        // 测试便捷方法 asInt()
        if (let Some(i) <- result.asInt()) {
            @Assert(i == 25)
            println("✓ asInt() 提取成功: ${i}")
            asIntSuccess = true
        }

        // 测试泛型方法 asCjValue<Int64>()
        if (let Some(i2) <- result.asCjValue<Int64>()) {
            @Assert(i2 == 25)
            println("✓ asCjValue<Int64>() 提取成功: ${i2}")
            asCjValueSuccess = true
        }

        @Assert(asIntSuccess && asCjValueSuccess)
    }

    @TestCase
    func testCallWithFloat() {
        println("\n=== 测试 call 方法 - 浮点类型 ===")

        let interpreter = LispInterpreter([withStdLib()])

        // 定义返回浮点数的函数
        interpreter.eval("(define (get-pi) 3.14159)")

        let result = interpreter.call("get-pi", [])

        var success = false
        if (let Some(f) <- result.asFloat()) {
            @Assert(f > 3.14 && f < 3.15)
            println("✓ asFloat() 提取成功: ${f}")
            success = true
        }
        @Assert(success)
    }

    @TestCase
    func testCallWithString() {
        println("\n=== 测试 call 方法 - 字符串类型 ===")

        let interpreter = LispInterpreter([withStdLib()])

        // 定义返回字符串的函数
        interpreter.eval("(define (greet name) (str \"Hello, \" name))")

        let result = interpreter.call("greet", ["World"])

        var success = false
        if (let Some(s) <- result.asString()) {
            @Assert(s == "Hello, World")
            println("✓ asString() 提取成功: ${s}")
            success = true
        }
        @Assert(success)
    }

    @TestCase
    func testCallWithBool() {
        println("\n=== 测试 call 方法 - 布尔类型 ===")

        let interpreter = LispInterpreter([withStdLib()])

        // 定义返回布尔值的函数
        interpreter.eval("(define (is-positive x) (> x 0))")

        let result1 = interpreter.call("is-positive", [5])
        var success1 = false
        if (let Some(b1) <- result1.asBool()) {
            @Assert(b1 == true)
            println("✓ is-positive(5) = ${b1}")
            success1 = true
        }
        @Assert(success1)

        let result2 = interpreter.call("is-positive", [-5])
        var success2 = false
        if (let Some(b2) <- result2.asBool()) {
            @Assert(b2 == false)
            println("✓ is-positive(-5) = ${b2}")
            success2 = true
        }
        @Assert(success2)
    }

    @TestCase
    func testCallWithMultipleArgs() {
        println("\n=== 测试 call 方法 - 多参数 ===")

        let interpreter = LispInterpreter([withStdLib()])

        // 定义多参数函数
        interpreter.eval("(define (add-three a b c) (+ a b c))")

        let result = interpreter.call("add-three", [1, 2, 3])

        var success = false
        if (let Some(i) <- result.asInt()) {
            @Assert(i == 6)
            println("✓ add-three(1, 2, 3) = ${i}")
            success = true
        }
        @Assert(success)
    }

    @TestCase
    func testCallNoArgs() {
        println("\n=== 测试 call 方法 - 无参数 ===")

        let interpreter = LispInterpreter([withStdLib()])

        // 定义无参数函数
        interpreter.eval("(define (constant) 42)")

        let result = interpreter.call("constant", [])

        var success = false
        if (let Some(i) <- result.asInt()) {
            @Assert(i == 42)
            println("✓ constant() = ${i}")
            success = true
        }
        @Assert(success)
    }

    @TestCase
    func testTypeMismatch() {
        println("\n=== 测试类型不匹配 ===")

        let interpreter = LispInterpreter([withStdLib()])

        // 定义返回字符串的函数
        interpreter.eval("(define (get-string) \"hello\")")

        let result = interpreter.call("get-string", [])

        // 尝试提取为 Int64 应该返回 None
        if (let Some(i) <- result.asInt()) {
            @Fail("不应该提取为 Int64: ${i}")
        } else {
            println("✓ 类型不匹配正确返回 None")
        }
    }

    @TestCase
    func testDirectFromLisp() {
        println("\n=== 测试直接使用 fromLisp 静态方法 ===")

        let lispInt = LispValue.Int(42)
        let lispFloat = LispValue.Float(3.14)
        let lispString = LispValue.Str("hello")
        let lispBool = LispValue.Boolean(true)

        // 测试 Int64.fromLisp
        match (Int64.fromLisp(lispInt)) {
            case Some(i) =>
                @Assert(i == 42)
                println("✓ Int64.fromLisp(Int(42)) = ${i}")
            case None => @Fail("Int64.fromLisp 失败")
        }

        // 测试 Float64.fromLisp
        match (Float64.fromLisp(lispFloat)) {
            case Some(f) =>
                @Assert(f > 3.13 && f < 3.15)
                println("✓ Float64.fromLisp(Float(3.14)) = ${f}")
            case None => @Fail("Float64.fromLisp 失败")
        }

        // 测试 String.fromLisp
        match (String.fromLisp(lispString)) {
            case Some(s) =>
                @Assert(s == "hello")
                println("✓ String.fromLisp(Str(\"hello\")) = ${s}")
            case None => @Fail("String.fromLisp 失败")
        }

        // 测试 Bool.fromLisp
        match (Bool.fromLisp(lispBool)) {
            case Some(b) =>
                @Assert(b == true)
                println("✓ Bool.fromLisp(Boolean(true)) = ${b}")
            case None => @Fail("Bool.fromLisp 失败")
        }
    }

    @TestCase
    func testIntFromFloat() {
        println("\n=== 测试 Int 从 Float 转换 ===")

        let lispFloat = LispValue.Float(3.14)

        match (Int64.fromLisp(lispFloat)) {
            case Some(i) =>
                @Assert(i == 3)
                println("✓ Int64.fromLisp(Float(3.14)) = ${i}")
            case None => @Fail("Int64.fromLisp 失败")
        }
    }

    @TestCase
    func testFloatFromInt() {
        println("\n=== 测试 Float 从 Int 转换 ===")

        let lispInt = LispValue.Int(42)

        match (Float64.fromLisp(lispInt)) {
            case Some(f) =>
                @Assert(f == 42.0)
                println("✓ Float64.fromLisp(Int(42)) = ${f}")
            case None => @Fail("Float64.fromLisp 失败")
        }
    }

    @TestCase
    func testStringFromSymbol() {
        println("\n=== 测试 String 从 Symbol 转换 ===")

        let lispSymbol = LispValue.Symbol("test-symbol")

        match (String.fromLisp(lispSymbol)) {
            case Some(s) =>
                @Assert(s == "test-symbol")
                println("✓ String.fromLisp(Symbol(\"test-symbol\")) = ${s}")
            case None => @Fail("String.fromLisp 失败")
        }
    }

    @TestCase
    func testComplexScenario() {
        println("\n=== 测试复杂场景 ===")

        let interpreter = LispInterpreter([withStdLib()])

        // 定义多个函数
        interpreter.eval("""
            (define (calculate-discount price discount-rate)
                (* price (- 1.0 discount-rate)))

            (define (is-expensive price)
                (> price 100))
        """)

        // 测试 1: 调用计算折扣
        let result1 = interpreter.call("calculate-discount", [100.0, 0.2])
        match (result1) {
            case LispValue.Float(f) => @Assert(f > 79.9 && f < 80.1)
            case _ => @Fail("期望返回 Float")
        }

        // 测试 2: 调用判断是否昂贵
        let result2 = interpreter.call("is-expensive", [150])
        match (result2) {
            case Boolean(b) => @Assert(b == true)
            case _ => @Fail("期望返回 Boolean")
        }
    }
}
