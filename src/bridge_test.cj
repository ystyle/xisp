package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.bridge.*

/**
 * Bridge 桥接功能单元测试
 */
@Test
class BridgeTest {
    @TestCase
    func testRegisterStdCollection() {
        println("\n=== 测试标准集合桥接 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试向量字面量 [1 2 3]
        let expr1 = Parser.parseSingleFromString("[1 2 3]")
        let result1 = evaluator.eval(expr1)
        println("Vector result: ${result1.toString()}")
        @Assert(result1.isList())

        // 测试哈希映射字面量
        let expr2 = Parser.parseSingleFromString("{:name \"Alice\" :age 30}")
        let result2 = evaluator.eval(expr2)
        println("Hash map result: ${result2.toString()}")
        @Assert(!result2.isNil())
    }

    @TestCase
    func testStringInterpolation() {
        println("\n=== 测试字符串插值桥接 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 测试字符串插值
        let expr = Parser.parseSingleFromString("#\"Value: #{+ 1 2}\"")
        println("Parsed expr: ${expr.toString()}")
        let result = evaluator.eval(expr)
        println("String interpolation result: ${result.toString()}")
        match (result) {
            case Str(s) => @Assert(s == "Value: 3")
            case _ => @Fail("String interpolation should work")
        }
    }

    @TestCase
    func testVectorWithBuiltin() {
        println("\n=== 测试向量与内置函数集成 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 创建向量
        let expr1 = Parser.parseSingleFromString("(define vec [1 2 3 4 5])")
        evaluator.eval(expr1)

        // 测试 length
        let expr2 = Parser.parseSingleFromString("(length vec)")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Int(i) => @Assert(i == 5)
            case LispValue.Float(f) => @Assert(f == 5.0)
            case _ => @Fail("Vector length should be 5")
        }

        // 测试 first
        let expr3 = Parser.parseSingleFromString("(first vec)")
        let result3 = evaluator.eval(expr3)
        match (result3) {
            case LispValue.Int(i) => @Assert(i == 1)
            case LispValue.Float(f) => @Assert(f == 1.0)
            case _ => @Fail("Vector first should be 1")
        }
    }

    @TestCase
    func testHashMapAccess() {
        println("\n=== 测试哈希映射访问 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 创建哈希映射
        let expr1 = Parser.parseSingleFromString("(define map {:a 1 :b 2 :c 3})")
        evaluator.eval(expr1)

        // 测试使用 hashmap-get 访问
        let expr2 = Parser.parseSingleFromString("(hashmap-get map ':a)")
        let result2 = evaluator.eval(expr2)
        println("Get key 'a': ${result2.toString()}")
        @Assert(!result2.isNil())

        // 测试获取所有键
        let expr3 = Parser.parseSingleFromString("(hashmap-keys map)")
        let result3 = evaluator.eval(expr3)
        println("Keys: ${result3.toString()}")
        @Assert(result3.isList())
    }

    @TestCase
    func testHashSetWithBuiltin() {
        println("\n=== 测试哈希集合 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        Bridge.registerStdCollection(env)
        let evaluator = Evaluator(env)

        // 创建哈希集合
        let expr1 = Parser.parseSingleFromString("(define set #{1 2 3 4 5})")
        evaluator.eval(expr1)

        // 测试 length
        let expr2 = Parser.parseSingleFromString("(length set)")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Int(i) => @Assert(i == 5)
            case LispValue.Float(f) => @Assert(f == 5.0)
            case _ => @Fail("HashSet length should be 5")
        }

        // 测试 first
        let expr3 = Parser.parseSingleFromString("(first set)")
        let result3 = evaluator.eval(expr3)
        match (result3) {
            case LispValue.Int(i) => @Assert(i == 1)
            case LispValue.Float(f) => @Assert(f == 1.0)
            case _ => @Fail("HashSet first should be 1")
        }
    }
}
