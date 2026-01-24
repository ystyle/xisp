package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*

/**
 * 哈希映射功能单元测试
 */
@Test
class BuiltinHashMapTest {
    @TestCase
    func testCreateHashMap() {
        println("\n=== 测试创建哈希映射 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 简单哈希映射
        let expr1 = Parser.parseSingleFromString("{:name \"Alice\" :age 30}")
        let result1 = evaluator.eval(expr1)
        println("HashMap: ${result1.toString()}")
        @Assert(result1.isHashMap())

        // 检查类型
        match (result1) {
            case HashMap(map) => @Assert(map.size == 2)
            case _ => @Fail("Should be a hashmap")
        }
    }

    @TestCase
    func testHashMapGet() {
        println("\n=== 测试获取哈希映射值 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(hashmap-get {:name \"Bob\" :age 25} :name)")
        let result = evaluator.eval(expr)
        println("Get name: ${result.toString()}")
        match (result) {
            case Str(s) => @Assert(s == "Bob")
            case _ => @Fail("Should return string")
        }
    }

    @TestCase
    func testHashMapSet() {
        println("\n=== 测试设置哈希映射值 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr1 = Parser.parseSingleFromString("(define config {:debug #f})")
        evaluator.eval(expr1)

        let expr2 = Parser.parseSingleFromString("(hashmap-set! config :debug #t)")
        evaluator.eval(expr2)

        // 验证修改
        let expr3 = Parser.parseSingleFromString("(hashmap-get config :debug)")
        let result3 = evaluator.eval(expr3)
        println("Get debug after set: ${result3.toString()}")
        match (result3) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("Should return true")
        }
    }

    @TestCase
    func testHashMapRemove() {
        println("\n=== 测试删除哈希映射键 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr1 = Parser.parseSingleFromString("(define data {:x 1 :y 2 :z 3})")
        evaluator.eval(expr1)

        let expr2 = Parser.parseSingleFromString("(hashmap-remove! data :y)")
        evaluator.eval(expr2)

        // 验证删除
        let expr3 = Parser.parseSingleFromString("(hashmap-contains? data :y)")
        let result3 = evaluator.eval(expr3)
        println("Contains y after remove: ${result3.toString()}")
        match (result3) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("Should return false")
        }
    }

    @TestCase
    func testHashMapContains() {
        println("\n=== 测试检查哈希映射键 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr1 = Parser.parseSingleFromString("(hashmap-contains? {:a 1 :b 2} :a)")
        let result1 = evaluator.eval(expr1)
        match (result1) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("Should return true")
        }

        let expr2 = Parser.parseSingleFromString("(hashmap-contains? {:a 1 :b 2} :c)")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("Should return false")
        }
    }

    @TestCase
    func testHashMapKeys() {
        println("\n=== 测试获取哈希映射所有键 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(hashmap-keys {:a 1 :b 2 :c 3})")
        let result = evaluator.eval(expr)
        println("Keys: ${result.toString()}")
        @Assert(result.isList())
    }

    @TestCase
    func testHashMapValues() {
        println("\n=== 测试获取哈希映射所有值 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(hashmap-values {:a 1 :b 2 :c 3})")
        let result = evaluator.eval(expr)
        println("Values: ${result.toString()}")
        @Assert(result.isList())
    }

    @TestCase
    func testHashMapSize() {
        println("\n=== 测试获取哈希映射大小 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr1 = Parser.parseSingleFromString("(hashmap-size {:a 1 :b 2 :c 3})")
        let result1 = evaluator.eval(expr1)
        match (result1) {
            case LispValue.Int(i) => @Assert(i == 3)
            case _ => @Fail("Size should be 3")
        }

        let expr2 = Parser.parseSingleFromString("(hashmap-size {})")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Int(i) => @Assert(i == 0)
            case _ => @Fail("Empty hashmap size should be 0")
        }
    }

    @TestCase
    func testHashMapPredicate() {
        println("\n=== 测试哈希映射谓词 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr1 = Parser.parseSingleFromString("(hashmap? {:a 1})")
        let result1 = evaluator.eval(expr1)
        match (result1) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("Should be hashmap")
        }

        let expr2 = Parser.parseSingleFromString("(hashmap? '(1 2 3))")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("List should not be hashmap")
        }

        let expr3 = Parser.parseSingleFromString("(hashmap? 42)")
        let result3 = evaluator.eval(expr3)
        match (result3) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("Number should not be hashmap")
        }
    }

    @TestCase
    func testNestedHashMap() {
        println("\n=== 测试嵌套哈希映射 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr1 = Parser.parseSingleFromString("(define config {:server {:host \"localhost\" :port 8080}})")
        evaluator.eval(expr1)

        let expr2 = Parser.parseSingleFromString("(hashmap-get config :server)")
        let server = evaluator.eval(expr2)

        // 直接使用 server 的值
        let result = match (server) {
            case HashMap(map) =>
                map.get("host") ?? Nil
            case _ => Nil
        }
        println("Nested get: ${result.toString()}")
        match (result) {
            case Str(s) => @Assert(s == "localhost")
            case _ => @Fail("Should return localhost")
        }
    }

    @TestCase
    func testHashMapWithSymbolKeys() {
        println("\n=== 测试符号键 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("{name \"Alice\" age 30}")
        let result = evaluator.eval(expr)
        println("Symbol keys: ${result.toString()}")
        @Assert(result.isHashMap())
    }

    @TestCase
    func testRedisStyle() {
        println("\n=== 测试 Redis 风格哈希操作 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr1 = Parser.parseSingleFromString("(define config {:host \"localhost\" :port 8080})")
        evaluator.eval(expr1)

        // hget
        let expr2 = Parser.parseSingleFromString("(hget config :host)")
        let result2 = evaluator.eval(expr2)
        println("HGET host: ${result2.toString()}")
        match (result2) {
            case Str(s) => @Assert(s == "localhost")
            case _ => @Fail("hget should return localhost")
        }

        // hset
        let expr3 = Parser.parseSingleFromString("(hset config :port 9090)")
        evaluator.eval(expr3)

        let expr4 = Parser.parseSingleFromString("(hget config :port)")
        let result4 = evaluator.eval(expr4)
        match (result4) {
            case LispValue.Int(i) => @Assert(i == 9090)
            case _ => @Fail("hset should update port")
        }

        // hexists
        let expr5 = Parser.parseSingleFromString("(hexists config :debug)")
        let result5 = evaluator.eval(expr5)
        match (result5) {
            case LispValue.Boolean(b) => @Assert(!b)  // debug 不存在
            case _ => @Fail("hexists should return false")
        }

        // hlen
        let expr6 = Parser.parseSingleFromString("(hlen config)")
        let result6 = evaluator.eval(expr6)
        match (result6) {
            case LispValue.Int(i) => @Assert(i == 2)
            case _ => @Fail("hlen should return 2")
        }

        // hkeys
        let expr7 = Parser.parseSingleFromString("(hkeys config)")
        let result7 = evaluator.eval(expr7)
        println("HKEYS: ${result7.toString()}")
        @Assert(result7.isList())

        // hvals
        let expr8 = Parser.parseSingleFromString("(hvals config)")
        let result8 = evaluator.eval(expr8)
        println("HVALS: ${result8.toString()}")
        @Assert(result8.isList())

        // hgetall
        let expr9 = Parser.parseSingleFromString("(hgetall config)")
        let result9 = evaluator.eval(expr9)
        println("HGETALL: ${result9.toString()}")
        @Assert(result9.isHashMap())

        // hdel
        let expr10 = Parser.parseSingleFromString("(hdel config :host)")
        evaluator.eval(expr10)

        let expr11 = Parser.parseSingleFromString("(hexists config :host)")
        let result11 = evaluator.eval(expr11)
        match (result11) {
            case LispValue.Boolean(b) => @Assert(!b)  // host 被删除
            case _ => @Fail("hdel should remove key")
        }
    }
}
