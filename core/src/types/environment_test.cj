package ystyle::xisp.types

import std.unittest.*
import std.unittest.testmacro.*

/**
 * Environment 测试
 */
@Test
class EnvironmentTest {
    /**
     * 测试基本变量定义和查找
     */
    @TestCase
    func testBasicDefineAndLookup() {
        let env = Environment()

        env.define("x", LispValue.Float(42.5))  // 使用能精确表示的值
        let result = env.lookup("x")

        @Assert(result.isNumber())
        match (result) {
            case LispValue.Int(n) => @Assert(n > 40)
            case LispValue.Float(f) => @Assert(f > 40.0)
            case _ => @Fail("Expected Number")
        }
    }

    /**
     * 测试变量修改
     */
    @TestCase
    func testSet() {
        let env = Environment()

        env.define("x", LispValue.Float(1.5))
        match (env.lookup("x")) {
            case LispValue.Int(n) => ()
            case LispValue.Float(f) => @Assert(f > 1.0)
            case _ => @Fail("Expected Number")
        }

        let success = env.set("x", LispValue.Float(2.5))
        @Assert(success)
        match (env.lookup("x")) {
            case LispValue.Int(n) => ()
            case LispValue.Float(f) => @Assert(f > 2.0)
            case _ => @Fail("Expected Number")
        }
    }

    /**
     * 测试父环境查找
     */
    @TestCase
    func testParentLookup() {
        let parent = Environment()
        let child = parent.createChild()

        parent.define("x", LispValue.Float(10.5))
        match (child.lookup("x")) {
            case LispValue.Int(n) => ()
            case LispValue.Float(f) => @Assert(f > 10.0)
            case _ => @Fail("Expected Number")
        }
    }

    /**
     * 测试变量遮蔽
     */
    @TestCase
    func testVariableShadowing() {
        let parent = Environment()
        let child = parent.createChild()

        parent.define("x", LispValue.Float(10.5))
        child.define("x", LispValue.Float(20.5))

        match (child.lookup("x")) {
            case LispValue.Int(n) => ()
            case LispValue.Float(f) => @Assert(f > 20.0)
            case _ => @Fail("Expected Number")
        }
        match (parent.lookup("x")) {
            case LispValue.Int(n) => ()
            case LispValue.Float(f) => @Assert(f > 10.0 && f < 15.0)
            case _ => @Fail("Expected Number")
        }
    }

    /**
     * 测试未定义变量
     */
    @TestCase
    func testUndefinedVariable() {
        let env = Environment()

        let result = env.lookup("undefined")
        // 未定义变量现在应该返回错误
        match (result) {
            case Error(err) =>
                @Assert(err.kind == ErrorType.UnboundVariable)
            case _ => @Fail("Undefined variable should return Error")
        }
    }

    /**
     * 测试 contains 方法
     */
    @TestCase
    func testContains() {
        let parent = Environment()
        let child = parent.createChild()

        parent.define("x", LispValue.Int(1))
        child.define("y", LispValue.Int(2))

        @Assert(child.contains("x"))
        @Assert(child.contains("y"))
        @Assert(!child.contains("z"))
    }

    /**
     * 测试环境克隆
     */
    @TestCase
    func testClone() {
        let env = Environment()
        env.define("x", LispValue.Float(42.5))

        let cloned = env.clone()
        match (cloned.lookup("x")) {
            case LispValue.Int(n) => ()
            case LispValue.Float(f) => @Assert(f > 40.0)
            case _ => @Fail("Expected Number")
        }

        // 修改克隆不应影响原环境
        cloned.set("x", LispValue.Float(100.5))
        match (env.lookup("x")) {
            case LispValue.Int(n) => ()
            case LispValue.Float(f) => @Assert(f > 40.0 && f < 50.0)
            case _ => @Fail("Expected Number")
        }
        match (cloned.lookup("x")) {
            case LispValue.Int(n) => ()
            case LispValue.Float(f) => @Assert(f > 100.0)
            case _ => @Fail("Expected Number")
        }
    }

    /**
     * 测试 getKeys 方法
     */
    @TestCase
    func testGetKeys() {
        let env = Environment()
        env.define("x", LispValue.Int(1))
        env.define("y", LispValue.Int(2))
        env.define("z", LispValue.Int(3))

        let keys = env.getKeys()
        @Assert(keys.size == 3)
    }

    /**
     * 测试关键字别名链短路
     * 无别名时 lookupKeyword 直接返回原符号（O(1)），
     * 有别名时子环境能查到父环境注册的别名
     */
    @TestCase
    func testKeywordAliasChain() {
        // 无别名环境：lookupKeyword 应直接返回原符号
        let plainEnv = Environment()
        @Assert(plainEnv.lookupKeyword("define") == "define")
        @Assert(plainEnv.lookupKeyword("if") == "if")

        // 子环境继承无别名状态，仍应短路
        let plainChild = plainEnv.createChild()
        @Assert(plainChild.lookupKeyword("define") == "define")

        // 注册别名后：本环境能解析
        let env = Environment()
        env.registerKeywordAlias("定义", "define")
        @Assert(env.lookupKeyword("定义") == "define")
        @Assert(env.lookupKeyword("define") == "define")

        // 子环境继承别名状态，能解析父环境别名
        let child = env.createChild()
        @Assert(child.lookupKeyword("定义") == "define")
        // 孙环境也能解析（多层链）
        let grandChild = child.createChild()
        @Assert(grandChild.lookupKeyword("定义") == "define")

        // 克隆环境保留别名状态
        let cloned = env.clone()
        @Assert(cloned.lookupKeyword("定义") == "define")
        let clonedChild = cloned.createChild()
        @Assert(clonedChild.lookupKeyword("定义") == "define")
    }
}
