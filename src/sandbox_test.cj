package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*

/**
 * 沙箱安全功能单元测试
 */
@Test
class SandboxTest {
    @TestCase
    func testBlockFunction() {
        println("\n=== 测试函数黑名单 ===")
        let interp = LispInterpreter()
        BuiltinFunctions.registerAll(interp.getEnvironment())

        // 不使用沙箱，正常调用 println
        let result1 = interp.eval("(println \"test\")")
        @Assert(result1.isNil())

        // 禁止 println
        interp.blockFunction("println")

        // 现在应该被禁止
        let result2 = interp.eval("(println \"blocked\")")

        match (result2) {
            case Str(s) =>
                // 应该返回错误信息
                @Assert(s.contains("not allowed"))
            case _ => @Fail("Blocked function should return error")
        }
    }

    @TestCase
    func testAllowedFunctions() {
        println("\n=== 测试函数白名单 ===")
        let interp = LispInterpreter()
        BuiltinFunctions.registerAll(interp.getEnvironment())

        // 设置只允许 + 和 - 函数
        interp.setAllowedFunctions(["+", "-"])

        // 允许的函数应该能正常工作
        let result1 = interp.eval("(+ 1 2)")
        match (result1) {
            case LispValue.Int(i) => @Assert(i == 3)
            case LispValue.Float(f) => @Assert(f == 3.0)
            case _ => @Fail("Allowed function should work")
        }

        // 不在白名单中的函数应该被禁止
        let result2 = interp.eval("(* 2 3)")
        match (result2) {
            case Str(s) => @Assert(s.contains("not allowed"))
            case _ => @Fail("Non-whitelisted function should be blocked")
        }
    }

    @TestCase
    func testStackDepthLimit() {
        println("\n=== 测试栈深度限制 ===")
        let interp = LispInterpreter()
        BuiltinFunctions.registerAll(interp.getEnvironment())

        // 设置非常小的栈深度限制
        interp.setMaxStackDepth(5)

        // 创建一个递归函数
        let code = """
        (define (recursive depth)
            (if (<= depth 0)
                0
                (+ 1 (recursive (- depth 1)))))
        """

        // 尝试执行（会超过栈深度）
        interp.eval(code)

        let result = interp.eval("(recursive 100)")

        match (result) {
            case Str(s) => @Assert(s.contains("stack depth"))
            case LispValue.Int(i) => @Fail("Should hit stack depth limit, got int: ${i}")
            case LispValue.Float(f) => @Fail("Should hit stack depth limit, got float: ${f}")
            case _ => @Fail("Should hit stack depth limit")
        }
    }

    @TestCase
    func testMultipleBlockedFunctions() {
        println("\n=== 测试批量禁止函数 ===")
        let interp = LispInterpreter()
        BuiltinFunctions.registerAll(interp.getEnvironment())

        // 批量禁止多个函数
        interp.blockFunctions(["println", "print", "display"])

        // 所有被禁止的函数都应该返回错误
        let funcs = ["println", "print", "display"]
        for (funcName in funcs) {
            let result = interp.eval("(${funcName} \"test\")")
            match (result) {
                case Str(s) => @Assert(s.contains("not allowed"))
                case _ => @Fail("Function ${funcName} should be blocked")
            }
        }

        // 未被禁止的函数应该正常工作
        let result = interp.eval("(+ 1 2)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 3)
            case LispValue.Float(f) => @Assert(f == 3.0)
            case _ => @Fail("Allowed function should work")
        }
    }

    @TestCase
    func testAllowedFunctionsWithBuiltins() {
        println("\n=== 测试白名单与内置函数 ===")
        let interp = LispInterpreter()
        BuiltinFunctions.registerAll(interp.getEnvironment())

        // 只允许算术运算函数
        let allowed = ["+", "-", "*", "/", "mod", "<", ">", "<=", ">=", "="]
        interp.setAllowedFunctions(allowed)

        // 测试所有允许的函数 - 测试算术运算
        let result1 = interp.eval("(+ 1 2)")
        match (result1) {
            case LispValue.Int(i) => @Assert(i == 3)
            case LispValue.Float(f) => @Assert(f == 3.0)
            case _ => @Fail("+ should work")
        }

        let result2 = interp.eval("(- 5 2)")
        match (result2) {
            case LispValue.Int(i) => @Assert(i == 3)
            case LispValue.Float(f) => @Assert(f == 3.0)
            case _ => @Fail("- should work")
        }

        let result3 = interp.eval("(* 2 3)")
        match (result3) {
            case LispValue.Int(i) => @Assert(i == 6)
            case LispValue.Float(f) => @Assert(f == 6.0)
            case _ => @Fail("* should work")
        }

        let result4 = interp.eval("(/ 6 2)")
        match (result4) {
            case LispValue.Int(i) => @Assert(i == 3)
            case LispValue.Float(f) => @Assert(f == 3.0)
            case _ => @Fail("/ should work")
        }

        let result5 = interp.eval("(< 1 2)")
        match (result5) {
            case Boolean(b) => @Assert(b == true)
            case _ => @Fail("< should work")
        }

        let result6 = interp.eval("(> 2 1)")
        match (result6) {
            case Boolean(b) => @Assert(b == true)
            case _ => @Fail("> should work")
        }

        // 不允许的函数应该被阻止
        let result = interp.eval("(println \"test\")")
        match (result) {
            case Str(s) => @Assert(s.contains("not allowed"))
            case _ => @Fail("println should be blocked")
        }
    }

    @TestCase
    func testBlockAndAllowCombination() {
        println("\n=== 测试黑名单和白名单组合 ===")
        let interp = LispInterpreter()
        BuiltinFunctions.registerAll(interp.getEnvironment())

        // 设置白名单
        interp.setAllowedFunctions(["+", "-", "*", "/"])

        // 在白名单的基础上再禁止某个函数
        interp.blockFunction("/")

        // 被黑名单阻止的函数
        let result1 = interp.eval("(/ 6 2)")
        match (result1) {
            case Str(s) => @Assert(s.contains("not allowed"))
            case _ => @Fail("Blocked function should not work")
        }

        // 在白名单中但未被黑名单阻止的函数
        let result2 = interp.eval("(* 2 3)")
        match (result2) {
            case LispValue.Int(i) => @Assert(i == 6)
            case LispValue.Float(f) => @Assert(f == 6.0)
            case _ => @Fail("Allowed function should work")
        }
    }

    @TestCase
    func testBlockModuleLoad() {
        println("\n=== 测试禁止模块加载 ===")
        let interp = LispInterpreter([
            withQuietMode()
        ])
        BuiltinFunctions.registerAll(interp.getEnvironment())

        // 禁止模块加载
        interp.blockModuleLoad()

        // 尝试 import 应该被禁止
        let result = interp.eval("(import std::io)")

        match (result) {
            case Str(s) =>
                // 应该返回错误信息
                @Assert(s.contains("not allowed"))
            case _ => @Fail("Module import should be blocked")
        }

        // 检查权限状态
        @Assert(!interp.isModuleLoadAllowed())
    }

    @TestCase
    func testAllowModuleLoad() {
        println("\n=== 测试允许模块加载 ===")
        let interp = LispInterpreter([
            withQuietMode()
        ])
        BuiltinFunctions.registerAll(interp.getEnvironment())

        // 默认应该允许
        @Assert(interp.isModuleLoadAllowed())

        // 禁止后再允许
        interp.blockModuleLoad()
        @Assert(!interp.isModuleLoadAllowed())

        interp.setModuleLoadAllowed(true)
        @Assert(interp.isModuleLoadAllowed())
    }

    @TestCase
    func testWithNoModuleLoadOption() {
        println("\n=== 测试 withNoModuleLoad 选项 ===")
        let interp = LispInterpreter([
            withNoModuleLoad(),
            withQuietMode()
        ])
        BuiltinFunctions.registerAll(interp.getEnvironment())

        // 模块加载应该被禁止
        @Assert(!interp.isModuleLoadAllowed())

        // 尝试 import 应该被禁止
        let result = interp.eval("(import test::module)")

        match (result) {
            case Str(s) =>
                @Assert(s.contains("not allowed"))
            case _ => @Fail("Module import should be blocked")
        }
    }

    @TestCase
    func testWithSandboxIncludesModuleLoadBlock() {
        println("\n=== 测试 withSandbox 包含模块加载禁止 ===")
        let interp = LispInterpreter([
            withSandbox(),
            withQuietMode()
        ])
        BuiltinFunctions.registerAll(interp.getEnvironment())

        // withSandbox 应该禁止模块加载
        @Assert(!interp.isModuleLoadAllowed())

        // 尝试 import 应该被禁止
        let result = interp.eval("(import std::io)")

        match (result) {
            case Str(s) =>
                @Assert(s.contains("not allowed"))
            case _ => @Fail("Module import should be blocked in sandbox")
        }
    }

    @TestCase
    func testPathCheckerIntegration() {
        println("\n=== 测试路径权限检查集成 ===")
        let interp = LispInterpreter([
            withAllowedPaths(["/tmp/"]),
            withNoFileWrite(),  // 禁止文件写入
            withQuietMode()
        ])
        BuiltinFunctions.registerAll(interp.getEnvironment())

        // 检查路径权限
        @Assert(interp.isPathAllowed("/tmp/test.txt", false))  // 读取允许（在白名单中）
        @Assert(!interp.isPathAllowed("/etc/passwd", false))  // 读取不允许（不在白名单中）
        @Assert(!interp.isPathAllowed("/tmp/test.txt", true))  // 写入不允许（被禁止）
    }
}
