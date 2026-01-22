package ystyle::xisp.examples.sandbox_demo

import ystyle::xisp.*

main() {
    println("=== Xisp 沙箱系统演示 ===\n")

    // ==================== 测试 1: 栈深度限制 ====================
    println("1. 测试栈深度限制")
    let test1 = LispInterpreter([
        withMaxStackDepth(10),
        withQuietMode()
    ])

    let code1 = "(define (deep-recurse n) (if (> n 0) (deep-recurse (- n 1)) n)) (deep-recurse 20)"
    println("代码: ${code1}")
    let result1 = test1.eval(code1)
    println("结果: ${result1.toString()}")
    println("说明: 应该返回栈深度超出错误")
    println()

    // ==================== 测试 2: 函数黑名单 ====================
    println("2. 测试函数黑名单")
    let test2 = LispInterpreter([
        withBlockedFunctions(["eval", "apply"]),
        withQuietMode()
    ])

    let code2 = "(+ 1 2)"
    println("允许调用 +:")
    println("代码: ${code2}")
    let result2 = test2.eval(code2)
    println("结果: ${result2}")

    let code2b = "(eval (+ 1 2))"
    println("禁止调用 eval:")
    println("代码: ${code2b}")
    let result2b = test2.eval(code2b)
    println("结果: ${result2b}")
    println("说明: eval 被禁止，应返回错误")
    println()

    // ==================== 测试 3: 文件写入限制 ====================
    println("3. 测试文件写入限制")
    let test3 = LispInterpreter([
        withNoFileWrite(),
        withStdIO(),
        withQuietMode()
    ])

    let code3 = "(cangjie:io:write-file \"/tmp/test.txt\" \"hello\")"
    println("代码: ${code3}")
    let result3 = test3.eval(code3)
    println("结果: ${result3}")
    println("说明: 文件写入被禁止，应返回错误")
    println()

    // ==================== 测试 4: 路径白名单 ====================
    println("4. 测试路径白名单")
    let test4 = LispInterpreter([
        withAllowedPaths(["/tmp/"]),
        withStdIO(),
        withQuietMode()
    ])

    println("允许写入 /tmp/safe.txt:")
    let code4a = "(cangjie:io:write-file \"/tmp/safe.txt\" \"safe content\")"
    let result4a = test4.eval(code4a)
    println("结果: ${result4a}")

    println("禁止写入 /etc/passwd:")
    let code4b = "(cangjie:io:write-file \"/etc/passwd\" \"hack\")"
    let result4b = test4.eval(code4b)
    println("结果: ${result4b}")
    println("说明: 只有白名单中的路径可以访问")
    println()

    // ==================== 测试 5: 严格沙箱模式 ====================
    println("5. 测试严格沙箱模式（预设配置）")
    let test5 = LispInterpreter([
        withSandbox(),
        withStdIO(),
        withQuietMode()
    ])

    println("尝试执行危险操作:")
    let code5 = "(cangjie:io:write-file \"/etc/passwd\" \"hack\")"
    let result5 = test5.eval(code5)
    println("结果: ${result5}")
    println("说明: 严格沙箱模式自动禁止文件写入和危险函数")
    println()

    // ==================== 测试 6: 沙箱中的正常操作 ====================
    println("6. 测试沙箱中的正常操作")
    let test6 = LispInterpreter([
        withSandbox(),
        withQuietMode()
    ])

    let code6 = "(define (square x) (* x x)) (square 5)"
    println("代码: 求平方函数")
    let result6 = test6.eval(code6)
    println("结果: ${result6.toString()}")
    println("说明: 正常的数学运算不受影响")
    println()

    // ==================== 测试 7: 函数白名单 ====================
    println("7. 测试函数白名单（只允许数学运算）")
    let test7 = LispInterpreter([
        withAllowedFunctions(["+", "-", "*", "/", "println", "define", "lambda", "if", ">", "<", "="]),
        withQuietMode()
    ])

    println("允许调用 +:")
    let code7a = "(+ 1 2 3)"
    let result7a = test7.eval(code7a)
    println("结果: ${result7a.toString()}")

    println("禁止调用 list:")
    let code7b = "(list 1 2 3)"
    let result7b = test7.eval(code7b)
    println("结果: ${result7b.toString()}")
    println("说明: 只有白名单中的函数可以调用")
    println()

    println("=== 演示完成 ===")
    println("\n沙箱系统已成功实现以下功能：")
    println("✅ 调用栈深度限制")
    println("✅ 函数黑名单/白名单")
    println("✅ 文件访问控制（读/写）")
    println("✅ 路径白名单")
    println("✅ 预设沙箱模式（严格/宽松）")
}
