package ystyle::xisp.examples.options_usage

import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.bridge.*
import ystyle::xisp.repl.*
import std.io.*

/**
 * Xisp 选项系统使用示例
 *
 * 演示如何使用不同的选项配置解释器
 *
 * 注意：由于主包 ystyle::xisp 是 executable 类型，无法直接导入
 * 这里手动创建解释器组件来演示选项系统的效果
 */
main(): Int64 {
    println("╔═══════════════════════════════════════════════════════════╗")
    println("║          Xisp 选项系统使用示例                           ║")
    println("╚═══════════════════════════════════════════════════════════╝")

    // 说明
    println("\n本示例演示如何使用选项系统配置解释器：")
    println("  1. withChineseKeywords()   - 启用中文关键字")
    println("  2. withJapaneseKeywords()  - 启用日文关键字")
    println("  3. withKeywordAlias()      - 自定义关键字别名")
    println("  4. withStdIO()             - 启用 std.io 模块")
    println("  5. withStdFS()             - 启用 std.fs 模块")
    println("  6. withQuietMode()         - 静默模式（不显示 Banner）")
    println("  7. withVerboseMode()       - 详细模式")
    println("  8. withDebugMode()         - 调试模式")

    println("\n" + "=" * 60)
    println("使用方法：")
    println("=" * 60)
    println("""
在 src/main.cj 中配置选项：

main(): Int64 {
    let interpreter = LispInterpreter([
        withChineseKeywords(),    // 启用中文关键字
        withStdIO(),              // 启用文件 I/O
        withVerboseMode(),        // 显示详细信息
    ])
    interpreter.runREPL()
}

或在 REPL 中动态启用：
xisp> ,lang zh        // 启用中文关键字
xisp> (定义 x 42)     // 使用中文定义变量
xisp> (打印 x)        // 使用中文打印
42.000000
    """)

    println("\n" + "=" * 60)
    println("代码示例：")
    println("=" * 60)
    println("""
// 示例 1：启用中文关键字
let interpreter1 = LispInterpreter([
    withChineseKeywords()
])
// 此时可以使用：(定义 x 10)、(过程 (x) (* x x)) 等

// 示例 2：自定义企业简化版
let interpreter2 = LispInterpreter([
    withKeywordAliases([
        ("def", "define"),
        ("fn", "lambda"),
        ("->", "thread-first")
    ])
])
// 此时可以使用：(def x 10)、(fn (x) (* x x)) 等

// 示例 3：多语言混合
let interpreter3 = LispInterpreter([
    withChineseKeywords(),
    withJapaneseKeywords(),
    withKeywordAlias("정의", "define"),  // 韩语
])
// 中日韩三种语言关键字都可以使用

// 示例 4：完整配置
let interpreter4 = LispInterpreter([
    withChineseKeywords(),
    withStdLib(),          // 启用所有标准库
    withVerboseMode(),     // 详细输出
    withDebugMode(),       // 调试模式
])
    """)

    println("\n" + "=" * 60)
    println("启动默认配置的 REPL...")
    println("=" * 60)

    // 创建一个默认配置的解释器组件
    let env = Environment()
    let evaluator = Evaluator(env)
    let repl = Repl(env)

    // 注册一些标准函数
    Bridge.registerFunc(env, "demo", { values =>
        println("选项系统演示成功！")
        Str("OK")
    })

    // 设置不显示 Banner
    repl.setShowBanner(false)

    println("提示：输入 ',lang zh' 启用中文关键字")
    println("      输入 ',help' 查看帮助")
    println("      输入 ',exit' 退出")
    println("")

    // 运行 REPL
    repl.run()

    0
}
