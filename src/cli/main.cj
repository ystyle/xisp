package ystyle::xisp.cli

import ystyle::xisp.*
import ystyle::xisp.types.*
import std.fs.*
import std.io.*

main(args: Array<String>): Int64 {
    // 解析命令行参数
    if (args.size < 1) {
        // 无参数：启动 REPL
        runRepl()
    } else {
        let firstArg = args[0]

        // 检查是否是 -h 参数
        if (firstArg == "-h" || firstArg == "--help") {
            printHelp()
            return 0
        }

        // 检查是否是 -c 参数
        if (firstArg == "-c" && args.size >= 2) {
            // -c 模式：直接执行代码
            let code = args[1]
            runCode(code)
        } else {
            // 有参数：执行脚本文件
            runScript(firstArg)
        }
    }
    0
}

/**
 * 创建解释器（公共方法）
 * @param options 解释器选项数组
 * @return 配置好的解释器
 */
func createInterpreter(options: Array<InterpreterOption>): LispInterpreter {
    LispInterpreter(options)
}

/**
 * 打印帮助信息
 */
func printHelp(): Unit {
    println("╔═══════════════════════════════════════════════════════════╗")
    println("║           星枢 - 仓颉嵌入式 Lisp 脚本语言                 ║")
    println("║                   版本 0.1.0 - MVP                        ║")
    println("╚═══════════════════════════════════════════════════════════╝")
    println("")
    println("用法:")
    println("  ystyle::xisp.cli                    启动 REPL 交互式环境")
    println("  ystyle::xisp.cli <script.lisp>       执行 Lisp 脚本文件")
    println("  ystyle::xisp.cli -c \"<code>\"        直接执行 Lisp 代码")
    println("  ystyle::xisp.cli -h|--help          显示此帮助信息")
    println("")
    println("环境变量:")
    println("  XISP_PATH=<paths>                   模块搜索路径（用 : 分隔）")
    println("")
    println("示例:")
    println("  ystyle::xisp.cli")
    println("  ystyle::xisp.cli script.lisp")
    println("  XISP_PATH=./examples ystyle::xisp.cli -c \"(import pkg1)\"")
    println("")
}

/**
 * 启动 REPL 交互式环境
 */
func runRepl(): Unit {
    // 默认配置：英文关键字，启用所有标准库
    let interpreter = createInterpreter([
        withStdLib()  // 启用标准库
        // 可选：withChineseKeywords(), withQuietMode()
    ])
    interpreter.runREPL()
}

/**
 * 执行代码字符串（-c 参数）
 */
func runCode(code: String): Unit {
    // 创建解释器
    let interpreter = createInterpreter([
        withStdLib(),
        withQuietMode()
    ])

    // 执行代码
    try {
        let result = interpreter.eval(code)
        // 如果有返回值，打印出来
        printResult(result)
    } catch (e: Exception) {
        eprintln("错误: ${e.message}")
    }
}

/**
 * 打印求值结果
 */
func printResult(result: LispValue): Unit {
    match (result) {
        case Nil => ()  // nil 不打印
        case _ => println(result.toString())
    }
}

/**
 * 执行脚本文件
 */
func runScript(filePath: String): Unit {
    // 创建解释器
    let interpreter = createInterpreter([
        withStdLib(),
        withQuietMode()
    ])

    // 使用 interpreter.evalFile() 执行文件
    interpreter.evalFile(filePath)
}