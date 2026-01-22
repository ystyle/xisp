package ystyle::xisp.cli

import ystyle::xisp.*
import std.fs.*
import std.io.*

main(args: Array<String>): Int64 {
    // 解析命令行参数
    if (args.size < 1) {
        // 无参数：启动 REPL
        runRepl()
    } else {
        // 有参数：执行脚本文件
        let filePath = args[0]
        runScript(filePath)
    }
    0
}

/**
 * 启动 REPL 交互式环境
 */
func runRepl(): Unit {
    // 默认配置：英文关键字，启用所有标准库
    let interpreter = LispInterpreter([
        // 取消注释以下行来启用对应功能：
        // withChineseKeywords(),    // 启用中文关键字
        // withQuietMode(),          // 不显示 Banner
    ])
    interpreter.runREPL()
}

/**
 * 执行脚本文件
 */
func runScript(filePath: String): Unit {
    // 创建解释器
    let interpreter = LispInterpreter([withQuietMode()])

    // 读取文件内容
    let content = try {
        let bytes = File.readFrom(filePath)
        String.fromUtf8(bytes)
    } catch (e: FSException) {
        eprintln("错误: 无法读取文件: ${e.message}")
        return
    } catch (e: Exception) {
        eprintln("错误: ${e.message}")
        return
    }

    // 执行脚本
    try {
        interpreter.evalMultiple(content)
    } catch (e: Exception) {
        eprintln("错误: ${e.message}")
    }
}