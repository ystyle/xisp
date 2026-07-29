package ystyle::xisp.cli

import ystyle::xisp.*
import ystyle::xisp.types.*
import std.fs.*
import std.io.*

main(args: Array<String>): Int64 {
    if (args.size < 1) {
        runRepl(false)
    } else {
        let firstArg = args[0]
        if (firstArg == "-h" || firstArg == "--help") {
            printHelp()
            return 0
        }
        if (firstArg == "-c" && args.size >= 2) {
            let code = args[1]
            runCode(code)
        } else if (firstArg == "--simple-repl") {
            runRepl(true)
        } else {
            runScript(firstArg)
        }
    }
    0
}

func createInterpreter(options: Array<InterpreterOption>): LispInterpreter {
    LispInterpreter(options)
}

func printHelp(): Unit {
    println("╔═══════════════════════════════════════════════════════════╗")
    println("║           星枢 - 仓颉嵌入式 Lisp 脚本语言                 ║")
    println("║                   版本 0.1.0 - MVP                        ║")
    println("╚═══════════════════════════════════════════════════════════╝")
    println("")
    println("用法:")
    println("  ystyle::xisp.cli                    启动 REPL 交互式环境")
    println("  ystyle::xisp.cli --simple-repl      启动简单 REPL（非 TTY 回退）")
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

func runRepl(useSimple: Bool): Unit {
    let interpreter = createInterpreter([
        withStdLib(),
        withScriptMode()
    ])
    if (useSimple) {
        interpreter.runSimpleREPL()
    } else {
        interpreter.runREPL()
    }
}

func runCode(code: String): Unit {
    let interpreter = createInterpreter([
        withStdLib(),
        withQuietMode(),
        withScriptMode()
    ])
    try {
        let result = interpreter.eval(code)
        printResult(result)
    } catch (e: Exception) {
        eprintln("错误: ${e.message}")
    }
}

func printResult(result: LispValue): Unit {
    match (result) {
        case Nil => ()
        case _ => println(result.toString())
    }
}

func runScript(filePath: String): Unit {
    let interpreter = createInterpreter([
        withStdLib(),
        withQuietMode(),
        withScriptMode()
    ])
    interpreter.evalFile(filePath)
}
