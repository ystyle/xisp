package ystyle::xisp.cli

import ystyle::xisp.*
import ystyle::xisp.types.*
import std.collection.ArrayList
import std.convert.*
import std.env.*
import std.fs.*
import std.io.*

/**
 * 命令行配置
 * 栈深度优先级：命令行参数 > 环境变量 > 默认值
 */
struct CliConfig {
    var stackDepth: Option<Int64> = None
    var simpleRepl: Bool = false
    var scriptPath: Option<String> = None
    var code: Option<String> = None
    var showHelp: Bool = false
}

main(args: Array<String>): Int64 {
    let config = parseArgs(args)

    if (config.showHelp) {
        printHelp()
        return 0
    }
    if (let Some(code) <- config.code) {
        runCode(code, config)
    } else if (let Some(path) <- config.scriptPath) {
        runScript(path, config)
    } else {
        runRepl(config.simpleRepl, config)
    }
    0
}

/**
 * 解析命令行参数
 * 支持：
 *   -h|--help                帮助
 *   -c "<code>"              执行代码
 *   --stack-depth <N>        栈深度限制（命令行优先于环境变量）
 *   --simple-repl            简单 REPL
 *   <script.lisp>            执行脚本
 */
func parseArgs(args: Array<String>): CliConfig {
    var config = CliConfig()
    var i: Int64 = 0
    while (i < args.size) {
        let arg = args[i]
        if (arg == "-h" || arg == "--help") {
            config.showHelp = true
        } else if (arg == "-c") {
            if (i + 1 < args.size) {
                config.code = Some(args[i + 1])
                i = i + 1
            }
        } else if (arg == "--stack-depth") {
            if (i + 1 < args.size) {
                let depth = parseStackDepth(args[i + 1])
                if (let Some(d) <- depth) {
                    config.stackDepth = Some(d)
                } else {
                    eprintln("警告: 无效的栈深度值 '${args[i + 1]}'，使用默认值")
                }
                i = i + 1
            }
        } else if (arg == "--simple-repl") {
            config.simpleRepl = true
        } else if (arg.startsWith("-") && arg.size > 1) {
            eprintln("未知参数: ${arg}")
        } else {
            config.scriptPath = Some(arg)
        }
        i = i + 1
    }
    config
}

/**
 * 解析栈深度值
 */
func parseStackDepth(value: String): Option<Int64> {
    try {
        let depth = Int64.parse(value)
        if (depth > 0) {
            Some(depth)
        } else {
            None
        }
    } catch (_: Exception) {
        None
    }
}

/**
 * 计算最终栈深度配置
 * 优先级：命令行参数 > 环境变量 XISP_MAX_STACK_DEPTH > 默认
 */
func resolveStackDepth(config: CliConfig): Option<Int64> {
    if (let Some(d) <- config.stackDepth) {
        return Some(d)
    }
    match (getVariable("XISP_MAX_STACK_DEPTH")) {
        case Some(value) => parseStackDepth(value)
        case None => None
    }
}

func createInterpreter(options: Array<InterpreterOption>): LispInterpreter {
    LispInterpreter(options)
}

func buildOptions(config: CliConfig): Array<InterpreterOption> {
    let opts = ArrayList<InterpreterOption>()
    opts.add(withStdLib())
    opts.add(withQuietMode())
    opts.add(withScriptMode())
    if (let Some(depth) <- resolveStackDepth(config)) {
        opts.add(withMaxStackDepth(depth))
    }
    var result = Array<InterpreterOption>(opts.size) { _ => { _ => () } }
    for (i in 0..opts.size) {
        result[i] = opts[i]
    }
    result
}

func printHelp(): Unit {
    println("╔═══════════════════════════════════════════════════════════╗")
    println("║           星枢 - 仓颉嵌入式 Lisp 脚本语言                 ║")
    println("║                  版本 ${XISP_VERSION}                           ║")
    println("╚═══════════════════════════════════════════════════════════╝")
    println("")
    println("用法:")
    println("  ystyle::xisp.cli                    启动 REPL 交互式环境")
    println("  ystyle::xisp.cli --simple-repl      启动简单 REPL（非 TTY 回退）")
    println("  ystyle::xisp.cli <script.lisp>       执行 Lisp 脚本文件")
    println("  ystyle::xisp.cli -c \"<code>\"        直接执行 Lisp 代码")
    println("  ystyle::xisp.cli -h|--help          显示此帮助信息")
    println("")
    println("栈深度配置:")
    println("  --stack-depth <N>                   设置最大调用栈深度（命令行优先）")
    println("  XISP_MAX_STACK_DEPTH=<N>             环境变量方式设置栈深度（默认 1000）")
    println("")
    println("环境变量:")
    println("  XISP_PATH=<paths>                   模块搜索路径（用 : 分隔）")
    println("")
    println("示例:")
    println("  ystyle::xisp.cli")
    println("  ystyle::xisp.cli script.lisp")
    println("  ystyle::xisp.cli --stack-depth 5000 script.lisp")
    println("  XISP_PATH=./examples ystyle::xisp.cli -c \"(import pkg1)\"")
    println("")
}

func runRepl(useSimple: Bool, config: CliConfig): Unit {
    let opts = ArrayList<InterpreterOption>()
    opts.add(withStdLib())
    opts.add(withScriptMode())
    if (let Some(depth) <- resolveStackDepth(config)) {
        opts.add(withMaxStackDepth(depth))
    }
    var options = Array<InterpreterOption>(opts.size) { _ => { _ => () } }
    for (i in 0..opts.size) {
        options[i] = opts[i]
    }
    let interpreter = createInterpreter(options)
    if (useSimple) {
        interpreter.runSimpleREPL()
    } else {
        interpreter.runREPL()
    }
}

func runCode(code: String, config: CliConfig): Unit {
    let interpreter = createInterpreter(buildOptions(config))
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

func runScript(filePath: String, config: CliConfig): Unit {
    let interpreter = createInterpreter(buildOptions(config))
    interpreter.evalFile(filePath)
}
