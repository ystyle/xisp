package ystyle::xisp.repl

import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import std.env.*
import std.io.*
import std.collection.ArrayList

/**
 * REPL - Read-Eval-Print Loop
 * 交互式 Lisp 解释器
 */
public class Repl {
    private var env: Environment
    private var evaluator: Evaluator
    private var running: Bool = true
    private var showBanner: Bool = true

    /**
     * 创建新的 REPL 实例
     * @param env Lisp 环境（应该从 LispInterpreter 获取）
     */
    public init(env: Environment) {
        this.env = env
        this.evaluator = Evaluator(this.env)
    }

    /**
     * 设置是否显示 Banner
     */
    public func setShowBanner(show: Bool) {
        this.showBanner = show
    }

    /**
     * 启动 REPL 主循环
     */
    public func run() {
        if (this.showBanner) {
            this.printWelcome()
        }

        while (this.running) {
            let input = this.readInput()

            if (input.size > 0) {
                this.processInput(input)
            }
        }

        println("\n再见！")
    }

    /**
     * 打印欢迎信息
     */
    private func printWelcome() {
        println("")
        println("╔═══════════════════════════════════════════════════════════╗")
        println("║           星枢 (Xisp) - 仓颉嵌入式 Lisp 脚本语言          ║")
        println("║                   版本 0.1.0 - MVP                        ║")
        println("╚═══════════════════════════════════════════════════════════╝")
        println("")
        println("输入 Lisp 表达式求值，输入 ,help 查看帮助，,exit 退出")
        println("")
    }

    /**
     * 读取用户输入
     * 支持多行输入（检测括号平衡）
     */
    private func readInput(): String {
        let stdin = getStdIn()
        let stdout = getStdOut()
        var input = ""
        var parenCount = 0
        var inString = false
        var firstLine = true

        while (true) {
            // 显示提示符
            if (firstLine) {
                print("xisp> ")
                stdout.flush()
                firstLine = false
            } else {
                print("    ... ")
                stdout.flush()
            }

            // 读取一行
            let line = stdin.readln()

            // EOF 检测
            match (line) {
                case None =>
                    println("")
                    this.running = false
                    return ""
                case Some(lineStr) =>
                    input = input + lineStr + "\n"

                    // 检查括号平衡（跳过字符串内的括号）
                    for (r in lineStr.toRuneArray()) {
                        if (r == r'"' && !inString) {
                            inString = true
                        } else if (r == r'"' && inString) {
                            inString = false
                        } else if (!inString) {
                            if (r == r'(' || r == r'[') {
                                parenCount = parenCount + 1
                            } else if (r == r')' || r == r']') {
                                parenCount = parenCount - 1
                            }
                        }
                    }

                    // 括号平衡或输入为空时结束
                    if (parenCount <= 0 && lineStr.size > 0) {
                        break
                    }
            }
        }

        input
    }

    /**
     * 处理用户输入
     */
    private func processInput(input: String) {
        // 去除首尾空白
        let trimmed = this.trimString(input)

        // 检查是否为特殊命令
        let chars = trimmed.toRuneArray()
        if (trimmed.size > 0 && chars[0] == r',') {
            this.handleCommand(trimmed)
            return
        }

        // 解析并求值
        match (this.evalInput(input)) {
            case Some(result) =>
                println("")  // 先换行，避免和 print 输出混在一起
                println(result.toString())
            case None => ()
        }
    }

    /**
     * 去除字符串首尾空白
     */
    private func trimString(s: String): String {
        var start = 0
        let chars = s.toRuneArray()
        var end = chars.size

        while (start < end && (chars[start] == r' ' || chars[start] == r'\t' || chars[start] == r'\n' || chars[start] == r'\r')) {
            start = start + 1
        }

        while (end > start) {
            let c = chars[end - 1]
            if (c != r' ' && c != r'\t' && c != r'\n' && c != r'\r') {
                break
            }
            end = end - 1
        }

        if (start >= end) {
            return ""
        }

        let result = ArrayList<Rune>()
        for (i in start..end) {
            result.add(chars[i])
        }
        String(result)
    }

    /**
     * 解析和求值输入
     */
    private func evalInput(input: String): ?LispValue {
        try {
            let exprs = Parser.parseFromString(input)

            if (exprs.size == 0) {
                return None
            }

            var result: ?LispValue = None

            for (i in 0..exprs.size) {
                result = this.evaluator.eval(exprs[i])
            }

            result
        } catch (e: Error) {
            println("错误: ${e.message}")
            None
        }
    }

    /**
     * 处理特殊命令
     */
    private func handleCommand(cmd: String) {
        if (cmd == ",exit" || cmd == ",quit") {
            this.running = false
        } else if (cmd == ",help") {
            this.printHelp()
        } else if (cmd == ",env") {
            this.printEnvironment()
        } else if (cmd == ",lang zh" || cmd == ",lang chinese" || cmd == ",lang 中文") {
            this.enableChineseKeywords()
        } else if (cmd == ",lang en" || cmd == ",lang english" || cmd == ",lang 英文") {
            this.disableChineseKeywords()
        } else if (cmd.startsWith(",lang ")) {
            // 提取语言名称
            let parts = cmd.split(" ")
            if (parts.size >= 2) {
                println("不支持的语言: ${parts[1]}")
            } else {
                println("请指定语言")
            }
            println("支持的语言: zh/chinese/中文, en/english/英文")
        } else {
            println("未知命令: ${cmd}")
            println("输入 ,help 查看可用命令")
        }
    }

    /**
     * 启用中文关键字
     */
    private func enableChineseKeywords() {
        println("")
        println("启用中文关键字支持...")
        println("现在可以使用:")
        println("  定义 (define)  过程 (lambda)  如果 (if)  让 (let)")
        println("  打印 (println)  显示 (print)")
        println("")
        // 注册中文关键字别名
        this.env.registerKeywordAlias("定义", "define")
        this.env.registerKeywordAlias("过程", "lambda")
        this.env.registerKeywordAlias("如果", "if")
        this.env.registerKeywordAlias("让", "let")
        this.env.registerKeywordAlias("打印", "println")
        this.env.registerKeywordAlias("显示", "print")
        this.env.registerKeywordAlias("当", "when")
        this.env.registerKeywordAlias("除非", "unless")
        this.env.registerKeywordAlias("循环", "loop")
        this.env.registerKeywordAlias("递归", "recur")
    }

    /**
     * 禁用中文关键字（通过创建新环境）
     */
    private func disableChineseKeywords() {
        println("")
        println("切换回英文关键字模式")
        println("")
        // 创建新环境（不包含关键字别名）
        let oldEnv = this.env
        this.env = Environment()
        // 复制所有变量绑定（但不复制关键字别名）
        let keys = oldEnv.getKeys()
        for (i in 0..keys.size) {
            let key = keys[i]
            let value = oldEnv.lookup(key)
            this.env.define(key, value)
        }
        this.evaluator = Evaluator(this.env)
    }

    /**
     * 打印帮助信息
     */
    private func printHelp() {
        println("")
        println("可用命令:")
        println("  ,exit     ,quit    - 退出 REPL")
        println("  ,help              - 显示此帮助信息")
        println("  ,env               - 查看当前环境中的所有变量")
        println("  ,lang zh           - 启用中文关键字")
        println("  ,lang en           - 切换回英文关键字")
        println("")
        println("内置函数:")
        println("  算术: + - * / mod")
        println("  比较: = < > <= >=")
        println("  逻辑: and or not")
        println("  列表: first rest cons list length map filter reduce")
        println("  打印: print println princ newline display")
        println("")
        println("语法示例:")
        println("  (+ 1 2 3)          => 6")
        println("  (define x 42)      => 定义变量 x = 42")
        println("  (lambda (x) (* x x)) => 匿名函数")
        println("  ((lambda (x) (* x x)) 5) => 25")
        println("  (first '(1 2 3))    => 1")
        println("  (map square '(1 2 3)) => (1 4 9)")
        println("  (println \"Hello\" \"World\") => 打印输出")
        println("")
        println("中文关键字示例（使用 ,lang zh 启用）:")
        println("  (定义 x 42)        => 定义变量 x = 42")
        println("  (过程 (x) (* x x))  => 匿名过程")
        println("  (如果 (> x 0) ...)  => 条件判断")
        println("")
    }

    /**
     * 打印当前环境
     */
    private func printEnvironment() {
        println("")
        println("当前环境变量:")

        let keys = this.env.getKeys()
        if (keys.size == 0) {
            println("  (空)")
        } else {
            for (i in 0..keys.size) {
                let key = keys[i]
                let value = this.env.lookup(key)
                println("  ${key} = ${value.toString()}")
            }
        }
        println("")
    }
}
