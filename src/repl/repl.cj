package ystyle::xisp.repl

import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.terminal.*
import std.env.*
import std.io.*
import std.collection.ArrayList
import std.fs.*

public class Repl {
    private var env: Environment
    private var evaluator: Evaluator
    private var running: Bool = true
    private var showBanner: Bool = true

    public init(env: Environment) {
        this.env = env
        this.evaluator = Evaluator(this.env)
    }

    public func setShowBanner(show: Bool): Unit {
        this.showBanner = show
    }

    public func run(): Unit {
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

    private func readInput(): String {
        let stdin = getStdIn()
        let stdout = getStdOut()
        var input = ""
        var parenCount = 0
        var inString = false
        var firstLine = true
        while (true) {
            if (firstLine) {
                print("xisp> ")
                stdout.flush()
                firstLine = false
            } else {
                print("    ... ")
                stdout.flush()
            }
            match (stdin.readln()) {
                case None =>
                    println("")
                    this.running = false
                    return ""
                case Some(lineStr) =>
                    input = input + lineStr + "\n"
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
                    if (parenCount <= 0 && lineStr.size > 0) {
                        break
                    }
            }
        }
        input
    }

    private func processInput(input: String) {
        let trimmed = this.trimString(input)
        let chars = trimmed.toRuneArray()
        if (trimmed.size > 0 && chars[0] == r',') {
            this.handleCommand(trimmed)
            return
        }
        match (this.evalInput(input)) {
            case Some(result) =>
                println("")
                println(result.toString())
            case None => ()
        }
    }

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
        if (start >= end) { return "" }
        let result = ArrayList<Rune>()
        for (i in start..end) { result.add(chars[i]) }
        String(result)
    }

    private func evalInput(input: String): ?LispValue {
        try {
            let exprs = Parser.parseFromString(input)
            if (exprs.size == 0) { return None }
            var result: ?LispValue = None
            for (i in 0..exprs.size) { result = this.evaluator.eval(exprs[i]) }
            result
        } catch (e: Error) {
            println("错误: ${e.message}")
            None
        }
    }

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
            let parts = cmd.split(" ")
            if (parts.size >= 2) {
                println("不支持的语言: ${parts[1]}")
            } else {
                println("请指定语言")
            }
            println("支持的语言: zh/chinese/中文, en/english/英文")
        } else if (cmd == ",history") {
            println("")
            println("历史记录（简单模式下不可用）")
            println("")
        } else {
            println("未知命令: ${cmd}")
            println("输入 ,help 查看可用命令")
        }
    }

    private func enableChineseKeywords() {
        println("")
        println("启用中文关键字支持...")
        println("现在可以使用:")
        println("  定义 (define)  过程 (lambda)  如果 (if)  让 (let)")
        println("  打印 (println)  显示 (print)")
        println("")
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

    private func disableChineseKeywords() {
        println("")
        println("切换回英文关键字模式")
        println("")
        let oldEnv = this.env
        this.env = Environment()
        let keys = oldEnv.getKeys()
        for (i in 0..keys.size) {
            let key = keys[i]
            let value = oldEnv.lookup(key)
            this.env.define(key, value)
        }
        this.evaluator = Evaluator(this.env)
    }

    private func printHelp() {
        println("")
        println("可用命令:")
        println("  ,exit     ,quit    - 退出 REPL")
        println("  ,help              - 显示此帮助信息")
        println("  ,env               - 查看当前环境中的所有变量")
        println("  ,history           - 查看历史记录")
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

public class EnhancedRepl {
    private var env: Environment
    private var evaluator: Evaluator
    private var running: Bool = true
    private var showBanner: Bool = true
    private var terminal: Terminal
    private var editor: LineEditor
    private var history: HistoryManager
    private var completer: Completer
    private var highlighter: Highlighter = Highlighter()
    private var historyPath: String = ""
    private var lastCursorRow: Int64 = 0

    private func rl(s: String): Unit { print(s + "\r\n") }

    public init(env: Environment) {
        this.env = env
        this.evaluator = Evaluator(this.env)
        this.terminal = createTerminal()
        this.completer = Completer(this.evaluator)
        this.history = HistoryManager(maxEntries: 1000)
        this.editor = LineEditor(this.history, this.completer)
        let home = getVariable("HOME")
        this.historyPath = match (home) {
            case Some(h) => "${h}/.xisp_history"
            case None => "/tmp/.xisp_history"
        }
    }

    public func setShowBanner(show: Bool): Unit {
        this.showBanner = show
    }

    public func run(): Unit {
        if (!terminal.isTTY() || !terminal.enableRawMode()) {
            let simple = Repl(this.env)
            if (!this.showBanner) { simple.setShowBanner(false) }
            simple.run()
            return
        }
        this.history.load(this.historyPath)
        clearScreen()
        moveTo(0, 0)
        if (this.showBanner) {
            this.printWelcome()
        }
        try {
            this.editor.clear()
        this.lastCursorRow = 0
            while (this.running) {
                this.renderEditor()
                match (this.terminal.readKey()) {
                    case Some(event) =>
                        let action = this.editor.processKey(event)
                        match (action) {
                            case EditorAction.Submit(text) =>
                                this.handleSubmit(text)
                            case EditorAction.Exit =>
                                this.running = false
                            case EditorAction.Cancel =>
                                this.editor.clear()
        this.lastCursorRow = 0
                            case EditorAction.Continue =>
                                let candidates = this.editor.getCompletionCandidates()
                                if (candidates.size > 0) {
                                    let idx = this.completer.getCycleIndex()
                                    let nextIdx = (idx + 1) % candidates.size
                                    this.completer.setCycleIndex(nextIdx)
                                    this.editor.clearCompletionCandidates()
                                }
                            case EditorAction.Complete(candidates) =>
                                if (candidates.size > 0) {
                                    this.showCompletionCandidates(candidates)
                                }
                        }
                    case None => ()
                }
            }
        } finally {
            this.terminal.disableRawMode()
            this.history.save(this.historyPath)
        }
        print("\r\n再见！\r\n")
    }

    private func renderEditor(): Unit {
        let lines = this.editor.getDisplayLines()
        let cursorRow = this.editor.getCursorRow()
        let cursorCol = this.editor.getCursorCol()

        if (cursorRow > this.lastCursorRow) {
            print("\r\n")
        } else if (cursorRow < this.lastCursorRow) {
            print("\u{1b}[A")
        }
        this.lastCursorRow = cursorRow
        if (cursorRow == 0) {
            print("\rxisp> " + this.highlighter.highlightInput(lines[0]))
            print("\u{1b}[K")
            let pos = 6 + cursorCol
            print("\r" + "\u{1b}[" + pos.toString() + "C")
        } else {
            print("\r    ... " + this.highlighter.highlightInput(lines[cursorRow]))
            print("\u{1b}[K")
            let pos = 8 + cursorCol
            print("\r" + "\u{1b}[" + pos.toString() + "C")
        }
        getStdOut().flush()
    }

    private func showCompletionCandidates(candidates: ArrayList<String>): Unit {
        print("\r\n")
        var line = ""
        for (i in 0..candidates.size) {
            if (line.size + candidates[i].size + 1 > 60) {
                rl(line)
                line = ""
            }
            if (line.size > 0) { line = line + " " }
            line = line + candidates[i]
        }
        if (line.size > 0) { rl(line) }
    }

    private func handleSubmit(text: String): Unit {
        let trimmed = text.trimAscii()
        if (trimmed.size == 0) { return }
        let chars = trimmed.toRuneArray()
        if (chars.size > 0 && chars[0] == r',') {
            this.runCommand(trimmed)
            return
        }
        print("\r\n")
        match (this.evalInput(text)) {
            case Some(result) =>
                match (result) {
                    case Error(err) =>
                        print(this.highlighter.highlightError(err) + "\r\n")
                    case _ =>
                        print(this.highlighter.highlight(result) + "\r\n")
                }
            case None => ()
        }
        this.editor.clear()
        this.lastCursorRow = 0
    }

    private func runCommand(cmd: String): Unit {
        print("\r\n")
        if (cmd == ",exit" || cmd == ",quit") {
            this.running = false
        } else if (cmd == ",help") {
            this.printHelp()
        } else if (cmd == ",env") {
            this.printEnvironment()
        } else if (cmd == ",history") {
            this.printHistory()
        } else if (cmd == ",lang zh" || cmd == ",lang chinese" || cmd == ",lang 中文") {
            this.enableChineseKeywords()
        } else if (cmd == ",lang en" || cmd == ",lang english" || cmd == ",lang 英文") {
            this.disableChineseKeywords()
        } else if (cmd.startsWith(",lang ")) {
            let parts = cmd.split(" ")
            if (parts.size >= 2) {
                rl("不支持的语言: ${parts[1]}")
            } else {
                rl("请指定语言")
            }
            rl("支持的语言: zh/chinese/中文, en/english/英文")
        } else if (cmd.startsWith(",load ")) {
            let parts = cmd.split(" ")
            if (parts.size >= 2) {
                let filePath = parts[1]
                try {
                    let bytes = File.readFrom(filePath)
                    let content = String.fromUtf8(bytes)
                    rl("加载文件: ${filePath}")
                    let exprs = Parser.parseFromString(content)
                    for (i in 0..exprs.size) {
                        let result = this.evaluator.eval(exprs[i])
                        match (result) {
                            case Error(err) => rl(this.highlighter.highlightError(err))
                            case _ => ()
                        }
                    }
                } catch (_: Exception) {
                    rl("无法加载文件: ${filePath}")
                }
            } else {
                rl("用法: ,load <文件路径>")
            }
        } else {
            rl("未知命令: ${cmd}")
            rl("输入 ,help 查看可用命令")
        }
        this.editor.clear()
        this.lastCursorRow = 0
    }

    private func evalInput(input: String): ?LispValue {
        try {
            let exprs = Parser.parseFromString(input)
            if (exprs.size == 0) { return None }
            var result: ?LispValue = None
            for (i in 0..exprs.size) { result = this.evaluator.eval(exprs[i]) }
            result
        } catch (e: Error) {
            rl("错误: ${e.message}")
            None
        }
    }

    private func printWelcome(): Unit {
        print("\r\n")
        print("╔═══════════════════════════════════════════════════════════╗\r\n")
        print("║           星枢 (Xisp) - 仓颉嵌入式 Lisp 脚本语言          ║\r\n")
        print("║                   版本 0.1.0 - MVP                        ║\r\n")
        print("╚═══════════════════════════════════════════════════════════╝\r\n")
        print("\r\n")
        print("输入 Lisp 表达式求值，输入 ,help 查看帮助，,exit 退出\r\n")
        print("\r\n")
        getStdOut().flush()
    }

    private func printHelp(): Unit {
        rl("")
        rl("可用命令:")
        rl("  ,exit     ,quit    - 退出 REPL")
        rl("  ,help              - 显示此帮助信息")
        rl("  ,env               - 查看当前环境中的所有变量")
        rl("  ,history           - 查看历史记录")
        rl("  ,load <文件>        - 加载并执行 Lisp 文件")
        rl("  ,lang zh           - 启用中文关键字")
        rl("  ,lang en           - 切换回英文关键字")
        rl("")
        rl("快捷键:")
        rl("  Enter                 - 括号平衡时提交，不平换行")
        rl("  Ctrl+Enter / Alt+Enter - 强制提交")
        rl("  Ctrl+C                 - 取消当前输入")
        rl("  Ctrl+D                 - 退出（空行时）")
        rl("  Tab                    - 补全")
        rl("  ↑（首行）/ Ctrl+P      - 上一条历史")
        rl("  ↓（末行）/ Ctrl+N      - 下一条历史")
        rl("  ← →                   - 光标移动")
        rl("  Ctrl+A / Home          - 行首")
        rl("  Ctrl+E / End           - 行尾")
        rl("  Ctrl+K                 - 删到行尾")
        rl("  Ctrl+U                 - 删整行")
        rl("  Backspace / Delete     - 删除字符")
        rl("  ↑ ↓ (多行)             - 行间上下")
        rl("")
        rl("内置函数:")
        rl("  算术: + - * / mod")
        rl("  比较: = < > <= >=")
        rl("  逻辑: and or not")
        rl("  列表: first rest cons list length map filter reduce")
        rl("  打印: print println princ newline display")
        rl("")
    }

    private func printEnvironment(): Unit {
        rl("")
        rl("当前环境变量:")
        let keys = this.env.getKeys()
        if (keys.size == 0) {
            rl("  (空)")
        } else {
            for (i in 0..keys.size) {
                let key = keys[i]
                let value = this.env.lookup(key)
                rl("  ${key} = ${value.toString()}")
            }
        }
        rl("")
    }

    private func printHistory(): Unit {
        rl("")
        rl("历史记录:")
        let all = this.history.getAll()
        if (all.size == 0) {
            rl("  (空)")
        } else {
            for (i in 0..all.size) {
                rl("  ${i + 1}: ${all[i]}")
            }
        }
        rl("")
    }

    private func enableChineseKeywords(): Unit {
        rl("")
        rl("启用中文关键字支持...")
        rl("现在可以使用:")
        rl("  定义 (define)  过程 (lambda)  如果 (if)  让 (let)")
        rl("  打印 (println)  显示 (print)")
        rl("")
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

    private func disableChineseKeywords(): Unit {
        rl("")
        rl("切换回英文关键字模式")
        rl("")
        let oldEnv = this.env
        this.env = Environment()
        let keys = oldEnv.getKeys()
        for (i in 0..keys.size) {
            let key = keys[i]
            let value = oldEnv.lookup(key)
            this.env.define(key, value)
        }
        this.evaluator = Evaluator(this.env)
        this.completer = Completer(this.evaluator)
    }
}
