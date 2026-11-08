package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp_bc.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*
import ystyle::xisp.bridge.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import std.fs.*

/**
 * 三后端**差分测试**（AST / 字节码 / JIT 兼容性回归网）
 *
 * 同一段代码分别在三个后端下执行，**逐表达式**比对结果。任何不一致都要显式列出，
 * 而不是靠"测试全绿"来暗示兼容。
 *
 * 两层覆盖：
 *
 * | 层 | 粒度 | 能定位到什么 |
 * |---|---|---|
 * | `testCorpusDifferential` | 逐表达式（把源码切成一棵棵顶层表达式） | "第 65 步不一致" |
 * | `testFileDifferential` | 逐文件（`evalFile` + 输出捕获） | 相对导入、模块搜索路径、副作用序列 |
 *
 * 文件层不能省：`(import "./utils.lisp")` 依赖 `currentFilePath`，逐表达式求值拿不到
 * 文件位置，相对导入必然失败——那会把"harness 缺口"误报成"后端差异"。
 *
 * 用法：`cjpm test --show-all-output --filter '*BackendDiffTest*'`
 */

/** 参与比对的后端名 */
func diffBackendNames(): Array<String> {
    ["AST", "BC", "JIT"]
}

/**
 * 提取错误结果的**种类**（如 `[TypeError]`）；非错误返回空串。
 *
 * 用于差分测试：两侧同为某一类错误即视为一致（文案差异不算语义差异）。
 */
func errorKind(s: String): String {
    // 错误结果可能带调用栈（多行），故在整串里找错误种类标记
    match (s.indexOf("[")) {
        case Some(b) =>
            match (s.indexOf("]")) {
                case Some(e) => if (e > b) { s[b..e + 1] } else { "" }
                case None => ""
            }
        case None => ""
    }
}

/**
 * 去掉调用栈只留结论。
 *
 * AST 侧的错误带 `调用栈:` 与若干 `  N. frame` 行，字节码侧目前不带 —— 这是
 * **诊断信息的完整度差异**，不是求值语义差异。逐字节比对会把每条错误都判成不同，
 * 于是真正的问题被淹没在噪声里（实测：91 个语料里 20 个报"差异"，其中 16 个
 * 只是少了调用栈）。故先归一化。
 */
func stripStackTrace(s: String): String {
    let out = ArrayList<String>()
    for (line in s.split("\n")) {
        let t = line.trimAscii()
        if (t == "调用栈:") {
            continue
        }
        // 调用栈条目形如 "1. foo"：1~2 位数字 + '.' + 空格
        let frame = isStackFrameLine(t)
        if (frame) {
            continue
        }
        out.add(line)
    }
    var joined = ""
    for (i in 0..out.size) {
        if (i > 0) {
            joined += "\n"
        }
        joined += out[i]
    }
    joined
}

/** 调用栈条目形如 `1. foo` / `12. bar` */
func isStackFrameLine(t: String): Bool {
    let runes = t.toRuneArray()
    var i = 0
    while (i < runes.size && runes[i] >= r'0' && runes[i] <= r'9') {
        i++
    }
    if (i == 0 || i > 2) {
        return false
    }
    i < runes.size && runes[i] == r'.' && i + 1 < runes.size && runes[i + 1] == r' '
}

@Test
class BackendDiffTest {
    /**
     * 逐表达式跑，记录每一步的结果（这样差异能定位到具体表达式）。
     *
     * 环境装配刻意用 `LispInterpreter` 的默认装配 + `withStdLib` 等选项，
     * 与 CLI 保持一致 —— 手写 `Environment` + 逐个 register 容易漏掉
     * 别名、桥接等装配步骤，从而把"harness 缺口"误报成"后端差异"。
     */
    func makeInterpreter(backend: String): LispInterpreter {
        let opts = ArrayList<InterpreterOption>()
        opts.add(withQuietMode())
        opts.add(withStdLib())
        opts.add(withChineseKeywords())
        opts.add(withJapaneseKeywords())
        // 模块搜索路径：`(import pkg1)` 这类绝对导入需要它发现 lisp-tests/pkg1
        opts.add(withModulePath("./lisp-tests"))
        if (backend != "AST") {
            opts.add(withBytecodeCompiler())
        }
        if (backend == "JIT") {
            opts.add(withJit())
        }
        LispInterpreter(opts.toArray())
    }

    func runSteps(code: String, backend: String): Array<String> {
        let out = ArrayList<String>()
        let it = this.makeInterpreter(backend)
        let exprs = try {
            Parser.parseFromString(code)
        } catch (e: Exception) {
            let only = ArrayList<String>()
            only.add("PARSE-ERROR: " + e.toString())
            return only.toArray()
        }
        for (expr in exprs) {
            var r = ""
            try {
                r = it.evalExpr(expr).toString()
            } catch (e: Exception) {
                r = "EXCEPTION: " + e.toString()
            }
            out.add(r)
        }
        out.toArray()
    }

    /**
     * 整文件跑：走 `evalFile`，从而带上 `currentFilePath`（相对导入依赖它），
     * 并把每个输出行收集起来（副作用序列也是语义的一部分）。
     */
    func runFile(path: String, backend: String): Array<String> {
        let out = ArrayList<String>()
        let it = this.makeInterpreter(backend)
        // 脚本模式（与 CLI 执行 .lisp 时一致）：允许 `(import "./xxx.lisp")`。
        // 默认的模块模式禁止相对导入 —— 不切过来，每个相对导入语料都会两侧
        // 都报"模块上下文中不支持相对路径导入"，看起来"一致"，实际上什么都没验。
        it.setScriptMode()
        // 捕获 Lisp 侧的 println/print 输出。
        //
        // 不能用 `setOutputFn`：那只接管**解释器自身**的日志（verbose / 沙箱提示），
        // Lisp 里的 `println` 是注册在环境里的 `NativeFunc`，直接写进程 stdout
        // （`BuiltinFunctions.registerPrint` → `registerOutput(env, { s => print(s) })`）。
        // 要抓它只能覆盖环境里那几个绑定。
        let env = it.getEnvironment()
        env.define("println", NativeFunc({ args =>
            var line = ""
            for (i in 0..args.size) {
                if (i > 0) {
                    line += " "
                }
                line += args[i].toString()
            }
            out.add(line)
            out.add("\n")
            Nil
        }))
        env.define("print", NativeFunc({ args =>
            var line = ""
            for (i in 0..args.size) {
                if (i > 0) {
                    line += " "
                }
                line += args[i].toString()
            }
            out.add(line)
            Nil
        }))
        var finalResult = ""
        try {
            finalResult = it.evalFile(path).toString()
        } catch (e: Exception) {
            finalResult = "EXCEPTION: " + e.toString()
        }
        out.add("=== 最终结果: " + stripStackTrace(finalResult))
        out.toArray()
    }

    /** 比对两次运行结果（已归一化），返回差异描述 */
    func compare(label: String, ast: Array<String>, other: Array<String>, otherName: String): String {
        if (ast.size != other.size) {
            return "${label} [${otherName}]: 输出行数不同 AST=${ast.size} ${otherName}=${other.size}"
        }
        var msg = ""
        for (i in 0..ast.size) {
            let a = stripStackTrace(ast[i])
            let b = stripStackTrace(other[i])
            if (a == b) {
                continue
            }
            // 两侧都是错误时只比较**错误种类**：后端与 AST 的错误文案由不同代码
            // 路径产出，措辞差异不等于语义差异（实测 `+: expected a number (got: true)`
            // vs `+: expected a number`）。
            let ak = errorKind(a)
            let bk = errorKind(b)
            if (ak.size > 0 && ak == bk) {
                continue
            }
            if (msg.size > 0) {
                msg += "\n"
            }
            msg += "${label} [${otherName}] 第${i+1}处: AST=${a} | ${otherName}=${b}"
        }
        msg
    }

    /** 逐表达式的三后端比对 */
    func diffSteps(code: String, label: String): String {
        let ast = this.runSteps(code, "AST")
        var msg = ""
        for (name in ["BC", "JIT"]) {
            let d = this.compare(label, ast, this.runSteps(code, name), name)
            if (d.size > 0) {
                msg = this.appendDiff(msg, d)
            }
        }
        msg
    }

    /** 逐文件的三后端比对 */
    func diffFile(path: String, label: String): String {
        let ast = this.runFile(path, "AST")
        var msg = ""
        for (name in ["BC", "JIT"]) {
            let d = this.compare(label, ast, this.runFile(path, name), name)
            if (d.size > 0) {
                msg = this.appendDiff(msg, d)
            }
        }
        msg
    }

    /** 追加一段差异描述（换行分隔） */
    func appendDiff(msg: String, d: String): String {
        if (msg.size == 0) {
            return d
        }
        msg + "\n" + d
    }

    /** 扫一个目录下的全部 Lisp 文件（递归） */
    func collectLispFiles(dir: String): Array<String> {
        let result = ArrayList<String>()
        let entries = try {
            Directory.readFrom(dir)
        } catch (e: Exception) {
            return result.toArray()
        }
        for (entry in entries) {
            let p = dir + "/" + entry.name
            if (entry.isDirectory()) {
                for (sub in this.collectLispFiles(p)) {
                    result.add(sub)
                }
            } else if (entry.name.endsWith(".lisp")) {
                result.add(p)
            }
        }
        result.toArray()
    }

    /**
     * 该文件是否**本来就不是**可独立执行的脚本。
     *
     * 与"因为 harness 不够而跳过"须严格区分：这里每一条都是语料自身的性质，
     * 且都在对应文档里写明。
     */
    func isNotRunnableScript(f: String): Bool {
        // `lisp-tests/perf/**`：**输出是确定的**（实测 15 个语料两两重跑哈希一致，
        // 耗时靠外部 `time`/`perf/run.sh` 度量，脚本内不打印时间）。跳过的真实理由
        // 是**太慢**：`01-fib-direct` 单次 AST 就要 15~20 秒，15 个语料 × 3 后端
        // 会把这份回归网拖成几分钟级。它们由 `lisp-tests/perf/run.sh` 单独度量。
        //
        // 注意别把这条写成"输出含耗时、不稳定"—— 那与事实不符，会让后来人
        // 误以为这些语料的**语义**不参与验证是合理的。
        if (f.contains("/perf/")) {
            return true
        }
        // REPL 交互演示：需要 TTY 与 `,lang zh` 等 REPL 命令，不是独立脚本
        if (f.contains("legacy/chinese_demo") || f.contains("/repl/")) {
            return true
        }
        // `module.lisp` 是模块元数据清单，由模块加载器特殊处理，不能当脚本执行
        if (f.endsWith("/module.lisp")) {
            return true
        }
        false
    }

    /** 收集全部可执行语料 */
    func allCorpus(): Array<String> {
        let files = ArrayList<String>()
        for (f in this.collectLispFiles("lisp-tests")) {
            files.add(f)
        }
        for (f in this.collectLispFiles("examples")) {
            files.add(f)
        }
        files.toArray()
    }

    /**
     * 核心差分：存量 Lisp 语料**逐文件**比对。
     *
     * 走 `evalFile` 而非逐表达式求值，因此相对导入、模块搜索路径、输出副作用
     * 全都在覆盖范围内 —— 这正是文件层不能用表达式层替代的原因。
     */
    @TestCase
    func testFileDifferential() {
        println("\n=== 三后端差分（存量语料 · 逐文件） ===")
        let files = this.allCorpus()
        println("扫到 " + files.size.toString() + " 个 .lisp 文件")

        var checked = 0
        var skipped = 0
        var diffs = ArrayList<String>()
        for (f in files) {
            if (this.isNotRunnableScript(f)) {
                skipped++
                continue
            }
            checked++
            let d = this.diffFile(f, f)
            if (d.size > 0) {
                diffs.add(d)
                println("差异: " + d)
            }
        }
        println("已比对 " + checked.toString() + " 个文件，跳过 " + skipped.toString() + " 个（非独立脚本）")
        if (diffs.size > 0) {
            var all = ""
            for (d in diffs) {
                all += d + "\n"
            }
            @Fail("三后端存在 " + diffs.size.toString() + " 个文件的差异:\n" + all)
        }
        println("✓ 全部 " + checked.toString() + " 个文件在三个后端下结果一致")
    }

    /**
     * 逐表达式差分：把文件切成一棵棵顶层表达式，**逐步**比对返回值。
     *
     * 比逐文件更细：能定位到"第 N 个顶层表达式不一致"。代价是拿不到文件上下文，
     * 所以跳过依赖相对导入的语料（那些由 `testFileDifferential` 覆盖）。
     */
    @TestCase
    func testCorpusDifferential() {
        println("\n=== 三后端差分（存量语料 · 逐表达式） ===")
        let files = this.allCorpus()

        var checked = 0
        var skipped = 0
        var diffs = ArrayList<String>()
        for (f in files) {
            if (this.isNotRunnableScript(f)) {
                skipped++
                continue
            }
            // 相对导入依赖 currentFilePath；逐表达式求值没有文件位置，
            // 两侧都会报"找不到"，这里的差异不反映后端语义
            if (f.contains("module-imports") || f.contains("05-modules") ||
                f.contains("testrel") || f.contains("reltest") || f.contains("rel-test") ||
                f.contains("ystyle/") || f.contains("pkg1/") || f.contains("pkg2/")) {
                skipped++
                continue
            }
            var content = ""
            try {
                content = String.fromUtf8(File.readFrom(f))
            } catch (e: Exception) {
                skipped++
                continue
            }
            checked++
            let d = this.diffSteps(content, f)
            if (d.size > 0) {
                diffs.add(d)
                println("差异: " + d)
            }
        }
        println("已比对 " + checked.toString() + " 个文件，跳过 " + skipped.toString() + " 个")
        if (diffs.size > 0) {
            var all = ""
            for (d in diffs) {
                all += d + "\n"
            }
            @Fail("三后端存在 " + diffs.size.toString() + " 处差异:\n" + all)
        }
        println("✓ 全部 " + checked.toString() + " 个文件在三个后端下逐步一致")
    }
}
