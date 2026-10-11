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
import std.collection.HashMap
import std.fs.*

/**
 * AST ↔ 字节码 **差分测试**（兼容性回归网）
 *
 * 同一段代码分别在两个后端下执行，**逐表达式**比对结果。任何不一致都要显式列出，
 * 而不是靠"测试全绿"来暗示兼容。
 *
 * 覆盖两个语料来源：
 * - 存量 Lisp 集成测试（lisp-tests 目录）
 * - 示例脚本（`examples/` 下的全部 .lisp）
 *
 * 用法：`cjpm test --show-all-output --filter 'BackendDiffTest.*'`
 */


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

@Test
class BackendDiffTest {
    /**
     * 逐表达式跑，记录每一步的结果（这样差异能定位到具体表达式）。
     *
     * 环境装配刻意用 `LispInterpreter` 的默认装配 + `withStdLib` 等选项，
     * 与 CLI 保持一致 —— 手写 `Environment` + 逐个 register 容易漏掉
     * 别名、桥接等装配步骤，从而把"harness 缺口"误报成"后端差异"。
     */
    func runSteps(code: String, useBc: Bool): Array<String> {
        let out = ArrayList<String>()
        let opts = ArrayList<InterpreterOption>()
        opts.add(withQuietMode())
        opts.add(withStdLib())
        opts.add(withChineseKeywords())
        opts.add(withJapaneseKeywords())
        if (useBc) {
            opts.add(withBytecodeCompiler())
        }
        let it = LispInterpreter(opts.toArray())
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

    /** 比对一份源码在两个后端下的逐步结果，返回差异描述（无差异返回空串） */
    func diff(code: String, label: String): String {
        // 两种后端用完全相同的装配，只有后端不同
        let ast = this.runSteps(code, false)
        let bc = this.runSteps(code, true)
        if (ast.size != bc.size) {
            return "${label}: 步数不同 AST=${ast.size} BC=${bc.size}"
        }
        var msg = ""
        for (i in 0..ast.size) {
            let a = ast[i]
            let b = bc[i]
            if (a == b) {
                continue
            }
            // 两侧都是错误时，只比较**错误种类**（`[TypeError]` / `[RuntimeError]` / …）：
            // 后端与 AST 的错误文案由不同代码路径产出，措辞差异不等于语义差异
            // （实测：`+: expected a number (got: true)` vs `expected a number`）。
            let ak = errorKind(a)
            let bk = errorKind(b)
            if (ak.size > 0 && ak == bk) {
                continue
            }
            if (msg.size > 0) {
                msg += "\n"
            }
            msg += "${label} 第${i+1}步: AST=${a} | BC=${b}"
        }
        msg
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
     * 核心差分：存量 Lisp 语料逐文件比对。
     *
     * 允许个别文件因"需要外部资源 / 依赖模块搜索路径 / 本身是性能基准"而跳过，
     * 但**任何语义差异都要报出来** —— 这是兼容性的证据。
     */
    @TestCase
    func testCorpusDifferential() {
        println("\n=== AST ↔ 字节码差分（存量语料） ===")
        let files = ArrayList<String>()
        for (f in this.collectLispFiles("lisp-tests")) {
            files.add(f)
        }
        for (f in this.collectLispFiles("examples")) {
            files.add(f)
        }
        println("扫到 " + files.size.toString() + " 个 .lisp 文件")

        var checked = 0
        var skipped = 0
        var diffs = ArrayList<String>()
        for (f in files) {
            var content = ""
            try {
                content = String.fromUtf8(File.readFrom(f))
            } catch (e: Exception) {
                skipped++
                continue
            }
            // 跳过明显的基准/交互脚本（打印耗时、读键盘等，输出不稳定）
            if (f.contains("perf/") || f.contains("benchmark") || f.contains("repl")) {
                skipped++
                continue
            }
            // 需要真实文件系统交互的语料：差异来自"环境未配置"而非两后端语义
            if (f.contains("file_io") || f.contains("debug_backslash")) {
                skipped++
                continue
            }
            // 依赖模块搜索路径（`(import pkg1)`）的语料：需要 withModulePath 装配，
            // 当前 harness 未配置，两侧都会报"模块找不到"（错误文本不同）。
            // 这类由 module_integration_test / evaluator_test 覆盖。
            if (f.contains("module-imports") || f.contains("error-tests") ||
                f.contains("05-modules") || f.contains("module_integration") ||
                f.contains("pkg1/module") || f.contains("pkg2/module") ||
                f.contains("/module.lisp") || f.contains("testrel") || f.contains("reltest") ||
                f.contains("rel-test") || f.contains("ystyle/")) {
                skipped++
                continue
            }
            let d = this.diff(content, f)
            checked++
            if (d.size > 0) {
                diffs.add(d)
            }
        }
        println("已比对 " + checked.toString() + " 个文件，跳过 " + skipped.toString() + " 个")
        if (diffs.size > 0) {
            var all = ""
            for (d in diffs) {
                all += d + "\n"
                println("差异: " + d)
            }
            @Fail("AST/字节码存在 " + diffs.size.toString() + " 处差异:\n" + all)
        }
        println("✓ 全部 " + checked.toString() + " 个文件在两后端下结果一致")
    }
}