package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp.core.builtin.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.HashSet

/**
 * 装配档位（`XispLevel` / `withLevel`）
 *
 * 宿主"选一档"而不是"逐个挑域"：`Expr` ⊂ `Pure` ⊂ `Script`。
 * 每档都**独占装配**（先清空环境），因此结果与解释器默认装了什么无关。
 */
@Test
class LevelTest {
    func evalIn(it: LispInterpreter, code: String): String {
        it.eval(code).toString()
    }

    func keys(it: LispInterpreter): HashSet<String> {
        let set = HashSet<String>()
        for (k in it.getEnvironment().getKeys()) {
            set.add(k)
        }
        set
    }

    func isUniformed(r: String): Bool {
        r.contains("UndefinedFunction") || r.contains("UnboundVariable")
    }

    /**
     * `Expr` = 纯计算：算术 / 字符串 / 数据结构 / 谓词都在，
     * **宏、`gensym`、输出**都不在。
     */
    @TestCase
    func testExprLevelIsPureCalculation() {
        println("\n=== Expr 档：纯计算，无元编程 ===")
        let it = LispInterpreter([withQuietMode(), withLevel(XispLevel.Expr)])
        // 计算与数据结构
        @Assert(this.evalIn(it, "(+ 1 2)") == "3")
        @Assert(this.evalIn(it, "(str 1 2)") == "\"12\"")
        @Assert(this.evalIn(it, "(number? 1)") == "true")
        @Assert(this.evalIn(it, "(hashmap? (hashmap))") == "true")
        @Assert(this.evalIn(it, "(first (quote (1 2)))") == "1")
        // ②类：既能调用也能作值（不属任何域，档位动不了它们）
        @Assert(this.evalIn(it, "(map (lambda (x) (* x 2)) '(1 2))") == "(2 4)")
        @Assert(this.evalIn(it, "(procedure? map)") == "true")
        // 元编程层不在
        @Assert(this.isUniformed(this.evalIn(it, "(when #t 1)")))
        @Assert(this.isUniformed(this.evalIn(it, "(unless #f 2)")))
        @Assert(this.isUniformed(this.evalIn(it, "(gensym)")))
        // 输出层不在
        @Assert(this.isUniformed(this.evalIn(it, "(println 1)")))
        println("✓ 算术/字符串/哈希表/谓词/别名可用；when/unless/gensym/println 不可用")
    }

    /** `Pure` = `Expr` + ④层宏包 + `gensym`，仍然没有输出 */
    @TestCase
    func testPureLevelAddsMacros() {
        println("\n=== Pure 档：表达式 + 宏包 ===")
        let it = LispInterpreter([withQuietMode(), withLevel(XispLevel.Pure)])
        @Assert(this.evalIn(it, "(when #t 1)") == "1")
        @Assert(this.evalIn(it, "(unless #f 2)") == "2")
        @Assert(this.evalIn(it, "(begin (define z 1) (incf z) z)") == "2")
        // gensym 回来了（它是宏的符号来源）
        @Assert(this.evalIn(it, "(symbol? (gensym))") == "true")
        // 输出仍然要宿主接管
        @Assert(this.isUniformed(this.evalIn(it, "(println 1)")))
        println("✓ when/unless/incf/gensym 可用；println 仍不可用")
    }

    /** `Script` = 全量域 + 标准库桥接 */
    @TestCase
    func testScriptLevelIsFull() {
        println("\n=== Script 档：全量 + 标准库 ===")
        let it = LispInterpreter([withQuietMode(), withLevel(XispLevel.Script)])
        @Assert(this.evalIn(it, "(println 1)") == "nil")
        @Assert(this.evalIn(it, "(when #t 1)") == "1")
        @Assert(this.evalIn(it, "(sum '(1 2 3))") == "6")
        // 标准库桥接：std.fs 的 cangjie::exists?
        @Assert(this.evalIn(it, "(cangjie::exists? \"/tmp\")") == "true")
        println("✓ println / 宏 / 聚合 / std.fs 桥接均可用")
    }

    /**
     * 三档严格包含：`Expr` 的每个符号都在 `Pure` 里，`Pure` 的每个符号都在 `Script` 里。
     *
     * 这是档位设计的核心不变量 —— 档位不是三份各写各的清单，而是同一集合的递增。
     */
    @TestCase
    func testLevelsAreNested() {
        println("\n=== 三档严格包含 ===")
        let exprIt = LispInterpreter([withQuietMode(), withLevel(XispLevel.Expr)])
        let pureIt = LispInterpreter([withQuietMode(), withLevel(XispLevel.Pure)])
        let scriptIt = LispInterpreter([withQuietMode(), withLevel(XispLevel.Script)])

        let exprKeys = this.keys(exprIt)
        let pureKeys = this.keys(pureIt)
        let scriptKeys = this.keys(scriptIt)

        for (k in exprKeys) {
            if (!pureKeys.contains(k)) {
                @Fail("Expr 有但 Pure 没有：${k}")
            }
        }
        for (k in pureKeys) {
            if (!scriptKeys.contains(k)) {
                @Fail("Pure 有但 Script 没有：${k}")
            }
        }
        if (!(exprKeys.size < pureKeys.size && pureKeys.size < scriptKeys.size)) {
            @Fail("符号数未严格递增：Expr=${exprKeys.size} Pure=${pureKeys.size} Script=${scriptKeys.size}")
        }
        println("✓ Expr(${exprKeys.size}) ⊂ Pure(${pureKeys.size}) ⊂ Script(${scriptKeys.size})")
    }

    /** 档位是**独占**装配：默认全量不会漏进来 */
    @TestCase
    func testLevelIsExclusive() {
        println("\n=== 档位独占装配 ===")
        let it = LispInterpreter([withQuietMode(), withLevel(XispLevel.Expr)])
        // 默认构造会装 println（hostPolicy 域）；Expr 档必须没有
        @Assert(this.isUniformed(this.evalIn(it, "(println 1)")))
        // 再叠一个追加式 withDomains 仍只增不减（独占标记生效）
        let it2 = LispInterpreter([
            withQuietMode(),
            withLevel(XispLevel.Expr),
            withDomains([BuiltinFunctions.domainMacrosCore()]),
        ])
        @Assert(this.evalIn(it2, "(when #t 1)") == "1")
        @Assert(this.isUniformed(this.evalIn(it2, "(println 1)")))
        println("✓ Expr 档下 println 不可用；后续追加域只增不减")
    }

    /** 档位（装什么）与策略（允许做什么）正交：可叠加沙箱 */
    @TestCase
    func testLevelComposesWithSandbox() {
        println("\n=== 档位 + 沙箱可叠加 ===")
        let it = LispInterpreter([withLevel(XispLevel.Script), withQuietMode(), withSandbox()])
        @Assert(this.evalIn(it, "(when #t 1)") == "1")
        @Assert(it.eval("(eval '(+ 1 2))").toString().contains("UndefinedFunction"))
        println("✓ Script 档 + withSandbox：宏可用、eval 被拦")
    }

    /**
     * 回归：`replace: true` 之后 ②类符号必须仍是一等值
     *
     * `clearEnvironmentBindings()` 换掉了整个环境，若不一并重建 ②类绑定，
     * `(procedure? map)` 会变成未绑定 —— 调用仍走快速路径，看起来"还能用"，
     * 却已不能再作为值传递，与默认装配不一致。
     */
    @TestCase
    func testReplaceKeepsFirstClassValues() {
        println("\n=== replace: true 后 ②类仍是一等值 ===")
        let it = LispInterpreter([withQuietMode(), withDomains(BuiltinFunctions.presetFull(), replace: true)])
        @Assert(this.evalIn(it, "(procedure? map)") == "true")
        @Assert(this.evalIn(it, "(procedure? for-each)") == "true")
        @Assert(this.evalIn(it, "(procedure? apply)") == "true")
        // 关闭开关仍要生效（档位/清空不能把开关覆盖掉）
        let off = LispInterpreter([withQuietMode(), withNoFirstClassValues(), withLevel(XispLevel.Pure)])
        @Assert(this.isUniformed(this.evalIn(off, "(procedure? map)")))
        @Assert(this.evalIn(off, "(map (lambda (x) x) '(1))") == "(1)")
        println("✓ replace 后 map/for-each/apply 均为 procedure；withNoFirstClassValues 仍生效")
    }
}
