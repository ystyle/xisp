package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList

/**
 * 能力域装配（能力域设计 §4，接缝 1/2）
 *
 * `registerAll` / `registerPure` 从"两个硬编码函数"改为**预置组合**：
 * 组装的语义是域集合的并集，`PRESET_PURE` 由域属性**推导**而非手写清单
 * （消除两份平行清单的隐式依赖）。
 */
@Test
class CapabilityDomainTest {
    func runWithDomains(domains: Array<LispCapability>, code: String): String {
        let env = Environment()
        BuiltinFunctions.installDomains(env, domains)
        let ev = Evaluator(env)
        var r: LispValue = Nil
        for (e in Parser.parseFromString(code)) {
            r = ev.eval(e)
        }
        r.toString()
    }

    /** `PRESET_PURE` 必须由属性推导：不含 `escapable` 与 `hostPolicy` 域 */
    @TestCase
    func testPresetPureIsDerived() {
        println("\n=== PRESET_PURE 由属性推导 ===")
        let pure = BuiltinFunctions.presetPure()
        for (d in pure) {
            @Assert(!d.escapable)
            @Assert(!d.hostPolicy)
        }
        // 输出域是 hostPolicy，必须被排除（导出通道由宿主接管）
        var hasPrint = false
        for (d in pure) {
            if (d.name == "print") {
                hasPrint = true
            }
        }
        @Assert(!hasPrint)
        println("✓ PURE 含 " + pure.size.toString() + " 个域，已排除 print（hostPolicy）")

        // 宏包是 escapable（保守），也必须被排除
        var hasMacros = false
        for (d in pure) {
            if (d.name == "macros.core") {
                hasMacros = true
            }
        }
        @Assert(!hasMacros)
        println("✓ 已排除 macros.core（escapable，保守判定）")
    }

    /** 域可任意组合：只装 base+arith+list，则宏与输出域不可用 */
    @TestCase
    func testComposableSubset() {
        println("\n=== 域可任意组合（最小集合） ===")
        let minimal = [
            BuiltinFunctions.domainBase(),
            BuiltinFunctions.domainArithmetic(),
            BuiltinFunctions.domainList(),
        ]
        @Assert(this.runWithDomains(minimal, "(+ 1 2)") == "3")
        println("✓ (+ 1 2) = 3")
        @Assert(this.runWithDomains(minimal, "(car (quote (1 2)))") == "1")
        println("✓ (car '(1 2)) = 1")
        // 未装的域：其符号不可用
        @Assert(this.runWithDomains(minimal, "(when #t 1)").contains("UndefinedFunction"))
        println("✓ 未装 macros.core → when 未定义")
        @Assert(this.runWithDomains(minimal, "(println 1)").contains("UndefinedFunction"))
        println("✓ 未装 print → println 未定义")
    }

    /** 全量组合包含全部域能力 */
    @TestCase
    func testPresetFull() {
        println("\n=== PRESET_FULL ===")
        let full = BuiltinFunctions.presetFull()
        @Assert(this.runWithDomains(full, "(+ 1 2)") == "3")
        @Assert(this.runWithDomains(full, "(when #t 1)") == "1")
        @Assert(this.runWithDomains(full, "(map (lambda (x) x) (quote (1 2)))") == "(1 2)")
        @Assert(this.runWithDomains(full, "(gensym)").startsWith("#:g"))
        println("✓ 全量下 算术/宏/高阶/gensym 均可用")
    }

    /** 用户自定义域可与 core 域混用 —— `LispCapability` 是两层共用的抽象 */
    @TestCase
    func testUserDomain() {
        println("\n=== 宿主自定义域 ===")
        let myDomain = SimpleCapability(
            "my.app",
            ["my-answer"],
            { ctx =>
                ctx.register("my-answer", { _ => LispValue.Int(42) })
                ctx.registerType("my-app-object")
            },
            escapable: false, hostPolicy: false
        )
        let domains = [BuiltinFunctions.domainBase(), myDomain]
        @Assert(this.runWithDomains(domains, "(my-answer)") == "42")
        println("✓ 自定义域注册的 my-answer 可用 = 42")
    }

    /**
     * 如实记录：①/② 类符号**关不掉**。
     *
     * 只装 base 时，`if`（①语言原语）、`map`（②求值器拦截）仍然可用 ——
     * 它们由编译进去的求值器提供，不在环境里，任何域组合都动不了。
     * 这是"域装配只覆盖③类"这一边界的事实依据（设计文档 §5.3）。
     */
    @TestCase
    func testPrimitivesCannotBeDisabledByDomains() {
        println("\n=== ①/② 类不受域装配影响（边界如实记录） ===")
        let baseOnly = [BuiltinFunctions.domainBase()]
        // ① 语言原语
        @Assert(this.runWithDomains(baseOnly, "(if #t 1 2)") == "1")
        println("✓ 只装 base: (if #t 1 2) = 1（①不可关）")
        // ② 求值器拦截
        @Assert(this.runWithDomains(baseOnly, "(map (lambda (x) x) (quote (1 2)))") == "(1 2)")
        println("✓ 只装 base: (map …) 仍可用（②不可关，但可被用户顶替）")
        // ③ 注册物则确实关掉了
        @Assert(this.runWithDomains(baseOnly, "(cons 1 (quote (2)))").contains("UndefinedFunction"))
        println("✓ 只装 base: (cons …) 未定义（③可关）")
    }

    /** `after` 顺序约束：域按依赖拓扑序安装 */
    @TestCase
    func testAfterOrdering() {
        println("\n=== after 顺序约束 ===")
        let ordered = BuiltinFunctions.orderDomains([
            BuiltinFunctions.domainMacrosCore(),  // 声明 after: base/list/logic
            BuiltinFunctions.domainBase(),
            BuiltinFunctions.domainList(),
            BuiltinFunctions.domainLogic(),
        ])
        var names = ArrayList<String>()
        for (d in ordered) {
            names.add(d.name)
        }
        // 断言 base 在 macros.core 之前（after 约束生效）
        var baseIdx: Int64 = -1
        var macroIdx: Int64 = -1
        for (i in 0..names.size) {
            if (names[i] == "base") { baseIdx = i }
            if (names[i] == "macros.core") { macroIdx = i }
        }
        @Assert(baseIdx >= 0 && macroIdx >= 0 && baseIdx < macroIdx)
        var order = ""
        for (n in names) { order += n + " " }
        println("✓ 拓扑序: " + order)
    }

    /** 命令行选项：`withPreset` / `withDomains` 可装配解释器 */
    @TestCase
    func testInterpreterOptions() {
        println("\n=== InterpreterOption 装配 ===")
        let it = LispInterpreter([withQuietMode(), withPreset(presetFull())])
        @Assert(it.eval("(+ 1 2)").toString() == "3")
        println("✓ withPreset(presetFull()) 后 (+ 1 2) = 3")

        // replace: true —— 独占装配：清掉默认全量，只留所选域
        let it2 = LispInterpreter([
            withQuietMode(),
            withDomains([BuiltinFunctions.domainBase(), BuiltinFunctions.domainArithmetic()],
                replace: true)
        ])
        @Assert(it2.eval("(+ 1 2)").toString() == "3")
        @Assert(it2.eval("(when #t 1)").toString().contains("UndefinedFunction"))
        @Assert(it2.eval("(println 1)").toString().contains("UndefinedFunction"))
        println("✓ withDomains(…, replace: true) 后 when / println 均未定义")

        // 默认（追加）模式：默认全量仍在
        let it3 = LispInterpreter([
            withQuietMode(),
            withDomains([BuiltinFunctions.domainBase()])
        ])
        @Assert(it3.eval("(when #t 1)").toString() == "1")
        println("✓ 默认追加模式：默认全量仍在（when 可用）")
    }
}
