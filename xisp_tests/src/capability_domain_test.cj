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

        // 宏包**必须**在纯函数子集里：宏是代码变换器，展开产物照样过权限检查，
        // 不提供任何逃逸手段（`registerPure` 一直包含 ④ 层宏包）。
        // 原先这里断言"看起来更安全"的排除，实际是语义错误：纯环境里连
        // `(when #t 1)` 都不认，两档装配无法互相替代。
        var hasMacros = false
        for (d in pure) {
            if (d.name == "macros.core") {
                hasMacros = true
            }
        }
        @Assert(hasMacros)
        println("✓ 已包含 macros.core（宏不逃逸，纯函数子集需要它）")
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

    /**
     * **独占装配后，别名必须仍然可用**（回归）
     *
     * `first` / `rest` / `second` / `third` / `fourth` / `prepend` / `more` /
     * `cadr`… 这些"现代别名"与 `setq` / `progn` 两个关键字别名，全部由
     * `registerModernAliases` 注册 —— 而它**曾经不属任何域**，于是
     * `withDomains(…, replace: true)`（"只留所选域"这一整条可裁剪路径）下
     * 它们集体消失，`domainList.symbols` 里声明的 `first` / `rest` 成了空话。
     */
    @TestCase
    func testAliasesAvailableUnderExclusiveAssembly() {
        println("\n=== 独占装配后别名仍可用 ===")
        let codes = [
            ("(first (quote (1 2 3)))", "1"),
            ("(rest (quote (1 2 3)))", "(2 3)"),
            ("(second (quote (1 2 3)))", "2"),
            ("(third (quote (1 2 3)))", "3"),
            ("(fourth (quote (1 2 3 4)))", "4"),
            ("(prepend 0 (quote (1 2)))", "(0 1 2)"),
            ("(more (quote (1 2 3)))", "(2 3)"),
            ("(cadr (quote (1 2 3)))", "2"),
            ("(caddr (quote (1 2 3)))", "3"),
            ("(cadddr (quote (1 2 3 4)))", "4"),
            ("(begin (define x 1) (setq x 5) x)", "5"),
            ("(progn 1 2 3)", "3"),
        ]
        for (domains in [BuiltinFunctions.presetFull(), BuiltinFunctions.coreDomains()]) {
            let env = Environment()
            BuiltinFunctions.installDomains(env, domains)
            let ev = Evaluator(env)
            for ((code, expect) in codes) {
                var r: LispValue = Nil
                for (e in Parser.parseFromString(code)) {
                    r = ev.eval(e)
                }
                if (r.toString() != expect) {
                    @Fail("独占装配下 `${code}` 期望 `${expect}`，实际 `${r.toString()}`")
                }
            }
        }
        println("✓ 12 条别名在预设全量 / 显式 coreDomains 独占装配下都可用")
    }

    /**
     * **域声明的符号必须真的被安装上**（回归）
     *
     * `symbols` 是域对外的契约（沙箱审计、文档生成、重名检出都读它），
     * 声明与安装脱节会让上层看到"有这个名字"却调不到。
     *
     * 关键字别名（`setq` / `progn`）不在环境绑定里，单独豁免。
     */
    @TestCase
    func testDeclaredSymbolsActuallyInstalled() {
        println("\n=== 域声明的符号与实际安装一致 ===")
        let keywordAliases = ["setq", "progn"]
        let env = Environment()
        BuiltinFunctions.installDomains(env, BuiltinFunctions.presetFull())
        var missing = ArrayList<String>()
        for (d in BuiltinFunctions.presetFull()) {
            for (sym in d.symbols) {
                if (keywordAliases.contains(sym)) {
                    continue
                }
                match (env.lookup(sym)) {
                    case Error(_) => missing.add("${d.name}.${sym}")
                    case _ => ()
                }
            }
        }
        if (missing.size > 0) {
            @Fail("域声明了但未安装的符号: ${missing}")
        }
        println("✓ 所有域的 symbols 声明均已实际安装")
    }

    /**
     * **`presetFull` 独占装配 ≡ `registerAll`**（回归）
     *
     * 这是能力域设计"核心接缝"最强的一条不变量：既然 `registerAll` 与
     * `presetFull` 是同一份能力的两种表达，那么独占装配后**一个符号都不能少**。
     *
     * 它曾漏掉两类共 20+ 个符号：
     * · `domainPrint` 压根不在 `coreDomains()` 里 → `println` / `str` / `string-length` 全缺；
     * · `registerModernAliases` 不属任何域 → `first` / `rest` / `second` / `setq` / `progn` 全缺。
     *
     * 只比"符号是否存在"，不比实现 —— 两侧由不同注册器产出，实现天然同源。
     */
    @TestCase
    func testPresetFullCoversRegisterAll() {
        println("\n=== presetFull 独占装配 ≡ registerAll ===")
        let refEnv = Environment()
        BuiltinFunctions.registerAll(refEnv)

        let env = Environment()
        BuiltinFunctions.installDomains(env, BuiltinFunctions.presetFull())

        var missing = ArrayList<String>()
        for (k in refEnv.getKeys()) {
            match (env.lookup(k)) {
                case Error(_) => missing.add(k)
                case _ => ()
            }
        }
        if (missing.size > 0) {
            @Fail("presetFull 独占装配缺少这些符号（registerAll 有）: ${missing}")
        }
        // 关键字别名不在绑定里，单独验证两条都得能用
        let ev = Evaluator(env)
        @Assert(ev.eval(Parser.parseSingleFromString("(begin (define q 1) (setq q 7) q)")).toString() == "7")
        @Assert(ev.eval(Parser.parseSingleFromString("(progn 1 2 3)")).toString() == "3")
        println("✓ presetFull 覆盖 registerAll 的全部 ${refEnv.getKeys().size} 个符号，且 setq/progn 可用")
    }

    /**
     * **`presetPure` 必须包含纯计算类能力**（回归）
     *
     * 输出通道（`hostPolicy`）应当被排除 —— 由宿主接管；但**字符串计算**是纯的，
     * 必须留着。`str` / `string-length` 曾因与 `print` 同域而被一起排除。
     */
    @TestCase
    func testPresetPureKeepsPureStringOps() {
        println("\n=== presetPure 保留字符串计算 ===")
        let env = Environment()
        BuiltinFunctions.installDomains(env, BuiltinFunctions.presetPure())
        let ev = Evaluator(env)
        @Assert(ev.eval(Parser.parseSingleFromString("(str 1 2)")).toString() == "\"12\"")
        @Assert(ev.eval(Parser.parseSingleFromString("(string-length \"abc\")")).toString() == "3")
        println("✓ presetPure 下 str / string-length 可用")
        // 输出通道仍被排除（宿主接管）
        @Assert(ev.eval(Parser.parseSingleFromString("(println 1)")).toString().contains("UndefinedFunction"))
        println("✓ presetPure 下 println 仍不可用（hostPolicy 域被排除）")
    }

    /**
     * **`presetPure` 独占装配 ≡ `registerPure`（不含输出）**（回归）
     *
     * 两档装配必须能互相替代，否则"收敛为预置组合"就只是换了个名字。
     * `registerPure` 一直包含 ④ 层宏包（`when` / `unless` / …），
     * 而 `macros.core` 原先标了 `escapable: true` 被属性推导排除 ——
     * 结果是 `presetPure` 下连 `(when #t 1)` 都不认。
     */
    @TestCase
    func testPresetPureCoversRegisterPure() {
        println("\n=== presetPure 独占装配 ≡ registerPure ===")
        let refEnv = Environment()
        BuiltinFunctions.registerPure(refEnv)

        let env = Environment()
        BuiltinFunctions.installDomains(env, BuiltinFunctions.presetPure())

        var missing = ArrayList<String>()
        for (k in refEnv.getKeys()) {
            match (env.lookup(k)) {
                case Error(_) => missing.add(k)
                case _ => ()
            }
        }
        if (missing.size > 0) {
            @Fail("presetPure 独占装配缺少这些符号（registerPure 有）: ${missing}")
        }
        // 宏包必须在纯函数子集里（registerPure 一直如此）
        let ev = Evaluator(env)
        @Assert(ev.eval(Parser.parseSingleFromString("(when #t 1)")).toString() == "1")
        @Assert(ev.eval(Parser.parseSingleFromString("(unless #f 2)")).toString() == "2")
        println("✓ presetPure 覆盖 registerPure 的全部 ${refEnv.getKeys().size} 个符号，宏包可用")
    }

    /**
     * **`evalSource` 安装不受脚本侧 `eval` 权限约束**（C15）
     *
     * 宏包是用 `evalSource`（内部 `Evaluator.eval`）装进环境的，属于**宿主安装期**行为；
     * 而 `withSandbox()` 的 `blockFunctions(["eval","apply"])` 管的是**脚本运行期**的
     * `eval` 调用。两者必须分开：沙箱里宏照样能用，脚本里的 `(eval …)` 照样被拦。
     */
    @TestCase
    func testEvalSourceInstallSurvivesSandbox() {
        println("\n=== evalSource 安装期不受脚本 eval 权限影响 ===")
        let it = LispInterpreter([withQuietMode(), withSandbox()])
        // 脚本运行期：eval 被拦
        @Assert(it.eval("(eval '(+ 1 2))").toString().contains("UndefinedFunction"))
        // 安装期已完成的宏包照常可用
        @Assert(it.eval("(when #t 1)").toString() == "1")
        @Assert(it.eval("(unless #f 2)").toString() == "2")
        @Assert(it.eval("(begin (define z 1) (incf z) z)").toString() == "2")
        println("✓ 沙箱下 (eval …) 被拦，而 when / unless / incf 正常")
    }
}
