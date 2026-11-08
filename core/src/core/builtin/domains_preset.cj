package ystyle::xisp.core.builtin

import ystyle::xisp.types.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.parser.*
import std.collection.ArrayList
import std.collection.HashSet

/**
 * 预置组合（preset）与安装器
 *
 * `registerAll` / `registerPure` 的定位从"两个硬编码函数"改为
 * **"命名 + 域列表"的组合**（能力域设计 §4.3）：
 *
 * | preset | 含义 |
 * |---|---|
 * | `PRESET_FULL` | 全部 core 内置域 |
 * | `PRESET_PURE` | 按属性推导：`!escapable && !hostPolicy` |
 *
 * 宿主也可以自造组合：`withDomains([domainArithmetic(), domainList(), MyDomain()])`。
 * 组合的语义是**并集**，按 `after` 约束排序后逐个 `install`。
 */
extend BuiltinFunctions {
    /**
     * 全部 core 内置域。
     *
     * 注意 `print` 域属 `hostPolicy`，**不在** `PRESET_PURE` 内 ——
     * 纯函数子集要求由宿主用 `output` 参数接管输出（保持既有语义）。
     */
    public static func presetFull(): Array<LispCapability> {
        coreDomains()
    }

    /**
     * 纯函数子集：按属性推导，而非手写清单。
     *
     * 规则：`!escapable && !hostPolicy`。新增域默认 `escapable = true`（保守），
     * 故不会意外进入 pure —— **默认安全**。
     */
    public static func presetPure(): Array<LispCapability> {
        let result = ArrayList<LispCapability>()
        for (d in coreDomains()) {
            if (!d.escapable && !d.hostPolicy) {
                result.add(d)
            }
        }
        result.toArray()
    }

    /**
     * 表达式层：**纯计算，无元编程**。
     *
     * = `presetPure` 去掉两处"只服务元编程"的域：
     * - `macros.core`（④层宏包）—— 宏是语法层设施，宿主只想求值表达式时不需要；
     * - `symbols`（`gensym`）—— 它为**非卫生宏**存在，没有宏就没有意义。
     *
     * 仍然包含数据结构（哈希表）、字符串计算、谓词、错误辅助与别名 ——
     * 这些是"算出一个值"就会用到的东西。不含输出域（`hostPolicy`），
     * 输出通道由宿主用 `output` 接管。
     */
    public static func exprDomains(): Array<LispCapability> {
        let result = ArrayList<LispCapability>()
        for (d in presetPure()) {
            match (d.name) {
                case "macros.core" => ()
                case "symbols" => ()
                case _ => result.add(d)
            }
        }
        result.toArray()
    }

    /**
     * 按 `after` 约束给域排序（拓扑序，稳定性由原顺序保证）。
     *
     * 顺序敏感是既有事实：`registerModernAliases` 依赖"在所有原函数注册完之后"。
     * 域化后该隐式约束显式化为 `after`。
     */
    public static func orderDomains(domains: Array<LispCapability>): Array<LispCapability> {
        let byName = HashSet<String>()
        for (d in domains) {
            byName.add(d.name)
        }
        let emitted = HashSet<String>()
        let result = ArrayList<LispCapability>()
        // 简单做法：反复扫描，把"依赖已就绪"的域依次发出。
        // 域数量是常数级（<20），无需复杂算法；有环时下面的兜底会全量发出。
        var remaining = ArrayList<LispCapability>()
        for (d in domains) {
            remaining.add(d)
        }
        var progress = true
        while (remaining.size > 0 && progress) {
            progress = false
            let next = ArrayList<LispCapability>()
            for (d in remaining) {
                var ready = true
                for (dep in d.after) {
                    // 依赖不在本次集合内则忽略（例如别名域可独立安装）
                    if (byName.contains(dep) && !emitted.contains(dep)) {
                        ready = false
                    }
                }
                if (ready) {
                    result.add(d)
                    emitted.add(d.name)
                    progress = true
                } else {
                    next.add(d)
                }
            }
            remaining = next
        }
        // 兜底：仍有剩余说明存在环，按原顺序补齐（不静默丢弃任何域）
        for (d in remaining) {
            result.add(d)
        }
        result.toArray()
    }

    /**
     * 安装一组域到环境（供 `withDomains` / `withPreset` 与手工装配使用）。
     *
     * 提供 `interpreter` 时，上下文里带上中断检查与函数值调用入口；
     * 手工装配（只有 `Environment`）时二者退化为安全默认。
     */
    public static func installDomains(
        env: Environment,
        domains: Array<LispCapability>,
        isInterrupted!: Option<() -> Bool> = None
    ): Unit {
        let ordered = orderDomains(domains)
        for (d in ordered) {
            let ctx = makeContext(env, isInterrupted)
            d.install(ctx)
        }
    }

    /**
     * 构造域安装上下文。
     *
     * `evalSource` 用 `Evaluator(env).eval` 装宏包 —— 这是**宿主安装期**行为，
     * 与"脚本运行期禁用 eval"是两件事（域安装由宿主发起，不受脚本侧权限约束）。
     */
    static func makeContext(env: Environment, interrupted: Option<() -> Bool>): CapabilityContext {
        let evaluator = Evaluator(env)
        // 声明过的外部类型名（域可登记，供沙箱审计）
        let typeNames = HashSet<String>()
        let isInterrupted: () -> Bool = match (interrupted) {
            case Some(f) => f
            case None => { => false }
        }
        let invoke: (LispValue, ArrayList<LispValue>) -> LispValue = { fn, args => evaluator.applyFunction(fn, args) }
        let evalSource: (String) -> Unit = { source =>
            let lexer = Lexer(source)
            let parser = Parser(lexer.tokenize())
            for (e in parser.parse()) {
                evaluator.eval(e)
            }
        }
        let register: (String, (ArrayList<LispValue>) -> LispValue) -> Unit = { name, handler =>
            env.define(name, NativeFunc(handler))
        }
        let registerType: (String) -> Unit = { t => typeNames.add(t) }
        CapabilityContext(env, isInterrupted, register, registerType, invoke, evalSource)
    }
}
