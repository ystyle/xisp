package ystyle::xisp.core.eval

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 能力域抽象（能力域设计 §4、接缝 2）
 *
 * ## 为什么需要"域"这个抽象
 *
 * 现状的装配入口有两档（`registerAll` 全量 / `registerPure` 纯函数子集），
 * 二者是**两份平行清单**，靠人肉同步（实测：`str` 在 `registerAll` 里是经
 * `registerPrint` **间接**注册的，而 `registerPure` 是**显式**调用 —— 谁调整
 * `registerPrint` 的内部组成，`registerAll` 就会静默丢掉 `str`）。
 *
 * 域把"一组顶层符号 + 安装方式 + 属性"打包成一个自描述单元：
 * - `PRESET_FULL` = 所有域的并集；
 * - `PRESET_PURE` = 按属性**推导**出的子集（不再手写清单）；
 * - 宿主可以自造组合，或用自定义域顶替 core 的同能力域。
 *
 * ## 域的两个来源（同一抽象）
 *
 * | 层 | 作用者 | 说明 |
 * |---|---|---|
 * | L1 core 内置域 | xisp 自己 | 包装现有 `BuiltinFunctions.registerXxx` |
 * | L2 宿主自定义域 | 宿主 / 外部模块 | 如 `xisp_net` 提供的 `NetCapability` |
 */

/**
 * 域安装上下文
 *
 * 域在 `install` 时需要的不止"环境"，还有：中断检查（长循环必须能被打断）、
 * 函数值调用（注册函数要能回调 Lisp 闭包）、源码安装（宏包形态）。
 * 这些都是**实例相关**的，故经上下文显式注入，不用全局状态。
 */
public class CapabilityContext {
    /** 目标环境：域在此注册自己的顶层符号 */
    public let env: Environment

    /**
     * 中断检查：外部域中任何 O(元素数) 以上的循环必须周期性调用它，
     * 为真时以 `Error(RuntimeError, INTERRUPTED_MESSAGE)` 中止。
     *
     * 理由：沙箱 `timeout` 与宿主取消**只能打断求值热路径** ——
     * 一个跑 30 秒的分解循环若不主动查中断，宿主就杀不掉它。
     */
    public let isInterrupted: () -> Bool

    /** 注册一个顶层函数（仓颉实现） */
    public let register: (String, (ArrayList<LispValue>) -> LispValue) -> Unit

    /** 声明本域产生的外部类型名（供沙箱审计与自省） */
    public let registerType: (String) -> Unit

    /** 调用任意 Lisp 函数值（接缝 0a）——注册函数回调闭包的唯一途径 */
    public let invoke: (LispValue, ArrayList<LispValue>) -> LispValue

    /** 以 Lisp 源码安装（宏包形态，§4.2） */
    public let evalSource: (String) -> Unit

    public init(
        env: Environment,
        isInterrupted: () -> Bool,
        register: (String, (ArrayList<LispValue>) -> LispValue) -> Unit,
        registerType: (String) -> Unit,
        invoke: (LispValue, ArrayList<LispValue>) -> LispValue,
        evalSource: (String) -> Unit
    ) {
        this.env = env
        this.isInterrupted = isInterrupted
        this.register = register
        this.registerType = registerType
        this.invoke = invoke
        this.evalSource = evalSource
    }
}

/**
 * 能力域
 *
 * 五个要素（设计文档 §4.1）：注册集合、实现位置、默认开关、属性、安装顺序。
 */
public interface LispCapability {
    /** 域标识，如 `"arith"` / `"macros.core"` */
    prop name: String

    /** 本域注册的顶层符号，供沙箱审计、文档生成、重名检出 */
    prop symbols: Array<String>

    /**
     * 该域能否**绕过权限检查**（如 `eval` / `apply` / 高阶回调）。
     *
     * 安全默认：新增域应保守地声明 `true`，只有确认不逃逸才置 `false`，
     * 这样它才可能进入 `PRESET_PURE`。
     */
    prop escapable: Bool

    /** 该域是否触及宿主资源（文件 / 网络 / 进程 / 输出） */
    prop hostPolicy: Bool

    /** 安装顺序约束：本域必须在这些域**之后**安装（如别名域依赖被别名域） */
    prop after: Array<String>

    /** 安装本域 */
    func install(ctx: CapabilityContext): Unit
}

/**
 * 通用域实现：用一个闭包承载"安装动作"，避免每个域写一个类。
 *
 * 域的实现方式只有两种，本类同时支持：
 * - 仓颉实现：闭包里调 `ctx.register(…)`（或复用 core 已有的 `registerXxx`）
 * - Lisp 源码：闭包里调 `ctx.evalSource(…)`（宏包形态）
 */
public class SimpleCapability <: LispCapability {
    private let domainName: String
    private let domainSymbols: Array<String>
    private let domainEscapable: Bool
    private let domainHostPolicy: Bool
    private let domainAfter: Array<String>
    private let action: (CapabilityContext) -> Unit

    public init(
        name: String,
        symbols: Array<String>,
        action: (CapabilityContext) -> Unit,
        escapable!: Bool = true,
        hostPolicy!: Bool = false,
        after!: Array<String> = Array<String>()
    ) {
        this.domainName = name
        this.domainSymbols = symbols
        this.domainEscapable = escapable
        this.domainHostPolicy = hostPolicy
        this.domainAfter = after
        this.action = action
    }

    public prop name: String {
        get() { this.domainName }
    }

    public prop symbols: Array<String> {
        get() { this.domainSymbols }
    }

    public prop escapable: Bool {
        get() { this.domainEscapable }
    }

    public prop hostPolicy: Bool {
        get() { this.domainHostPolicy }
    }

    public prop after: Array<String> {
        get() { this.domainAfter }
    }

    public func install(ctx: CapabilityContext): Unit {
        this.action(ctx)
    }
}
