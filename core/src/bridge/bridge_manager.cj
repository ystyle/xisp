package ystyle::xisp.bridge

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 桥接管理器
 * 管理解释器的桥接函数注册
 */
public class BridgeManager {
    private var env: Environment
    private var pathChecker: ?(String, Bool) -> Bool = None

    public init(env: Environment) {
        this.env = env
    }

    public func setPathChecker(checker: (String, Bool) -> Bool) {
        this.pathChecker = Some(checker)
    }

    /**
     * 把桥接的目标环境换成 `env`。
     *
     * 桥接持有的是**构造时那个环境**；环境被整体替换后（`clearEnvironmentBindings`
     * 为独占装配换新环境）必须同步，否则后续 `registerStdFS()` 之类会把函数注册进
     * 一个已经被丢弃的环境 —— 现象是"装配完标准库却 `cangjie::exists?` 不存在"。
     */
    public func retarget(env: Environment): Unit {
        this.env = env
    }

    /**
     * 注册桥接函数（不带命名空间）
     */
    public func register(
        name: String,
        handler: (ArrayList<LispValue>) -> LispValue
    ): Unit {
        Bridge.registerFunc(this.env, name, handler)
    }

    /**
     * 注册带命名空间的桥接函数
     *
     * 生成的完整符号为 `ns::name`（双冒号），与 `Bridge.registerFuncWithNS`
     * 和标准库桥接（`cangjie::read-file`）保持同一约定。
     */
    public func registerWithNS(
        ns: String,
        name: String,
        handler: (ArrayList<LispValue>) -> LispValue
    ): Unit {
        Bridge.registerFuncWithNS(this.env, ns, name, handler)
    }

    /**
     * 批量注册 std.io 模块
     */
    public func registerStdIO(): Unit {
        match (this.pathChecker) {
            case Some(checker) =>
                Bridge.registerStdIO(this.env, Some(checker))
            case None =>
                Bridge.registerStdIO(this.env)
        }
    }

    /**
     * 批量注册 std.fs 模块
     */
    public func registerStdFS(): Unit {
        Bridge.registerStdFS(this.env)
    }
}
