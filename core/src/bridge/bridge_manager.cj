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
     * 注册桥接函数（不带命名空间）
     */
    public func register(
        name: String,
        handler: (ArrayList<LispValue>) -> LispValue
    ): Unit {
        this.env.define(name, NativeFunc(handler))
    }

    /**
     * 注册带命名空间的桥接函数
     */
    public func registerWithNS(
        ns: String,
        name: String,
        handler: (ArrayList<LispValue>) -> LispValue
    ): Unit {
        let fullName = "${ns}:${name}"
        this.env.define(fullName, NativeFunc(handler))
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
