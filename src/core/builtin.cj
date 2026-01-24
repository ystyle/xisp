package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList
import std.math.fmod

/**
 * 基础函数库 - 注册所有内置函数到环境
 */
public class BuiltinFunctions {}

extend BuiltinFunctions {
    /**
     * 注册所有基础函数到环境
     */
    public static func registerAll(env: Environment) {
        registerArithmetic(env)
        registerModulo(env)
        registerComparison(env)
        registerListOps(env)
        registerHigherOrder(env)
        registerLogic(env)
        registerPredicates(env)
        registerPrint(env)
        registerHashMap(env)
        BuiltinMacros.registerAll(env)
        registerRedisAliases(env)     // Redis 风格别名
        registerModernAliases(env)      // 最后注册别名，确保所有原函数已注册
    }
}
