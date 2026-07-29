package ystyle::xisp.core.builtin

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
    public static func registerAll(env: Environment): Unit {
        // 注册基础字面量（nil 需要定义为符号，因为解析器不处理 nil）
        env.define("nil", Nil)

        registerArithmetic(env)
        registerModulo(env)
        registerMath(env)
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
        registerErrorHelpers(env)
    }

    static func registerErrorHelpers(env: Environment): Unit {
        // (error message) - 创建一个运行时错误
        env.define("error", NativeFunc({ args =>
            let msg = if (args.size > 0) { args[0].toString() } else { "error" }
            LispValue.Error(XispError(ErrorType.RuntimeError, msg))
        }))

        // (raise message) - error 的别名
        env.define("raise", NativeFunc({ args =>
            let msg = if (args.size > 0) { args[0].toString() } else { "error" }
            LispValue.Error(XispError(ErrorType.RuntimeError, msg))
        }))

        // (assert condition message) - 断言
        env.define("assert", NativeFunc({ args =>
            if (args.size < 1) {
                return LispValue.Error(XispError(ErrorType.ArityMismatch, "assert requires at least 1 argument"))
            }
            match (args[0]) {
                case LispValue.Boolean(b) =>
                    if (b) {
                        LispValue.Boolean(true)
                    } else {
                        let msg = if (args.size >= 2) { args[1].toString() } else { "assertion failed" }
                        LispValue.Error(XispError(ErrorType.RuntimeError, msg))
                    }
                case _ =>
                    LispValue.Boolean(true)
            }
        }))

        // (for-each proc lst1 lst2 ...) - 对每个元素应用过程，返回 nil
        env.define("for-each", NativeFunc({ args =>
            if (args.size < 2) {
                return Nil
            }
            if (let NativeFunc(proc) <- args[0]) {
                let lists = ArrayList<ArrayList<LispValue>>()
                for (i in 1..args.size) {
                    let list = ArrayList<LispValue>()
                    var cell = args[i]
                    while (let Cons(c) <- cell) {
                        list.add(c.car)
                        cell = c.cdr
                    }
                    lists.add(list)
                }
                let minLen = lists[0].size
                var minLenVar = minLen
                for (i in 1..lists.size) {
                    if (lists[i].size < minLenVar) { minLenVar = lists[i].size }
                }
                for (i in 0..minLenVar) {
                    let procArgs = ArrayList<LispValue>()
                    for (j in 0..lists.size) {
                        procArgs.add(lists[j][i])
                    }
                    proc(procArgs)
                }
            }
            Nil
        }))
    }
}
