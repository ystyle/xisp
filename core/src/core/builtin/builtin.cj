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

    /**
     * 注册纯函数子集：只含不触及文件系统、子进程、模块加载与求值逃逸的表达式能力。
     *
     * 面向嵌入式宿主的最小权限环境。注意权限检查（`Evaluator.setFuncChecker`）对
     * 特殊形式无效——`eval` / `apply` / `map` / `filter` / `reduce` / `for-each` 会先于
     * 检查执行，且函数值可被当作参数传递，因此「注册后过滤」不可靠，只能用
     * 「不注册」来表达禁止。本方法即为此提供入口。
     *
     * 不含输出函数：脚本输出的去向由宿主决定，用 `output` 参数把
     * print / println / princ / newline / display 接到自己的通道；不传则不注册它们。
     *
     * @param env 目标环境
     * @param output 输出接收器（每个文本片段一次回调，换行以 "\n" 片段表达）
     */
    public static func registerPure(env: Environment, output!: Option<(String) -> Unit> = None): Unit {
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
        registerHashMap(env)
        registerStringOps(env)
        BuiltinMacros.registerAll(env)
        registerRedisAliases(env)     // 哈希表别名
        registerModernAliases(env)    // 最后注册别名，确保所有原函数已注册
        registerErrorHelpers(env)

        match (output) {
            case Some(sink) => registerOutput(env, sink)
            case None => ()
        }
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
