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
        // 全量装配 = `presetFull` 的域并集。
        //
        // 不再逐个调注册器：那份"平行清单"曾与域集合漂移（`domainPrint` 不在
        // `coreDomains()` 里、`registerModernAliases` 不属任何域），而两份清单
        // 各自的测试都只覆盖自己那条路径，漂移不会被发现。
        // 现在只有**一份**装配逻辑；`testPresetFullCoversRegisterAll` 反向钉住等价性。
        installDomains(env, presetFull())
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
        // 纯函数子集 = `presetPure`（按 `!escapable && !hostPolicy` 推导）的域并集。
        // 输出通道是 `hostPolicy` 域、默认被排除，由调用方用 `output` 显式接管。
        installDomains(env, presetPure())

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

    }
}
