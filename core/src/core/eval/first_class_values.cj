package ystyle::xisp.core.eval

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * ②类符号的「一等值」（能力域设计 §8.1 接缝 0e）
 *
 * ## 问题
 *
 * `map` / `filter` / `reduce` / `apply` / `eval` / `->` / `for-each` 由求值器
 * 直接分派（②类），**环境里没有绑定**。后果：
 *
 * ```
 * (procedure? map)          → [UnboundVariable] Undefined variable: 'map'
 * (define (go f) (f …)) (go map) → [UnboundVariable]
 * ```
 *
 * 即：它们是"能力"，却**不能作为值传递**。这是"可替换"之外的另一半目标。
 *
 * ## 为什么不能简单地把它们改成普通函数
 *
 * 三件事冲突：
 *
 * 1. **性能**：求值器路径对 `map` 等高阶函数是专门的迭代实现；改成普通函数会退化。
 * 2. **栈深**：纯 Lisp 版 `map` **500 元素就爆栈**（实测，默认栈深 1000），
 *    而内置版能处理长列表。
 * 3. **语义**：`apply` / `eval` 需要"展开实参列表"与"对数据求值"——纯 Lisp
 *    无法表达（鸡生蛋）。
 *
 * ## 做法：**双轨**——保留求值器快速路径，另注册一个等价的一等值
 *
 * `installFirstClassValues` 在环境里注册同名 `NativeFunc`，其实现直接复用求值器
 * 已经写好的逻辑（不是重新实现）。随后：
 *
 * - **调用位置** `(map f lst)`：仍走求值器快速路径（性能与栈深不变）；
 * - **值位置** `(procedure? map)` / `(go map)`：从环境取到那个 `NativeFunc`。
 *
 * 而"让位"判据也随之调整：core 自己注册的值会被标记为**非用户所有**，
 * 用户覆盖后才让位（判据在 `defer_special_form.cj` 的 `hasUserBinding`，
 * 依靠 `Environment.markCoreInstalled` / `isCoreInstalled` 区分"core 装的"与"用户装的"）。
 */
/** 提供一等值的②类符号名 */
let FIRST_CLASS_NAMES: Array<String> = ["map", "filter", "reduce", "for-each", "apply", "eval", "->"]

extend Evaluator {
    /**
     * 为②类符号注册一等值。
     *
     * 由 `LispInterpreter` 在装配时调用（宿主可用 `withNoFirstClassValues()` 关闭）。
     */
    public func installFirstClassValues(env: Environment): Unit {
        // map / filter / reduce / for-each：复用求值器实现（构造等价的调用式再求值）
        this.defineCallable(env, "map", 2)
        this.defineCallable(env, "filter", 2)
        this.defineCallable(env, "reduce", 3)
        this.defineCallable(env, "for-each", 2)
        this.defineCallable(env, "apply", 2)
        this.defineCallable(env, "eval", 1)
        this.defineCallable(env, "->", 2)
    }

    /**
     * 把一个②类符号包装成 `NativeFunc`：调用时把实参重新拼成调用式，
     * 交给求值器原路径执行 —— **不重复实现逻辑**，行为必然一致。
     *
     * `minArity` 仅用于早退校验（实参不足时返回 `nil`，与求值器路径一致）。
     */
    func defineCallable(env: Environment, name: String, minArity: Int64): Unit {
        let value = NativeFunc({ args =>
            if (Int64(args.size) < minArity) {
                return Nil
            }
            let headFn: LispValue = LispValue.Symbol(name)
            var tail: LispValue = Nil
            var i = args.size - 1
            while (i >= 0) {
                // 实参已是**值**，需按值注入：列表值包 quote 防止被当代码再求值
                // （与 LispInterpreter.call 同一处理，见 interpreter.cj）
                let a = args[i]
                let quoted = if (a.isList()) {
                    consValue(LispValue.Symbol("quote"), consValue(a, Nil))
                } else {
                    a
                }
                tail = consValue(quoted, tail)
                i--
            }
            this.eval(consValue(headFn, tail))
        })
        env.define(name, value)
        // 标记为"core 安装的一等值"，供让位判据区分
        // （`defer_special_form.cj` 的 hasUserBinding → Environment.isCoreInstalled）
        env.markCoreInstalled(name)
    }
}

extend Evaluator {
    /**
     * 移除②类符号的一等值（`withNoFirstClassValues()` 用）。
     *
     * 只移除环境里**仍是我们安装的那份**（值仍是 NativeFunc 且带 core 标记）；
     * 用户已覆盖的名字不动。
     */
    public func uninstallFirstClassValues(env: Environment): Unit {
        for (name in FIRST_CLASS_NAMES) {
            if (env.isCoreInstalled(name)) {
                env.removeBinding(name)
            }
        }
    }
}
