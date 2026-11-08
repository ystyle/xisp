package ystyle::xisp.core.eval

import ystyle::xisp.types.*

/**
 * 特殊形式的「让位」机制（能力域设计 §3.2 接缝 B）
 *
 * ## 问题
 *
 * `map` / `filter` / `reduce` / `apply` / `eval` / `->` 被求值器**硬编码**处理
 * （`trySpecialForm` 直接拦截）。用户即使 `(define map 我的实现)`，调用
 * `(map f lst)` 仍然走 core 的实现 —— **顶不掉**：
 *
 * ```
 * (define map 42)   →  map 求值为 42，但 (map f lst) 照常工作
 * ```
 *
 * 这与"可替换"目标冲突：用户应当能用宏或自己的实现，替换掉 core 提供的同能力符号。
 *
 * ## 让位规则
 *
 * **环境里若存在该名字的绑定，就让位**（返回 `None`，交给普通调用路径）。
 *
 * 之所以能用这么简单的判据：这 6 个名字 core **从不注册**（它们纯靠求值器拦截），
 * 所以环境里一旦出现绑定，必然是用户放的。对照组是算术符号 `+ - * / < > =`——
 * 它们 core **会**注册，故判据是"绑定还是不是 core 的 `NativeFunc`"（见
 * `arithmetic_fast.cj` 的 `isNativeArith` / `checkNative`）。
 *
 * ## 性能
 *
 * 用 `Environment.defineVersion`（**每次 `define` 都递增**的全局计数）做守卫：
 * 版本未变则直接读缓存，热路径无额外开销。这与 `isNativeArith` 用 `arithVersion`
 * 的做法同源，区别只是后者只跟踪算术符号，而"让位"关心任意 `define`。
 */
/** 可让位的特殊形式名（下标与 `deferSlot` 一致） */
let DEFER_NAMES: Array<String> = ["map", "filter", "reduce", "apply", "eval", "->", "for-each"]

extend Evaluator {
    /** 可让位符号在缓存数组中的下标；不属于可让位集合时返回 -1 */
    private func deferSlot(name: String): Int64 {
        match (name) {
            case "map" => 0
            case "filter" => 1
            case "reduce" => 2
            case "apply" => 3
            case "eval" => 4
            case "->" => 5
            case "for-each" => 6
            case _ => -1
        }
    }

    /**
     * 该特殊形式是否应让位给用户的实现。
     *
     * @param name 特殊形式名（不在可让位集合内时返回 false）
     */
    func shouldDeferToUser(name: String): Bool {
        let slot = this.deferSlot(name)
        if (slot < 0) {
            return false
        }
        // 守卫：define 过才重算（Environment.defineVersion 对每次 define 递增）
        if (this.deferGuardVersion != Environment.defineVersion.value) {
            this.deferGuardVersion = Environment.defineVersion.value
            for (i in 0..DEFER_NAMES.size) {
                this.deferGuardCache[i] = this.hasUserBinding(DEFER_NAMES[i])
            }
        }
        this.deferGuardCache[slot]
    }

    /**
     * 表达式头是否为"已让位"的可让位符号。
     *
     * 供 `evalInner` 在**编译分派之前**判断：让位必须优先于编译，否则后端会按
     * core 的语义编译该表达式，用户实现被绕过。
     */
    func headIsDeferred(expr: LispValue): Bool {
        match (expr) {
            case Cons(cell) => match (cell.car) {
                case Symbol(s) => this.shouldDeferToUser(s)
                case _ => false
            }
            case _ => false
        }
    }

    /**
     * 环境里是否存在该名字的绑定（非 `Error` 即视为存在）。
     *
     * core 不注册这 6 个名字，因此"存在绑定"等价于"用户放了实现"。
     */
    private func hasUserBinding(name: String): Bool {
        // core 自己安装的一等值（②类符号，接缝 0e）不算"用户实现"——
        // 否则快速路径会被自己的注册顶掉。
        if (this.env.isCoreInstalled(name)) {
            return false
        }
        match (this.env.lookup(name)) {
            case Error(_) => false
            case _ => true
        }
    }
}

extend Evaluator {
    /**
     * 特殊形式是否被宿主权限策略禁止。
     *
     * `funcChecker` 原本只作用于普通函数调用路径，特殊形式（`eval` / `apply` /
     * `map` / `filter` / `reduce` / `for-each` / `->` 等）由求值器直接分派、
     * 从不经过检查 → 沙箱的 `blockFunctions(["eval","apply"])` 形同虚设。
     *
     * 覆盖范围：仅"可逃逸"的那部分特殊形式（与 `shouldDeferToUser` 同一集合），
     * **不含** `if` / `define` / `lambda` 等语言原语 —— 它们是语法结构，
     * 拦掉会让任何脚本都无法运行，而非"限制能力"。
     */
    func specialFormBlocked(sym: String): Bool {
        match (this.funcChecker) {
            case Some(checker) =>
                // 只对可逃逸集合做检查（与可让位集合同一批名字）
                var inSet = false
                for (n in DEFER_NAMES) {
                    if (n == sym) {
                        inSet = true
                    }
                }
                inSet && !checker(sym)
            case None => false
        }
    }
}
