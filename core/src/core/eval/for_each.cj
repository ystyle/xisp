package ystyle::xisp.core.eval

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * `for-each` —— 逐元素执行，返回 `nil`
 *
 * ## 为什么从注册函数移到求值器
 *
 * 旧实现是 `NativeFunc`（`core/src/core/builtin/builtin.cj`），它的 handler
 * **只接收实参列表、拿不到求值器**，因此无法调用 Lisp 函数值：
 *
 * ```cangjie
 * if (let NativeFunc(proc) <- args[0]) { … }   // Lisp 闭包被静默跳过
 * ```
 *
 * 后果：`(for-each (lambda (x) x) '(1 2 3))` 返回 `nil` —— **什么都不做，也不报错**
 * （文档曾把这写成"目前只能接受原生内置函数"的已知限制）。
 *
 * 求值器实现天然能调闭包（与 `map`/`filter`/`reduce` 走同一条 `applyProcedure`）。
 * 同时它列入可让位集合：用户可自行定义 `for-each` 顶掉本实现 —— 与其余高阶函数一致。
 */
extend Evaluator {
    /**
     * 特殊形式：for-each
     *
     * `(for-each proc lst1 lst2 ...)`：对每个下标取各列表的同位元素组成实参，
     * 依次调用 `proc`；以**最短列表**为准（多列表时）；返回 `nil`。
     */
    func evalForEach(expr: LispValue): LispValue {
        // 参数列表形态校验：必须是 Cons（非 Cons 时无事可做，返回 nil）
        if (expr.isNil()) {
            return Nil
        }
        let cell = match (expr) {
            case Cons(c) => c
            case _ => return Nil
        }
        if (cell.length() < 2) {
            return Nil
        }
        let procValue = this.eval(cell.car)

        // 非函数值：明确报错（旧实现的静默跳过是缺陷，不保留该行为）
        if (!this.isCallableValue(procValue)) {
            return LispValue.Error(XispError(ErrorType.TypeError,
                "for-each: expected a procedure, got ${procValue.toString()}"))
        }

        // 收集各列表的元素（列表实参需先求值）
        let lists = ArrayList<ArrayList<LispValue>>()
        var current: LispValue = cell.cdr
        while (let Cons(currCell) <- current) {
            let listValue = this.eval(currCell.car)
            let items = ArrayList<LispValue>()
            var item: LispValue = listValue
            while (let Cons(itemCell) <- item) {
                items.add(itemCell.car)
                item = itemCell.cdr
            }
            lists.add(items)
            current = currCell.cdr
        }

        if (lists.size == 0) {
            return Nil
        }

        // 以最短列表为准
        var minLen = lists[0].size
        for (i in 1..lists.size) {
            if (lists[i].size < minLen) {
                minLen = lists[i].size
            }
        }

        for (i in 0..minLen) {
            // 中断检查点：与 map/filter/reduce 一致，长列表可及时停下
            if (this.isInterrupted()) {
                return this.interruptError()
            }
            let procArgs = ArrayList<LispValue>()
            for (j in 0..lists.size) {
                procArgs.add(lists[j][i])
            }
            let r = this.applyFunction(procValue, procArgs)
            match (r) {
                case Error(_) => return r
                case _ => ()
            }
        }

        Nil
    }
}
