package ystyle::xisp.core.eval

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 函数值调用入口（能力域设计 §3.1 接缝 A）
 *
 * ## 问题
 *
 * 用仓颉注册的函数（`NativeFunc` 的 handler）只接收实参列表，**拿不到求值器**，
 * 因此无法调用一个 Lisp 函数值。后果实测：
 *
 * ```cangjie
 * // core/src/core/builtin/builtin.cj:113（for-each 的旧实现）
 * if (let NativeFunc(proc) <- args[0]) { … }   // 只认 NativeFunc，Lisp 闭包被静默跳过
 * ```
 *
 * `(for-each (lambda (x) x) '(1 2 3))` 返回 `nil` —— **什么都不做，也不报错**。
 * 而 `map`（求值器实现）能正常调用闭包，因为它走内部的 `applyProcedure`。
 *
 * ## 为什么这是"可替换"的硬门槛
 *
 * 用户想用**仓颉**复刻一个 core 已有的高阶函数（如 `map`）时，必须能调用 Lisp 回调。
 * 纯 Lisp 实现没这个问题（由求值器执行、天然能调闭包），**门槛只卡在仓颉注册的函数上**。
 *
 * ## 用法
 *
 * `applyFunction` 接收**已求值**的函数值与实参列表，按函数类型分派：
 * `Procedure` / `ProcedureFromParams` / `Closure`（编译闭包，经 `ExecutionBackend.callClosure`）
 * / `NativeFunc`。不认识的类型返回 `Error`（而不是旧的静默 `Nil`）。
 */
extend Evaluator {
    /**
     * 调用一个 Lisp 函数值。
     *
     * 与 `LispInterpreter.call` 的区别：后者按**函数名**查找，且实参是 `Array<T>`（同构）；
     * 本方法直接接收函数**值**与 `ArrayList<LispValue>` 实参，供注册函数在自己的
     * handler 里回调 Lisp 闭包。
     *
     * @param procValue 函数值（`Procedure` / `ProcedureFromParams` / `Closure` / `NativeFunc`）
     * @param args 已求值的实参
     * @return 调用结果；`procValue` 不是可调用值时为 `Error`
     */
    public func applyFunction(procValue: LispValue, args: ArrayList<LispValue>): LispValue {
        match (procValue) {
            case Procedure(_, _, _) | ProcedureFromParams(_, _, _) | Closure(_, _) =>
                this.applyProcedure(procValue, args)
            case NativeFunc(f) =>
                f(args)
            // 外部值：实现 LispExternalCallable 的可以被调用
            case External(ext) =>
                match (ext) {
                    case c: LispExternalCallable => c.invoke(args)
                    case _ =>
                        LispValue.Error(XispError(ErrorType.TypeError,
                            "Attempt to call a non-callable external value (${ext.typeName})"))
                }
            // 宏不是函数值：它不是"调用"的对象，而是"展开"的对象
            case Macro(_, _, _) | MacroFromParams(_, _, _) =>
                LispValue.Error(XispError(ErrorType.RuntimeError,
                    "applyFunction: macro is not callable (it expands, not applies)"))
            case other =>
                // 不静默返回 nil：类型错误必须可见（旧的 for-each 正是静默跳过）
                LispValue.Error(XispError(ErrorType.TypeError,
                    "Attempt to call a non-function value: ${other.toString()}"))
        }
    }

    /**
     * 判断一个值是否可调用（即 `applyFunction` 能成功处理）。
     *
     * 供注册函数在调用前做类型校验，避免把非函数值的错误处理写成静默跳过。
     */
    public func isCallableValue(value: LispValue): Bool {
        match (value) {
            case Procedure(_, _, _) | ProcedureFromParams(_, _, _) | Closure(_, _) | NativeFunc(_) => true
            case External(ext) =>
                match (ext) {
                    case _: LispExternalCallable => true
                    case _ => false
                }
            case _ => false
        }
    }
}
