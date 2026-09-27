package ystyle::xisp.core.eval

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 算术快速路径特殊形式
 *
 * 将 + - * / < > = 从「函数调用」提升为「特殊形式」，跳过：
 *   - env.lookup 查找函数名
 *   - funcChecker 权限检查
 *   - pushFrame/popFrame 调用栈管理
 *   - NativeFunc 闭包分派
 *   - evalArguments 的 ArrayList 构建（二元 Int 情况）
 *
 * 常见形态 (+ a b) / (- a b) / (< a b) 且 a、b 均为 Int 时，
 * 直接提取 Int64 计算，零中间分配。
 *
 * 语义与 src/core/builtin/arithmetic.cj 和 comparison.cj 保持一致。
 */
extend Evaluator {
    /**
     * 特殊形式：(+ n1 n2 ...) 加法
     * 二元 Int 走快速路径，否则回退原生函数
     */
    func evalAdd(rest: LispValue): LispValue {
        if (!this.isNativeArith("+")) {
            return this.evalFunctionCall(Symbol("+"), rest)
        }
        match (rest) {
            case Nil => LispValue.Int(0)
            case Cons(c1) =>
                match (c1.cdr) {
                    case Nil =>
                        // (+ x) — 单参数返回自身
                        this.eval(c1.car)
                    case Cons(c2) =>
                        match (c2.cdr) {
                            case Nil =>
                                // (+ a b) — 二元快速路径
                                this.evalArithBin(c1.car, c2.car, "+")
                            case _ =>
                                // 3+ 参数 — 通用路径
                                this.callNativeArith("+", rest)
                        }
                    case _ => Nil
                }
            case _ => Nil
        }
    }

    /**
     * 特殊形式：(- n1 n2 ...) 减法
     */
    func evalSub(rest: LispValue): LispValue {
        if (!this.isNativeArith("-")) {
            return this.evalFunctionCall(Symbol("-"), rest)
        }
        match (rest) {
            case Nil => Nil
            case Cons(c1) =>
                match (c1.cdr) {
                    case Nil =>
                        // (- x) — 取反
                        let v = this.eval(c1.car)
                        match (v) {
                            case LispValue.Int(i) => LispValue.Int(-i)
                            case LispValue.Float(f) => LispValue.Float(-f)
                            case Error(_) => v
                            case _ => Nil
                        }
                    case Cons(c2) =>
                        match (c2.cdr) {
                            case Nil =>
                                // (- a b) — 二元快速路径
                                this.evalArithBin(c1.car, c2.car, "-")
                            case _ =>
                                this.callNativeArith("-", rest)
                        }
                    case _ => Nil
                }
            case _ => Nil
        }
    }

    /**
     * 特殊形式：(* n1 n2 ...) 乘法
     */
    func evalMul(rest: LispValue): LispValue {
        if (!this.isNativeArith("*")) {
            return this.evalFunctionCall(Symbol("*"), rest)
        }
        match (rest) {
            case Nil => LispValue.Int(1)
            case Cons(c1) =>
                match (c1.cdr) {
                    case Nil =>
                        this.eval(c1.car)
                    case Cons(c2) =>
                        match (c2.cdr) {
                            case Nil =>
                                this.evalArithBin(c1.car, c2.car, "*")
                            case _ =>
                                this.callNativeArith("*", rest)
                        }
                    case _ => Nil
                }
            case _ => Nil
        }
    }

    /**
     * 特殊形式：(/ n1 n2 ...) 除法（总是返回浮点数）
     */
    func evalDiv(rest: LispValue): LispValue {
        if (!this.isNativeArith("/")) {
            return this.evalFunctionCall(Symbol("/"), rest)
        }
        match (rest) {
            case Nil => Nil
            case Cons(c1) =>
                match (c1.cdr) {
                    case Nil => Nil  // 单参数除法返回 nil
                    case Cons(c2) =>
                        match (c2.cdr) {
                            case Nil =>
                                this.evalArithBin(c1.car, c2.car, "/")
                            case _ =>
                                this.callNativeArith("/", rest)
                        }
                    case _ => Nil
                }
            case _ => Nil
        }
    }

    /**
     * 特殊形式：(< n1 n2 ...) 小于
     */
    func evalLt(rest: LispValue): LispValue {
        if (!this.isNativeArith("<")) {
            return this.evalFunctionCall(Symbol("<"), rest)
        }
        match (rest) {
            case Nil => LispValue.Boolean(true)
            case Cons(c1) =>
                match (c1.cdr) {
                    case Nil => LispValue.Boolean(true)
                    case Cons(c2) =>
                        match (c2.cdr) {
                            case Nil =>
                                this.evalArithBin(c1.car, c2.car, "<")
                            case _ =>
                                this.callNativeArith("<", rest)
                        }
                    case _ => LispValue.Boolean(true)
                }
            case _ => LispValue.Boolean(true)
        }
    }

    /**
     * 特殊形式：(> n1 n2 ...) 大于
     */
    func evalGt(rest: LispValue): LispValue {
        if (!this.isNativeArith(">")) {
            return this.evalFunctionCall(Symbol(">"), rest)
        }
        match (rest) {
            case Nil => LispValue.Boolean(true)
            case Cons(c1) =>
                match (c1.cdr) {
                    case Nil => LispValue.Boolean(true)
                    case Cons(c2) =>
                        match (c2.cdr) {
                            case Nil =>
                                this.evalArithBin(c1.car, c2.car, ">")
                            case _ =>
                                this.callNativeArith(">", rest)
                        }
                    case _ => LispValue.Boolean(true)
                }
            case _ => LispValue.Boolean(true)
        }
    }

    /**
     * 特殊形式：(= n1 n2 ...) 等于
     */
    func evalEq(rest: LispValue): LispValue {
        if (!this.isNativeArith("=")) {
            return this.evalFunctionCall(Symbol("="), rest)
        }
        match (rest) {
            case Nil => LispValue.Boolean(true)
            case Cons(c1) =>
                match (c1.cdr) {
                    case Nil => LispValue.Boolean(true)
                    case Cons(c2) =>
                        match (c2.cdr) {
                            case Nil =>
                                this.evalArithBin(c1.car, c2.car, "=")
                            case _ =>
                                this.callNativeArith("=", rest)
                        }
                    case _ => LispValue.Boolean(true)
                }
            case _ => LispValue.Boolean(true)
        }
    }

    /**
     * 二元算术快速路径：先求值两个参数，
     * 均为 Int 时直接计算（零中间分配），否则回退原生函数
     */
    private func evalArithBin(e1: LispValue, e2: LispValue, opName: String): LispValue {
        let a = this.eval(e1)
        match (a) {
            case Error(_) => return a
            case _ => ()
        }
        let b = this.eval(e2)
        match (b) {
            case Error(_) => return b
            case _ => ()
        }
        match ((a, b)) {
            case (LispValue.Int(ai), LispValue.Int(bi)) =>
                match (opName) {
                    case "+" => LispValue.Int(ai + bi)
                    case "-" => LispValue.Int(ai - bi)
                    case "*" => LispValue.Int(ai * bi)
                    case "/" =>
                        if (bi == 0) { Nil } else { LispValue.Float(Float64(ai) / Float64(bi)) }
                    case "<" => LispValue.Boolean(ai < bi)
                    case ">" => LispValue.Boolean(ai > bi)
                    case _ => LispValue.Boolean(ai == bi)
                }
            case _ =>
                // 非 Int 组合，回退原生函数（复用已求值参数，避免重复求值）
                let args = ArrayList<LispValue>()
                args.add(a)
                args.add(b)
                match (this.env.lookup(opName)) {
                    case NativeFunc(f) => f(args)
                    case _ => Nil
                }
        }
    }

    /**
     * 3+ 参数算术：求值全部参数后调用原生函数
     */
    private func callNativeArith(opName: String, rest: LispValue): LispValue {
        let args = this.evalArguments(rest)
        for (arg in args) {
            match (arg) {
                case Error(_) => return arg
                case _ => ()
            }
        }
        match (this.env.lookup(opName)) {
            case NativeFunc(f) => f(args)
            case _ => Nil
        }
    }

    /**
     * 检查操作符是否仍绑定到原生函数且未被权限禁止
     * 用户重定义操作符（如 (define + ...)）时应回退到普通函数调用
     *
     * 使用版本缓存：Environment 对 + - * / < > = 的 define/set 递增
     * arithVersion，版本未变时直接返回缓存结果（O(1)），
     * 避免每次算术都递归遍历环境链。
     *
     * 权限检查（funcChecker）实时执行，不参与缓存：
     * 沙箱白名单/黑名单禁止的操作符回退到普通调用，
     * 由普通调用路径返回权限错误。
     */
    private func isNativeArith(opName: String): Bool {
        if (this.arithGuardVersion != this.env.arithVersion.value) {
            this.arithGuardVersion = this.env.arithVersion.value
            this.arithGuardCache[0] = this.checkNative("+")
            this.arithGuardCache[1] = this.checkNative("-")
            this.arithGuardCache[2] = this.checkNative("*")
            this.arithGuardCache[3] = this.checkNative("/")
            this.arithGuardCache[4] = this.checkNative("<")
            this.arithGuardCache[5] = this.checkNative(">")
            this.arithGuardCache[6] = this.checkNative("=")
        }
        let isNative = match (opName) {
            case "+" => this.arithGuardCache[0]
            case "-" => this.arithGuardCache[1]
            case "*" => this.arithGuardCache[2]
            case "/" => this.arithGuardCache[3]
            case "<" => this.arithGuardCache[4]
            case ">" => this.arithGuardCache[5]
            case _ => this.arithGuardCache[6]
        }
        // 沙箱权限检查：被禁止的操作符回退普通调用（返回权限错误）
        if (isNative) {
            if (let Some(checker) <- this.funcChecker) {
                if (!checker(opName)) {
                    return false
                }
            }
        }
        isNative
    }

    /**
     * 检查单个操作符是否绑定到原生函数
     */
    private func checkNative(opName: String): Bool {
        match (this.env.lookup(opName)) {
            case NativeFunc(_) => true
            case _ => false
        }
    }
}
