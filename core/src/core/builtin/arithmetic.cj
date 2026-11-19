package ystyle::xisp.core.builtin

import ystyle::xisp.types.*
import std.math.fmod
import std.math.round
import std.math.*
import std.collection.ArrayList

/**
 * 算术运算扩展
 * 包含加、减、乘、除、取模等算术运算
 * 支持整数和浮点数的区分和类型提升
 */
extend BuiltinFunctions {
    /**
     * 算术运算函数
     *
     * 类型提升规则：
     * - 如果所有操作数都是整数，结果也是整数
     * - 如果有任何操作数是浮点数，结果就是浮点数
     * - 除法总是返回浮点数（避免整数除法）
     */
    static func registerArithmetic(env: Environment) {
        // (+ n1 n2 ...) - 加法
        env.define("+", NativeFunc({ args =>
            if (args.size == 0) {
                return LispValue.Int(0)
            }

            // 外部值（数组等）自己决定怎么参与运算；不支持的组合仍走下面的类型错误
            if (let Some(v) <- BuiltinFunctions.dispatchArith("+", args)) {
                return v
            }

            // 检查是否有浮点数
            var hasFloat = false
            for (arg in args) {
                match (arg) {
                    case LispValue.Float(_) => hasFloat = true
                    case _ => ()
                }
            }

            if (hasFloat) {
                // 浮点数加法
                var result: Float64 = 0.0
                for (arg in args) {
                    match (arg) {
                        case LispValue.Int(i) => result = result + Float64(i)
                        case LispValue.Float(f) => result = result + f
                        case other => return typeMismatch("+", other)
                    }
                }
                LispValue.Float(result)
            } else {
                // 整数加法
                var result: Int64 = 0
                for (arg in args) {
                    match (arg) {
                        case LispValue.Int(i) => result = result + i
                        case other => return typeMismatch("+", other)
                    }
                }
                LispValue.Int(result)
            }
        }))

        // (- n1 n2 ...) - 减法
        env.define("-", NativeFunc({ args =>
            if (args.size == 0) {
                return Nil
            }

            // 一元 `(- x)` 由 dispatchArith 处理成 `0 - x`（取负）
            if (let Some(v) <- BuiltinFunctions.dispatchArith("-", args)) {
                return v
            }

            // 检查是否有浮点数
            var hasFloat = false
            for (arg in args) {
                match (arg) {
                    case LispValue.Float(_) => hasFloat = true
                    case _ => ()
                }
            }

            match (args[0]) {
                case LispValue.Int(first) =>
                    if (args.size == 1) {
                        LispValue.Int(-first)
                    } else if (hasFloat) {
                        // 浮点数减法
                        var result: Float64 = Float64(first)
                        for (i in 1..args.size) {
                            match (args[i]) {
                                case LispValue.Int(iv) => result = result - Float64(iv)
                                case LispValue.Float(f) => result = result - f
                                case other => return typeMismatch("-", other)
                            }
                        }
                        LispValue.Float(result)
                    } else {
                        // 整数减法
                        var result: Int64 = first
                        for (i in 1..args.size) {
                            match (args[i]) {
                                case LispValue.Int(iv) => result = result - iv
                                case other => return typeMismatch("-", other)
                            }
                        }
                        LispValue.Int(result)
                    }
                case LispValue.Float(first) =>
                    var result: Float64 = first
                    for (i in 1..args.size) {
                        match (args[i]) {
                            case LispValue.Int(iv) => result = result - Float64(iv)
                            case LispValue.Float(f) => result = result - f
                            case other => return typeMismatch("-", other)
                        }
                    }
                    LispValue.Float(result)
                case other => typeMismatch("-", other)
            }
        }))

        // (* n1 n2 ...) - 乘法
        env.define("*", NativeFunc({ args =>
            if (args.size == 0) {
                return LispValue.Int(1)
            }

            if (let Some(v) <- BuiltinFunctions.dispatchArith("*", args)) {
                return v
            }

            // 检查是否有浮点数
            var hasFloat = false
            for (arg in args) {
                match (arg) {
                    case LispValue.Float(_) => hasFloat = true
                    case _ => ()
                }
            }

            if (hasFloat) {
                // 浮点数乘法
                var result: Float64 = 1.0
                for (arg in args) {
                    match (arg) {
                        case LispValue.Int(i) => result = result * Float64(i)
                        case LispValue.Float(f) => result = result * f
                        case other => return typeMismatch("*", other)
                    }
                }
                LispValue.Float(result)
            } else {
                // 整数乘法
                var result: Int64 = 1
                for (arg in args) {
                    match (arg) {
                        case LispValue.Int(i) => result = result * i
                        case other => return typeMismatch("*", other)
                    }
                }
                LispValue.Int(result)
            }
        }))

        // (/ n1 n2 ...) - 除法（总是返回浮点数）
        env.define("/", NativeFunc({ args =>
            if (args.size < 2) {
                return Nil
            }

            if (let Some(v) <- BuiltinFunctions.dispatchArith("/", args)) {
                return v
            }

            match (args[0]) {
                case LispValue.Int(first) =>
                    var result: Float64 = Float64(first)
                    for (i in 1..args.size) {
                        match (args[i]) {
                            case LispValue.Int(iv) =>
                                if (iv == 0) {
                                    return Nil  // 除零错误
                                }
                                result = result / Float64(iv)
                            case LispValue.Float(f) =>
                                if (f == 0.0) {
                                    return Nil  // 除零错误
                                }
                                result = result / f
                            case other => return typeMismatch("/", other)
                        }
                    }
                    LispValue.Float(result)
                case LispValue.Float(first) =>
                    var result: Float64 = first
                    for (i in 1..args.size) {
                        match (args[i]) {
                            case LispValue.Int(iv) =>
                                if (iv == 0) {
                                    return Nil
                                }
                                result = result / Float64(iv)
                            case LispValue.Float(f) =>
                                if (f == 0.0) {
                                    return Nil
                                }
                                result = result / f
                            case other => return typeMismatch("/", other)
                        }
                    }
                    LispValue.Float(result)
                case other => typeMismatch("/", other)
            }
        }))
    }

    /**
     * 注册取模运算函数
     */
    static func registerModulo(env: Environment) {
        // (mod n1 n2) - 取模
        env.define("mod", NativeFunc({ args =>
            if (args.size != 2) {
                return Nil
            }

            match ((args[0], args[1])) {
                case (LispValue.Int(i1), LispValue.Int(i2)) =>
                    if (i2 == 0) {
                        return Nil
                    }
                    LispValue.Int(i1 % i2)
                case (LispValue.Int(i1), LispValue.Float(f2)) =>
                    if (f2 == 0.0) {
                        return Nil
                    }
                    LispValue.Float(fmod(Float64(i1), f2))
                case (LispValue.Float(f1), LispValue.Int(i2)) =>
                    if (i2 == 0) {
                        return Nil
                    }
                    LispValue.Float(fmod(f1, Float64(i2)))
                case (LispValue.Float(f1), LispValue.Float(f2)) =>
                    if (f2 == 0.0) {
                        return Nil
                    }
                    LispValue.Float(fmod(f1, f2))
                case (a, b) => typeMismatch("mod", a)
            }
        }))
    }

    /**
     * 注册数学函数
     */
    static func registerMath(env: Environment) {
        // (round n [digits]) - 四舍五入
        // digits: 保留的小数位数，默认为0
        env.define("round", NativeFunc({ args =>
            if (args.size < 1 || args.size > 2) {
                return LispValue.Error(XispError(ErrorType.ArityMismatch, "round requires 1 or 2 arguments"))
            }

            // 检查 digits 参数并提取值
            var digitsVal: Int64 = 0
            if (args.size == 2) {
                match (args[1]) {
                    case LispValue.Int(d) => digitsVal = d
                    case _ => return LispValue.Error(XispError(ErrorType.TypeError, "round digits must be an integer"))
                }
            }

            match (args[0]) {
                case LispValue.Int(i) =>
                    if (digitsVal == 0) {
                        LispValue.Int(i)
                    } else {
                        let f = Float64(i)
                        let multiplier = BuiltinFunctions.pow10Static(Float64(digitsVal))
                        LispValue.Float(round(f * multiplier) / multiplier)
                    }
                case LispValue.Float(f) =>
                    let multiplier = BuiltinFunctions.pow10Static(Float64(digitsVal))
                    LispValue.Float(round(f * multiplier) / multiplier)
                case other => typeMismatch("round", other)
            }
        }))

        // ===== 标量数学函数 =====
        //
        // 这些是**语言能力**而不是科学计算专属：`(sin 1.0)` 与 `(sin arr)` 是同一个
        // 符号，外部值靠 `LispExternalElementwise` 拿到逐元素版本，不搞两套名字。
        //
        // 结果恒为浮点 —— 开方、对数、三角的结果不保证落在整数上，
        // 整型输入先转 `Float64` 是有意的（`(sqrt 4)` 是 `2.0` 而不是 `2`）。

        defineFloatUnary(env, "sin", { x => sin(x) })
        defineFloatUnary(env, "cos", { x => cos(x) })
        defineFloatUnary(env, "tan", { x => tan(x) })
        defineFloatUnary(env, "asin", { x => asin(x) })
        defineFloatUnary(env, "acos", { x => acos(x) })
        defineFloatUnary(env, "atan", { x => atan(x) })
        defineFloatUnary(env, "exp", { x => exp(x) })
        // 对数在 0 处的极限是 -inf 而不是 nan：`(log 0)` 得到 -inf 才能让
        // 后续的 `(< (log x) 0)` 之类判断照常工作。负数是 nan。
        defineFloatUnary(env, "log", { x =>
            if (x == 0.0) { -Float64.getInf() } else if (x < 0.0) { Float64.getNaN() } else { log(x) }
        })
        defineFloatUnary(env, "log10", { x =>
            if (x == 0.0) { -Float64.getInf() } else if (x < 0.0) { Float64.getNaN() } else { log10(x) }
        })
        defineFloatUnary(env, "sqrt", { x => sqrt(x) })
        defineFloatUnary(env, "cbrt", { x => cbrt(x) })

        defineFloatBinary(env, "pow", { a, b => pow(a, b) })
        defineFloatBinary(env, "hypot", { a, b => safeHypot(a, b) })

        // (abs x) —— **保留整数性**：`(abs -3)` 是 `3` 而不是 `3.0`。
        // 这条与上面那些"恒浮点"的函数故意不同：取绝对值不改变量纲。
        env.define("abs", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Error(XispError(ErrorType.ArityMismatch,
                    "abs requires exactly 1 argument"))
            }
            match (args[0]) {
                case LispValue.Int(i) => LispValue.Int(if (i < 0) { -i } else { i })
                case LispValue.Float(x) => LispValue.Float(abs(x))
                case other => applyElementwiseTo("abs", other)
            }
        }))

        // (floor x) / (ceil x) → **整数**：这两个的主要用途是取下标与计数，
        // 返回浮点会让 `(nth a (floor 3.7))` 直接类型错。
        env.define("floor", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Error(XispError(ErrorType.ArityMismatch,
                    "floor requires exactly 1 argument"))
            }
            match (args[0]) {
                case LispValue.Int(i) => LispValue.Int(i)
                case LispValue.Float(x) => LispValue.Int(Int64(floor(x)))
                case other => applyElementwiseTo("floor", other)
            }
        }))

        env.define("ceil", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Error(XispError(ErrorType.ArityMismatch,
                    "ceil requires exactly 1 argument"))
            }
            match (args[0]) {
                case LispValue.Int(i) => LispValue.Int(i)
                case LispValue.Float(x) => LispValue.Int(Int64(ceil(x)))
                case other => applyElementwiseTo("ceil", other)
            }
        }))

        // (sign x) —— 符号函数，保留入参的量纲：`(sign -3)` 是 `-1`，`(sign -3.0)` 是 `-1.0`
        env.define("sign", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Error(XispError(ErrorType.ArityMismatch,
                    "sign requires exactly 1 argument"))
            }
            match (args[0]) {
                case LispValue.Int(i) =>
                    LispValue.Int(if (i > 0) { 1 } else if (i < 0) { -1 } else { 0 })
                case LispValue.Float(x) =>
                    LispValue.Float(if (x > 0.0) { 1.0 } else if (x < 0.0) { -1.0 } else { 0.0 })
                case other => applyElementwiseTo("sign", other)
            }
        }))
    }

    /**
     * 10的幂次方静态辅助函数
     */
    public static func pow10Static(n: Float64): Float64 {
        if (n == 0.0) {
            return 1.0
        } else if (n == 1.0) {
            return 10.0
        } else if (n == 2.0) {
            return 100.0
        } else if (n == 3.0) {
            return 1000.0
        } else {
            var result = 1.0
            var i = 0
            while (i < Int64(n)) {
                result = result * 10.0
                i = i + 1
            }
            result
        }
    }

    /**
     * 统一的算术类型错误
     *
     * 此前这些位置 `return Nil` —— **静默吞掉类型错误**，而字节码后端会报
     * `TypeError`。同一段脚本在两个后端下一个悄悄算错、一个明确报错，
     * 属实测确认的不兼容。改为与后端一致的明确错误。
     */
    static func typeMismatch(op: String, value: LispValue): LispValue {
        LispValue.Error(XispError(ErrorType.TypeError,
            "${op}: expected a number (got: ${value.toString()})"))
    }

    // ===== 外部值分派 =====

    /** 取出可作为算术操作数的外部值 */
    public static func asArith(v: LispValue): Option<LispExternalArithmetic> {
        match (v) {
            case LispValue.External(e) =>
                match (e) {
                    case a: LispExternalArithmetic => Some(a)
                    case _ => None
                }
            case _ => None
        }
    }

    /**
     * 四则运算的外部值分派
     *
     * 规则（按顺序试）：
     *
     * 1. **首参是可算外部值** → 从左往右折叠。`(+ arr1 arr2 arr3)` 是
     *    `((arr1 + arr2) + arr3)`，每一步的中间结果仍须是可算外部值；
     * 2. **一元 `(- x)`** → 交给 `arithRight("-", 0)` 当作 `0 - x`（取负）。
     *    其它运算符的一元形式没有意义，直接不认；
     * 3. **只有两个参数且第二个可算** → `arithRight`，覆盖 `(+ 1 arr)` 这种
     *    外部值在右边的写法。减法/除法不满足交换律，所以必须走"作为右操作数"
     *    而不是把两个参数对调。
     *
     * 返回 `None` 表示"这次分派不适用"，调用方照常报类型错误 ——
     * 静默返回 `nil` 会让 `(+ arr "x")` 看起来算成功了。
     */
    public static func dispatchArith(op: String, args: ArrayList<LispValue>): Option<LispValue> {
        if (args.size == 0) {
            return None
        }
        match (asArith(args[0])) {
            case Some(first) =>
                if (args.size == 1) {
                    // 只有 `(- x)` 有外部值语义（取负）；`(+ x)` 是恒等，其它无意义
                    if (op == "-") {
                        return first.arithRight("-", LispValue.Int(0))
                    }
                    return None
                }
                var acc: LispValue = args[0]
                var i: Int64 = 1
                while (i < args.size) {
                    match (asArith(acc)) {
                        case Some(w) =>
                            match (w.arithLeft(op, args[i])) {
                                case Some(v) => acc = v
                                case None => return None
                            }
                        case None => return None
                    }
                    i++
                }
                Some(acc)
            case None =>
                if (args.size == 2) {
                    match (asArith(args[1])) {
                        case Some(second) => second.arithRight(op, args[0])
                        case None => None
                    }
                } else {
                    None
                }
        }
    }

    /**
     * 归约的外部值分派（`sum` / `product` / `max` / `min`）
     *
     * 不复用 `LispExternalIndexable` 逐元素取值：那会把每个元素装箱成 `LispValue`，
     * 一百万次求和就是一百万次装箱。归约由实现方在自己的连续存储上直接做。
     */
    static func reduceExternalOr(op: String, value: LispValue): Option<LispValue> {
        match (value) {
            case LispValue.External(e) =>
                match (e) {
                    case r: LispExternalReducible =>
                        match (r.reduceAll(op)) {
                            case Some(v) => Some(v)
                            // 是外部值却拒绝该归约（典型：空数组求 max）→ 必须报错。
                            // 静默返回 None 会落到下面的数值遍历，最后得到 nil 或 0，
                            // 那是"看着像结果"的错。
                            case None => Some(LispValue.Error(XispError(ErrorType.TypeError,
                                "${op}: 外部值不支持此归约（空数组？）")))
                        }
                    case _ => Some(LispValue.Error(XispError(ErrorType.TypeError,
                        "${op}: 不支持外部值 ${e.typeName}")))
                }
            case _ => None
        }
    }

    // ===== 标量数学 =====
    /**
     * 参数不是数值时的统一出口：能逐元素分派就分派，否则报类型错误
     *
     * 这是 `(sin 1.0)` 与 `(sin arr)` 同名可用的关键 —— `sin` 只有一个符号，
     * 标量走数值分支，外部值走 `LispExternalElementwise`。
     */
    static func applyElementwiseTo(fn: String, value: LispValue): LispValue {
        match (value) {
            case LispValue.External(e) =>
                match (e) {
                    case w: LispExternalElementwise =>
                        match (w.applyElementwise(fn)) {
                            case Some(v) => v
                            case None => typeMismatch(fn, value)
                        }
                    case _ => typeMismatch(fn, value)
                }
            case _ => typeMismatch(fn, value)
        }
    }

    /**
     * `hypot(a, b)` = √(a² + b²)，且**不会中间溢出**
     *
     * 直接写 `sqrt(a*a + b*b)` 在 `a = 1e200` 时会得到 `inf`（平方已经溢出），
     * 而正确答案是 `1.414e200`。先除以较大的绝对值再乘回去就没有这个问题。
     * 仓颉 std.math 没有 `hypot`，所以自己实现。
     */
    static func safeHypot(a: Float64, b: Float64): Float64 {
        let aa = abs(a)
        let ab = abs(b)
        let m = if (aa > ab) { aa } else { ab }
        if (m == 0.0) {
            return 0.0
        }
        if (m.isInf()) {
            return Float64.getInf()
        }
        let p = a / m
        let q = b / m
        m * sqrt(p * p + q * q)
    }

    /**
     * 注册一元数学函数（结果恒为浮点）
     *
     * 整型输入先转 `Float64`：`(sqrt 4)` 得到 `2.0` 而不是 `2`，
     * 因为开方、对数、三角函数的结果不保证落在整数上。
     */
    static func defineFloatUnary(env: Environment, name: String, f: (Float64) -> Float64): Unit {
        env.define(name, NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Error(XispError(ErrorType.ArityMismatch,
                    "${name} requires exactly 1 argument"))
            }
            match (args[0]) {
                case LispValue.Int(i) => LispValue.Float(safeApplyFloat(f, Float64(i)))
                case LispValue.Float(x) => LispValue.Float(safeApplyFloat(f, x))
                case other => applyElementwiseTo(name, other)
            }
        }))
    }

    /**
     * 调用一元数学函数，定义域外返回 `nan` 而不是抛异常
     *
     * 仓颉 `std.math` 的 `sqrt` / `asin` 等对定义域外输入抛
     * `IllegalArgumentException`，而数值计算的惯例是让 `nan` 在算式里传播：
     * 一批数据里只要有一个非法值，抛异常会让整批计算中断；`nan` 能让用户看到
     * "这里坏了"并继续处理其余部分。判空由调用方做。
     */
    static func safeApplyFloat(f: (Float64) -> Float64, x: Float64): Float64 {
        try {
            f(x)
        } catch (e: Exception) {
            Float64.getNaN()
        }
    }

    /**
     * 注册二元数学函数（结果恒为浮点）
     *
     * 二元函数**不做**外部值分派：`LispExternalElementwise` 的契约定成一元的，
     * 为了 `pow` 单独开个二元分派不值得。数组要幂运算直接写 `(* a a)`。
     */
    static func defineFloatBinary(env: Environment, name: String,
                                  f: (Float64, Float64) -> Float64): Unit {
        env.define(name, NativeFunc({ args =>
            if (args.size != 2) {
                return LispValue.Error(XispError(ErrorType.ArityMismatch,
                    "${name} requires exactly 2 arguments"))
            }
            let a = match (args[0]) {
                case LispValue.Int(i) => Float64(i)
                case LispValue.Float(x) => x
                case other => return typeMismatch(name, other)
            }
            let b = match (args[1]) {
                case LispValue.Int(i) => Float64(i)
                case LispValue.Float(x) => x
                case other => return typeMismatch(name, other)
            }
            LispValue.Float(f(a, b))
        }))
    }
}