package ystyle::xisp.core

import ystyle::xisp.types.*
import std.math.fmod
import std.math.round
import std.math.*

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
                        case _ => return Nil
                    }
                }
                LispValue.Float(result)
            } else {
                // 整数加法
                var result: Int64 = 0
                for (arg in args) {
                    match (arg) {
                        case LispValue.Int(i) => result = result + i
                        case _ => return Nil
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
                                case _ => return Nil
                            }
                        }
                        LispValue.Float(result)
                    } else {
                        // 整数减法
                        var result: Int64 = first
                        for (i in 1..args.size) {
                            match (args[i]) {
                                case LispValue.Int(iv) => result = result - iv
                                case _ => return Nil
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
                            case _ => return Nil
                        }
                    }
                    LispValue.Float(result)
                case _ => Nil
            }
        }))

        // (* n1 n2 ...) - 乘法
        env.define("*", NativeFunc({ args =>
            if (args.size == 0) {
                return LispValue.Int(1)
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
                        case _ => return Nil
                    }
                }
                LispValue.Float(result)
            } else {
                // 整数乘法
                var result: Int64 = 1
                for (arg in args) {
                    match (arg) {
                        case LispValue.Int(i) => result = result * i
                        case _ => return Nil
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
                            case _ => return Nil
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
                            case _ => return Nil
                        }
                    }
                    LispValue.Float(result)
                case _ => Nil
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
                case _ => Nil
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
                return Str("Error: round requires 1 or 2 arguments")
            }

            // 检查 digits 参数并提取值
            var digitsVal: Int64 = 0
            if (args.size == 2) {
                match (args[1]) {
                    case LispValue.Int(d) => digitsVal = d
                    case _ => return Str("Error: round digits must be an integer")
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
                case _ => Nil
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
}
