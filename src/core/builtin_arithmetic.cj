package ystyle::xisp.core

import ystyle::xisp.types.*
import std.math.fmod

/**
 * 算术运算扩展
 * 包含加、减、乘、除、取模等算术运算
 */
extend BuiltinFunctions {
    /**
     * 算术运算函数
     */
    static func registerArithmetic(env: Environment) {
        // (+ n1 n2 ...) - 加法
        env.define("+", NativeFunc({ args =>
            var result = 0.0
            for (i in 0..args.size) {
                if (let Number(n) <- args[i]) {
                    result = result + n
                } else {
                    return Nil
                }
            }
            Number(result)
        }))

        // (- n1 n2 ...) - 减法
        env.define("-", NativeFunc({ args =>
            if (args.size == 0) {
                return Nil
            }

            if (let Number(first) <- args[0]) {
                if (args.size == 1) {
                    Number(-first)
                } else {
                    var result = first
                    for (i in 1..args.size) {
                        if (let Number(n) <- args[i]) {
                            result = result - n
                        } else {
                            return Nil
                        }
                    }
                    Number(result)
                }
            } else {
                Nil
            }
        }))

        // (* n1 n2 ...) - 乘法
        env.define("*", NativeFunc({ args =>
            var result = 1.0
            for (i in 0..args.size) {
                if (let Number(n) <- args[i]) {
                    result = result * n
                } else {
                    return Nil
                }
            }
            Number(result)
        }))

        // (/ n1 n2 ...) - 除法
        env.define("/", NativeFunc({ args =>
            if (args.size < 2) {
                return Nil
            }

            if (let Number(first) <- args[0]) {
                var result = first
                for (i in 1..args.size) {
                    if (let Number(n) <- args[i]) {
                        if (n == 0.0) {
                            return Nil  // 除零错误
                        }
                        result = result / n
                    } else {
                        return Nil
                    }
                }
                Number(result)
            } else {
                Nil
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
                case (Number(n1), Number(n2)) =>
                    if (n2 == 0.0) {
                        return Nil
                    }
                    Number(fmod(n1, n2))
                case _ => Nil
            }
        }))
    }
}
