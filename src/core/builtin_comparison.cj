package ystyle::xisp.core
import ystyle::xisp.types.*

/**
 * 比较运算扩展
 * 包含等于、大于、小于等比较运算
 * 支持整数和浮点数的比较
 */
extend BuiltinFunctions {
    static func registerComparison(env: Environment) {
        // (= n1 n2) - 等于
        env.define("=", NativeFunc({ args =>
            if (args.size < 2) {
                return LispValue.Boolean(true)
            }

            // 获取第一个值
            let first = args[0]
            var result = true

            for (i in 1..args.size) {
                let current = args[i]

                // 检查类型和值是否相等
                match ((first, current)) {
                    case (LispValue.Int(a), LispValue.Int(b)) =>
                        if (a != b) {
                            result = false
                            break
                        }
                    case (LispValue.Float(a), LispValue.Float(b)) =>
                        if (a != b) {
                            result = false
                            break
                        }
                    case (LispValue.Int(a), LispValue.Float(b)) =>
                        if (Float64(a) != b) {
                            result = false
                            break
                        }
                    case (LispValue.Float(a), LispValue.Int(b)) =>
                        if (a != Float64(b)) {
                            result = false
                            break
                        }
                    case _ =>
                        // 类型不同，不相等
                        result = false
                        break
                }
            }

            LispValue.Boolean(result)
        }))

        // (< n1 n2 ...) - 小于
        env.define("<", NativeFunc({ args =>
            if (args.size < 2) {
                return LispValue.Boolean(true)
            }

            var result = true
            var prevValue = args[0]

            for (i in 1..args.size) {
                let currentValue = args[i]

                match ((prevValue, currentValue)) {
                    case (LispValue.Int(a), LispValue.Int(b)) =>
                        if (!(a < b)) {
                            result = false
                            break
                        }
                    case (LispValue.Float(a), LispValue.Float(b)) =>
                        if (!(a < b)) {
                            result = false
                            break
                        }
                    case (LispValue.Int(a), LispValue.Float(b)) =>
                        if (!(Float64(a) < b)) {
                            result = false
                            break
                        }
                    case (LispValue.Float(a), LispValue.Int(b)) =>
                        if (!(a < Float64(b))) {
                            result = false
                            break
                        }
                    case _ =>
                        result = false
                        break
                }

                prevValue = currentValue
            }

            LispValue.Boolean(result)
        }))

        // (> n1 n2 ...) - 大于
        env.define(">", NativeFunc({ args =>
            if (args.size < 2) {
                return LispValue.Boolean(true)
            }

            var result = true
            var prevValue = args[0]

            for (i in 1..args.size) {
                let currentValue = args[i]

                match ((prevValue, currentValue)) {
                    case (LispValue.Int(a), LispValue.Int(b)) =>
                        if (!(a > b)) {
                            result = false
                            break
                        }
                    case (LispValue.Float(a), LispValue.Float(b)) =>
                        if (!(a > b)) {
                            result = false
                            break
                        }
                    case (LispValue.Int(a), LispValue.Float(b)) =>
                        if (!(Float64(a) > b)) {
                            result = false
                            break
                        }
                    case (LispValue.Float(a), LispValue.Int(b)) =>
                        if (!(a > Float64(b))) {
                            result = false
                            break
                        }
                    case _ =>
                        result = false
                        break
                }

                prevValue = currentValue
            }

            LispValue.Boolean(result)
        }))

        // (<= n1 n2 ...) - 小于等于
        env.define("<=", NativeFunc({ args =>
            if (args.size < 2) {
                return LispValue.Boolean(true)
            }

            var result = true
            var prevValue = args[0]

            for (i in 1..args.size) {
                let currentValue = args[i]

                match ((prevValue, currentValue)) {
                    case (LispValue.Int(a), LispValue.Int(b)) =>
                        if (!(a <= b)) {
                            result = false
                            break
                        }
                    case (LispValue.Float(a), LispValue.Float(b)) =>
                        if (!(a <= b)) {
                            result = false
                            break
                        }
                    case (LispValue.Int(a), LispValue.Float(b)) =>
                        if (!(Float64(a) <= b)) {
                            result = false
                            break
                        }
                    case (LispValue.Float(a), LispValue.Int(b)) =>
                        if (!(a <= Float64(b))) {
                            result = false
                            break
                        }
                    case _ =>
                        result = false
                        break
                }

                prevValue = currentValue
            }

            LispValue.Boolean(result)
        }))

        // (>= n1 n2 ...) - 大于等于
        env.define(">=", NativeFunc({ args =>
            if (args.size < 2) {
                return LispValue.Boolean(true)
            }

            var result = true
            var prevValue = args[0]

            for (i in 1..args.size) {
                let currentValue = args[i]

                match ((prevValue, currentValue)) {
                    case (LispValue.Int(a), LispValue.Int(b)) =>
                        if (!(a >= b)) {
                            result = false
                            break
                        }
                    case (LispValue.Float(a), LispValue.Float(b)) =>
                        if (!(a >= b)) {
                            result = false
                            break
                        }
                    case (LispValue.Int(a), LispValue.Float(b)) =>
                        if (!(Float64(a) >= b)) {
                            result = false
                            break
                        }
                    case (LispValue.Float(a), LispValue.Int(b)) =>
                        if (!(a >= Float64(b))) {
                            result = false
                            break
                        }
                    case _ =>
                        result = false
                        break
                }

                prevValue = currentValue
            }

            LispValue.Boolean(result)
        }))
    }
}
