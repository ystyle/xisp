package ystyle::xisp.core.builtin
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
                    // 外部值：两边都实现 LispExternalEquatable 才谈得上相等，
                    // 否则一律不等（未实现 `equals` 的外部值本就不能作 HashMap 键）
                    case (LispValue.External(x), LispValue.External(y)) =>
                        let eq = match ((x, y)) {
                            case (a: LispExternalEquatable, b: LispExternalEquatable) => a.equals(b)
                            case _ => false
                        }
                        if (!eq) {
                            result = false
                            break
                        }
                    case (LispValue.External(_), _) | (_, LispValue.External(_)) =>
                        // 外部值与其它类型从不相等（不做隐式转换）
                        result = false
                        break
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
                    case (LispValue.External(x), LispValue.External(y)) =>
                        // 两边都实现 LispExternalComparable 才谈得上比大小；
                        // 否则**不满足**该比较（不能当成相等，否则 `<=` 会误判为真）
                        let cmp: Option<Int64> = match ((x, y)) {
                            case (a: LispExternalComparable, b: LispExternalComparable) => Some(a.compare(b))
                            case _ => None<Int64>
                        }
                        let ok = match (cmp) {
                            case Some(c) => c < 0
                            case None => false
                        }
                        if (!ok) {
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
                    case (LispValue.External(x), LispValue.External(y)) =>
                        // 两边都实现 LispExternalComparable 才谈得上比大小；
                        // 否则**不满足**该比较（不能当成相等，否则 `<=` 会误判为真）
                        let cmp: Option<Int64> = match ((x, y)) {
                            case (a: LispExternalComparable, b: LispExternalComparable) => Some(a.compare(b))
                            case _ => None<Int64>
                        }
                        let ok = match (cmp) {
                            case Some(c) => c > 0
                            case None => false
                        }
                        if (!ok) {
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
                    case (LispValue.External(x), LispValue.External(y)) =>
                        // 两边都实现 LispExternalComparable 才谈得上比大小；
                        // 否则**不满足**该比较（不能当成相等，否则 `<=` 会误判为真）
                        let cmp: Option<Int64> = match ((x, y)) {
                            case (a: LispExternalComparable, b: LispExternalComparable) => Some(a.compare(b))
                            case _ => None<Int64>
                        }
                        let ok = match (cmp) {
                            case Some(c) => c <= 0
                            case None => false
                        }
                        if (!ok) {
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
                    case (LispValue.External(x), LispValue.External(y)) =>
                        // 两边都实现 LispExternalComparable 才谈得上比大小；
                        // 否则**不满足**该比较（不能当成相等，否则 `<=` 会误判为真）
                        let cmp: Option<Int64> = match ((x, y)) {
                            case (a: LispExternalComparable, b: LispExternalComparable) => Some(a.compare(b))
                            case _ => None<Int64>
                        }
                        let ok = match (cmp) {
                            case Some(c) => c >= 0
                            case None => false
                        }
                        if (!ok) {
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

        // (!= n1 n2) - 不等于
        env.define("!=", NativeFunc({ args =>
            if (args.size < 2) {
                return LispValue.Boolean(false)
            }
            let eqFunc = env.lookup("=")
            match (eqFunc) {
                case NativeFunc(f) =>
                    let eqResult = f(args)
                    match (eqResult) {
                        case LispValue.Boolean(b) => LispValue.Boolean(!b)
                        case _ => LispValue.Boolean(true)
                    }
                case _ => LispValue.Boolean(true)
            }
        }))
    }
}
