package ystyle::xisp.core
import ystyle::xisp.types.*

/**
 * 逻辑运算扩展
 * 包含 and, or, not 等逻辑运算
 */
extend BuiltinFunctions {
    static func registerLogic(env: Environment) {
        // (and x1 x2 ...) - 逻辑与
        env.define("and", NativeFunc({ args =>
            var result: LispValue = LispValue.Boolean(true)
            for (i in 0..args.size) {
                if (!args[i].isTruthy()) {
                    return LispValue.Boolean(false)
                }
                result = args[i]
            }
            result
        }))

        // (or x1 x2 ...) - 逻辑或
        env.define("or", NativeFunc({ args =>
            for (i in 0..args.size) {
                if (args[i].isTruthy()) {
                    return args[i]
                }
            }
            LispValue.Boolean(false)
        }))

        // (not x) - 逻辑非
        env.define("not", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(true)
            }
            LispValue.Boolean(!args[0].isTruthy())
        }))

        // (eq? x y) - 相等性比较（符号、字符串、整数）
        // 用于比较两个值是否相同（指针相等或值相等）
        env.define("eq?", NativeFunc({ args =>
            if (args.size != 2) {
                return Nil
            }
            let arg0 = args[0]
            let arg1 = args[1]

            // 使用嵌套的 match 表达式比较不同类型
            match (arg0) {
                case Symbol(s1) =>
                    match (arg1) {
                        case Symbol(s2) => LispValue.Boolean(s1 == s2)
                        case _ => LispValue.Boolean(false)
                    }
                case Str(s1) =>
                    match (arg1) {
                        case Str(s2) => LispValue.Boolean(s1 == s2)
                        case _ => LispValue.Boolean(false)
                    }
                case LispValue.Int(i1) =>
                    match (arg1) {
                        case LispValue.Int(i2) => LispValue.Boolean(i1 == i2)
                        case _ => LispValue.Boolean(false)
                    }
                case Nil =>
                    match (arg1) {
                        case Nil => LispValue.Boolean(true)
                        case _ => LispValue.Boolean(false)
                    }
                case Boolean(b1) =>
                    match (arg1) {
                        case Boolean(b2) => LispValue.Boolean(b1 == b2)
                        case _ => LispValue.Boolean(false)
                    }
                case _ => LispValue.Boolean(false)
            }
        }))
    }
}
