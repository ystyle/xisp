package ystyle::xisp.core
import ystyle::xisp.types.*

/**
 * 谓词函数扩展
 * 包含类型检查谓词函数
 */
extend BuiltinFunctions {
    static func registerPredicates(env: Environment) {
        // (integer? x) - 是否为整数
        env.define("integer?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            LispValue.Boolean(args[0].isInteger())
        }))

        // (float? x) - 是否为浮点数
        env.define("float?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            LispValue.Boolean(args[0].isFloat())
        }))

        // (number? x) - 是否为数字（整数或浮点数）
        env.define("number?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            LispValue.Boolean(args[0].isNumber())
        }))

        // (string? x) - 是否为字符串
        env.define("string?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            LispValue.Boolean(args[0].isString())
        }))

        // (symbol? x) - 是否为符号
        env.define("symbol?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            LispValue.Boolean(args[0].isSymbol())
        }))

        // (list? x) - 是否为列表
        env.define("list?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            LispValue.Boolean(args[0].isList())
        }))

        // (null? x) - 是否为空
        env.define("null?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            LispValue.Boolean(args[0].isNil())
        }))

        // (procedure? x) - 是否为过程
        env.define("procedure?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            LispValue.Boolean(args[0].isProcedure())
        }))

        // (zero? x) - 是否为 0
        env.define("zero?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            match (args[0]) {
                case LispValue.Int(n) => LispValue.Boolean(n == 0)
                case LispValue.Float(n) => LispValue.Boolean(n == 0.0)
                case _ => LispValue.Boolean(false)
            }
        }))

        // (positive? x) - 是否为正数（大于 0）
        env.define("positive?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            match (args[0]) {
                case LispValue.Int(n) => LispValue.Boolean(n > 0)
                case LispValue.Float(n) => LispValue.Boolean(n > 0.0)
                case _ => LispValue.Boolean(false)
            }
        }))

        // (negative? x) - 是否为负数（小于 0）
        env.define("negative?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            match (args[0]) {
                case LispValue.Int(n) => LispValue.Boolean(n < 0)
                case LispValue.Float(n) => LispValue.Boolean(n < 0.0)
                case _ => LispValue.Boolean(false)
            }
        }))

        // (even? x) - 是否为偶数
        env.define("even?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            match (args[0]) {
                case LispValue.Int(n) => LispValue.Boolean(n % 2 == 0)
                case _ => LispValue.Boolean(false)
            }
        }))

        // (odd? x) - 是否为奇数
        env.define("odd?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            match (args[0]) {
                case LispValue.Int(n) => LispValue.Boolean(n % 2 != 0)
                case _ => LispValue.Boolean(false)
            }
        }))

        // (type-of x) - 返回值的类型名称
        env.define("type-of", NativeFunc({ args =>
            if (args.size != 1) {
                return Str("error")
            }
            match (args[0]) {
                case Nil => Str("nil")
                case Boolean(_) => Str("boolean")
                case LispValue.Int(_) => Str("integer")
                case LispValue.Float(_) => Str("float")
                case Str(_) => Str("string")
                case Symbol(_) => Str("symbol")
                case Cons(_) => Str("list")
                case HashMap(_) => Str("hashmap")
                case NativeFunc(_) => Str("procedure")
                case Procedure(_, _, _) => Str("procedure")
                case Macro(_, _, _) => Str("macro")
                case _ => Str("unknown")
            }
        }))
    }
}
