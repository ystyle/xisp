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
    }
}
