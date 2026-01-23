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
            var result = Boolean(true)
            for (i in 0..args.size) {
                if (!args[i].isTruthy()) {
                    return Boolean(false)
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
            Boolean(false)
        }))

        // (not x) - 逻辑非
        env.define("not", NativeFunc({ args =>
            if (args.size != 1) {
                return Boolean(true)
            }
            Boolean(!args[0].isTruthy())
        }))
    }
}
