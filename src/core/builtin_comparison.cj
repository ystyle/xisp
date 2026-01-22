package ystyle::xisp.core

/**
 * 比较运算扩展
 * 包含等于、大于、小于等比较运算
 */
extend BuiltinFunctions {
    static func registerComparison(env: Environment) {
        // (= n1 n2) - 等于
        env.define("=", NativeFunc({ args =>
            if (args.size < 2) {
                return Boolean(true)
            }

            if (let Number(first) <- args[0]) {
                var result = true
                for (i in 1..args.size) {
                    if (let Number(n) <- args[i]) {
                        if (!(first == n)) {
                            result = false
                            break
                        }
                    } else {
                        return Boolean(false)
                    }
                }
                Boolean(result)
            } else {
                Boolean(false)
            }
        }))

        // (< n1 n2 ...) - 小于
        env.define("<", NativeFunc({ args =>
            if (args.size < 2) {
                return Boolean(true)
            }

            var prev: ?Float64 = None
            var result = true

            for (i in 0..args.size) {
                if (let Number(n) <- args[i]) {
                    if (let Some(p) <- prev) {
                        if (!(p < n)) {
                            result = false
                            break
                        }
                    }
                    prev = Some(n)
                } else {
                    return Boolean(false)
                }
            }

            Boolean(result)
        }))

        // (> n1 n2 ...) - 大于
        env.define(">", NativeFunc({ args =>
            if (args.size < 2) {
                return Boolean(true)
            }

            var prev: ?Float64 = None
            var result = true

            for (i in 0..args.size) {
                if (let Number(n) <- args[i]) {
                    if (let Some(p) <- prev) {
                        if (!(p > n)) {
                            result = false
                            break
                        }
                    }
                    prev = Some(n)
                } else {
                    return Boolean(false)
                }
            }

            Boolean(result)
        }))

        // (<= n1 n2 ...) - 小于等于
        env.define("<=", NativeFunc({ args =>
            if (args.size < 2) {
                return Boolean(true)
            }

            var prev: ?Float64 = None
            var result = true

            for (i in 0..args.size) {
                if (let Number(n) <- args[i]) {
                    if (let Some(p) <- prev) {
                        if (!(p <= n)) {
                            result = false
                            break
                        }
                    }
                    prev = Some(n)
                } else {
                    return Boolean(false)
                }
            }

            Boolean(result)
        }))

        // (>= n1 n2 ...) - 大于等于
        env.define(">=", NativeFunc({ args =>
            if (args.size < 2) {
                return Boolean(true)
            }

            var prev: ?Float64 = None
            var result = true

            for (i in 0..args.size) {
                if (let Number(n) <- args[i]) {
                    if (let Some(p) <- prev) {
                        if (!(p >= n)) {
                            result = false
                            break
                        }
                    }
                    prev = Some(n)
                } else {
                    return Boolean(false)
                }
            }

            Boolean(result)
        }))
    }
}
