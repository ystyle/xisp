package ystyle::xisp.core.builtin
import ystyle::xisp.types.*

/**
 * 字符串与输出函数扩展
 *
 * 字符串函数（str / string=? / string< / string> / string-contains?）是纯计算，
 * 与输出函数分开注册：纯函数子集需要前者，而后者允许宿主接管输出通道。
 */
extend BuiltinFunctions {
    /**
     * 注册纯字符串函数：只做计算，不产生输出。
     */
    static func registerStringOps(env: Environment) {
        // (string=? str1 str2) - 字符串相等比较
        env.define("string=?", NativeFunc({ args =>
            if (args.size != 2) {
                return LispValue.Boolean(false)
            }
            let arg0 = args[0]
            let arg1 = args[1]
            if (let Str(s1) <- arg0) {
                if (let Str(s2) <- arg1) {
                    LispValue.Boolean(s1 == s2)
                } else {
                    LispValue.Boolean(false)
                }
            } else {
                LispValue.Boolean(false)
            }
        }))

        // (string< str1 str2) - 字符串小于比较
        env.define("string<", NativeFunc({ args =>
            if (args.size != 2) {
                return Nil
            }
            let arg0 = args[0]
            let arg1 = args[1]
            if (let Str(s1) <- arg0) {
                if (let Str(s2) <- arg1) {
                    LispValue.Boolean(s1 < s2)
                } else {
                    Nil
                }
            } else {
                Nil
            }
        }))

        // (string> str1 str2) - 字符串大于比较
        env.define("string>", NativeFunc({ args =>
            if (args.size != 2) {
                return Nil
            }
            let arg0 = args[0]
            let arg1 = args[1]
            if (let Str(s1) <- arg0) {
                if (let Str(s2) <- arg1) {
                    LispValue.Boolean(s1 > s2)
                } else {
                    Nil
                }
            } else {
                Nil
            }
        }))

        // (string-contains? str substr) - 字符串包含子串判断
        env.define("string-contains?", NativeFunc({ args =>
            if (args.size != 2) {
                return LispValue.Boolean(false)
            }
            let arg0 = args[0]
            let arg1 = args[1]
            if (let Str(s1) <- arg0) {
                if (let Str(s2) <- arg1) {
                    LispValue.Boolean(s1.contains(s2))
                } else {
                    LispValue.Boolean(false)
                }
            } else {
                LispValue.Boolean(false)
            }
        }))

        // (str x1 x2 ...) - 字符串拼接（将所有参数转换为字符串并拼接）
        env.define("str", NativeFunc({ args =>
            let sb = StringBuilder()
            for (i in 0..args.size) {
                match (args[i]) {
                    case Str(s) => sb.append(s)  // 字符串直接使用内容，不加引号
                    case _ => sb.append(args[i].toString())
                }
            }
            Str(sb.toString())
        }))
    }

    /**
     * 注册输出函数，全部写入 output。
     *
     * 每个片段一次回调；换行以 "\n" 片段表达，不额外插入其它字符。
     * 供宿主把脚本输出接到自己的日志/渲染通道，而不是进程 stdout。
     *
     * @param output 输出接收器
     */
    static func registerOutput(env: Environment, output: (String) -> Unit) {
        // (print x1 x2 ...) - 打印值（不换行，参数间用空格分隔）
        env.define("print", NativeFunc({ args =>
            for (i in 0..args.size) {
                if (i > 0) {
                    output(" ")
                }
                output(args[i].toString())
            }
            Nil
        }))

        // (println x1 x2 ...) - 打印值并换行（参数间用空格分隔）
        env.define("println", NativeFunc({ args =>
            var i = 0
            while (i < args.size) {
                if (i > 0) {
                    output(" ")
                }
                output(args[i].toString())
                i = i + 1
            }
            output("\n")
            Nil
        }))

        // (princ x) - 打印值（类似 Lisp，不换行）
        env.define("princ", NativeFunc({ args =>
            if (args.size > 0) {
                output(args[0].toString())
            }
            Nil
        }))

        // (newline) - 输出换行
        env.define("newline", NativeFunc({ _ =>
            output("\n")
            Nil
        }))

        // (display x) - 显示值（不换行）
        env.define("display", NativeFunc({ args =>
            if (args.size > 0) {
                output(args[0].toString())
            }
            Nil
        }))
    }

    /**
     * 注册字符串与输出函数（输出到进程标准输出）。
     */
    static func registerPrint(env: Environment) {
        registerStringOps(env)
        registerOutput(env, { s => print(s) })
    }
}
