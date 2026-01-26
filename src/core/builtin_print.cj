package ystyle::xisp.core
import ystyle::xisp.types.*

/**
 * 打印和输出函数扩展
 * 包含 print, println, str 等输出函数
 */
extend BuiltinFunctions {
    static func registerPrint(env: Environment) {
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

        // (print x1 x2 ...) - 打印值（不换行，参数间用空格分隔）
        env.define("print", NativeFunc({ args =>
            for (i in 0..args.size) {
                if (i > 0) {
                    print(" ")
                }
                print(args[i].toString())
            }
            Nil
        }))

        // (println x1 x2 ...) - 打印值并换行（参数间用空格分隔）
        env.define("println", NativeFunc({ args =>
            var i = 0
            while (i < args.size) {
                if (i > 0) {
                    print(" ")
                }
                print(args[i].toString())
                i = i + 1
            }
            println("")
            Nil
        }))

        // (princ x) - 打印值（类似 Lisp，不换行）
        env.define("princ", NativeFunc({ args =>
            if (args.size > 0) {
                print(args[0].toString())
            }
            Nil
        }))

        // (newline) - 输出换行
        env.define("newline", NativeFunc({ _ =>
            println("")
            Nil
        }))

        // (display x) - 显示值（不换行）
        env.define("display", NativeFunc({ args =>
            if (args.size > 0) {
                print(args[0].toString())
            }
            Nil
        }))
    }
}
