package ystyle::xisp.core

/**
 * 现代化别名扩展
 * 为传统 Lisp 函数提供现代化的别名
 */
extend BuiltinFunctions {
    static func registerModernAliases(env: Environment) {
        // (first lst) - car 的现代别名
        let carFunc = env.lookup("car")
        env.define("first", carFunc)

        // (rest lst) - cdr 的现代别名
        let cdrFunc = env.lookup("cdr")
        env.define("rest", cdrFunc)

        // (second lst) - 获取第二个元素 (cadr 的现代别名)
        env.define("second", NativeFunc({ args =>
            if (args.size != 1) {
                return Nil
            }
            if (let Cons(cell1) <- args[0]) {
                if (let Cons(cell2) <- cell1.cdr) {
                    cell2.car
                } else {
                    Nil
                }
            } else {
                Nil
            }
        }))

        // (third lst) - 获取第三个元素 (caddr 的现代别名)
        env.define("third", NativeFunc({ args =>
            if (args.size != 1) {
                return Nil
            }
            if (let Cons(cell1) <- args[0]) {
                if (let Cons(cell2) <- cell1.cdr) {
                    if (let Cons(cell3) <- cell2.cdr) {
                        cell3.car
                    } else {
                        Nil
                    }
                } else {
                    Nil
                }
            } else {
                Nil
            }
        }))

        // (prepend x lst) - cons 的现代别名
        let consFunc = env.lookup("cons")
        env.define("prepend", consFunc)

        // (more lst) - rest 的另一个别名
        env.define("more", cdrFunc)  // 复用上面的 cdrFunc
    }
}
