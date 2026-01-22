package ystyle::xisp.core

/**
 * 列表操作扩展
 * 包含 cons, car, cdr, list, length 等列表操作
 */
extend BuiltinFunctions {
    static func registerListOps(env: Environment) {
        // (cons x y) - 构造序对
        env.define("cons", NativeFunc({ args =>
            if (args.size != 2) {
                return Nil
            }
            consValue(args[0], args[1])
        }))

        // (car list) - 获取列表第一个元素
        env.define("car", NativeFunc({ args =>
            if (args.size != 1) {
                return Nil
            }
            match (args[0]) {
                case Cons(cell) => cell.car
                case _ => Nil
            }
        }))

        // (cdr list) - 获取列表除第一个元素外的部分
        env.define("cdr", NativeFunc({ args =>
            if (args.size != 1) {
                return Nil
            }
            match (args[0]) {
                case Cons(cell) => cell.cdr
                case _ => Nil
            }
        }))

        // (list x1 x2 ...) - 构造列表
        env.define("list", NativeFunc({ args =>
            list(args)
        }))

        // (length list) - 列表长度
        env.define("length", NativeFunc({ args =>
            if (args.size != 1) {
                return Nil
            }
            match (args[0]) {
                case Cons(cell) => Number(Float64(cell.length()))
                case _ => Number(0.0)
            }
        }))
    }
}
