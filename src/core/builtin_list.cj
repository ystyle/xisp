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

        // (append list1 list2 ...) - 连接多个列表
        env.define("append", NativeFunc({ args =>
            // 从后往前构建结果列表
            var result: LispValue = Nil
            var i = args.size - 1
            while (i >= 0) {
                let lst = args[i]
                match (lst) {
                    case Cons(cell) =>
                        // 遍历列表，将每个元素添加到结果前面
                        var current: LispValue = Cons(cell)
                        while (!current.isNil()) {
                            if (let Cons(currCell) <- current) {
                                result = consValue(currCell.car, result)
                                current = currCell.cdr
                            } else {
                                break
                            }
                        }
                    case Nil =>
                        // 空列表，跳过
                        ()
                    case _ =>
                        // 非列表参数，忽略或返回错误
                        return Nil
                }
                i = i - 1
            }
            result
        }))
    }
}
