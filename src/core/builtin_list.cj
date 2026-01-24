package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList

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
                case Cons(cell) => LispValue.Int(cell.length())
                case _ => LispValue.Int(0)
            }
        }))

        // (append list1 list2 ...) - 连接多个列表
        env.define("append", NativeFunc({ args =>
            // 从前往后收集所有元素
            var elements = ArrayList<LispValue>()
            var i = 0
            while (i < args.size) {
                let lst = args[i]
                match (lst) {
                    case Cons(cell) =>
                        var current: LispValue = Cons(cell)
                        while (!current.isNil()) {
                            if (let Cons(currCell) <- current) {
                                elements.add(currCell.car)
                                current = currCell.cdr
                            } else {
                                break
                            }
                        }
                    case Nil => ()
                    case _ => return Nil
                }
                i = i + 1
            }

            // 构建列表（从 ArrayList 的末尾开始，保持顺序）
            var result: LispValue = Nil
            var j = elements.size - 1
            while (j >= 0) {
                result = consValue(elements[j], result)
                j = j - 1
            }
            result
        }))
    }
}
