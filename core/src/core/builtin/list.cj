package ystyle::xisp.core.builtin

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
        // (length x) - 列表长度；外部值若实现 LispExternalIndexable 则按其 length()
        env.define("length", NativeFunc({ args =>
            if (args.size != 1) {
                return Nil
            }
            match (args[0]) {
                case Cons(cell) => LispValue.Int(cell.length())
                case Nil => LispValue.Int(0)
                // 能力接口分派：外部值靠"是否实现 LispExternalIndexable"获得内建支持，
                // 无需 core 认识任何具体外部类型（能力域设计 §7.2.1）
                case External(ext) =>
                    match (ext) {
                        case idx: LispExternalIndexable => LispValue.Int(idx.length())
                        case _ => LispValue.Error(XispError(ErrorType.TypeError,
                            "length: ${ext.typeName} is not indexable"))
                    }
                case other => LispValue.Error(XispError(ErrorType.TypeError,
                    "length: expected a list or indexable value (got: ${other.toString()})"))
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
