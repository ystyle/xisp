package ystyle::xisp.core

import std.collection.ArrayList

/**
 * 高阶函数扩展
 * 包含 map, filter, reduce 等函数式编程工具
 */
extend BuiltinFunctions {
    static func registerHigherOrder(env: Environment) {
        // (map func lst) - 对列表中每个元素应用函数
        env.define("map", NativeFunc({ args =>
            if (args.size != 2) {
                return Nil
            }

            let funcValue = args[0]
            var result = ArrayList<LispValue>()

            match (args[1]) {
                case Cons(cell) =>
                    var current: LispValue = Cons(cell)
                    while (!current.isNil()) {
                        if (let Cons(currCell) <- current) {
                            // 调用函数（暂时只支持原生函数）
                            if (let NativeFunc(f) <- funcValue) {
                                let argList = ArrayList<LispValue>()
                                argList.add(currCell.car)
                                result.add(f(argList))
                            } else {
                                result.add(currCell.car)
                            }
                            current = currCell.cdr
                        } else {
                            break
                        }
                    }
                    list(result)
                case _ => Nil
            }
        }))

        // (filter pred lst) - 过滤列表
        env.define("filter", NativeFunc({ args =>
            if (args.size != 2) {
                return Nil
            }

            let pred = args[0]
            let result = ArrayList<LispValue>()

            match (args[1]) {
                case Cons(cell) =>
                    var current: LispValue = Cons(cell)
                    while (!current.isNil()) {
                        if (let Cons(currCell) <- current) {
                            let testResult = if (let NativeFunc(f) <- pred) {
                                let argList = ArrayList<LispValue>()
                                argList.add(currCell.car)
                                f(argList)
                            } else {
                                Boolean(false)
                            }

                            if (testResult.isTruthy()) {
                                result.add(currCell.car)
                            }
                            current = currCell.cdr
                        } else {
                            break
                        }
                    }
                    list(result)
                case _ => Nil
            }
        }))

        // (reduce func init lst) - 归约操作
        env.define("reduce", NativeFunc({ args =>
            if (args.size != 3) {
                return Nil
            }

            let `func` = args[0]
            var acc = args[1]

            match (args[2]) {
                case Cons(cell) =>
                    var current: LispValue = Cons(cell)
                    while (!current.isNil()) {
                        if (let Cons(currCell) <- current) {
                            let argList = ArrayList<LispValue>()
                            argList.add(acc)
                            argList.add(currCell.car)

                            acc = if (let NativeFunc(f) <- `func`) {
                                f(argList)
                            } else {
                                acc
                            }
                            current = currCell.cdr
                        } else {
                            break
                        }
                    }
                    acc
                case _ => acc
            }
        }))

        // (sum lst) - 求和
        env.define("sum", NativeFunc({ args =>
            if (args.size != 1) {
                return Number(0.0)
            }

            var total = 0.0
            if (let Cons(cell) <- args[0]) {
                var current: LispValue = Cons(cell)
                while (!current.isNil()) {
                    if (let Cons(currCell) <- current) {
                        if (let Number(n) <- currCell.car) {
                            total = total + n
                        }
                        current = currCell.cdr
                    } else {
                        break
                    }
                }
            }

            Number(total)
        }))

        // (product lst) - 求积
        env.define("product", NativeFunc({ args =>
            if (args.size != 1) {
                return Number(1.0)
            }

            var total = 1.0
            if (let Cons(cell) <- args[0]) {
                var current: LispValue = Cons(cell)
                while (!current.isNil()) {
                    if (let Cons(currCell) <- current) {
                        if (let Number(n) <- currCell.car) {
                            total = total * n
                        }
                        current = currCell.cdr
                    } else {
                        break
                    }
                }
            }

            Number(total)
        }))

        // (max lst) - 最大值
        env.define("max", NativeFunc({ args =>
            if (args.size != 1) {
                return Nil
            }

            var maxValue: ?Float64 = None
            if (let Cons(cell) <- args[0]) {
                var current: LispValue = Cons(cell)
                while (!current.isNil()) {
                    if (let Cons(currCell) <- current) {
                        if (let Number(n) <- currCell.car) {
                            if (let Some(m) <- maxValue) {
                                if (n > m) {
                                    maxValue = Some(n)
                                }
                            } else {
                                maxValue = Some(n)
                            }
                        }
                        current = currCell.cdr
                    } else {
                        break
                    }
                }
            }

            if (let Some(m) <- maxValue) {
                Number(m)
            } else {
                Nil
            }
        }))

        // (min lst) - 最小值
        env.define("min", NativeFunc({ args =>
            if (args.size != 1) {
                return Nil
            }

            var minValue: ?Float64 = None
            if (let Cons(cell) <- args[0]) {
                var current: LispValue = Cons(cell)
                while (!current.isNil()) {
                    if (let Cons(currCell) <- current) {
                        if (let Number(n) <- currCell.car) {
                            if (let Some(m) <- minValue) {
                                if (n < m) {
                                    minValue = Some(n)
                                }
                            } else {
                                minValue = Some(n)
                            }
                        }
                        current = currCell.cdr
                    } else {
                        break
                    }
                }
            }

            if (let Some(m) <- minValue) {
                Number(m)
            } else {
                Nil
            }
        }))

        // (reverse lst) - 反转列表
        env.define("reverse", NativeFunc({ args =>
            if (args.size != 1) {
                return Nil
            }

            var result: LispValue = Nil

            if (let Cons(cell) <- args[0]) {
                var current: LispValue = Cons(cell)
                while (!current.isNil()) {
                    if (let Cons(currCell) <- current) {
                        result = consValue(currCell.car, result)
                        current = currCell.cdr
                    } else {
                        break
                    }
                }
                result
            } else {
                Nil
            }
        }))

        // (range start end) - 生成范围列表
        env.define("range", NativeFunc({ args =>
            if (args.size < 2) {
                return Nil
            }

            var start = 0.0
            var end = 0.0
            var step = 1.0

            match (args[0]) {
                case Number(n) => start = n
                case _ => return Nil
            }

            match (args[1]) {
                case Number(n) => end = n
                case _ => return Nil
            }

            if (args.size >= 3) {
                match (args[2]) {
                    case Number(n) => step = n
                    case _ => ()
                }
            }

            let result = ArrayList<LispValue>()
            var current = start
            while (current < end) {
                result.add(Number(current))
                current = current + step
            }

            list(result)
        }))
    }
}
