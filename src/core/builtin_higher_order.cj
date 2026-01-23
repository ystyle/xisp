package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 高阶函数扩展
 * 包含 apply, sum, product, max, min, reverse, range 等工具函数
 *
 * 注意：map, filter, reduce 已作为特殊形式实现，不在这里注册
 */
extend BuiltinFunctions {
    static func registerHigherOrder(env: Environment) {
        // map, filter, reduce 已作为特殊形式实现，见 eval_higher_order.cj

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

        // (max lst) - 最大值，支持两种调用方式：
        // - (max '(1 2 3)) - 传入列表
        // - (apply max '(1 2 3)) - 通过 apply 传入多个参数
        env.define("max", NativeFunc({ args =>
            var maxValue: ?Float64 = None

            // 情况 1: 单个列表参数 (max '(1 2 3))
            if (args.size == 1) {
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
            } else {
                // 情况 2: 多个参数 (apply max '(1 2 3)) -> args = [1, 2, 3]
                for (arg in args) {
                    if (let Number(n) <- arg) {
                        if (let Some(m) <- maxValue) {
                            if (n > m) {
                                maxValue = Some(n)
                            }
                        } else {
                            maxValue = Some(n)
                        }
                    }
                }
            }

            if (let Some(m) <- maxValue) {
                Number(m)
            } else {
                Nil
            }
        }))

        // (min lst) - 最小值，支持两种调用方式：
        // - (min '(1 2 3)) - 传入列表
        // - (apply min '(1 2 3)) - 通过 apply 传入多个参数
        env.define("min", NativeFunc({ args =>
            var minValue: ?Float64 = None

            // 情况 1: 单个列表参数 (min '(1 2 3))
            if (args.size == 1) {
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
            } else {
                // 情况 2: 多个参数 (apply min '(1 2 3)) -> args = [1, 2, 3]
                for (arg in args) {
                    if (let Number(n) <- arg) {
                        if (let Some(m) <- minValue) {
                            if (n < m) {
                                minValue = Some(n)
                            }
                        } else {
                            minValue = Some(n)
                        }
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
