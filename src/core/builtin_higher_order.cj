package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 高阶函数扩展
 * 包含 apply, sum, product, max, min, reverse, range 等工具函数
 *
 * 注意：map, filter, reduce 已作为特殊形式实现，不在这里注册
 * 支持整数和浮点数
 */
extend BuiltinFunctions {
    static func registerHigherOrder(env: Environment) {
        // map, filter, reduce 已作为特殊形式实现，见 eval_higher_order.cj

        // (sum lst) - 求和
        env.define("sum", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Float(0.0)
            }

            var hasFloat = false
            var intTotal: Int64 = 0
            var floatTotal: Float64 = 0.0

            if (let Cons(cell) <- args[0]) {
                var current: LispValue = Cons(cell)
                while (!current.isNil()) {
                    if (let Cons(currCell) <- current) {
                        match (currCell.car) {
                            case LispValue.Int(i) =>
                                if (!hasFloat) {
                                    intTotal = intTotal + i
                                } else {
                                    floatTotal = floatTotal + Float64(i)
                                }
                            case LispValue.Float(f) =>
                                if (!hasFloat) {
                                    hasFloat = true
                                    floatTotal = Float64(intTotal) + f
                                } else {
                                    floatTotal = floatTotal + f
                                }
                            case _ => ()
                        }
                        current = currCell.cdr
                    } else {
                        break
                    }
                }
            }

            if (hasFloat) {
                LispValue.Float(floatTotal)
            } else {
                LispValue.Int(intTotal)
            }
        }))

        // (product lst) - 求积
        env.define("product", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Float(1.0)
            }

            var hasFloat = false
            var intTotal: Int64 = 1
            var floatTotal: Float64 = 1.0

            if (let Cons(cell) <- args[0]) {
                var current: LispValue = Cons(cell)
                while (!current.isNil()) {
                    if (let Cons(currCell) <- current) {
                        match (currCell.car) {
                            case LispValue.Int(i) =>
                                if (!hasFloat) {
                                    intTotal = intTotal * i
                                } else {
                                    floatTotal = floatTotal * Float64(i)
                                }
                            case LispValue.Float(f) =>
                                if (!hasFloat) {
                                    hasFloat = true
                                    floatTotal = Float64(intTotal) * f
                                } else {
                                    floatTotal = floatTotal * f
                                }
                            case _ => ()
                        }
                        current = currCell.cdr
                    } else {
                        break
                    }
                }
            }

            if (hasFloat) {
                LispValue.Float(floatTotal)
            } else {
                LispValue.Int(intTotal)
            }
        }))

        // (max lst) - 最大值，支持两种调用方式：
        // - (max '(1 2 3)) - 传入列表
        // - (apply max '(1 2 3)) - 通过 apply 传入多个参数
        env.define("max", NativeFunc({ args =>
            var maxIntValue: ?Int64 = None
            var maxFloatValue: ?Float64 = None
            var hasFloat = false

            // 辅助函数：处理单个值
            func processValue(val: LispValue) {
                match (val) {
                    case LispValue.Int(i) =>
                        if (!hasFloat) {
                            if (let Some(m) <- maxIntValue) {
                                if (i > m) {
                                    maxIntValue = Some(i)
                                }
                            } else {
                                maxIntValue = Some(i)
                            }
                        } else {
                            let f = Float64(i)
                            if (let Some(m) <- maxFloatValue) {
                                if (f > m) {
                                    maxFloatValue = Some(f)
                                }
                            } else {
                                maxFloatValue = Some(f)
                            }
                        }
                    case LispValue.Float(f) =>
                        hasFloat = true
                        if (let Some(m) <- maxFloatValue) {
                            if (f > m) {
                                maxFloatValue = Some(f)
                            }
                        } else {
                            maxFloatValue = Some(f)
                        }
                    case _ => ()
                }
            }

            // 情况 1: 单个列表参数 (max '(1 2 3))
            if (args.size == 1) {
                if (let Cons(cell) <- args[0]) {
                    var current: LispValue = Cons(cell)
                    while (!current.isNil()) {
                        if (let Cons(currCell) <- current) {
                            processValue(currCell.car)
                            current = currCell.cdr
                        } else {
                            break
                        }
                    }
                }
            } else {
                // 情况 2: 多个参数 (apply max '(1 2 3)) -> args = [1, 2, 3]
                for (arg in args) {
                    processValue(arg)
                }
            }

            if (hasFloat) {
                if (let Some(m) <- maxFloatValue) {
                    LispValue.Float(m)
                } else {
                    Nil
                }
            } else {
                if (let Some(m) <- maxIntValue) {
                    LispValue.Int(m)
                } else {
                    Nil
                }
            }
        }))

        // (min lst) - 最小值，支持两种调用方式：
        // - (min '(1 2 3)) - 传入列表
        // - (apply min '(1 2 3)) - 通过 apply 传入多个参数
        env.define("min", NativeFunc({ args =>
            var minIntValue: ?Int64 = None
            var minFloatValue: ?Float64 = None
            var hasFloat = false

            // 辅助函数：处理单个值
            func processValue(val: LispValue) {
                match (val) {
                    case LispValue.Int(i) =>
                        if (!hasFloat) {
                            if (let Some(m) <- minIntValue) {
                                if (i < m) {
                                    minIntValue = Some(i)
                                }
                            } else {
                                minIntValue = Some(i)
                            }
                        } else {
                            let f = Float64(i)
                            if (let Some(m) <- minFloatValue) {
                                if (f < m) {
                                    minFloatValue = Some(f)
                                }
                            } else {
                                minFloatValue = Some(f)
                            }
                        }
                    case LispValue.Float(f) =>
                        hasFloat = true
                        if (let Some(m) <- minFloatValue) {
                            if (f < m) {
                                minFloatValue = Some(f)
                            }
                        } else {
                            minFloatValue = Some(f)
                        }
                    case _ => ()
                }
            }

            // 情况 1: 单个列表参数 (min '(1 2 3))
            if (args.size == 1) {
                if (let Cons(cell) <- args[0]) {
                    var current: LispValue = Cons(cell)
                    while (!current.isNil()) {
                        if (let Cons(currCell) <- current) {
                            processValue(currCell.car)
                            current = currCell.cdr
                        } else {
                            break
                        }
                    }
                }
            } else {
                // 情况 2: 多个参数 (apply min '(1 2 3)) -> args = [1, 2, 3]
                for (arg in args) {
                    processValue(arg)
                }
            }

            if (hasFloat) {
                if (let Some(m) <- minFloatValue) {
                    LispValue.Float(m)
                } else {
                    Nil
                }
            } else {
                if (let Some(m) <- minIntValue) {
                    LispValue.Int(m)
                } else {
                    Nil
                }
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

        // (range start end [step]) - 生成范围列表
        env.define("range", NativeFunc({ args =>
            if (args.size < 2) {
                return Nil
            }

            var startInt: Int64 = 0
            var endInt: Int64 = 0
            var stepInt: Int64 = 1
            var startFloat: Float64 = 0.0
            var endFloat: Float64 = 0.0
            var stepFloat: Float64 = 1.0
            var useFloat = false

            // 解析 start
            match (args[0]) {
                case LispValue.Int(i) => startInt = i
                case LispValue.Float(f) =>
                    startFloat = f
                    useFloat = true
                case _ => return Nil
            }

            // 解析 end
            match (args[1]) {
                case LispValue.Int(i) =>
                    if (useFloat) {
                        endFloat = Float64(i)
                    } else {
                        endInt = i
                    }
                case LispValue.Float(f) =>
                    if (useFloat) {
                        endFloat = f
                    } else {
                        useFloat = true
                        endFloat = f
                        startFloat = Float64(startInt)
                    }
                case _ => return Nil
            }

            // 解析 step（可选）
            if (args.size >= 3) {
                match (args[2]) {
                    case LispValue.Int(i) =>
                        if (useFloat) {
                            stepFloat = Float64(i)
                        } else {
                            stepInt = i
                        }
                    case LispValue.Float(f) =>
                        if (useFloat) {
                            stepFloat = f
                        } else {
                            useFloat = true
                            stepFloat = f
                            startFloat = Float64(startInt)
                            endFloat = Float64(endInt)
                        }
                    case _ => ()
                }
            }

            let result = ArrayList<LispValue>()

            if (useFloat) {
                var current = startFloat
                while (current < endFloat) {
                    result.add(LispValue.Float(current))
                    current = current + stepFloat
                }
            } else {
                var current = startInt
                while (current < endInt) {
                    result.add(LispValue.Int(current))
                    current = current + stepInt
                }
            }

            list(result)
        }))
    }
}
