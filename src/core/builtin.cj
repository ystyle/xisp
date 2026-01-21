package ystyle::xisp.core

import std.collection.ArrayList

/**
 * 基础函数库 - 注册所有内置函数到环境
 */
public class BuiltinFunctions {
    /**
     * 注册所有基础函数到环境
     */
    public static func registerAll(env: Environment) {
        registerArithmetic(env)
        registerComparison(env)
        registerListOps(env)
        registerModernAliases(env)  // 现代化别名（依赖 registerListOps 先注册）
        registerHigherOrder(env)     // 高阶函数
        registerLogic(env)
        registerPredicates(env)
        registerPrint(env)           // 打印函数
    }

    /**
     * 算术运算函数
     */
    private static func registerArithmetic(env: Environment) {
        // (+ n1 n2 ...) - 加法
        env.define("+", NativeFunc({ args =>
            var result = 0.0
            for (i in 0..args.size) {
                if (let Number(n) <- args[i]) {
                    result = result + n
                } else {
                    return Nil
                }
            }
            Number(result)
        }))

        // (- n1 n2 ...) - 减法
        env.define("-", NativeFunc({ args =>
            if (args.size == 0) {
                return Nil
            }

            if (let Number(first) <- args[0]) {
                if (args.size == 1) {
                    Number(-first)
                } else {
                    var result = first
                    for (i in 1..args.size) {
                        if (let Number(n) <- args[i]) {
                            result = result - n
                        } else {
                            return Nil
                        }
                    }
                    Number(result)
                }
            } else {
                Nil
            }
        }))

        // (* n1 n2 ...) - 乘法
        env.define("*", NativeFunc({ args =>
            var result = 1.0
            for (i in 0..args.size) {
                if (let Number(n) <- args[i]) {
                    result = result * n
                } else {
                    return Nil
                }
            }
            Number(result)
        }))

        // (/ n1 n2 ...) - 除法
        env.define("/", NativeFunc({ args =>
            if (args.size < 2) {
                return Nil
            }

            if (let Number(first) <- args[0]) {
                var result = first
                for (i in 1..args.size) {
                    if (let Number(n) <- args[i]) {
                        if (n == 0.0) {
                            return Nil  // 除零错误
                        }
                        result = result / n
                    } else {
                        return Nil
                    }
                }
                Number(result)
            } else {
                Nil
            }
        }))

        // (mod n1 n2) - 取模
        env.define("mod", NativeFunc({ args =>
            if (args.size != 2) {
                return Nil
            }

            match ((args[0], args[1])) {
                case (Number(n1), Number(n2)) =>
                    if (n2 == 0.0) {
                        return Nil
                    }
                    // Float64 取模: n1 - n2 * floor(n1 / n2)
                    let quotient = n1 / n2
                    // 手动实现 floor：转为 Int64 再转回来
                    let intQuotient = Int64(quotient)
                    if (quotient < 0.0 && quotient > Float64(intQuotient)) {
                        // 负数需要减1
                        Number(n1 - n2 * Float64(intQuotient - 1))
                    } else {
                        Number(n1 - n2 * Float64(intQuotient))
                    }
                case _ => Nil
            }
        }))
    }

    /**
     * 比较运算函数
     */
    private static func registerComparison(env: Environment) {
        // (= n1 n2) - 等于
        env.define("=", NativeFunc({ args =>
            if (args.size < 2) {
                return Boolean(true)
            }

            if (let Number(first) <- args[0]) {
                var result = true
                for (i in 1..args.size) {
                    if (let Number(n) <- args[i]) {
                        if (!(first == n)) {
                            result = false
                            break
                        }
                    } else {
                        return Boolean(false)
                    }
                }
                Boolean(result)
            } else {
                Boolean(false)
            }
        }))

        // (< n1 n2 ...) - 小于
        env.define("<", NativeFunc({ args =>
            if (args.size < 2) {
                return Boolean(true)
            }

            var prev: ?Float64 = None
            var result = true

            for (i in 0..args.size) {
                if (let Number(n) <- args[i]) {
                    if (let Some(p) <- prev) {
                        if (!(p < n)) {
                            result = false
                            break
                        }
                    }
                    prev = Some(n)
                } else {
                    return Boolean(false)
                }
            }

            Boolean(result)
        }))

        // (> n1 n2 ...) - 大于
        env.define(">", NativeFunc({ args =>
            if (args.size < 2) {
                return Boolean(true)
            }

            var prev: ?Float64 = None
            var result = true

            for (i in 0..args.size) {
                if (let Number(n) <- args[i]) {
                    if (let Some(p) <- prev) {
                        if (!(p > n)) {
                            result = false
                            break
                        }
                    }
                    prev = Some(n)
                } else {
                    return Boolean(false)
                }
            }

            Boolean(result)
        }))

        // (<= n1 n2 ...) - 小于等于
        env.define("<=", NativeFunc({ args =>
            if (args.size < 2) {
                return Boolean(true)
            }

            var prev: ?Float64 = None
            var result = true

            for (i in 0..args.size) {
                if (let Number(n) <- args[i]) {
                    if (let Some(p) <- prev) {
                        if (!(p <= n)) {
                            result = false
                            break
                        }
                    }
                    prev = Some(n)
                } else {
                    return Boolean(false)
                }
            }

            Boolean(result)
        }))

        // (>= n1 n2 ...) - 大于等于
        env.define(">=", NativeFunc({ args =>
            if (args.size < 2) {
                return Boolean(true)
            }

            var prev: ?Float64 = None
            var result = true

            for (i in 0..args.size) {
                if (let Number(n) <- args[i]) {
                    if (let Some(p) <- prev) {
                        if (!(p >= n)) {
                            result = false
                            break
                        }
                    }
                    prev = Some(n)
                } else {
                    return Boolean(false)
                }
            }

            Boolean(result)
        }))
    }

    /**
     * 列表操作函数
     */
    private static func registerListOps(env: Environment) {
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

    /**
     * 逻辑运算函数
     */
    private static func registerLogic(env: Environment) {
        // (and x1 x2 ...) - 逻辑与
        env.define("and", NativeFunc({ args =>
            var result = Boolean(true)
            for (i in 0..args.size) {
                if (!args[i].isTruthy()) {
                    return Boolean(false)
                }
                result = args[i]
            }
            result
        }))

        // (or x1 x2 ...) - 逻辑或
        env.define("or", NativeFunc({ args =>
            for (i in 0..args.size) {
                if (args[i].isTruthy()) {
                    return args[i]
                }
            }
            Boolean(false)
        }))

        // (not x) - 逻辑非
        env.define("not", NativeFunc({ args =>
            if (args.size != 1) {
                return Boolean(true)
            }
            Boolean(!args[0].isTruthy())
        }))
    }

    /**
     * 谓词函数
     */
    private static func registerPredicates(env: Environment) {
        // (number? x) - 是否为数字
        env.define("number?", NativeFunc({ args =>
            if (args.size != 1) {
                return Boolean(false)
            }
            Boolean(args[0].isNumber())
        }))

        // (string? x) - 是否为字符串
        env.define("string?", NativeFunc({ args =>
            if (args.size != 1) {
                return Boolean(false)
            }
            Boolean(args[0].isString())
        }))

        // (symbol? x) - 是否为符号
        env.define("symbol?", NativeFunc({ args =>
            if (args.size != 1) {
                return Boolean(false)
            }
            Boolean(args[0].isSymbol())
        }))

        // (list? x) - 是否为列表
        env.define("list?", NativeFunc({ args =>
            if (args.size != 1) {
                return Boolean(false)
            }
            Boolean(args[0].isList())
        }))

        // (null? x) - 是否为空
        env.define("null?", NativeFunc({ args =>
            if (args.size != 1) {
                return Boolean(false)
            }
            Boolean(args[0].isNil())
        }))

        // (procedure? x) - 是否为过程
        env.define("procedure?", NativeFunc({ args =>
            if (args.size != 1) {
                return Boolean(false)
            }
            Boolean(args[0].isProcedure())
        }))
    }

    /**
     * 现代化别名函数
     * 为传统 Lisp 函数提供现代化的别名
     */
    private static func registerModernAliases(env: Environment) {
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

    /**
     * 高阶函数
     * map, filter, reduce 等函数式编程工具
     */
    private static func registerHigherOrder(env: Environment) {
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

    /**
     * 打印和输出函数
     */
    private static func registerPrint(env: Environment) {
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
            for (i in 0..args.size) {
                if (i > 0) {
                    print(" ")
                }
                print(args[i].toString())
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
