package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList
import std.collection.HashMap

/**
 * 高阶函数扩展
 * 包含 apply, thread-first, map, filter, reduce
 */
extend Evaluator {
    /**
     * 特殊形式：eval
     * 对表达式进行求值
     */
    func evalEval(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && cell.length() == 1) {
            // 先求值参数，得到表达式
            let exprValue = this.eval(cell.car)
            // 对表达式求值
            this.eval(exprValue)
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：apply
     */
    func evalApply(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr) {
            if (cell.length() < 2) {
                return Nil
            }

            let funcExpr = cell.car
            let rest = cell.cdr
            let funcValue = this.eval(funcExpr)

            let args = ArrayList<LispValue>()
            var current: LispValue = rest
            var lastList: LispValue = Nil

            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    let cdr = currCell.cdr
                    if (cdr.isNil()) {
                        lastList = this.eval(currCell.car)
                    } else {
                        args.add(this.eval(currCell.car))
                    }
                    current = cdr
                } else {
                    break
                }
            }

            if (let Cons(lastListCell) <- lastList) {
                var listCurrent: LispValue = Cons(lastListCell)
                while (!listCurrent.isNil()) {
                    if (let Cons(lc) <- listCurrent) {
                        args.add(lc.car)
                        listCurrent = lc.cdr
                    } else {
                        break
                    }
                }
            }

            match (funcValue) {
                case Procedure(_, _, _) => this.applyProcedure(funcValue, args)
                case NativeFunc(f) => f(args)
                case _ => Nil
            }
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：-> (thread-first)
     */
    func evalThreadFirst(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr) {
            if (cell.isNil()) {
                return Nil
            }

            var value = this.eval(cell.car)
            var current: LispValue = cell.cdr

            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    let form = currCell.car
                    value = this.threadFirstStep(value, form)
                    current = currCell.cdr
                } else {
                    break
                }
            }

            value
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：map
     */
    func evalMap(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && cell.length() == 2) {
            let funcValue = this.eval(cell.car)

            // 提取第二个参数
            let lst = if (let Cons(cdrCell) <- cell.cdr) {
                let arg = cdrCell.car
                // 检查是否是值列表（来自管道，已经求值）
                match (arg) {
                    case Cons(argCell) => match (argCell.car) {
                        case LispValue.Int(_) | LispValue.Float(_) | Str(_) | Boolean(_) | Nil => arg
                        case _ => this.eval(arg)
                    }
                    case _ => this.eval(arg)
                }
            } else {
                Nil
            }

            var result = ArrayList<LispValue>()
            var current: LispValue = lst

            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    let args = ArrayList<LispValue>()
                    args.add(currCell.car)

                    let mapped = match (funcValue) {
                        case Procedure(_, _, _) => this.applyProcedure(funcValue, args)
                        case NativeFunc(f) => f(args)
                        case _ => Nil
                    }
                    result.add(mapped)
                    current = currCell.cdr
                } else {
                    break
                }
            }

            list(result)
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：filter
     */
    func evalFilter(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && cell.length() == 2) {
            let pred = this.eval(cell.car)

            let lst = if (let Cons(cdrCell) <- cell.cdr) {
                let arg = cdrCell.car
                match (arg) {
                    case Cons(argCell) => match (argCell.car) {
                        case LispValue.Int(_) | LispValue.Float(_) | Str(_) | Boolean(_) | Nil => arg
                        case _ => this.eval(arg)
                    }
                    case _ => this.eval(arg)
                }
            } else {
                Nil
            }

            var result = ArrayList<LispValue>()
            var current: LispValue = lst

            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    let args = ArrayList<LispValue>()
                    args.add(currCell.car)

                    let testResult = match (pred) {
                        case Procedure(_, _, _) => this.applyProcedure(pred, args)
                        case NativeFunc(f) => f(args)
                        case _ => LispValue.Boolean(false)
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
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：reduce
     */
    func evalReduce(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && cell.length() == 3) {
            let funcValue = this.eval(cell.car)

            var acc = if (let Cons(cdrCell) <- cell.cdr) {
                let arg = cdrCell.car
                match (arg) {
                    case Cons(argCell) => match (argCell.car) {
                        case LispValue.Int(_) | LispValue.Float(_) | Str(_) | Boolean(_) | Nil => arg
                        case _ => this.eval(arg)
                    }
                    case _ => this.eval(arg)
                }
            } else {
                Nil
            }

            let lst = if (let Cons(cdrCell) <- cell.cdr) {
                if (let Cons(cdr2Cell) <- cdrCell.cdr) {
                    let arg = cdr2Cell.car
                    match (arg) {
                        case Cons(argCell) => match (argCell.car) {
                            case LispValue.Int(_) | LispValue.Float(_) | Str(_) | Boolean(_) | Nil => arg
                            case _ => this.eval(arg)
                        }
                        case _ => this.eval(arg)
                    }
                } else {
                    Nil
                }
            } else {
                Nil
            }

            var current: LispValue = lst

            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    let args = ArrayList<LispValue>()
                    args.add(acc)
                    args.add(currCell.car)

                    acc = match (funcValue) {
                        case Procedure(_, _, _) => this.applyProcedure(funcValue, args)
                        case NativeFunc(f) => f(args)
                        case _ => acc
                    }
                    current = currCell.cdr
                } else {
                    break
                }
            }

            acc
        } else {
            Nil
        }
    }

    /**
     * 求值参数列表
     */
    func evalArguments(args: LispValue): ArrayList<LispValue> {
        let result = ArrayList<LispValue>()
        var current: LispValue = args

        while (!current.isNil()) {
            if (let Cons(cell) <- current) {
                let arg = cell.car
                let evaluated = if (let Symbol(sym) <- arg && sym.startsWith(":")) {
                    // 关键字符号，不求值，直接返回
                    arg
                } else {
                    this.eval(arg)
                }
                // 检查是否有错误
                match (evaluated) {
                    case Str(s) =>
                        if (s.startsWith("Error:")) {
                            // 返回包含错误的结果（这里不能用 ArrayList，所以先添加）
                            result.add(evaluated)
                            return result
                        } else {
                            result.add(evaluated)
                        }
                    case _ => result.add(evaluated)
                }
                current = cell.cdr
            } else {
                break
            }
        }

        result
    }

    /**
     * 应用过程
     */
    func applyProcedure(procValue: LispValue, args: ArrayList<LispValue>): LispValue {
        match (procValue) {
            case Procedure(params, body, procEnv) =>
                // 检查栈深度（在增加之前检查）
                if (this.stackDepth >= this.maxStackDepth) {
                    return Str("Error: Maximum stack depth exceeded (limit: ${this.maxStackDepth})")
                }

                this.stackDepth = this.stackDepth + 1
                let newEnv = procEnv.createChild()

                var i = 0
                var argIndex = 0

                while (i < params.size) {
                    let param = params[i]

                    // 检查是否为可变参数（带 &rest- 前缀）
                    if (param.startsWith("&rest-")) {
                        let restName = param[6..]  // 去掉 "&rest-" 前缀

                        // 将剩余参数转换为列表
                        let restArgs = ArrayList<LispValue>()
                        while (argIndex < args.size) {
                            restArgs.add(args[argIndex])
                            argIndex = argIndex + 1
                        }

                        // 将 ArrayList 转换为 Lisp 列表
                        var restList: LispValue = Nil
                        var j = restArgs.size - 1
                        while (j >= 0) {
                            restList = Cons(ConsCell(restArgs[j], restList))
                            j = j - 1
                        }

                        newEnv.define(restName, restList)
                        break
                    } else {
                        // 普通参数
                        if (argIndex < args.size) {
                            newEnv.define(param, args[argIndex])
                            argIndex = argIndex + 1
                        } else {
                            newEnv.define(param, Nil)
                        }
                        i = i + 1
                    }
                }

                let oldEnv = this.env
                this.env = newEnv

                let result = this.evalBegin(body)

                this.env = oldEnv
                this.stackDepth = this.stackDepth - 1

                result
            case ProcedureFromParams(paramInfos, body, procEnv) =>
                // 检查栈深度（在增加之前检查）
                if (this.stackDepth >= this.maxStackDepth) {
                    return Str("Error: Maximum stack depth exceeded (limit: ${this.maxStackDepth})")
                }

                this.stackDepth = this.stackDepth + 1
                let newEnv = procEnv.createChild()

                var argIndex = 0
                var i = 0

                while (i < paramInfos.size && argIndex < args.size) {
                    let paramInfo = paramInfos[i]

                    // 遇到命名参数或可变参数，停止位置参数绑定
                    if (paramInfo.isKey || paramInfo.isRest) {
                        break
                    }

                    // 绑定位置参数
                    newEnv.define(paramInfo.name, args[argIndex])
                    i = i + 1
                    argIndex = argIndex + 1
                }

                // 跳过位置参数区域
                while (i < paramInfos.size && !paramInfos[i].isKey && !paramInfos[i].isRest) {
                    i = i + 1
                }

                let keywordArgs = HashMap<String, LispValue>()
                var j = argIndex
                while (j < args.size - 1) {
                    // 检查是否是关键字符号（以 : 开头）
                    if (let Symbol(sym) <- args[j] && sym.startsWith(":")) {
                        let keyName = sym[1..]  // 去掉 ":" 前缀
                        keywordArgs[keyName] = args[j + 1]
                        j = j + 2
                    } else {
                        // 不是关键字，停止解析命名参数
                        break
                    }
                }

                // 绑定所有命名参数
                while (i < paramInfos.size) {
                    let paramInfo = paramInfos[i]

                    if (paramInfo.isRest) {
                        // 遇到可变参数，停止命名参数绑定
                        break
                    }

                    if (paramInfo.isKey) {
                        // 这是一个命名参数
                        let value = if (keywordArgs.contains(paramInfo.name)) {
                            keywordArgs.get(paramInfo.name)
                        } else {
                            // 使用默认值或 Nil
                            match (paramInfo.defaultValue) {
                                case Some(defaultVal) => defaultVal
                                case None => Nil
                            }
                        }
                        match (value) {
                            case Some(v) => newEnv.define(paramInfo.name, v)
                            case None => newEnv.define(paramInfo.name, Nil)
                        }
                    }

                    i = i + 1
                }

                // 找到可变参数
                var foundRest = false
                for (paramInfo in paramInfos) {
                    if (paramInfo.isRest) {
                        foundRest = true
                        let restArgs = ArrayList<LispValue>()
                        var restIndex = argIndex
                        // 跳过命名参数（它们已经在上面的循环中处理了）
                        // 现在需要找到命名参数之后的参数
                        // 重新计算 argIndex，跳过命名参数
                        while (restIndex < args.size) {
                            if (let Symbol(sym) <- args[restIndex] && sym.startsWith(":")) {
                                // 跳过关键字参数对
                                restIndex = restIndex + 2
                            } else {
                                // 位置参数
                                restArgs.add(args[restIndex])
                                restIndex = restIndex + 1
                            }
                        }

                        // 将 ArrayList 转换为 Lisp 列表
                        var restList: LispValue = Nil
                        var k = restArgs.size - 1
                        while (k >= 0) {
                            restList = Cons(ConsCell(restArgs[k], restList))
                            k = k - 1
                        }

                        newEnv.define(paramInfo.name, restList)
                        break
                    }
                }

                // 如果还有未绑定的位置参数（在可变参数之前），绑定它们为 Nil
                while (i < paramInfos.size) {
                    let paramInfo = paramInfos[i]
                    if (!paramInfo.isRest && !paramInfo.isKey) {
                        newEnv.define(paramInfo.name, Nil)
                    }
                    i = i + 1
                }

                let oldEnv = this.env
                this.env = newEnv

                let result = this.evalBegin(body)

                this.env = oldEnv
                this.stackDepth = this.stackDepth - 1

                result
            case _ => Nil
        }
    }

    /**
     * Thread-first 步骤 - 辅助方法：构造符号调用
     */
    func threadFirstSymbol(sym: String, value: LispValue): LispValue {
        // 构造 (sym value) 形式的调用
        // 如果 value 是列表，需要用 quote 防止求值；否则直接使用
        let quotedValue = if (value.isList()) {
            // (quote value)
            consValue(Symbol("quote"), consValue(value, Nil))
        } else {
            value
        }
        let args = consValue(quotedValue, Nil)
        let expr = consValue(Symbol(sym), args)
        this.eval(expr)
    }

    /**
     * Thread-first 步骤 - 辅助方法：构造列表调用
     */
    func threadFirstList(fn: LispValue, restArgs: LispValue, value: LispValue): LispValue {
        let args = ArrayList<LispValue>()

        // 检查函数名，某些函数需要特殊的参数顺序
        let funcName = if (let Symbol(sym) <- fn) {
            sym
        } else {
            ""
        }

        // 对于 map/filter/reduce/apply，参数顺序是 (func list ...others)
        // 所以 value 应该在 restArgs 的函数参数之后添加
        let needsSpecialOrder = funcName == "map" || funcName == "filter" || funcName == "reduce" || funcName == "apply"

        if (needsSpecialOrder) {
            // 先添加 restArgs 中的参数（第一个是函数，需要求值）
            var argCurrent: LispValue = restArgs
            while (!argCurrent.isNil()) {
                if (let Cons(argCell) <- argCurrent) {
                    args.add(this.eval(argCell.car))
                    argCurrent = argCell.cdr
                } else {
                    break
                }
            }

            // 再添加 value（如果 value 是列表，需要用 quote 防止求值）
            if (value.isList()) {
                args.add(consValue(Symbol("quote"), consValue(value, Nil)))
            } else {
                args.add(value)
            }
        } else {
            // 默认顺序：value 在前面
            // 先添加 value（已经求值过了，直接使用）
            // 如果 value 是列表，需要用 quote 防止求值
            if (value.isList()) {
                args.add(consValue(Symbol("quote"), consValue(value, Nil)))
            } else {
                args.add(value)
            }

            // 再添加 restArgs 中的参数（需要求值）
            var argCurrent: LispValue = restArgs
            while (!argCurrent.isNil()) {
                if (let Cons(argCell) <- argCurrent) {
                    args.add(this.eval(argCell.car))
                    argCurrent = argCell.cdr
                } else {
                    break
                }
            }
        }

        let cell = ConsCell(fn, list(args))
        this.eval(Cons(cell))
    }

    /**
     * Thread-first 步骤
     */
    func threadFirstStep(value: LispValue, form: LispValue): LispValue {
        match (form) {
            case Symbol(sym) => this.threadFirstSymbol(sym, value)
            case Cons(formCell) => this.threadFirstList(formCell.car, formCell.cdr, value)
            case _ => this.eval(form)
        }
    }
}
