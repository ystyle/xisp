package ystyle::xisp.core

import std.collection.ArrayList

/**
 * 高阶函数扩展
 * 包含 apply, thread-first, map, filter, reduce
 */
extend Evaluator {
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

            let lst = if (let Cons(cdrCell) <- cell.cdr) {
                this.eval(cdrCell.car)
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
                this.eval(cdrCell.car)
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
                        case _ => Boolean(false)
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
                this.eval(cdrCell.car)
            } else {
                Nil
            }

            let lst = if (let Cons(cdrCell) <- cell.cdr) {
                if (let Cons(cdr2Cell) <- cdrCell.cdr) {
                    this.eval(cdr2Cell.car)
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
                let evaluated = this.eval(cell.car)
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

                for (i in 0..params.size) {
                    if (i < args.size) {
                        newEnv.define(params[i], args[i])
                    }
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
     * Thread-first 步骤
     */
    func threadFirstStep(value: LispValue, form: LispValue): LispValue {
        match (form) {
            case Symbol(sym) =>
                let funcValue = this.env.lookup(sym)
                let args = ArrayList<LispValue>()
                args.add(value)

                match (funcValue) {
                    case Procedure(_, _, _) => this.applyProcedure(funcValue, args)
                    case NativeFunc(f) => f(args)
                    case _ => Nil
                }
            case Cons(formCell) =>
                let fn = formCell.car
                let restArgs = formCell.cdr

                let args = ArrayList<LispValue>()
                args.add(value)

                var argCurrent: LispValue = restArgs
                while (!argCurrent.isNil()) {
                    if (let Cons(argCell) <- argCurrent) {
                        args.add(this.eval(argCell.car))
                        argCurrent = argCell.cdr
                    } else {
                        break
                    }
                }

                let funcValue = this.eval(fn)
                match (funcValue) {
                    case Procedure(_, _, _) => this.applyProcedure(funcValue, args)
                    case NativeFunc(f) => f(args)
                    case _ => Nil
                }
            case _ => this.eval(form)
        }
    }
}
