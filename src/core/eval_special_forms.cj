package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 特殊形式扩展
 * 包含 quote, backquote, comma, comma-at, define, set!, if, lambda, begin, let
 */
extend Evaluator {
    /**
     * 特殊形式：quote
     */
    func evalQuote(expr: LispValue): LispValue {
        match (expr) {
            case Cons(cell) =>
                if (cell.length() == 1) {
                    cell.car
                } else {
                    Nil
                }
            case _ => expr
        }
    }

    /**
     * 特殊形式：backquote (反引用)
     * 类似 quote，但允许在内部使用 comma 和 comma-at 来取消引用
     */
    func evalBackquote(expr: LispValue): LispValue {
        match (expr) {
            case Cons(cell) =>
                if (cell.length() == 1) {
                    this.expandBackquote(cell.car)
                } else {
                    Nil
                }
            case _ => expr
        }
    }

    /**
     * 展开 backquote 表达式
     * 递归处理列表，查找 comma 和 comma-at
     */
    private func expandBackquote(expr: LispValue): LispValue {
        match (expr) {
            case Nil => Nil
            case Symbol(s) => Symbol(s)
            case Number(_) => expr
            case Str(_) => expr
            case Boolean(_) => expr
            case Cons(cell) => this.expandBackquoteCons(cell)
            case _ => expr
        }
    }

    /**
     * 处理 ConsCell 类型的 backquote 展开
     */
    private func expandBackquoteCons(cell: ConsCell): LispValue {
        // 检查是否是 comma 或 comma-at 表达式
        match (cell.car) {
            case Symbol("comma") => this.evalCommaExpr(cell.cdr)
            case Symbol("comma-at") => this.evalCommaAtExpr(cell.cdr)
            case _ => this.backquoteList(cell)
        }
    }

    /**
     * 求值 comma 表达式
     */
    private func evalCommaExpr(cdr: LispValue): LispValue {
        match (cdr) {
            case Nil => Nil
            case Cons(restCell) => this.eval(restCell.car)
            case _ => Nil
        }
    }

    /**
     * 求值 comma-at 表达式
     */
    private func evalCommaAtExpr(cdr: LispValue): LispValue {
        match (cdr) {
            case Nil => Nil
            case Cons(restCell) => this.eval(restCell.car)
            case _ => Nil
        }
    }

    /**
     * 处理 backquote 中的列表
     * 对每个元素递归调用 expandBackquote，然后根据 comma-at 决定是否拼接
     */
    private func backquoteList(cell: ConsCell): LispValue {
        if (cell.isNil()) {
            Nil
        } else {
            let carResult = this.expandBackquote(cell.car)
            this.processBackquoteCdr(carResult, cell.cdr)
        }
    }

    /**
     * 处理 backquote 列表的 cdr 部分
     */
    private func processBackquoteCdr(carResult: LispValue, cdr: LispValue): LispValue {
        match (cdr) {
            case Nil => consValue(carResult, Nil)
            case Cons(cdrCell) => this.checkCommaAtInCdr(carResult, cdrCell)
            case _ => consValue(carResult, Nil)
        }
    }

    /**
     * 检查 cdr 中是否有 comma-at
     */
    private func checkCommaAtInCdr(carResult: LispValue, cdrCell: ConsCell): LispValue {
        match (cdrCell.car) {
            case Cons(innerCell) => this.handleCommaAtInCdr(carResult, cdrCell, innerCell)
            case _ => this.buildNormalCons(carResult, cdrCell)
        }
    }

    /**
     * 检查 cdr 中是否有 comma-at
     */
    private func handleCommaAtInCdr(carResult: LispValue, cdrCell: ConsCell, innerCell: ConsCell): LispValue {
        match (innerCell.car) {
            case Symbol("comma-at") => this.spliceCommaAt(carResult, innerCell, cdrCell)
            case _ => this.buildNormalCons(carResult, cdrCell)
        }
    }

    /**
     * 构建普通的 cons 对
     */
    private func buildNormalCons(carResult: LispValue, cdrCell: ConsCell): LispValue {
        let cdrResult = this.backquoteList(cdrCell)
        consValue(carResult, cdrResult)
    }

    /**
     * 拼接 comma-at 的结果
     */
    private func spliceCommaAt(carResult: LispValue, innerCell: ConsCell, cdrCell: ConsCell): LispValue {
        match (innerCell.cdr) {
            case Nil => consValue(carResult, this.backquoteList(cdrCell))
            case Cons(restCell) => this.evalAndSplice(carResult, restCell.car, cdrCell)
            case _ => consValue(carResult, this.backquoteList(cdrCell))
        }
    }

    /**
     * 求值并拼接
     */
    private func evalAndSplice(carResult: LispValue, expr: LispValue, cdrCell: ConsCell): LispValue {
        let spliceValue = this.eval(expr)
        match (spliceValue) {
            case Nil => consValue(carResult, Nil)
            case Cons(_) => consValue(carResult, spliceValue)
            case _ => consValue(carResult, Nil)
        }
    }

    /**
     * 特殊形式：comma
     * 只在 backquote 内有效，单独使用时返回错误
     */
    func evalComma(expr: LispValue): LispValue {
        Str("Error: comma used outside of backquote")
    }

    /**
     * 特殊形式：comma-at
     * 只在 backquote 内有效，单独使用时返回错误
     */
    func evalCommaAt(expr: LispValue): LispValue {
        Str("Error: comma-at used outside of backquote")
    }

    /**
     * 特殊形式：define
     */
    func evalDefine(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr) {
            let first = cell.car
            let rest = cell.cdr

            if (let Symbol(name) <- first && let Cons(restCell) <- rest) {
                if (restCell.length() == 1) {
                    let value = this.eval(restCell.car)
                    this.env.define(name, value)
                    value
                } else {
                    Nil
                }
            } else if (let Cons(paramsCell) <- first && let Symbol(name) <- paramsCell.car) {
                let params = this.extractSymbols(paramsCell.cdr)
                let body = rest
                let proc = Procedure(params, body, this.env)
                this.env.define(name, proc)
                proc
            } else {
                Nil
            }
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：set!
     */
    func evalSet(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && let Symbol(name) <- cell.car && let Cons(restCell) <- cell.cdr) {
            if (restCell.length() == 1) {
                let value = this.eval(restCell.car)
                let success = this.env.set(name, value)
                if (success) {
                    value
                } else {
                    Nil
                }
            } else {
                Nil
            }
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：if
     */
    func evalIf(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && let Cons(restCell) <- cell.cdr) {
            let testExpr = cell.car
            let thenExpr = restCell.car
            let elseExpr = restCell.cdr

            let testResult = this.eval(testExpr)

            if (testResult.isTruthy()) {
                this.eval(thenExpr)
            } else {
                if (let Cons(elseCell) <- elseExpr && !elseCell.isNil()) {
                    this.eval(elseCell.car)
                } else {
                    this.eval(elseExpr)
                }
            }
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：lambda
     */
    func evalLambda(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr) {
            let paramsExpr = cell.car
            let body = cell.cdr

            if (let Cons(_) <- paramsExpr) {
                let params = this.extractSymbols(paramsExpr)
                Procedure(params, body, this.env)
            } else {
                Procedure(ArrayList<String>(), body, this.env)
            }
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：begin
     */
    func evalBegin(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr) {
            var result: LispValue = Nil
            var current: LispValue = Cons(cell)

            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    result = this.eval(currCell.car)
                    current = currCell.cdr
                } else {
                    break
                }
            }

            result
        } else {
            this.eval(expr)
        }
    }

    /**
     * 特殊形式：let
     */
    func evalLet(expr: LispValue): LispValue {
        match (expr) {
            case Cons(cell) =>
                let bindingsExpr = cell.car
                let body = cell.cdr

                let letEnv = this.env.createChild()

                match (bindingsExpr) {
                    case Cons(bindingsCell) =>
                        this.processBindings(bindingsCell, letEnv)
                    case _ => ()
                }

                let oldEnv = this.env
                this.env = letEnv
                let result = this.evalBegin(body)
                this.env = oldEnv

                result
            case _ => Nil
        }
    }

    /**
     * 特殊形式：let* (顺序绑定)
     * 将嵌套的 let* 展开为嵌套的 let
     */
    func evalLetStar(expr: LispValue): LispValue {
        match (expr) {
            case Cons(cell) =>
                let bindingsExpr = cell.car
                let body = cell.cdr

                match (bindingsExpr) {
                    case Nil =>
                        // 没有绑定，直接执行 body
                        this.evalBegin(body)
                    case Cons(bindingsCell) =>
                        this.expandLetStar(bindingsCell, body)
                    case _ =>
                        Nil
                }
            case _ => Nil
        }
    }

    /**
     * 展开 let* 绑定
     * 递归地将 ((a 1) (b 2)) 展开为 (let ((a 1)) (let* ((b 2)) body))
     */
    private func expandLetStar(bindingsCell: ConsCell, body: LispValue): LispValue {
        let firstBinding = bindingsCell.car
        let restBindings = bindingsCell.cdr

        if (restBindings.isNil()) {
            // 最后一个绑定，直接返回 let
            let bindingsList = consValue(firstBinding, Nil)
            let letExpr = consValue(Symbol("let"), consValue(bindingsList, body))
            this.eval(letExpr)
        } else {
            // 还有更多绑定，创建嵌套的 let*
            // 外层 let 绑定第一个变量
            let bindingsList = consValue(firstBinding, Nil)
            // 内层 let* 处理剩余的绑定
            let innerLetStar = consValue(Symbol("let*"), consValue(restBindings, body))
            let innerBody = consValue(innerLetStar, Nil)
            let letExpr = consValue(Symbol("let"), consValue(bindingsList, innerBody))
            this.eval(letExpr)
        }
    }

    /**
     * 特殊形式：when-let* (条件+顺序绑定)
     * 如果所有绑定成功（最后一个值为真），则执行 body
     * 语法：(when-let* ((var1 expr1) (var2 expr2) ...) body...)
     */
    func evalWhenLetStar(expr: LispValue): LispValue {
        match (expr) {
            case Cons(cell) =>
                let bindingsExpr = cell.car
                let body = cell.cdr

                match (bindingsExpr) {
                    case Nil =>
                        // 没有绑定，返回 nil
                        Nil
                    case Cons(bindingsCell) =>
                        // 创建新的作用域
                        let whenLetEnv = this.env.createChild()
                        let oldEnv = this.env
                        this.env = whenLetEnv

                        var lastValue: LispValue = Nil
                        var hasError = false

                        // 按顺序绑定变量
                        var current: LispValue = Cons(bindingsCell)
                        while (!current.isNil()) {
                            match (current) {
                                case Cons(currCell) =>
                                    let binding = currCell.car
                                    match (binding) {
                                        case Cons(bindingCell) =>
                                            match (bindingCell.car) {
                                                case Symbol(varName) =>
                                                    match (bindingCell.cdr) {
                                                        case Cons(valueCell) =>
                                                            let value = this.eval(valueCell.car)

                                                            // 检查是否有错误
                                                            match (value) {
                                                                case Str(s) =>
                                                                    if (s.startsWith("Error:")) {
                                                                        hasError = true
                                                                    }
                                                                case _ => ()
                                                            }

                                                            if (!hasError) {
                                                                whenLetEnv.define(varName, value)
                                                                lastValue = value
                                                            }
                                                        case _ => ()
                                                    }
                                                case _ => ()
                                            }
                                        case _ => ()
                                    }
                                    current = currCell.cdr
                                case _ => break
                            }

                            if (hasError) {
                                break
                            }
                        }

                        // 恢复环境
                        this.env = oldEnv

                        // 检查最后一个值是否为真
                        if (!hasError && lastValue.isTruthy()) {
                            // 在新环境中执行 body
                            this.env = whenLetEnv
                            let result = this.evalBegin(body)
                            this.env = oldEnv
                            result
                        } else {
                            Nil
                        }
                    case _ => Nil
                }
            case _ => Nil
        }
    }

    /**
     * 特殊形式：if-let (条件+解构绑定)
     * 如果绑定成功且值为真，则执行 then，否则执行 else
     * 语法：(if-let (var value) then else)
     */
    func evalIfLet(expr: LispValue): LispValue {
        match (expr) {
            case Cons(cell) =>
                // (if-let (var value) then else)
                let binding = cell.car      // (var value)
                let rest = cell.cdr

                match (binding) {
                    case Cons(bindingCell) =>
                        let varSymbol = bindingCell.car      // var
                        let valueExpr = bindingCell.cdr     // (value) - should be single value

                        match (valueExpr) {
                            case Cons(valueCell) =>
                                let value = valueCell.car  // value

                                // 提取 then 和 else 分支
                                match (rest) {
                                    case Cons(restCell) =>
                                        let thenExpr = restCell.car
                                        let elseExpr = restCell.cdr

                                        // 展开为 (let ((var value)) (if var then else))
                                        let bindingsList = consValue(consValue(varSymbol, consValue(value, Nil)), Nil)
                                        let ifExpr = consValue(Symbol("if"), consValue(varSymbol, consValue(thenExpr, elseExpr)))
                                        let letExpr = consValue(Symbol("let"), consValue(bindingsList, consValue(ifExpr, Nil)))

                                        this.eval(letExpr)
                                    case _ => Nil
                                }
                            case _ => Nil
                        }
                    case _ => Nil
                }
            case _ => Nil
        }
    }
}
