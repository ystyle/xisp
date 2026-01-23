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
}
