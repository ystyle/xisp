package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 特殊形式扩展
 * 包含 quote, define, set!, if, lambda, begin, let
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
