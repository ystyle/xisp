package ystyle::xisp.core

import std.collection.ArrayList

/**
 * 辅助方法扩展
 * 包含提取符号、处理绑定、解构等辅助方法
 */
extend Evaluator {
    /**
     * 从 ConsCell 中提取符号列表
     */
    public func extractSymbols(expr: LispValue): ArrayList<String> {
        let result = ArrayList<String>()
        var current: LispValue = expr

        while (!current.isNil()) {
            if (let Cons(cell) <- current) {
                if (let Symbol(s) <- cell.car) {
                    result.add(s)
                }
                current = cell.cdr
            } else {
                break
            }
        }

        result
    }

    /**
     * 处理 let 绑定列表
     */
    func processBindings(bindings: ConsCell, env: Environment) {
        var current: LispValue = Cons(bindings)

        while (!current.isNil()) {
            if (let Cons(currCell) <- current) {
                let binding = currCell.car

                if (let Cons(bindingCell) <- binding) {
                    let pattern = bindingCell.car
                    let valueExpr = if (let Cons(cdrCell) <- bindingCell.cdr) { cdrCell.car } else { bindingCell.cdr }

                    let value = this.eval(valueExpr)

                    match (pattern) {
                        case Symbol(name) => env.define(name, value)
                        case Cons(_) => this.destructurePattern(pattern, value, env)
                        case _ => ()
                    }
                }

                current = currCell.cdr
            } else {
                break
            }
        }
    }

    /**
     * 解构模式匹配
     */
    func destructurePattern(pattern: LispValue, value: LispValue, env: Environment): Unit {
        match (pattern) {
            case Cons(patternCell) =>
                match (value) {
                    case Cons(valueCell) =>
                        match (patternCell.car) {
                            case Symbol(sym) => env.define(sym, valueCell.car)
                            case Cons(subPattern) => this.destructurePattern(Cons(subPattern), valueCell.car, env)
                            case _ => ()
                        }

                        match (patternCell.cdr) {
                            case Symbol(restSym) => env.define(restSym, valueCell.cdr)
                            case Cons(restPatternCell) =>
                                match (valueCell.cdr) {
                                    case Cons(restValueCell) =>
                                        this.destructurePattern(Cons(restPatternCell), Cons(restValueCell), env)
                                    case _ => ()
                                }
                            case _ => ()
                        }
                    case Nil => ()
                    case _ => ()
                }
            case _ => ()
        }
    }
}
