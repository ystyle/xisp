package ystyle::xisp.core

/**
 * 模式匹配扩展
 * 包含 match 表达式及相关方法
 */
extend Evaluator {
    /**
     * 特殊形式：match
     */
    func evalMatch(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr) {
            let value = this.eval(cell.car)
            var clauses = cell.cdr

            while (!clauses.isNil()) {
                if (let Cons(clauseCell) <- clauses) {
                    let clause = clauseCell.car
                    let matchEnv = this.matchPatternValue(value, clause, this.env)

                    match (matchEnv) {
                        case Some(env) =>
                            let nextClauses = clauseCell.cdr
                            if (let Cons(resultCell) <- nextClauses) {
                                return this.evalInEnv(resultCell.car, env)
                            } else {
                                return value
                            }
                        case None =>
                            clauses = clauseCell.cdr
                    }
                } else {
                    break
                }
            }

            Str("Error: No matching pattern for value: ${value.toString()}")
        } else {
            Str("Error: Invalid match syntax")
        }
    }

    /**
     * 匹配模式值
     */
    func matchPatternValue(value: LispValue, pattern: LispValue, env: Environment): ?Environment {
        match (pattern) {
            case Symbol("_") => Some(env)
            case Symbol(s) =>
                let newEnv = env.createChild()
                newEnv.define(s, value)
                Some(newEnv)
            case Number(n) =>
                if (let Number(v) <- value && v == n) { Some(env) } else { None<Environment> }
            case Str(s) =>
                if (let Str(v) <- value && v == s) { Some(env) } else { None<Environment> }
            case Boolean(b) =>
                if (let Boolean(v) <- value && v == b) { Some(env) } else { None<Environment> }
            case Nil =>
                if (value.isNil()) { Some(env) } else { None<Environment> }
            case Cons(_) =>
                if (value.isList()) {
                    this.matchListPattern(value, pattern, env)
                } else {
                    None<Environment>
                }
            case _ => None<Environment>
        }
    }

    /**
     * 匹配列表模式
     */
    func matchListPattern(value: LispValue, pattern: LispValue, env: Environment): ?Environment {
        var currentEnv = env
        var valueList: LispValue = value
        var patternList: LispValue = pattern

        while (!patternList.isNil()) {
            if (let Cons(patternCell) <- patternList) {
                let patternElem = patternCell.car

                match (patternElem) {
                    case Symbol("&") =>
                        if (let Cons(restPattern) <- patternCell.cdr) {
                            match (restPattern.car) {
                                case Symbol(restVar) =>
                                    let newEnv = currentEnv.createChild()
                                    newEnv.define(restVar, valueList)
                                    return Some(newEnv)
                                case _ => return None
                            }
                        } else {
                            return None
                        }
                    case _ =>
                        if (valueList.isNil()) {
                            return None
                        }

                        match (valueList) {
                            case Cons(vCell) =>
                                let valueElem = vCell.car
                                match (patternElem) {
                                    case Symbol(sym) =>
                                        match (sym) {
                                            case "_" => ()
                                            case _ => currentEnv.define(sym, valueElem)
                                        }
                                    case _ =>
                                        match (this.matchPatternValue(valueElem, patternElem, currentEnv)) {
                                            case Some(newEnv) => currentEnv = newEnv
                                            case None => return None
                                        }
                                }
                                valueList = vCell.cdr
                                patternList = patternCell.cdr
                            case _ => return None
                        }
                }
            } else {
                break
            }
        }

        if (patternList.isNil() && valueList.isNil()) {
            Some(currentEnv)
        } else {
            None
        }
    }
}
