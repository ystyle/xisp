package ystyle::xisp.core
import ystyle::xisp.types.*

/**
 * 模式匹配扩展
 * 包含 match 表达式及相关方法
 */
extend Evaluator {
    /**
     * 提取守卫条件
     * @return (hasGuard, guardExpr, actualBody)
     */
    private func extractGuard(body: LispValue): (Bool, LispValue, LispValue) {
        match (body) {
            case Cons(cell) =>
                let first = cell.car
                match (first) {
                    case Symbol("when") =>
                        // (when guard expr...)
                        if (let Cons(rest) <- cell.cdr) {
                            let guard = rest.car
                            let actualBody = rest.cdr
                            (true, guard, actualBody)
                        } else {
                            (false, Nil, body)
                        }
                    case _ =>
                        (false, Nil, body)
                }
            case _ =>
                (false, Nil, body)
        }
    }

    /**
     * 执行体表达式
     */
    private func evalBody(body: LispValue, env: Environment): LispValue {
        var result: LispValue = Nil
        var current: LispValue = body

        while (!current.isNil()) {
            if (let Cons(currCell) <- current) {
                result = this.evalInEnv(currCell.car, env)
                current = currCell.cdr
            } else {
                break
            }
        }

        result
    }

    /**
     * 判断一个元素是否是模式元素
     */
    private func isPatternElement(elem: LispValue): Bool {
        match (elem) {
            case Symbol(_) => true
            case Cons(_) => true
            case Number(_) => true  // 数字也算作模式（用于列表模式中的常量）
            case _ => false  // 其他（字符串、布尔值）不是模式
        }
    }

    /**
     * 特殊形式：match
     */
    func evalMatch(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr) {
            // 求值要匹配的表达式
            let value = this.eval(cell.car)

            // 遍历所有模式分支
            var clauses = cell.cdr
            var clauseIndex = 0
            while (!clauses.isNil()) {
                if (let Cons(clauseCell) <- clauses) {
                    let clause = clauseCell.car

                    // 检查 clause 是否是列表形式
                    match (clause) {
                        case Cons(clauseCons) =>
                            // 列表形式：可能是 (pattern result) 或 (pattern body1 body2...)
                            // 特殊情况：如果 clause 是 (x y) 这样的列表，且 rest 为空，
                            // 应该把整个 clause 当作列表模式，结果在下一个元素
                            let rest = clauseCons.cdr

                            // 判断是否是列表模式
                            // 条件：rest 不为空（即这是一个多元素列表）
                            let needNextResult = if (!rest.isNil()) {
                                // 可能的情况：
                                // 1. (x y z body...) - 多个符号，整个是列表模式
                                // 2. (x y) "ok" - 两个符号，(x y)是列表模式，"ok"在下一个元素
                                // 3. (x "result") - first是模式，rest是结果

                                let areAllPatterns = if (this.isPatternElement(clauseCons.car)) {
                                    var allPatterns = true
                                    var current = rest
                                    while (!current.isNil() && allPatterns) {
                                        if (let Cons(c) <- current) {
                                            if (this.isPatternElement(c.car)) {
                                                current = c.cdr
                                            } else {
                                                allPatterns = false
                                            }
                                        } else {
                                            allPatterns = false
                                        }
                                    }
                                    allPatterns
                                } else {
                                    false
                                }


                                if (areAllPatterns) {
                                    // 检查是否有守卫条件
                                    let hasGuard = if (let Cons(restCell) <- rest) {
                                        match (restCell.car) {
                                            case Symbol("when") => true
                                            case _ => false
                                        }
                                    } else {
                                        false
                                    }

                                    if (hasGuard) {
                                        false  // 使用单元素模式
                                    } else {
                                        true  // 列表模式
                                    }
                                } else {
                                    false  // 使用第一个元素作为pattern
                                }
                            } else {
                                false
                            }

                            let pattern = if (needNextResult) {
                                // 列表模式，整个clause是pattern
                                clause
                            } else {
                                // 单元素模式
                                clauseCons.car
                            }



                            if (needNextResult) {
                                // 列表模式在 clause，结果在下一个元素
                                let matchEnv = this.matchPattern(value, pattern, this.env)
                                var needReturnValue: ?LispValue = None
                                match (matchEnv) {
                                    case Some(matchedEnv) =>
                                        // 获取下一个元素作为结果
                                        match (clauses) {
                                            case Cons(restClauses) =>
                                                match (restClauses.cdr) {
                                                    case Cons(nextClauseCell) =>
                                                        let resultExpr = nextClauseCell.car
                                                        let result = this.evalInEnv(resultExpr, matchedEnv)
                                                        needReturnValue = Some(result)
                                                    case _ =>
                                                        needReturnValue = Some(Nil)
                                                }
                                            case _ =>
                                                needReturnValue = Some(Nil)
                                        }
                                    case None =>
                                        // 不匹配，需要跳过下一个元素（结果表达式）
                                        match (clauses) {
                                            case Cons(restClauses) =>
                                                match (restClauses.cdr) {
                                                    case Cons(nextClauses) =>
                                                        clauses = Cons(nextClauses.clone())
                                                    case _ =>
                                                        clauses = Nil
                                                }
                                            case _ =>
                                                clauses = Nil
                                        }
                                }

                                // 检查是否需要返回值
                                match (needReturnValue) {
                                    case Some(v) =>
                                        return v
                                    case None => ()
                                }
                            } else {
                                // 正常处理：pattern 是模式，rest 是结果表达式

                                // 检查是否有守卫条件 when
                                let (hasGuard, guard, actualBody) = this.extractGuard(rest)

                                let matchEnv = this.matchPattern(value, pattern, this.env)
                                match (matchEnv) {
                                    case Some(matchedEnv) =>
                                        if (hasGuard) {
                                            let guardResult = this.evalInEnv(guard, matchedEnv)
                                            if (guardResult.isTruthy()) {
                                                return this.evalBody(actualBody, matchedEnv)
                                            }
                                        } else {
                                            // 执行结果表达式
                                            if (let Cons(restCell) <- rest) {
                                                if (restCell.cdr.isNil()) {
                                                    // 单个表达式
                                                    return this.evalInEnv(restCell.car, matchedEnv)
                                                } else {
                                                    // 多个表达式
                                                    return this.evalBody(rest, matchedEnv)
                                                }
                                            } else {
                                                return Nil
                                            }
                                        }
                                    case None =>
                                        // 不匹配，继续下一个循环（clauses 已经在下面更新）
                                        ()
                                }
                        }  // 关闭 case Cons(clauseCons) =>
                    case _ =>
                        // 简单值形式：pattern result
                        // 这里的 clause 是模式，下一个元素是结果
                        // 检查是否匹配（常量、符号、通配符）
                        let isMatch = match (clause) {
                            case Symbol("_") => true  // 通配符
                            case Symbol(s) => true     // 变量符号，总是匹配
                            case Number(n) => if (let Number(v) <- value) { v == n } else { false }
                            case Str(s) => if (let Str(v) <- value) { v == s } else { false }
                            case Boolean(b) => if (let Boolean(v) <- value) { v == b } else { false }
                            case Nil => value.isNil()
                            case _ => false
                        }


                        if (isMatch) {
                            // 匹配成功，处理结果表达式
                            // result表达式在下一个clause中，使用clauseCell.cdr获取
                            let nextClauses = clauseCell.cdr

                            match (clause) {
                                case Symbol(s) =>
                                    // 符号处理
                                    match (s) {
                                        case "_" =>
                                            // 通配符：返回下一个表达式的值
                                            match (nextClauses) {
                                                case Cons(resultCell) =>
                                                    let resultExpr = resultCell.car
                                                    return this.eval(resultExpr)
                                                case _ => return value
                                            }
                                        case _ =>
                                            // 变量绑定：创建新环境并执行下一个表达式
                                            match (nextClauses) {
                                                case Cons(resultCell) =>
                                                    let resultExpr = resultCell.car
                                                    let newEnv = this.env.createChild()
                                                    newEnv.define(s, value)
                                                    let result = this.evalInEnv(resultExpr, newEnv)
                                                    return result
                                                case _ =>
                                                    return value
                                            }
                                    }
                                case _ =>
                                    // 常量：返回下一个表达式的值
                                    match (nextClauses) {
                                        case Cons(resultCell) =>
                                            let resultExpr = resultCell.car
                                            return this.eval(resultExpr)
                                        case _ => return value
                                    }
                            }
                            // 不应该执行到这里，因为上面已经 return 了
                        } else {
                            // 不匹配，需要跳过下一个元素（结果表达式）
                            match (clauses) {
                                case Cons(restClauses) =>
                                    match (restClauses.cdr) {
                                        case Cons(nextClauses) =>
                                            clauses = Cons(nextClauses.clone())
                                        case _ =>
                                            // 没有更多元素了
                                            clauses = Nil
                                    }
                                case _ =>
                                    clauses = Nil
                            }
                        }
                    }  // 关闭 match (clause)

                    clauses = clauseCell.cdr
                    clauseIndex = clauseIndex + 1
                } else {
                    break
                }
            }

            // 没有匹配的分支
            let result = Str("Error: No matching pattern for value: ${value.toString()}")
            result
        } else {
            Str("Error: Invalid match syntax")
        }
    }

    /**
     * 匹配单个模式
     * @return 匹配成功返回新环境，失败返回 None
     */
    private func matchPattern(value: LispValue, pattern: LispValue, env: Environment): ?Environment {
        match (pattern) {
            // 通配符 _ - 匹配任何值
            case Symbol("_") => Some(env)

            // 常量匹配
            case Number(n) =>
                if (let Number(v) <- value && v == n) { Some(env) } else { None<Environment> }
            case Str(s) =>
                if (let Str(v) <- value && v == s) { Some(env) } else { None<Environment> }
            case Boolean(b) =>
                if (let Boolean(v) <- value && v == b) { Some(env) } else { None<Environment> }
            case Nil =>
                if (value.isNil()) { Some(env) } else { None<Environment> }

            // 变量绑定
            case Symbol(sym) =>
                let newEnv = env.createChild()
                newEnv.define(sym, value)
                Some(newEnv)

            // 列表模式 (x y z) 或 (x . y)
            case Cons(patternCell) =>
                if (value.isList()) {
                    this.matchListPattern(value, patternCell, env)
                } else {
                    None<Environment>
                }

            // 向量模式 [x y z]
            // 注意：向量在解析时转换为 (cangjie:vector ...)
            // 这里需要特殊处理
            case _ =>
                // 尝试作为值直接匹配
                if (this.matchPatternValue(value, pattern)) {
                    Some(env)
                } else {
                    None<Environment>
                }
        }
    }

    /**
     * 匹配列表模式
     * 支持：普通列表 (x y z)、点对 (x . y)、向量语法 [x y z]
     */
    func matchListPattern(value: LispValue, pattern: ConsCell, env: Environment): ?Environment {
        var currentEnv = env
        var valueList: LispValue = value
        var patternList: LispValue = Cons(pattern)

        // 检查是否是向量语法
        let isVector = if (let Cons(p) <- patternList) {
            match (p.car) {
                case Symbol("cangjie:vector") => true
                case _ => false
            }
        } else {
            false
        }

        if (isVector) {
            // 向量模式：提取向量元素
            if (let Cons(vecCall) <- patternList) {
                // 向量参数列表是vecCall.cdr（去掉cangjie:vector后的部分）
                patternList = vecCall.cdr

                // 值也应该是向量，或者是普通列表（因为cangjie:vector函数返回列表）
                if (let Cons(valueVecCall) <- value) {
                    match (valueVecCall.car) {
                        case Symbol("cangjie:vector") =>
                            // 值是向量形式，提取参数
                            if (let Cons(valueVecArgs) <- valueVecCall.cdr) {
                                valueList = valueVecArgs.car
                            }
                        case _ =>
                            // 值不是向量形式，可能是普通列表（cangjie:vector函数返回的）
                            // 直接使用这个值作为valueList
                            valueList = value
                    }
                } else {
                    // value不是Cons，可能是Nil或其他类型
                    valueList = value
                }
            }
        }

        // 遍历模式和值
        var iteration = 0
        while (!patternList.isNil()) {
            iteration = iteration + 1

            if (let Cons(patternCell) <- patternList) {
                let patternElem = patternCell.car

                // 检查是否有 & rest 语法（优先检查，不依赖 valueList）
                match (patternElem) {
                    case Symbol("&") =>
                        // 找到 rest 变量
                        if (let Cons(restPattern) <- patternCell.cdr) {
                            match (restPattern.car) {
                                case Symbol(restVar) =>
                                    let newEnv = currentEnv.createChild()
                                    newEnv.define(restVar, valueList)
                                    return Some(newEnv)
                                case _ =>
                                    return None
                            }
                        } else {
                            return None
                        }
                    case _ =>
                        // 普通元素匹配 - 需要 valueList 有元素
                        if (valueList.isNil()) {
                            return None  // 模式还有元素，但值列表已空，匹配失败
                        }

                        match (valueList) {
                            case Cons(vCell) =>
                                let valueElem = vCell.car

                                // 特殊处理：如果是 Symbol，直接在当前环境中绑定，避免创建嵌套环境
                                match (patternElem) {
                                    case Symbol(sym) =>
                                        match (sym) {
                                            case "_" =>
                                                // 通配符：不做任何事
                                                ()
                                            case _ =>
                                                // 变量绑定：直接在当前环境中定义
                                                currentEnv.define(sym, valueElem)
                                        }
                                    case _ =>
                                        // 其他模式：调用 matchPattern
                                        match (this.matchPattern(valueElem, patternElem, currentEnv)) {
                                            case Some(newEnv) =>
                                                currentEnv = newEnv
                                            case None =>
                                                return None
                                        }
                                }

                                // 前进到下一个元素
                                valueList = vCell.cdr

                                patternList = patternCell.cdr
                            case _ => return None
                        }
                }
            } else {
                break
            }
        }


        // 检查是否同时到达末尾
        if (patternList.isNil() && valueList.isNil()) {
            Some(currentEnv)
        } else {
            None
        }
    }

    /**
     * 简单值匹配（不绑定变量）
     */
    private func matchPatternValue(value: LispValue, pattern: LispValue): Bool {
        match (pattern) {
            case Symbol("_") => true
            case Number(n) => if (let Number(v) <- value) { v == n } else { false }
            case Str(s) => if (let Str(v) <- value) { v == s } else { false }
            case Boolean(b) => if (let Boolean(v) <- value) { v == b } else { false }
            case Nil => value.isNil()
            case _ =>
                // 尝试深度比较
                match (value) {
                    case Number(v) => if (let Number(p) <- pattern) { v == p } else { false }
                    case Str(v) => if (let Str(p) <- pattern) { v == p } else { false }
                    case Boolean(v) => if (let Boolean(p) <- pattern) { v == p } else { false }
                    case _ => false
                }
        }
    }
}
