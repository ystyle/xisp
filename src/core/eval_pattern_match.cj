package ystyle::xisp.core
import ystyle::xisp.types.*
import std.collection.ArrayList
import std.collection.HashMap

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
                // 使用 Tuple 模式一次性匹配 car 和 cdr
                match ((cell.car, cell.cdr)) {
                    case (Symbol("when"), Cons(rest)) =>
                        // (when guard expr...)
                        (true, rest.car, rest.cdr)
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

    /**
     * 获取下一个子句的表达式并跳过
     * @return (剩余子句, 获取的表达式或None)
     */
    private func getNextAndSkip(clauses: LispValue): (LispValue, ?LispValue) {
        if (let Cons(restClauses) <- clauses) {
            if (let Cons(nextClauses) <- restClauses.cdr) {
                (Cons(nextClauses.clone()), Some(nextClauses.car))
            } else {
                (Nil, None)
            }
        } else {
            (Nil, None)
        }
    }

    /**
     * 处理列表模式匹配成功后的结果求值
     */
    private func evalListPatternMatchResult(clauses: LispValue, matchedEnv: Environment): ?LispValue {
        if (let Cons(restClauses) <- clauses && let Cons(nextClauseCell) <- restClauses.cdr) {
            let resultExpr = nextClauseCell.car
            Some(this.evalInEnv(resultExpr, matchedEnv))
        } else {
            Some(Nil)
        }
    }
    private func isPatternElement(elem: LispValue): Bool {
        match (elem) {
            case Symbol(_) => true
            case Cons(_) => true
            case LispValue.Int(_) => true  // 数字也算作模式（用于列表模式中的常量）
            case LispValue.Float(_) => true  // 数字也算作模式（用于列表模式中的常量）
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
                var skipped = false  // 标记是否已经跳过了子句
                if (let Cons(clauseCell) <- clauses) {
                    let clause = clauseCell.car

                    // 检查 clause 是否是列表形式
                    match (clause) {
                        case Cons(clauseCons) =>
                            // 列表形式：可能是 (pattern result) 或 (pattern body1 body2...)
                            // 特殊情况：如果 clause 是 (x y) 这样的列表，且 rest 为空，
                            // 应该把整个 clause 当作列表模式，结果在下一个元素
                            let rest = clauseCons.cdr

                            // 检查是否是守卫条件单独在一个括号内的格式：(pattern when guard)
                            // 格式特征：when 是倒数第二个元素，后面跟着一个守卫表达式
                            let isGuardClause = if (let Cons(restCell) <- rest && let Symbol("when") <- restCell.car) {
                                // when 存在，检查 when 后面是否只有一个元素（守卫表达式）
                                if (let Cons(afterWhen) <- restCell.cdr && afterWhen.cdr.isNil()) {
                                    true  // (pattern when guard) 格式
                                } else {
                                    false  // when 后面有多个元素，不是这种格式
                                }
                            } else {
                                false
                            }

                            // 判断是否是列表模式
                            // 条件：rest 不为空（即这是一个多元素列表）
                            let needNextResult = if (!rest.isNil()) {
                                // 可能的情况：
                                // 1. (x y z body...) - 多个符号，整个是列表模式
                                // 2. (x y) "ok" - 两个符号，(x y)是列表模式，"ok"在下一个元素
                                // 3. (x "result") - first是模式，rest是结果
                                // 4. {:key val} - HashMap 模式
                                // 5. (x when guard) - 守卫条件单独在一个括号内

                                // 首先检查是否是 HashMap 模式
                                let isHashMapPattern = match (clauseCons.car) {
                                    case Symbol("hashmap") => true
                                    case _ => false
                                }

                                if (isGuardClause) {
                                    // 守卫条件单独在一个括号内：(x when guard) - 结果在下一个元素
                                    false  // 使用单元素模式，守卫条件在 rest 中
                                } else if (isHashMapPattern) {
                                    true  // HashMap 模式，整个clause是模式，结果在下一个元素
                                } else if (this.isPatternElement(clauseCons.car)) {
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
                                    if (allPatterns) {
                                        // 检查是否有守卫条件在开头 - 使用 if-let
                                        if (let Cons(restCell) <- rest && let Symbol("when") <- restCell.car) {
                                            false  // 使用单元素模式
                                        } else {
                                            true  // 列表模式
                                        }
                                    } else {
                                        false  // 使用第一个元素作为pattern
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
                                match (matchEnv) {
                                    case Some(matchedEnv) =>
                                        // 匹配成功，获取并求值下一个元素
                                        if (let Some(result) <- this.evalListPatternMatchResult(clauses, matchedEnv)) {
                                            return result
                                        }
                                    case None =>
                                        // 不匹配，需要跳过下一个元素（结果表达式）
                                        let (newClauses, _) = this.getNextAndSkip(clauses)
                                        clauses = newClauses
                                }
                            } else {
                                // 正常处理：pattern 是模式，rest 是结果表达式（或者守卫条件）

                                // 检查是否有守卫条件 when
                                let (hasGuard, guard, actualBody) = this.extractGuard(rest)

                                let matchEnv = this.matchPattern(value, pattern, this.env)
                                match (matchEnv) {
                                    case Some(matchedEnv) =>
                                        if (hasGuard) {
                                            let guardResult = this.evalInEnv(guard, matchedEnv)
                                            if (guardResult.isTruthy()) {
                                                // 守卫条件成功，执行结果表达式
                                                if (isGuardClause && actualBody.isNil()) {
                                                    // (x when guard) 格式：结果在下一个子句
                                                    if (let Some(result) <- this.evalListPatternMatchResult(clauses, matchedEnv)) {
                                                        return result
                                                    }
                                                } else {
                                                    // (x when guard expr...) 格式：结果在 actualBody
                                                    return this.evalBody(actualBody, matchedEnv)
                                                }
                                            } else {
                                                // 守卫条件失败，跳过下一个子句（结果表达式）并继续
                                                let (newClauses, _) = this.getNextAndSkip(clauses)
                                                clauses = newClauses
                                                skipped = true
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
                            case Symbol(_) => true     // 变量符号，总是匹配
                            case LispValue.Int(n) => this.compareNumbers(value, clause)
                            case LispValue.Float(f) => this.compareNumbers(value, clause)
                            case Str(s) => if (let Str(v) <- value && v == s) { true } else { false }
                            case Boolean(b) => if (let Boolean(v) <- value && v == b) { true } else { false }
                            case Nil => value.isNil()
                            case _ => false
                        }


                        if (isMatch) {
                            // 匹配成功，处理结果表达式
                            if (let Symbol(s) <- clause) {
                                // 符号处理
                                if (s == "_") {
                                    // 通配符：返回下一个表达式的值
                                    if (let Cons(resultCell) <- clauseCell.cdr) {
                                        return this.eval(resultCell.car)
                                    } else {
                                        return value
                                    }
                                } else {
                                    // 变量绑定：创建新环境并执行下一个表达式
                                    if (let Cons(resultCell) <- clauseCell.cdr) {
                                        let newEnv = this.env.createChild()
                                        newEnv.define(s, value)
                                        return this.evalInEnv(resultCell.car, newEnv)
                                    } else {
                                        return value
                                    }
                                }
                            } else {
                                // 常量：返回下一个表达式的值
                                if (let Cons(resultCell) <- clauseCell.cdr) {
                                    return this.eval(resultCell.car)
                                } else {
                                    return value
                                }
                            }
                        } else {
                            // 不匹配，需要跳过下一个元素（结果表达式）
                            let (newClauses, _) = this.getNextAndSkip(clauses)
                            clauses = newClauses
                        }
                    }  // 关闭 match (clause)

                    if (!skipped) {
                        clauses = clauseCell.cdr
                    }
                    clauseIndex = clauseIndex + 1
                } else {
                    break
                }
            }

            // 没有匹配的分支
            LispValue.Error(XispError(ErrorType.RuntimeError, "No matching pattern for value: ${value.toString()}"))
        } else {
            LispValue.Error(XispError(ErrorType.SyntaxError, "Invalid match syntax"))
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

            // 常量匹配 - 使用 compareNumbers 辅助函数
            case LispValue.Int(n) =>
                if (this.compareNumbers(value, pattern)) {
                    Some(env)
                } else {
                    None<Environment>
                }
            case LispValue.Float(n) =>
                if (this.compareNumbers(value, pattern)) {
                    Some(env)
                } else {
                    None<Environment>
                }
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
                // 检查是否是 HashMap 模式 {:key1 var1 :key2 var2}
                if (let Symbol("hashmap") <- patternCell.car) {
                    this.matchHashMapPattern(value, patternCell, env)
                } else if (value.isList()) {
                    this.matchListPattern(value, patternCell, env)
                } else {
                    None<Environment>
                }

            // 向量模式 [x y z]
            // 注意：向量在解析时转换为 (cangjie::vector ...)
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
        let isVector = if (let Cons(p) <- patternList && let Symbol("cangjie::vector") <- p.car) {
            true
        } else {
            false
        }

        if (isVector) {
            // 向量模式：提取向量元素
            if (let Cons(vecCall) <- patternList) {
                // 向量参数列表是vecCall.cdr（去掉cangjie::vector后的部分）
                patternList = vecCall.cdr

                // 值也应该是向量，或者是普通列表（因为cangjie::vector函数返回列表）
                if (let Cons(valueVecCall) <- value &&
                    let Symbol("cangjie::vector") <- valueVecCall.car &&
                    let Cons(valueVecArgs) <- valueVecCall.cdr) {
                    // 值是向量形式，提取参数
                    valueList = valueVecArgs.car
                } else {
                    // 值不是向量形式，可能是普通列表（cangjie::vector函数返回的）
                    // 直接使用这个值作为valueList
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
                if (let Symbol("&") <- patternElem) {
                    // 找到 rest 变量
                    if (let Cons(restPattern) <- patternCell.cdr &&
                        let Symbol(restVar) <- restPattern.car) {
                        let newEnv = currentEnv.createChild()
                        newEnv.define(restVar, valueList)
                        return Some(newEnv)
                    } else {
                        return None
                    }
                } else {
                    // 普通元素匹配 - 需要 valueList 有元素
                    if (valueList.isNil()) {
                        return None  // 模式还有元素，但值列表已空，匹配失败
                    }

                    if (let Cons(vCell) <- valueList) {
                        let valueElem = vCell.car

                        // 特殊处理：如果是 Symbol，直接在当前环境中绑定，避免创建嵌套环境
                        if (let Symbol(sym) <- patternElem) {
                            if (sym != "_") {
                                // 变量绑定：直接在当前环境中定义
                                currentEnv.define(sym, valueElem)
                            }
                            // 通配符 "_"：不做任何事
                        } else {
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
                    } else {
                        return None
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
     * 匹配 HashMap 模式
     * 模式格式：{:key1 var1 :key2 var2}
     * 解析为：(hashmap (quote :key1) var1 (quote :key2) var2)
     *
     * 语义：
     * - {:key symbol} - symbol 是变量，绑定任何值
     * - {:key value} - value 是常量，需要精确匹配
     */
    /**
     * 匹配 HashMap 模式中的单个键值对
     * @return (成功标志, 新环境)
     */
    private func matchHashMapKeyValue(valueMap: HashMap<String, LispValue>, key: String, patternValue: LispValue, currentEnv: Environment): (Bool, ?Environment) {
        match ((valueMap.get(key), patternValue)) {
            case (Some(v), Symbol(varName)) =>
                // 变量：创建子环境并绑定
                let newEnv = currentEnv.createChild()
                newEnv.define(varName, v)
                (true, Some(newEnv))
            case (Some(v), Str(s)) =>
                // 字符串常量：需要精确匹配
                if (let Str(vs) <- v && vs == s) {
                    (true, Some(currentEnv))
                } else {
                    (false, None)
                }
            case (Some(v), Int(n)) =>
                // 整数常量：需要精确匹配
                if (let Int(vn) <- v && vn == n) {
                    (true, Some(currentEnv))
                } else {
                    (false, None)
                }
            case (Some(v), _) =>
                // 其他常量类型：使用 matchPatternValue
                if (this.matchPatternValue(v, patternValue)) {
                    (true, Some(currentEnv))
                } else {
                    (false, None)
                }
            case (None, _) =>
                // 键不存在，匹配失败
                (false, None)
        }
    }

    private func matchHashMapPattern(value: LispValue, pattern: ConsCell, env: Environment): ?Environment {
        // value 必须是 HashMap
        if (let HashMap(valueMap) <- value) {
            // 从模式中提取键值对
            let bindings = this.extractHashMapBindings(Cons(pattern))

            // 创建新环境并绑定变量
            var currentEnv = env
            var i = 0
            while (i < bindings.size) {
                if (let (key, patternValue) <- bindings[i]) {
                    let (success, newEnv) = this.matchHashMapKeyValue(valueMap, key, patternValue, currentEnv)
                    if (!success) {
                        return None
                    }
                    if (let Some(env) <- newEnv) {
                        currentEnv = env
                    }
                }
                i = i + 1
            }

            // 所有键都匹配成功
            Some(currentEnv)
        } else {
            // value 不是 HashMap，匹配失败
            None
        }
    }

    /**
     * 比较两个数字是否相等（Int 和 Float 可以混合比较）
     */
    private func compareNumbers(left: LispValue, right: LispValue): Bool {
        match ((left, right)) {
            case (LispValue.Int(l), LispValue.Int(r)) => l == r
            case (LispValue.Int(l), LispValue.Float(r)) => Float64(l) == r
            case (LispValue.Float(l), LispValue.Int(r)) => l == Float64(r)
            case (LispValue.Float(l), LispValue.Float(r)) => l == r
            case _ => false
        }
    }

    /**
     * 简单值匹配（不绑定变量）
     */
    private func matchPatternValue(value: LispValue, pattern: LispValue): Bool {
        match (pattern) {
            case Symbol("_") => true
            case LispValue.Int(n) => this.compareNumbers(value, pattern)
            case LispValue.Float(f) => this.compareNumbers(value, pattern)
            case Str(s) => if (let Str(v) <- value) { v == s } else { false }
            case Boolean(b) => if (let Boolean(v) <- value) { v == b } else { false }
            case Nil => value.isNil()
            case _ =>
                // 尝试深度比较（交换参数）
                if (isNumber(value)) {
                    this.compareNumbers(value, pattern)
                } else {
                    match (value) {
                        case Str(v) => if (let Str(p) <- pattern) { v == p } else { false }
                        case Boolean(v) => if (let Boolean(p) <- pattern) { v == p } else { false }
                        case _ => false
                    }
                }
        }
    }
}
