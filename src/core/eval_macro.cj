package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 宏系统扩展
 * 包含宏展开机制、defmacro、macroexpand
 */
extend Evaluator {
    /**
     * 辅助函数：获取 LispValue 的类型名称（用于调试）
     */
    func typeOfLispValue(value: LispValue): String {
        match (value) {
            case Nil => "Nil"
            case Boolean(_) => "Boolean"
            case LispValue.Int(_) => "Int"
            case LispValue.Float(_) => "Float"
            case Str(_) => "Str"
            case Symbol(_) => "Symbol"
            case Cons(_) => "Cons"
            case Procedure(_, _, _) => "Procedure"
            case ProcedureFromParams(_, _, _) => "ProcedureFromParams"
            case Macro(_, _, _) => "Macro"
            case MacroFromParams(_, _, _) => "MacroFromParams"
            case NativeFunc(_) => "NativeFunc"
            case HashMap(_) => "HashMap"
            case _ => "Unknown"
        }
    }
    /**
     * 设置最大宏展开深度
     */
    public func setMaxMacroExpandDepth(depth: Int64) {
        this.maxMacroExpandDepth = depth
    }

    /**
     * 展开宏（一次）
     * 只展开顶层宏，不递归展开子表达式
     */
    func macroExpand(expr: LispValue): LispValue {
        if (this.macroExpandDepth >= this.maxMacroExpandDepth) {
            return LispValue.Error(XispError(ErrorType.RuntimeError, "Maximum macro expand depth exceeded (limit: ${this.maxMacroExpandDepth})"))
        }

        match (expr) {
            case Nil => expr
            case Boolean(_) => expr
            case LispValue.Int(_) => expr
            case LispValue.Float(_) => expr
            case Str(_) => expr
            case Symbol(_) => expr
            case Cons(cell) => this.macroExpandConsCell(cell)
            case _ => expr
        }
    }

    /**
     * 展开 ConsCell
     */
    func macroExpandConsCell(cell: ConsCell): LispValue {
        if (let Symbol(sym) <- cell.car) {
            let resolvedSym = this.env.lookupKeyword(sym)
            let value = this.env.lookup(resolvedSym)
            if (let Macro(_, _, _) <- value || let MacroFromParams(_, _, _) <- value) {
                // 这是一个宏，展开它（不递归展开子表达式）
                this.macroExpandDepth = this.macroExpandDepth + 1
                let expanded = this.expandMacro(value, cell.cdr)
                this.macroExpandDepth = this.macroExpandDepth - 1
                expanded
            } else if (resolvedSym == "quote" || resolvedSym == "quasiquote") {
                // 检查 quoted 表达式是否包含宏调用
                if (let Cons(quoted) <- cell.cdr) {
                    // quoted.car 包含整个 quoted 表达式，例如 (id 42)
                    // 我们需要展开 quoted.car（这是一个 LispValue）
                    let quotedExpr = quoted.car

                    // 对 quotedExpr 调用 macroExpand
                    let expanded = this.macroExpand(quotedExpr)

                    // 检查是否展开了（expanded 是否与 quotedExpr 不同）
                    if (this.equal(quotedExpr, expanded)) {
                        // 没有展开，保留 quote
                        Cons(cell)
                    } else {
                        // 展开了，返回展开后的结果（去掉 quote）
                        expanded
                    }
                } else {
                    Cons(cell)
                }
            } else {
                // 不是宏，返回原表达式
                Cons(cell)
            }
        } else {
            // car 不是符号，返回原表达式
            Cons(cell)
        }
    }

    /**
     * 递归展开 cdr
     */
    func macroExpandCdr(cdr: LispValue): LispValue {
        match (cdr) {
            case Nil => Nil
            case Cons(currCell) => this.macroExpandCdrConsCell(currCell)
            case _ => cdr
        }
    }

    /**
     * 辅助：展开 cdr 中的 ConsCell
     */
    func macroExpandCdrConsCell(currCell: ConsCell): LispValue {
        let expandedCar = this.macroExpand(currCell.car)
        let expandedCdr = this.macroExpandCdr(currCell.cdr)
        consValue(expandedCar, expandedCdr)
    }

    /**
     * 完全展开所有宏
     */
    func macroExpandAll(expr: LispValue): LispValue {
        var expanded = this.macroExpand(expr)
        var lastExpanded = expr
        var iteration = 0
        let maxIterations = 1000

        while (!this.equal(expanded, lastExpanded) && iteration < maxIterations) {
            lastExpanded = expanded
            expanded = this.macroExpand(expanded)
            iteration = iteration + 1
        }

        if (iteration >= maxIterations) {
            LispValue.Error(XispError(ErrorType.RuntimeError, "Macro expansion did not terminate after ${maxIterations} iterations"))
        } else {
            expanded
        }
    }

    /**
     * 展开宏
     */
    func expandMacro(macroValue: LispValue, args: LispValue): LispValue {
        match (macroValue) {
            case Macro(params, body, macroEnv) =>
                let newEnv = macroEnv.createChild()
                this.bindMacroParams(params, args, newEnv)

                let oldEnv = this.env
                this.env = newEnv

                // body 是一个 cons cell (类似 begin 块)，需要按顺序求值
                // 检查是否是单个表达式 (cdr 是 nil)
                let result = if (let Cons(bodyCell) <- body) {
                    if (bodyCell.cdr.isNil()) {
                        // 单个表达式
                        let expr = bodyCell.car
                        if (this.inMacroExpandCall && this.isBackquoteExpr(expr)) {
                            // 在 macroexpand 调用中，且是 backquote 表达式
                            // 展开 backquote 但不执行结果
                            this.expandBackquoteInMacro(expr)
                        } else {
                            // 正常求值
                            this.eval(expr)
                        }
                    } else {
                        // 多个表达式，按顺序求值（类似 begin）
                        this.evalBegin(body)
                    }
                } else {
                    // body 不是 cons，直接求值
                    this.eval(body)
                }

                this.env = oldEnv
                result
            case MacroFromParams(paramInfos, body, macroEnv) =>
                let newEnv = macroEnv.createChild()
                this.bindMacroParamsFromParamInfo(paramInfos, args, newEnv)

                let oldEnv = this.env
                this.env = newEnv

                // body 是一个 cons cell (类似 begin 块)，需要按顺序求值
                // 检查是否是单个表达式 (cdr 是 nil)
                let result = if (let Cons(bodyCell) <- body) {
                    if (bodyCell.cdr.isNil()) {
                        // 单个表达式
                        let expr = bodyCell.car
                        if (this.inMacroExpandCall && this.isBackquoteExpr(expr)) {
                            // 在 macroexpand 调用中，且是 backquote 表达式
                            // 展开 backquote 但不执行结果
                            this.expandBackquoteInMacro(expr)
                        } else {
                            // 正常求值
                            this.eval(expr)
                        }
                    } else {
                        // 多个表达式，按顺序求值（类似 begin）
                        this.evalBegin(body)
                    }
                } else {
                    // body 不是 cons，直接求值
                    this.eval(body)
                }

                this.env = oldEnv
                result
            case _ => LispValue.Error(XispError(ErrorType.TypeError, "expandMacro called with non-macro value"))
        }
    }

    /**
     * 检查表达式是否是 backquote 表达式
     */
    private func isBackquoteExpr(expr: LispValue): Bool {
        match (expr) {
            case Cons(cell) =>
                match (cell.car) {
                    case Symbol("backquote") => true
                    case _ => false
                }
            case _ => false
        }
    }

    /**
     * 在宏展开模式下展开 backquote 表达式
     * 展开 backquote 结构但保留 lambda 等形式不执行
     */
    private func expandBackquoteInMacro(expr: LispValue): LispValue {
        match (expr) {
            case Cons(cell) =>
                match (cell.car) {
                    case Symbol("backquote") =>
                        // 展开内部但保留 lambda 等形式
                        if (let Cons(backquoted) <- cell.cdr) {
                            this.expandBackquoteWithoutEval(backquoted.car)
                        } else {
                            Nil
                        }
                    case _ => expr
                }
            case _ => expr
        }
    }

    /**
     * 展开 backquote 但不对 lambda 调用求值
     */
    private func expandBackquoteWithoutEval(expr: LispValue): LispValue {
        // 使用 expandBackquote 展开，它会处理 comma 和 comma-at
        // 但不会执行最终的 lambda 调用
        this.expandBackquote(expr)
    }

    /**
     * 绑定宏参数到环境
     */
    func bindMacroParams(params: ArrayList<String>, args: LispValue, env: Environment) {
        var currentArg: LispValue = args
        var i = 0

        while (i < params.size) {
            let param = params[i]

            // 检查是否为可变参数（带 &rest- 前缀）
            if (param.startsWith("&rest-")) {
                let restName = param[6..]  // 去掉 "&rest-" 前缀
                // 可变参数：将剩余所有参数绑定为一个列表
                env.define(restName, currentArg)
                break
            } else {
                // 普通参数：绑定单个参数
                currentArg = this.bindOneMacroParam(param, currentArg, env)
                i = i + 1
            }
        }
    }

    /**
     * 绑定宏参数到环境（使用 ParamInfo）
     */
    func bindMacroParamsFromParamInfo(paramInfos: ArrayList<ParamInfo>, args: LispValue, env: Environment) {
        var currentArg: LispValue = args
        var i = 0

        while (i < paramInfos.size) {
            let paramInfo = paramInfos[i]

            if (paramInfo.isRest) {
                // 可变参数：将剩余所有参数绑定为一个列表
                env.define(paramInfo.name, currentArg)
                break
            } else {
                // 普通参数或命名参数：绑定单个参数
                // 对于宏，命名参数也当作普通参数处理（因为宏接收的是未求值的语法树）
                currentArg = this.bindOneMacroParam(paramInfo.name, currentArg, env)
                i = i + 1
            }
        }
    }

    /**
     * 绑定单个宏参数
     */
    func bindOneMacroParam(param: String, arg: LispValue, env: Environment): LispValue {
        if (let Cons(argCell) <- arg) {
            env.define(param, argCell.car)
            argCell.cdr
        } else {
            env.define(param, arg)
            Nil
        }
    }

    /**
     * defmacro 特殊形式
     */
    func evalDefmacro(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr) {
            if (cell.length() < 3) {
                return LispValue.Error(XispError(ErrorType.ArityMismatch, "defmacro requires at least 2 arguments"))
            }
            return this.evalDefmacroWithName(cell)
        } else {
            LispValue.Error(XispError(ErrorType.SyntaxError, "defmacro invalid syntax"))
        }
    }

    /**
     * evalDefmacro 辅助：已验证有 cell
     */
    func evalDefmacroWithName(cell: ConsCell): LispValue {
        if (let Symbol(name) <- cell.car) {
            if (let Cons(restCell) <- cell.cdr) {
                let paramsExpr = restCell.car
                let paramInfos = this.extractParamsWithRest(paramsExpr)
                let body = restCell.cdr

                // 检查是否有命名参数或默认值
                let hasKeyOrDefault = this.hasKeyOrDefaultParams(paramInfos)

                let m = if (hasKeyOrDefault) {
                    // 求值所有默认值表达式
                    let evaluatedParamInfos = ArrayList<ParamInfo>()
                    for (param in paramInfos) {
                        let evaluatedDefault = match (param.defaultValue) {
                            case Some(defaultExpr) => Some(this.eval(defaultExpr))
                            case None => None<LispValue>
                        }
                        evaluatedParamInfos.add(ParamInfo(param.name, param.isRest, param.isKey, evaluatedDefault))
                    }
                    macroFromParamsValue(evaluatedParamInfos, body, this.env)
                } else {
                    // 使用旧的参数格式（向后兼容）
                    let params = this.extractParams(paramsExpr)
                    macroValue(params, body, this.env)
                }

                this.env.define(name, m)
                Symbol(name)
            } else {
                LispValue.Error(XispError(ErrorType.SyntaxError, "defmacro invalid syntax"))
            }
        } else {
            LispValue.Error(XispError(ErrorType.SyntaxError, "defmacro first argument must be a symbol"))
        }
    }

    /**
     * 从参数表达式中提取参数名列表（旧版本，不包含可变参数信息）
     */
    func extractParams(paramsExpr: LispValue): ArrayList<String> {
        let paramInfos = this.extractParamsWithRest(paramsExpr)
        let params = ArrayList<String>()
        for (paramInfo in paramInfos) {
            // 为可变参数添加特殊前缀
            if (paramInfo.isRest) {
                params.add("&rest-" + paramInfo.name)
            } else {
                params.add(paramInfo.name)
            }
        }
        params
    }

    /**
     * 从参数表达式中提取参数信息列表（支持可变参数和命名参数）
     * 支持语法：
     * - Common Lisp 风格：(x y . rest)
     * - Scheme 风格：(x y &rest rest)
     * - 命名参数：(x y &key z w)
     * - 带默认值：(x y &key (z 0) (w 1))
     * - 纯可变参数：args (直接是一个符号，用于 (func . args) 的情况)
     */
    func extractParamsWithRest(paramsExpr: LispValue): ArrayList<ParamInfo> {
        let paramInfos = ArrayList<ParamInfo>()

        // 特殊情况：纯可变参数
        // 如果 paramsExpr 本身就是一个符号（不是 ConsCell），则表示纯可变参数
        // 例如：(define (func . args) ...) 中的 args
        if (let Symbol(restName) <- paramsExpr) {
            paramInfos.add(ParamInfo(restName, true, false, None<LispValue>))
            return paramInfos
        }

        var current: LispValue = paramsExpr
        var iteration = 0
        let maxIterations = 100
        var inKeySection = false  // 是否进入命名参数区域

        while (!current.isNil() && iteration < maxIterations) {
            iteration = iteration + 1
            if (let Cons(currCell) <- current) {
                let car = currCell.car

                // 检查是否为特殊标记
                if (let Symbol(marker) <- car) {
                    if (marker == "." || marker == "&rest") {
                        // 可变参数标记
                        current = currCell.cdr
                        if (let Cons(nextCell) <- current) {
                            if (let Symbol(restName) <- nextCell.car) {
                                paramInfos.add(ParamInfo(restName, true, false, None))
                            }
                            current = nextCell.cdr
                        } else if (let Symbol(restName) <- current) {
                            paramInfos.add(ParamInfo(restName, true, false, None))
                            current = Nil
                        }
                        break
                    } else if (marker == "&key") {
                        // 命名参数标记
                        inKeySection = true
                        current = currCell.cdr
                    } else {
                        // 普通符号参数
                        let (name, defaultVal) = if (inKeySection) {
                            // 在命名参数区域，检查是否有默认值
                            this.extractParamNameAndDefault(marker, currCell.cdr)
                        } else {
                            // 位置参数，没有默认值
                            (marker, None<LispValue>)
                        }
                        paramInfos.add(ParamInfo(name, false, inKeySection, defaultVal))

                        // 检查 cdr 是否为可变参数（点对情况）
                        if (let Symbol(restName) <- currCell.cdr) {
                            paramInfos.add(ParamInfo(restName, true, false, None<LispValue>))
                            break
                        }

                        current = currCell.cdr
                    }
                } else if (let Cons(paramCell) <- car && inKeySection) {
                    // 命名参数的列表形式：(param default)
                    if (let Symbol(paramName) <- paramCell.car) {
                        let defaultVal = if (let Cons(defaultCell) <- paramCell.cdr) {
                            Some(defaultCell.car)
                        } else {
                            None<LispValue>
                        }
                        paramInfos.add(ParamInfo(paramName, false, true, defaultVal))
                    }
                    current = currCell.cdr
                } else {
                    // 非符号，跳过
                    current = currCell.cdr
                }
            } else {
                break
            }
        }

        paramInfos
    }

    /**
     * 提取参数名和默认值
     * 用于命名参数区域
     * 返回：(参数名, 默认值表达式)
     */
    func extractParamNameAndDefault(symbol: String, cdr: LispValue): (String, ?LispValue) {
        // 检查下一个元素是否是一个列表，且以该符号开头
        // 如果是，则说明使用了 (name default) 语法
        if (let Cons(nextCell) <- cdr) {
            if (let Cons(nextCarCell) <- nextCell.car) {
                if (let Symbol(nextSym) <- nextCarCell.car && nextSym == symbol) {
                    // 这是一个 (name default) 列表
                    let defaultVal = if (let Cons(defaultCell) <- nextCarCell.cdr) {
                        Some(defaultCell.car)
                    } else {
                        None<LispValue>
                    }
                    return (symbol, defaultVal)
                }
            }
        }
        // 普通符号，没有默认值
        (symbol, None<LispValue>)
    }

    /**
     * macroexpand 函数
     */
    func evalMacroexpand(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && cell.length() == 1) {
            // 不求值参数，直接对表达式进行宏展开
            // (macroexpand '(my-let ...)) 应该返回展开后的语法树，不执行
            let oldFlag = this.inMacroExpandCall
            this.inMacroExpandCall = true
            let result = this.macroExpand(cell.car)
            this.inMacroExpandCall = oldFlag
            result
        } else {
            LispValue.Error(XispError(ErrorType.ArityMismatch, "macroexpand requires exactly 1 argument"))
        }
    }

    /**
     * macroexpand-all 函数
     */
    func evalMacroexpandAll(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && cell.length() == 1) {
            // 不求值参数，直接对表达式进行完全宏展开
            let oldFlag = this.inMacroExpandCall
            this.inMacroExpandCall = true
            let result = this.macroExpandAll(cell.car)
            this.inMacroExpandCall = oldFlag
            result
        } else {
            LispValue.Error(XispError(ErrorType.ArityMismatch, "macroexpand-all requires exactly 1 argument"))
        }
    }

    /**
     * 判断两个值是否相等
     */
    func equal(a: LispValue, b: LispValue): Bool {
        match (a) {
            case Nil => match (b) {
                case Nil => true
                case _ => false
            }
            case Boolean(b1) => match (b) {
                case Boolean(b2) => b1 == b2
                case _ => false
            }
            case LispValue.Int(n1) => match (b) {
                case LispValue.Int(n2) => n1 == n2
                case LispValue.Float(f) => Float64(n1) == f
                case _ => false
            }
            case LispValue.Float(f1) => match (b) {
                case LispValue.Int(n) => f1 == Float64(n)
                case LispValue.Float(f2) => f1 == f2
                case _ => false
            }
            case Str(s1) => match (b) {
                case Str(s2) => s1 == s2
                case _ => false
            }
            case Symbol(s1) => match (b) {
                case Symbol(s2) => s1 == s2
                case _ => false
            }
            case Cons(c1) => this.equalCons(c1, b)
            case _ => false
        }
    }

    /**
     * 比较 Cons 类型
     */
    func equalCons(c1: ConsCell, b: LispValue): Bool {
        match (b) {
            case Cons(c2) => this.equalConsBodies(c1, c2)
            case _ => false
        }
    }

    /**
     * 比较 Cons 类型的内容
     */
    func equalConsBodies(c1: ConsCell, c2: ConsCell): Bool {
        if (!this.equal(c1.car, c2.car)) {
            false
        } else {
            this.equal(c1.cdr, c2.cdr)
        }
    }
}
