package ystyle::xisp.core
import ystyle::xisp.types.*

/**
 * 核心求值逻辑扩展
 * 包含主入口和列表求值分发
 */
extend Evaluator {
    /**
     * 对表达式求值（主入口）
     */
    public func eval(expr: LispValue): LispValue {
        match (expr) {
            case LispValue.Int(_) => expr
            case LispValue.Float(_) => expr
            case Str(_) => expr
            case Boolean(_) => expr
            case Nil => expr
            case Symbol(s) =>
                // 关键字符号（以 : 开头）是自求值的
                if (s.size > 0 && s.startsWith(":")) {
                    Symbol(s)  // 直接返回符号本身
                } else {
                    let value = this.env.lookup(s)
                    // 检查是否是错误
                    match (value) {
                        case Error(_) => value  // 传播错误
                        case _ => value
                    }
                }
            case Cons(cell) => this.evalCons(expr, cell)
            case _ => expr
        }
    }

    /**
     * 求值 Cons 类型的表达式
     * 先展开宏，然后求值
     */
    func evalCons(_: LispValue, cell: ConsCell): LispValue {
        // 检查是否是宏调用
        if (let Symbol(sym) <- cell.car) {
            let resolvedSym = this.env.lookupKeyword(sym)
            let value = this.env.lookup(resolvedSym)
            // 检查是否是宏（忽略错误，当作不是宏处理）
            match (value) {
                case Macro(_, _, _) | MacroFromParams(_, _, _) =>
                    // 这是一个宏，展开并重新求值
                    let expanded = this.expandMacro(value, cell.cdr)
                    // 对展开后的表达式求值
                    this.eval(expanded)
                case Error(_) =>
                    // lookup 返回错误，说明符号未定义，继续正常求值流程
                    this.doEvalList(cell)
                case _ =>
                    // 不是宏，正常求值（会先求值参数）
                    this.doEvalList(cell)
            }
        } else {
            // car 不是符号，正常求值
            this.doEvalList(cell)
        }
    }

    /**
     * 求值列表（管理栈深度）
     */
    func doEvalList(cell: ConsCell): LispValue {
        if (cell.isNil()) {
            return Nil
        }

        if (this.stackDepth > this.maxStackDepth) {
            return LispValue.Error(XispError(ErrorType.RuntimeError, "Maximum stack depth exceeded (limit: ${this.maxStackDepth})"))
        }

        this.stackDepth = this.stackDepth + 1
        let result = this.evalListInternal(cell)
        this.stackDepth = this.stackDepth - 1

        result
    }

    /**
     * evalList 的内部实现（分发到各个特殊形式）
     */
    func evalListInternal(cell: ConsCell): LispValue {
        let first = cell.car
        let rest = cell.cdr

        match (first) {
            case Symbol(sym) =>
                let actualSym = this.env.lookupKeyword(sym)

                if (actualSym == "quote") {
                    return this.evalQuote(rest)
                } else if (actualSym == "backquote") {
                    return this.evalBackquote(rest)
                } else if (actualSym == "comma") {
                    return this.evalComma(rest)
                } else if (actualSym == "comma-at") {
                    return this.evalCommaAt(rest)
                } else if (actualSym == "define") {
                    return this.evalDefine(rest)
                } else if (actualSym == "defmacro") {
                    return this.evalDefmacro(rest)
                } else if (actualSym == "set!") {
                    return this.evalSet(rest)
                } else if (actualSym == "if") {
                    return this.evalIf(rest)
                } else if (actualSym == "lambda") {
                    return this.evalLambda(rest)
                } else if (actualSym == "begin") {
                    return this.evalBegin(rest)
                } else if (actualSym == "let") {
                    return this.evalLet(rest)
                } else if (actualSym == "let*") {
                    return this.evalLetStar(rest)
                } else if (actualSym == "when-let*") {
                    return this.evalWhenLetStar(rest)
                } else if (actualSym == "if-let") {
                    return this.evalIfLet(rest)
                } else if (actualSym == "condb") {
                    return this.evalCondb(rest)
                } else if (actualSym == "apply") {
                    return this.evalApply(rest)
                } else if (actualSym == "eval") {
                    return this.evalEval(rest)
                } else if (actualSym == "->") {
                    return this.evalThreadFirst(rest)
                } else if (actualSym == "map") {
                    return this.evalMap(rest)
                } else if (actualSym == "filter") {
                    return this.evalFilter(rest)
                } else if (actualSym == "reduce") {
                    return this.evalReduce(rest)
                } else if (actualSym == "match") {
                    return this.evalMatch(rest)
                } else if (actualSym == "macroexpand") {
                    return this.evalMacroexpand(rest)
                } else if (actualSym == "macroexpand-all") {
                    return this.evalMacroexpandAll(rest)
                } else if (actualSym == "import") {
                    return this.evalImport(rest)
                } else if (actualSym == "export") {
                    return this.evalExport(rest)
                } else {
                    let actualFirst = if (sym != actualSym) {
                        LispValue.Symbol(actualSym)
                    } else {
                        first
                    }
                    return this.evalFunctionCall(actualFirst, rest)
                }
            case _ =>
                this.evalFunctionCall(first, rest)
        }
    }

    /**
     * 从函数调用表达式中提取函数名称（用于调用栈追踪）
     */
    private func extractFuncName(first: LispValue): String {
        match (first) {
            case Symbol(name) => name
            case Procedure(_, _, _) => "<anonymous>"
            case _ => "<expression>"
        }
    }

    /**
     * 函数调用
     */
    func evalFunctionCall(first: LispValue, rest: LispValue): LispValue {
        let funcName = this.extractFuncName(first)

        // 权限检查
        match (first) {
            case Symbol(fn) =>
                match (this.funcChecker) {
                    case Some(checker) =>
                        if (!checker(fn)) {
                            return LispValue.Error(XispError(ErrorType.UndefinedFunction, "Function '${fn}' is not allowed"))
                        }
                    case None => ()
                }
            case _ => ()
        }

        // 压入调用栈帧
        this.pushFrame(funcName)

        let funcValue = this.eval(first)

        // 检查 funcValue 是否是错误
        match (funcValue) {
            case Error(err) =>
                // 先注入栈，再 pop
                match (err.kind) {
                    case UnboundVariable =>
                        let name = if (let Symbol(sym) <- first) { sym } else { first.toString() }
                        let result = this.injectStack(LispValue.Error(XispError(ErrorType.UndefinedFunction, "Undefined function: '${name}'")))
                        this.popFrame()
                        return result
                    case _ =>
                        let result = this.injectStack(funcValue)
                        this.popFrame()
                        return result
                }
            case _ => ()
        }

        let result = match (funcValue) {
            case Macro(_, _, _) | MacroFromParams(_, _, _) =>
                let expanded = this.expandMacro(funcValue, rest)
                this.eval(expanded)
            case Procedure(_, _, _) | ProcedureFromParams(_, _, _) =>
                let args = this.evalArguments(rest)
                for (arg in args) {
                    match (arg) {
                        case Error(_) => return this.injectStack(arg)
                        case _ => ()
                    }
                }
                let procResult = this.applyProcedure(funcValue, args)
                match (procResult) {
                    case Error(_) => this.injectStack(procResult)
                    case _ => procResult
                }
            case NativeFunc(f) =>
                let args = this.evalArguments(rest)
                for (arg in args) {
                    match (arg) {
                        case Error(_) => return this.injectStack(arg)
                        case _ => ()
                    }
                }
                f(args)
            case Nil =>
                if (let Symbol(fn) <- first) {
                    this.injectStack(LispValue.Error(XispError(ErrorType.UndefinedFunction, "Undefined function: '${fn}'")))
                } else {
                    this.injectStack(LispValue.Error(XispError(ErrorType.TypeError, "Attempt to call a non-function value: ${first.toString()}")))
                }
            case _ =>
                this.injectStack(LispValue.Error(XispError(ErrorType.TypeError, "Attempt to call a non-function value: ${first.toString()}")))
        }

        let finalResult = this.injectStack(result)
        this.popFrame()
        finalResult
    }

    /**
     * 在指定环境中求值
     */
    func evalInEnv(expr: LispValue, env: Environment): LispValue {
        let oldEnv = this.env
        this.env = env
        let result = this.eval(expr)
        this.env = oldEnv
        result
    }
}
