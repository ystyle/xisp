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
    func evalCons(expr: LispValue, cell: ConsCell): LispValue {
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
     * 函数调用
     */
    func evalFunctionCall(first: LispValue, rest: LispValue): LispValue {
        match (first) {
            case Symbol(funcName) =>
                match (this.funcChecker) {
                    case Some(checker) =>
                        if (!checker(funcName)) {
                            return LispValue.Error(XispError(ErrorType.UndefinedFunction, "Function '${funcName}' is not allowed"))
                        }
                    case None => ()
                }
            case _ => ()
        }

        let funcValue = this.eval(first)

        // 检查 funcValue 是否是错误
        match (funcValue) {
            case Error(err) =>
                // 如果是未定义变量错误，且在函数调用位置，转换为未定义函数错误
                match (err.kind) {
                    case UnboundVariable =>
                        // 区分是变量还是函数：如果是函数调用位置，转换为函数错误
                        let name = if (let Symbol(sym) <- first) { sym } else { first.toString() }
                        return LispValue.Error(XispError(ErrorType.UndefinedFunction, "Undefined function: '${name}'"))
                    case _ => return funcValue  // 其他错误直接传播
                }
            case _ => ()
        }

        match (funcValue) {
            case Macro(_, _, _) | MacroFromParams(_, _, _) =>
                // 这是一个宏，展开它并递归求值
                let expanded = this.expandMacro(funcValue, rest)
                this.eval(expanded)
            case Procedure(_, _, _) | ProcedureFromParams(_, _, _) =>
                let args = this.evalArguments(rest)
                // 检查参数中是否有错误
                for (arg in args) {
                    match (arg) {
                        case Error(_) => return arg  // 传播第一个错误
                        case _ => ()
                    }
                }
                let result = this.applyProcedure(funcValue, args)
                // 检查是否返回错误
                match (result) {
                    case Error(_) => result  // 传播错误
                    case _ => result
                }
            case NativeFunc(f) =>
                let args = this.evalArguments(rest)
                // 检查参数中是否有错误
                for (arg in args) {
                    match (arg) {
                        case Error(_) => return arg  // 传播第一个错误
                        case _ => ()
                    }
                }
                f(args)
            case Nil =>
                // 函数未定义（符号定义为 nil 后作为函数调用）
                if (let Symbol(funcName) <- first) {
                    LispValue.Error(XispError(ErrorType.UndefinedFunction, "Undefined function: '${funcName}'"))
                } else {
                    LispValue.Error(XispError(ErrorType.TypeError, "Attempt to call a non-function value: ${first.toString()}"))
                }
            case _ =>
                // 不是可调用的类型
                LispValue.Error(XispError(ErrorType.TypeError, "Attempt to call a non-function value: ${first.toString()}"))
        }
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
