package ystyle::xisp.core.eval
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
        // 尝试编译（默认返回 None，走 AST 解释）
        if (let Some(code) <- this.compiler.compile(expr)) {
            return this.vm.execute(code, this.env)
        }
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
            case Cons(cell) => this.doEvalList(cell)
            case _ => expr
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

                match (actualSym) {
                    case "+" => return this.evalAdd(rest)
                    case "-" => return this.evalSub(rest)
                    case "*" => return this.evalMul(rest)
                    case "/" => return this.evalDiv(rest)
                    case "<" => return this.evalLt(rest)
                    case ">" => return this.evalGt(rest)
                    case "=" => return this.evalEq(rest)
                    case "if" => return this.evalIf(rest)
                    case "quote" => return this.evalQuote(rest)
                    case "backquote" => return this.evalBackquote(rest)
                    case "comma" => return this.evalComma(rest)
                    case "comma-at" => return this.evalCommaAt(rest)
                    case "define" => return this.evalDefine(rest)
                    case "defmacro" => return this.evalDefmacro(rest)
                    case "set!" => return this.evalSet(rest)
                    case "lambda" => return this.evalLambda(rest)
                    case "begin" => return this.evalBegin(rest)
                    case "let" => return this.evalLet(rest)
                    case "let*" => return this.evalLetStar(rest)
                    case "when-let*" => return this.evalWhenLetStar(rest)
                    case "if-let" => return this.evalIfLet(rest)
                    case "condb" => return this.evalCondb(rest)
                    case "apply" => return this.evalApply(rest)
                    case "eval" => return this.evalEval(rest)
                    case "->" => return this.evalThreadFirst(rest)
                    case "map" => return this.evalMap(rest)
                    case "filter" => return this.evalFilter(rest)
                    case "reduce" => return this.evalReduce(rest)
                    case "match" => return this.evalMatch(rest)
                    case "macroexpand" => return this.evalMacroexpand(rest)
                    case "macroexpand-all" => return this.evalMacroexpandAll(rest)
                    case "import" => return this.evalImport(rest)
                    case "export" => return this.evalExport(rest)
                    case _ =>
                        // 非特殊形式：先查是否为宏
                        let value = this.env.lookup(actualSym)
                        let actualFirst = if (sym != actualSym) {
                            LispValue.Symbol(actualSym)
                        } else {
                            first
                        }
                        match (value) {
                            case Macro(_, _, _) | MacroFromParams(_, _, _) =>
                                let expanded = this.expandMacro(value, rest)
                                this.eval(expanded)
                            case _ =>
                                this.evalFunctionCallWithValue(actualFirst, rest, value)
                        }
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
     * 函数调用（先求值函数位置）
     */
    func evalFunctionCall(first: LispValue, rest: LispValue): LispValue {
        let funcValue = this.eval(first)
        this.evalFunctionCallWithValue(first, rest, funcValue)
    }

    /**
     * 函数调用（使用已解析的函数值，避免重复 lookup）
     */
    func evalFunctionCallWithValue(first: LispValue, rest: LispValue, funcValue: LispValue): LispValue {
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
