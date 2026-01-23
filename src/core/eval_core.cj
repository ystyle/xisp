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
            case Number(_) => expr
            case Str(_) => expr
            case Boolean(_) => expr
            case Nil => expr
            case Symbol(s) => this.env.lookup(s)
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
            if (let Macro(_, _, _) <- value) {
                // 这是一个宏，展开并重新求值
                let expanded = this.expandMacro(value, cell.cdr)
                // 对展开后的表达式求值
                this.eval(expanded)
            } else {
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
            return Str("Error: Maximum stack depth exceeded (limit: ${this.maxStackDepth})")
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
                } else if (actualSym == "apply") {
                    return this.evalApply(rest)
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
                } else {
                    let actualFirst = if (sym != actualSym) {
                        Symbol(actualSym)
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
                            return Str("Error: Function '${funcName}' is not allowed")
                        }
                    case None => ()
                }
            case _ => ()
        }

        let funcValue = this.eval(first)

        match (funcValue) {
            case Procedure(_, _, _) =>
                let args = this.evalArguments(rest)
                // 检查参数中是否有错误
                for (arg in args) {
                    match (arg) {
                        case Str(s) =>
                            if (s.startsWith("Error:")) {
                                return arg
                            }
                        case _ => ()
                    }
                }
                let result = this.applyProcedure(funcValue, args)
                // 检查是否返回错误
                match (result) {
                    case Str(s) =>
                        if (s.startsWith("Error:")) {
                            result  // 传播错误
                        } else {
                            result
                        }
                    case _ => result
                }
            case NativeFunc(f) =>
                let args = this.evalArguments(rest)
                // 检查参数中是否有错误
                for (arg in args) {
                    match (arg) {
                        case Str(s) =>
                            if (s.startsWith("Error:")) {
                                return arg
                            }
                        case _ => ()
                    }
                }
                f(args)
            case _ => Nil
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
