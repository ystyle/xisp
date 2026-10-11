package ystyle::xisp.core.eval
import ystyle::xisp.types.*

/**
 * 核心求值逻辑扩展
 * 包含主入口和列表求值分发
 */
extend Evaluator {
    /**
     * 对表达式求值（主入口）
     * 编译纪律：
     * - 只有顶层表达式（当前求值树的根）才尝试编译；
     * - 编译失败、宏调用、或已回退 AST 的子树，禁止本树内再编译
     *   （AST 局部变量不在编译帧槽中，子表达式编译后按全局引用会产生 UnboundVariable）
     */
    public func eval(expr: LispValue): LispValue {
        // 中断检查点（1/4）：所有表达式求值都经过这里，是覆盖面最广的一处。
        // 未设置检查回调时 isInterrupted() 是常数 false，无额外开销。
        if (this.isInterrupted()) {
            return this.interruptError()
        }
        let isRoot = this.evalDepth == 0
        this.evalDepth = this.evalDepth + 1
        var result: LispValue = Nil
        try {
            result = this.evalInner(expr, isRoot)
        } finally {
            this.evalDepth = this.evalDepth - 1
        }
        result
    }

    /**
     * 求值内部实现（含编译分派）
     */
    func evalInner(expr: LispValue, isRoot: Bool): LispValue {
        if (isRoot) {
            this.compileSuspended = false
        }
        if (!this.compileSuspended && let Some(b) <- this.backend) {
            // 关键字别名必须先于编译解析：编译器不认识别名表，`setq` 会被当普通函数
            // 调用（环境里并无此绑定）→ **静默什么都不做**，脚本悄悄算错。
            // 解析只动头位置（见 alias_for_compile.cj）。
            var toCompile = expr
            match (this.resolveHeadAlias(expr)) {
                case Some(resolved) => toCompile = resolved
                case None => ()
            }
            // 让位优先于编译：若表达式头是"已被用户重定义"的可让位符号
            // （map / filter / reduce / apply / eval / -> / for-each），必须回退 AST
            // —— 否则后端会按 core 的语义编译它，用户实现被绕过
            // （实测：用户 define apply 后 AST 走用户实现，BC 仍返回 core 的结果）。
            if (this.headIsDeferred(toCompile)) {
                this.compileSuspended = true
            } else if (this.containsMacroCall(toCompile) || this.containsAliasCall(toCompile)) {
                // 宏是语法层的东西：必须在编译前展开，否则会被后端编译成"运行时调宏"
                // （每次调用重新展开，且参数已被求值 → 与 AST 语义可能不等价）。
                // 展开后重新走本函数，让展开产物正常编译。
                // 先做零分配探测：绝大多数表达式不含宏，不付展开成本。
                let expanded = this.expandForCompile(toCompile)
                match (expanded) {
                    case Error(_) => return expanded
                    case _ =>
                        if (!this.isMacroCallExpr(expanded)) {
                            // 展开产物里可能仍有别名（expandForCompile 已一并解析，
                            // 但宏产出可能新增别名），此处再兜一次头位置
                            var finalExpr = expanded
                            match (this.resolveHeadAlias(expanded)) {
                                case Some(r) => finalExpr = r
                                case None => ()
                            }
                            if (let Some(code) <- b.compile(finalExpr)) {
                                return b.execute(code, this.env)
                            }
                        }
                        // 仍含宏（如 defmacro 体等边界内）或编译失败：走 AST
                        this.compileSuspended = true
                }
            } else if (let Some(code) <- b.compile(toCompile)) {
                return b.execute(code, this.env)
            } else {
                // 编译失败：回退 AST，本树禁用后续编译
                this.compileSuspended = true
            }
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
     * 判断表达式是否为宏调用（编译前必须先展开宏，否则宏调用会被编译成
     * 普通函数调用并在运行时反复展开，导致无限递归）
     */
    func isMacroCallExpr(expr: LispValue): Bool {
        match (expr) {
            case Cons(cell) =>
                if (let Symbol(sym) <- cell.car) {
                    let resolvedSym = this.env.lookupKeyword(sym)
                    match (this.env.lookup(resolvedSym)) {
                        case Macro(_, _, _) | MacroFromParams(_, _, _) => true
                        case _ => false
                    }
                } else {
                    false
                }
            case _ => false
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
     * 特殊形式分派
     * @return 如果符号是特殊形式，返回 Some(结果)；否则返回 None
     *
     * 先以原始符号匹配（不查别名链），命中则直接分派。
     * 避免对每个特殊形式符号（如 + < if 等）都遍历环境链查询关键字别名。
     */
    private func trySpecialForm(sym: String, rest: LispValue): Option<LispValue> {
        // 让位：map / filter / reduce / apply / eval / -> 若已被用户重新定义，
        // 则不拦截，交给普通调用路径（判据与缓存见 defer_special_form.cj）。
        // 算术符号（+ - * / < > =）的让位走各自 evalXxx 内部的 isNativeArith 守卫。
        if (this.shouldDeferToUser(sym)) {
            return None
        }

        // 权限检查覆盖特殊形式（能力域设计 §5.4 接缝 4）
        //
        // 此前 `funcChecker` 只挂在普通调用路径（`evalFunctionCallWithValue`），
        // 而特殊形式在分派时从不经过那里 —— 于是 `withSandbox()` 明确声明的
        // `blockFunctions(["eval", "apply"])` **实测拦不住**（`(eval …)` / `(apply …)`
        // 照常执行），而③类注册函数（如 `length`）能被正常拦住。
        //
        // 特殊形式是**能绕过检查**的那一类（`registerPure` 的文档注释已论证：
        // `eval`/`apply`/`map`/`filter`/`reduce`/`for-each` 会先于检查执行），
        // 因此这里必须显式检查，否则沙箱的核心意图失效。
        if (this.specialFormBlocked(sym)) {
            return Some(LispValue.Error(XispError(ErrorType.UndefinedFunction,
                "Function '${sym}' is not allowed")))
        }

        match (sym) {
            case "+" => Some(this.evalAdd(rest))
            case "-" => Some(this.evalSub(rest))
            case "*" => Some(this.evalMul(rest))
            case "/" => Some(this.evalDiv(rest))
            case "<" => Some(this.evalLt(rest))
            case ">" => Some(this.evalGt(rest))
            case "=" => Some(this.evalEq(rest))
            case "if" => Some(this.evalIf(rest))
            case "quote" => Some(this.evalQuote(rest))
            case "backquote" => Some(this.evalBackquote(rest))
            case "comma" => Some(this.evalComma(rest))
            case "comma-at" => Some(this.evalCommaAt(rest))
            case "define" => Some(this.evalDefine(rest))
            case "defmacro" => Some(this.evalDefmacro(rest))
            case "set!" => Some(this.evalSet(rest))
            case "lambda" => Some(this.evalLambda(rest))
            case "begin" => Some(this.evalBegin(rest))
            case "let" => Some(this.evalLet(rest))
            case "apply" => Some(this.evalApply(rest))
            case "eval" => Some(this.evalEval(rest))
            case "->" => Some(this.evalThreadFirst(rest))
            case "map" => Some(this.evalMap(rest))
            case "filter" => Some(this.evalFilter(rest))
            case "reduce" => Some(this.evalReduce(rest))
            case "for-each" => Some(this.evalForEach(rest))
            case "match" => Some(this.evalMatch(rest))
            case "macroexpand" => Some(this.evalMacroexpand(rest))
            case "macroexpand-all" => Some(this.evalMacroexpandAll(rest))
            case "import" => Some(this.evalImport(rest))
            case "export" => Some(this.evalExport(rest))
            case _ => None
        }
    }

    /**
     * evalList 的内部实现（分发到各个特殊形式）
     */
    func evalListInternal(cell: ConsCell): LispValue {
        let first = cell.car
        let rest = cell.cdr

        match (first) {
            case Symbol(sym) =>
                // 快速路径：以原始符号直接匹配特殊形式，不查别名链
                if (let Some(result) <- this.trySpecialForm(sym, rest)) {
                    return result
                }
                // 慢路径：符号可能是关键字别名，查链
                let actualSym = this.env.lookupKeyword(sym)
                if (actualSym != sym) {
                    // 是别名，按实际符号再次分派
                    if (let Some(result) <- this.trySpecialForm(actualSym, rest)) {
                        return result
                    }
                    // 别名映射到非特殊形式，走普通调用
                    let actualFirst = LispValue.Symbol(actualSym)
                    let value = this.env.lookup(actualSym)
                    match (value) {
                        case Macro(_, _, _) | MacroFromParams(_, _, _) =>
                            let expanded = this.expandMacro(value, rest)
                            this.eval(expanded)
                        case _ =>
                            this.evalFunctionCallWithValue(actualFirst, rest, value)
                    }
                } else {
                    // 非特殊形式：先查是否为宏
                    let value = this.env.lookup(actualSym)
                    match (value) {
                        case Macro(_, _, _) | MacroFromParams(_, _, _) =>
                            let expanded = this.expandMacro(value, rest)
                            this.eval(expanded)
                        case _ =>
                            this.evalFunctionCallWithValue(first, rest, value)
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
                // 中断检查点（2/4）：宿主函数可以长时间阻塞（等锁、等外部事件），
                // 求值器无法打断它，但必须保证它返回后立刻有机会停下。
                if (this.isInterrupted()) {
                    return this.injectStack(this.interruptError())
                }
                let nativeOut = f(args)
                if (this.isInterrupted()) {
                    return this.injectStack(this.interruptError())
                }
                nativeOut
            case Closure(_, _) =>
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
