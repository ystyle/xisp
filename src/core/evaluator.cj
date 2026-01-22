package ystyle::xisp.core

import std.collection.ArrayList

/**
 * 求值器 - Lisp 表达式求值
 */
public class Evaluator {
    private var env: Environment
    private var stackDepth: Int64 = 0
    private var maxStackDepth: Int64 = 1000  // 默认栈深度限制
    private var funcChecker: ?((String) -> Bool) = None  // 函数权限检查回调

    public init(env: Environment) {
        this.env = env
    }

    /**
     * 设置最大栈深度
     */
    public func setMaxStackDepth(depth: Int64) {
        this.maxStackDepth = depth
    }

    /**
     * 设置函数权限检查回调
     * @param checker 检查函数，返回 true 表示允许调用，false 表示禁止
     */
    public func setFuncChecker(checker: (String) -> Bool) {
        this.funcChecker = Some(checker)
    }

    /**
     * 对表达式求值
     */
    public func eval(expr: LispValue): LispValue {
        match (expr) {
            // 字面量直接返回
            case Number(_) => expr
            case Str(_) => expr
            case Boolean(_) => expr
            case Nil => expr

            // 符号查找变量
            case Symbol(s) => this.env.lookup(s)

            // 列表求值
            case Cons(cell) => this.evalList(cell)

            // 其他类型直接返回
            case _ => expr
        }
    }

    /**
     * 求值列表（函数调用或特殊形式）
     */
    private func evalList(cell: ConsCell): LispValue {
        // 空列表
        if (cell.isNil()) {
            return Nil
        }

        // 沙箱检查：栈深度
        if (this.stackDepth > this.maxStackDepth) {
            return Str("Error: Maximum stack depth exceeded (limit: ${this.maxStackDepth})")
        }

        // 增加栈深度
        this.stackDepth = this.stackDepth + 1

        // 执行求值
        let result = this.evalListInternal(cell)

        // 减少栈深度
        this.stackDepth = this.stackDepth - 1

        result
    }

    /**
     * evalList 的内部实现（不管理栈深度）
     */
    private func evalListInternal(cell: ConsCell): LispValue {
        let first = cell.car
        let rest = cell.cdr

        // 检查第一个元素是否为符号
        match (first) {
            case Symbol(sym) =>
                // 先查找关键字别名（支持多语言关键字）
                let actualSym = this.env.lookupKeyword(sym)

                // 处理特殊形式
                if (actualSym == "quote") {
                    return this.evalQuote(rest)
                } else if (actualSym == "define") {
                    return this.evalDefine(rest)
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
                    let matchResult = this.evalMatch(rest);
                    return matchResult
                } else {
                    // 函数调用：使用映射后的符号
                    // 如果符号被映射（如 "打印" -> "println"），使用映射后的符号
                    let actualFirst = if (sym != actualSym) {
                        Symbol(actualSym)
                    } else {
                        first
                    }
                    return this.evalFunctionCall(actualFirst, rest)
                }
            case _ =>
                // 第一个元素不是符号，尝试作为函数调用
                this.evalFunctionCall(first, rest)
        }
    }

    /**
     * 特殊形式：quote
     * (quote x) => 不求值直接返回 x
     */
    private func evalQuote(expr: LispValue): LispValue {
        match (expr) {
            case Cons(cell) =>
                if (cell.length() == 1) {
                    cell.car
                } else {
                    Nil
                }
            case _ => expr
        }
    }

    /**
     * 特殊形式：define
     * (define x 10) => 定义变量
     * (define (f x) body) => 定义函数
     */
    private func evalDefine(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr) {
            let first = cell.car
            let rest = cell.cdr

            if (let Symbol(name) <- first && let Cons(restCell) <- rest) {
                // (define x value)
                if (restCell.length() == 1) {
                    let value = this.eval(restCell.car)
                    this.env.define(name, value)
                    value
                } else {
                    Nil
                }
            } else if (let Cons(paramsCell) <- first && let Symbol(name) <- paramsCell.car) {
                // (define (f params) body)
                // 提取参数列表
                let params = this.extractSymbols(paramsCell.cdr)
                // 函数体是 rest
                let body = rest
                // 创建 lambda 过程
                let proc = Procedure(params, body, this.env)
                // 定义函数名
                this.env.define(name, proc)
                proc
            } else {
                Nil
            }
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：set!
     * (set! x value) => 修改已有变量
     */
    private func evalSet(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && let Symbol(name) <- cell.car && let Cons(restCell) <- cell.cdr) {
            if (restCell.length() == 1) {
                let value = this.eval(restCell.car)
                let success = this.env.set(name, value)
                if (success) {
                    value
                } else {
                    // 未定义变量
                    Nil
                }
            } else {
                Nil
            }
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：if
     * (if test then else) => 条件判断
     */
    private func evalIf(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && let Cons(restCell) <- cell.cdr) {
            let testExpr = cell.car
            let thenExpr = restCell.car
            let elseExpr = restCell.cdr

            // 求值条件
            let testResult = this.eval(testExpr)

            if (testResult.isTruthy()) {
                this.eval(thenExpr)
            } else {
                if (let Cons(elseCell) <- elseExpr && !elseCell.isNil()) {
                    this.eval(elseCell.car)
                } else {
                    this.eval(elseExpr)
                }
            }
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：lambda
     * (lambda (params) body) => 创建匿名函数
     */
    private func evalLambda(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr) {
            let paramsExpr = cell.car
            let body = cell.cdr

            if (let Cons(_) <- paramsExpr) {
                let params = this.extractSymbols(paramsExpr)
                Procedure(params, body, this.env)
            } else {
                // 无参数函数
                Procedure(ArrayList<String>(), body, this.env)
            }
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：begin
     * (begin expr1 expr2 ...) => 顺序执行，返回最后一个表达式的值
     */
    private func evalBegin(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr) {
            var result: LispValue = Nil
            var current: LispValue = Cons(cell)
            var iteration = 0

            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    result = this.eval(currCell.car)
                    current = currCell.cdr
                    iteration = iteration + 1
                } else {
                    break
                }
            }

            result
        } else {
            this.eval(expr)
        }
    }

    /**
     * 特殊形式：let
     * (let ([x 1] [y 2]) body) => 局部绑定
     */
    private func evalLet(expr: LispValue): LispValue {
        match (expr) {
            case Cons(cell) =>
                let bindingsExpr = cell.car
                let body = cell.cdr

                // 创建新的环境
                let letEnv = this.env.createChild()

                // 处理绑定列表
                match (bindingsExpr) {
                    case Cons(bindingsCell) =>
                        this.processBindings(bindingsCell, letEnv)
                    case _ => ()
                }

                // 在新环境中求值 body
                let oldEnv = this.env
                this.env = letEnv
                let result = this.evalBegin(body)
                this.env = oldEnv

                result
            case _ => Nil
        }
    }

    /**
     * 处理单个绑定
     */
    private func bindPattern(pattern: LispValue, value: LispValue, env: Environment): Unit {
        match (pattern) {
            case Symbol(name) => env.define(name, value)
            case Cons(_) => this.destructurePattern(pattern, value, env)
            case _ => ()
        }
    }

    /**
     * 处理单个 binding（可能是普通绑定或解构绑定）
     */
    private func processBinding(binding: LispValue, env: Environment): Unit {
        match (binding) {
            case Cons(bindingCell) => this.processBindingCell(bindingCell, env)
            case _ => ()
        }
    }

    /**
     * 检查是否为 quote 表达式并提取参数
     */
    private func extractQuoteArg(quoteExpr: LispValue): ?LispValue {
        match (quoteExpr) {
            case Cons(quoteCell) =>
                match (quoteCell.car) {
                    case Symbol(sym) where sym == "quote" =>
                        match (quoteCell.cdr) {
                            case Cons(argsCell) => Some(argsCell.car)
                            case _ => None
                        }
                    case _ => None
                }
            case _ => None
        }
    }

    /**
     * 处理解构绑定的 value（可能是 quote 表达式）
     */
    private func extractDestructureValue(valueExpr: LispValue): LispValue {
        let quoteArg = this.extractQuoteArg(valueExpr)
        match (quoteArg) {
            case Some(v) => v
            case None => this.eval(valueExpr)
        }
    }

    /**
     * 处理普通绑定（已确认 bindingCell.car 是 Symbol）
     */
    private func processSymbolBinding(name: String, bindingCell: ConsCell, env: Environment): Unit {
        match (bindingCell.cdr) {
            case Cons(valueListCell) => env.define(name, this.eval(valueListCell.car))
            case _ => ()
        }
    }

    /**
     * 处理普通绑定
     */
    private func processNormalBinding(bindingCell: ConsCell, env: Environment): Unit {
        match (bindingCell.car) {
            case Symbol(name) => this.processSymbolBinding(name, bindingCell, env)
            case _ => ()
        }
    }

    /**
     * 处理 binding cell（已经知道 binding 是 ConsCell）
     */
    /**
     * 处理解构绑定
     */
    private func processDestructBinding(bindingCell: ConsCell, env: Environment): Unit {
        let pattern = Cons(bindingCell)
        let value = this.extractDestructureValue(bindingCell.cdr)
        this.bindPattern(pattern, value, env)
    }

    /**
     * 处理 binding cell（已经知道 binding 是 ConsCell）
     */
    private func processBindingCell(bindingCell: ConsCell, env: Environment): Unit {
        match (bindingCell.cdr) {
            case Cons(_) => this.processNormalBinding(bindingCell, env)
            case _ => this.processDestructBinding(bindingCell, env)
        }
    }

    /**
     * 处理解构绑定：(pattern . value)
     * pattern 可以是符号或列表模式
     * value 可以是 quote 表达式或普通表达式
     */
    private func processDestructuringBinding(pattern: LispValue, valueExpr: LispValue, env: Environment): Unit {
        // 提取值（处理 quote 表达式）
        let value = this.extractDestructureValue(valueExpr)
        // 绑定模式到值
        this.bindPattern(pattern, value, env)
    }

    /**
     * 检查是否为 improper list（解构模式）
     */
    private func isImproperList(value: LispValue): Bool {
        match (value) {
            case Cons(cell) => this.isConsCellImproper(cell)
            case _ => false
        }
    }

    /**
     * 检查 ConsCell 是否为 improper list
     */
    private func isConsCellImproper(cell: ConsCell): Bool {
        let cdrIsCons = match (cell.cdr) {
            case Cons(_) => true
            case _ => false
        }
        !cdrIsCons && !cell.cdr.isNil()
    }

    /**
     * 处理 let 绑定列表
     * 支持两种格式：
     * 1. 正常绑定：([x 1] [y 2]) => 多个 (pattern value) 对
     * 2. 解构绑定：(pattern . value) 格式，其中 pattern 可能是 improper list
     */
    private func processBindings(bindings: ConsCell, env: Environment) {
        var current: LispValue = Cons(bindings)

        while (!current.isNil()) {
            if (let Cons(currCell) <- current) {
                let binding = currCell.car
                let cdr = currCell.cdr

                if (this.isImproperList(binding)) {
                    // 这是一个解构绑定，下一个元素是值
                    let value = if (let Cons(valueCell) <- cdr) {
                        valueCell.car
                    } else {
                        Nil
                    }
                    this.processDestructuringBinding(binding, value, env)
                    // 跳过下一个元素（已被作为 value 使用）
                    current = cdr
                } else {
                    // 正常绑定
                    this.processBinding(binding, env)
                    current = cdr
                }
            } else {
                break
            }
        }
    }

    /**
     * 解构模式匹配
     * 支持列表解构：(x . y) pattern => x 是第一个元素，y 是剩余部分
     */
    private func destructurePattern(pattern: LispValue, value: LispValue, env: Environment): Unit {
        match (pattern) {
            case Cons(patternCell) =>
                match (value) {
                    case Cons(valueCell) =>
                        // 绑定第一个元素
                        match (patternCell.car) {
                            case Symbol(sym) => env.define(sym, valueCell.car)
                            case Cons(subPattern) => this.destructurePattern(Cons(subPattern), valueCell.car, env)
                            case _ => ()
                        }

                        // 绑定剩余部分
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

    /**
     * 函数调用
     */
    private func evalFunctionCall(first: LispValue, rest: LispValue): LispValue {
        // 检查函数权限（如果是符号）
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

        // 求值函数
        let funcValue = this.eval(first)

        match (funcValue) {
            case Procedure(_, _, _) =>
                // 求值参数
                let args = this.evalArguments(rest)
                // 调用函数
                this.applyProcedure(funcValue, args)
            case NativeFunc(f) =>
                // 求值参数
                let args = this.evalArguments(rest)
                // 调用原生函数
                f(args)
            case _ =>
                // 不是函数，返回错误
                Nil
        }
    }

    /**
     * 求值参数列表
     */
    private func evalArguments(args: LispValue): ArrayList<LispValue> {
        let result = ArrayList<LispValue>()
        var current: LispValue = args

        while (!current.isNil()) {
            if (let Cons(cell) <- current) {
                result.add(this.eval(cell.car))
                current = cell.cdr
            } else {
                break
            }
        }

        result
    }

    /**
     * 应用过程
     */
    private func applyProcedure(procValue: LispValue, args: ArrayList<LispValue>): LispValue {
        match (procValue) {
            case Procedure(params, body, procEnv) =>

                // 创建新的环境
                let newEnv = procEnv.createChild()

                // 绑定参数
                for (i in 0..params.size) {
                    if (i < args.size) {
                        newEnv.define(params[i], args[i])
                    }
                }

                // 在新环境中求值函数体
                let oldEnv = this.env
                this.env = newEnv

                let result = this.evalBegin(body)

                this.env = oldEnv

                result
            case _ => Nil
        }
    }

    /**
     * 特殊形式：apply
     * (apply func arg1 arg2 ... args-list)
     * 将最后的列表参数展开，与前面的参数一起调用函数
     */
    private func evalApply(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr) {
            // 检查参数数量：至少需要函数 + 一个列表参数
            if (cell.length() < 2) {
                return Nil
            }

            // 第一个元素是函数
            let funcExpr = cell.car
            let rest = cell.cdr

            // 求值函数
            let funcValue = this.eval(funcExpr)

            // 收集所有参数（除了最后的列表）
            let args = ArrayList<LispValue>()
            var current: LispValue = rest
            var lastList: LispValue = Nil

            // 遍历参数，将所有参数求值
            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    let cdr = currCell.cdr
                    // 如果是最后一个元素
                    if (cdr.isNil()) {
                        // 最后一个元素需要先求值（可能是 (quote (1 2 3)) 这样的形式）
                        lastList = this.eval(currCell.car)
                    } else {
                        // 中间的参数要求值
                        args.add(this.eval(currCell.car))
                    }
                    current = cdr
                } else {
                    break
                }
            }

            // 将最后的列表展开并添加到 args
            if (let Cons(lastListCell) <- lastList) {
                var listCurrent: LispValue = Cons(lastListCell)
                while (!listCurrent.isNil()) {
                    if (let Cons(lc) <- listCurrent) {
                        args.add(lc.car)
                        listCurrent = lc.cdr
                    } else {
                        break
                    }
                }
            }

            // 调用函数
            match (funcValue) {
                case Procedure(_, _, _) => this.applyProcedure(funcValue, args)
                case NativeFunc(f) => f(args)
                case _ => Nil
            }
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：-> (thread-first 宏)
     * (-> value form1 form2 ...)
     * 将 value 作为第一个参数传递给每个 form
     *
     * 示例：
     * (-> 5 (+ 3) (* 2)) => (* (+ 5 3) 2) => 16
     * (-> x f) => (f x)
     * (-> x (f 1 2)) => (f x 1 2)
     */
    private func evalThreadFirst(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr) {
            // 至少需要一个初始值
            if (cell.isNil()) {
                return Nil
            }

            // 求值初始值
            var value = this.eval(cell.car)

            // 遍历后续的每个形式
            var current: LispValue = cell.cdr

            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    let form = currCell.car

                    // 处理不同形式
                    value = this.threadFirstStep(value, form)
                    current = currCell.cdr
                } else {
                    break
                }
            }

            value
        } else {
            Nil
        }
    }

    /**
     * 执行单个 thread-first 步骤
     */
    private func threadFirstStep(value: LispValue, form: LispValue): LispValue {
        match (form) {
            case Symbol(sym) => this.threadFirstWithSymbol(value, sym)
            case Cons(formCell) => this.threadFirstWithList(value, formCell)
            case _ => this.eval(form)
        }
    }

    /**
     * Thread-first with symbol: (f value)
     */
    private func threadFirstWithSymbol(value: LispValue, sym: String): LispValue {
        let funcValue = this.env.lookup(sym)
        let args = ArrayList<LispValue>()
        args.add(value)

        match (funcValue) {
            case Procedure(_, _, _) => this.applyProcedure(funcValue, args)
            case NativeFunc(f) => f(args)
            case _ => Nil
        }
    }

    /**
     * Thread-first with list: (f arg1 arg2 ...) => (f value arg1 arg2 ...)
     */
    private func threadFirstWithList(value: LispValue, formCell: ConsCell): LispValue {
        let fn = formCell.car
        let restArgs = formCell.cdr

        // 构建新的参数列表
        let args = ArrayList<LispValue>()
        args.add(value)

        // 添加原有参数
        var argCurrent: LispValue = restArgs
        while (!argCurrent.isNil()) {
            if (let Cons(argCell) <- argCurrent) {
                args.add(this.eval(argCell.car))
                argCurrent = argCell.cdr
            } else {
                break
            }
        }

        // 调用函数
        let funcValue = this.eval(fn)
        match (funcValue) {
            case Procedure(_, _, _) => this.applyProcedure(funcValue, args)
            case NativeFunc(f) => f(args)
            case _ => Nil
        }
    }

    /**
     * 从 ConsCell 中提取符号列表
     */
    private func extractSymbols(expr: LispValue): ArrayList<String> {
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
     * 特殊形式：map
     * (map func list) - 对列表中每个元素应用函数，支持 lambda
     */
    private func evalMap(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && cell.length() == 2) {
            let funcValue = this.eval(cell.car)

            // 提取列表参数
            let lst = if (let Cons(cdrCell) <- cell.cdr) {
                this.eval(cdrCell.car)
            } else {
                Nil
            }

            var result = ArrayList<LispValue>()
            var current: LispValue = lst

            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    let args = ArrayList<LispValue>()
                    args.add(currCell.car)

                    // 应用函数（支持 Procedure 和 NativeFunc）
                    let mapped = match (funcValue) {
                        case Procedure(_, _, _) => this.applyProcedure(funcValue, args)
                        case NativeFunc(f) => f(args)
                        case _ => Nil
                    }
                    result.add(mapped)
                    current = currCell.cdr
                } else {
                    break
                }
            }

            list(result)
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：filter
     * (filter pred list) - 过滤列表，支持 lambda
     */
    private func evalFilter(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && cell.length() == 2) {
            let pred = this.eval(cell.car)

            // 提取列表参数
            let lst = if (let Cons(cdrCell) <- cell.cdr) {
                this.eval(cdrCell.car)
            } else {
                Nil
            }

            var result = ArrayList<LispValue>()
            var current: LispValue = lst

            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    let args = ArrayList<LispValue>()
                    args.add(currCell.car)

                    // 应用谓词函数
                    let testResult = match (pred) {
                        case Procedure(_, _, _) => this.applyProcedure(pred, args)
                        case NativeFunc(f) => f(args)
                        case _ => Boolean(false)
                    }

                    if (testResult.isTruthy()) {
                        result.add(currCell.car)
                    }
                    current = currCell.cdr
                } else {
                    break
                }
            }

            list(result)
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：reduce
     * (reduce func init list) - 归约操作，支持 lambda
     */
    private func evalReduce(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && cell.length() == 3) {
            let funcValue = this.eval(cell.car)

            // 提取初始值
            var acc = if (let Cons(cdrCell) <- cell.cdr) {
                this.eval(cdrCell.car)
            } else {
                Nil
            }

            // 提取列表
            let lst = if (let Cons(cdrCell) <- cell.cdr) {
                if (let Cons(cdr2Cell) <- cdrCell.cdr) {
                    this.eval(cdr2Cell.car)
                } else {
                    Nil
                }
            } else {
                Nil
            }

            var current: LispValue = lst

            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    let args = ArrayList<LispValue>()
                    args.add(acc)
                    args.add(currCell.car)

                    // 应用函数
                    acc = match (funcValue) {
                        case Procedure(_, _, _) => this.applyProcedure(funcValue, args)
                        case NativeFunc(f) => f(args)
                        case _ => acc
                    }
                    current = currCell.cdr
                } else {
                    break
                }
            }

            acc
        } else {
            Nil
        }
    }

    /**
     * 特殊形式：match 模式匹配
     * (match expr
     *   pattern1 result1           ; 简单形式：匹配返回 result1
     *   (pattern2 result2)         ; 单表达式形式
     *   (pattern3 body3...)        ; 多表达式形式
     *   (_ default-body...))       ; 通配符
     */
    private func evalMatch(expr: LispValue): LispValue {
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

                                func isPattern(elem: LispValue): Bool {
                                    match (elem) {
                                        case Symbol(_) => true
                                        case Cons(_) => true
                                        case Number(_) => true  // 数字也算作模式（用于列表模式中的常量）
                                        case _ => false  // 其他（字符串、布尔值）不是模式
                                    }
                                }

                                let areAllPatterns = if (isPattern(clauseCons.car)) {
                                    var allPatterns = true
                                    var current = rest
                                    while (!current.isNil() && allPatterns) {
                                        if (let Cons(c) <- current) {
                                            if (isPattern(c.car)) {
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
                                                        clauses = Cons(nextClauses)
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
                                            clauses = Cons(nextClauses)
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
     * 从 body 中提取守卫条件
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
    private func matchListPattern(value: LispValue, pattern: ConsCell, env: Environment): ?Environment {
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

    /**
     * 在指定环境中求值
     */
    private func evalInEnv(expr: LispValue, env: Environment): LispValue {
        let oldEnv = this.env
        this.env = env
        let result = this.eval(expr)
        this.env = oldEnv
        result
    }

    /**
     * 执行 body 序列，返回最后一个结果
     */
    private func evalBody(body: LispValue, env: Environment): LispValue {
        var result: LispValue = Nil
        var current = body

        while (!current.isNil()) {
            if (let Cons(cell) <- current) {
                result = this.evalInEnv(cell.car, env)
                current = cell.cdr
            } else {
                break
            }
        }

        result
    }

    /**
     * 检查列表中的所有元素是否都是符号
     * 用于判断是 (x y z body) 还是 ((x y z) body)
     */
    private func isAllSymbols(lst: LispValue): Bool {
        var current = lst
        while (!current.isNil()) {
            if (let Cons(cell) <- current) {
                match (cell.car) {
                    case Symbol(_) => ()
                    case _ => return false
                }
                current = cell.cdr
            } else {
                return false
            }
        }
        true
    }

    /**
     * 向列表追加元素，返回新列表
     * 用于构造模式 (x y) -> (x y z)
     */
    private func appendToList(lst: LispValue, elem: LispValue): LispValue {
        match (lst) {
            case Nil => consValue(elem, Nil)
            case Cons(cell) => consValue(cell.car, this.appendToList(cell.cdr, elem))
            case _ => consValue(elem, Nil)
        }
    }
}
