package ystyle::xisp.core

import std.collection.ArrayList

/**
 * 求值器 - Lisp 表达式求值
 */
public class Evaluator {
    private var env: Environment

    public init(env: Environment) {
        this.env = env
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

            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    result = this.eval(currCell.car)
                    current = currCell.cdr
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
}
