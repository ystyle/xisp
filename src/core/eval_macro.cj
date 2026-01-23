package ystyle::xisp.core

import std.collection.ArrayList

/**
 * 宏系统扩展
 * 包含宏展开机制、defmacro、macroexpand
 */
extend Evaluator {
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
            return Str("Error: Maximum macro expand depth exceeded (limit: ${this.maxMacroExpandDepth})")
        }

        match (expr) {
            case Nil => expr
            case Boolean(_) => expr
            case Number(_) => expr
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
            if (let Macro(_, _, _) <- value) {
                // 这是一个宏，展开它（不递归展开子表达式）
                this.macroExpandDepth = this.macroExpandDepth + 1
                let expanded = this.expandMacro(value, cell.cdr)
                this.macroExpandDepth = this.macroExpandDepth - 1
                expanded
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
            Str("Error: Macro expansion did not terminate after ${maxIterations} iterations")
        } else {
            expanded
        }
    }

    /**
     * 展开宏
     */
    func expandMacro(macroValue: LispValue, args: LispValue): LispValue {
        if (let Macro(params, body, macroEnv) <- macroValue) {
            let newEnv = macroEnv.createChild()
            this.bindMacroParams(params, args, newEnv)

            let oldEnv = this.env
            this.env = newEnv

            // body 是一个 cons cell (类似 begin 块)，需要按顺序求值
            // 检查是否是单个表达式 (cdr 是 nil)
            let result = if (let Cons(bodyCell) <- body) {
                if (bodyCell.cdr.isNil()) {
                    // 单个表达式，直接求值 car
                    this.eval(bodyCell.car)
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
        } else {
            Str("Error: expandMacro called with non-macro value")
        }
    }

    /**
     * 绑定宏参数到环境
     */
    func bindMacroParams(params: ArrayList<String>, args: LispValue, env: Environment) {
        var currentArg: LispValue = args
        for (param in params) {
            currentArg = this.bindOneMacroParam(param, currentArg, env)
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
                return Str("Error: defmacro requires at least 2 arguments")
            }
            return this.evalDefmacroWithName(cell)
        } else {
            Str("Error: defmacro invalid syntax")
        }
    }

    /**
     * evalDefmacro 辅助：已验证有 cell
     */
    func evalDefmacroWithName(cell: ConsCell): LispValue {
        if (let Symbol(name) <- cell.car) {
            if (let Cons(restCell) <- cell.cdr) {
                let paramsExpr = restCell.car
                let params = this.extractParams(paramsExpr)
                let body = restCell.cdr
                let m = macroValue(params, body, this.env)
                this.env.define(name, m)
                Symbol(name)
            } else {
                Str("Error: defmacro invalid syntax")
            }
        } else {
            Str("Error: defmacro first argument must be a symbol")
        }
    }

    /**
     * 从参数表达式中提取参数名列表
     */
    func extractParams(paramsExpr: LispValue): ArrayList<String> {
        let params = ArrayList<String>()
        var current: LispValue = paramsExpr
        var iteration = 0
        let maxIterations = 100

        while (!current.isNil() && iteration < maxIterations) {
            iteration = iteration + 1
            if (let Cons(currCell) <- current) {
                if (let Symbol(paramName) <- currCell.car) {
                    params.add(paramName)
                }
                current = currCell.cdr
            } else {
                break
            }
        }

        params
    }

    /**
     * macroexpand 函数
     */
    func evalMacroexpand(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && cell.length() == 2) {
            if (let Cons(cdrCell) <- cell.cdr) {
                this.macroExpand(cdrCell.car)
            } else {
                this.macroExpand(Nil)
            }
        } else {
            Str("Error: macroexpand requires exactly 1 argument")
        }
    }

    /**
     * macroexpand-all 函数
     */
    func evalMacroexpandAll(expr: LispValue): LispValue {
        if (let Cons(cell) <- expr && cell.length() == 2) {
            if (let Cons(cdrCell) <- cell.cdr) {
                this.macroExpandAll(cdrCell.car)
            } else {
                this.macroExpandAll(Nil)
            }
        } else {
            Str("Error: macroexpand-all requires exactly 1 argument")
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
            case Number(n1) => match (b) {
                case Number(n2) => n1 == n2
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
