package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 辅助方法扩展
 * 包含提取符号、处理绑定、解构等辅助方法
 */
extend Evaluator {
    /**
     * 检查是否为向量解构模式 (cangjie::vector ...)
     */
    func isVectorDestructurePattern(value: LispValue): Bool {
        match (value) {
            case Cons(cell) =>
                match (cell.car) {
                    case Symbol("cangjie::vector") => true
                    case _ => false
                }
            case _ => false
        }
    }

    /**
     * 提取向量元素列表
     */
    func extractVectorElements(pattern: LispValue): ArrayList<LispValue> {
        let result = ArrayList<LispValue>()
        match (pattern) {
            case Cons(cell) =>
                var current = cell.cdr
                while (!current.isNil()) {
                    match (current) {
                        case Cons(currCell) =>
                            result.add(currCell.car)
                            current = currCell.cdr
                        case _ => break
                    }
                }
            case _ => ()
        }
        result
    }
    /**
     * 从 ConsCell 中提取符号列表（支持可变参数）
     * 支持语法：
     * - Common Lisp 风格：(x y . rest)
     * - Scheme 风格：(x y &rest rest)
     */
    public func extractSymbols(expr: LispValue): ArrayList<String> {
        let result = ArrayList<String>()
        var current: LispValue = expr

        // 特殊情况：如果 expr 本身就是一个符号，说明是纯可变参数
        // 例如：(define (func . args) ...) 中的 args
        if (let Symbol(restName) <- expr) {
            result.add("&rest-" + restName)
            return result
        }

        // 特殊情况：处理纯可变参数 (. all) 或 (&rest all)
        // 这些会被 Parser 解析为 (nil . (all)) 或 (nil all)
        if (let Cons(cell) <- expr) {
            if (let Nil <- cell.car) {
                // 第一个元素是 nil，检查是否为纯可变参数
                if (let Cons(cdrCell) <- cell.cdr) {
                    if (let Symbol(restName) <- cdrCell.car && cdrCell.cdr.isNil()) {
                        // (nil all) 或 (nil . (all)) 的情况，all 是可变参数
                        result.add("&rest-" + restName)
                        return result
                    }
                } else if (let Symbol(restName) <- cell.cdr) {
                    // (nil . all) 的情况，all 是可变参数
                    result.add("&rest-" + restName)
                    return result
                }
            }
        }

        while (!current.isNil()) {
            if (let Cons(cell) <- current) {
                if (let Symbol(s) <- cell.car) {
                    // 检查是否为 . 或 &rest 标记
                    if (s == "." || s == "&rest") {
                        // 跳到下一个参数，它应该是可变参数名
                        current = cell.cdr
                        if (let Cons(nextCell) <- current) {
                            if (let Symbol(restName) <- nextCell.car) {
                                // 为可变参数添加特殊前缀
                                result.add("&rest-" + restName)
                            }
                            current = nextCell.cdr
                        } else if (let Symbol(restName) <- current) {
                            // (. rest) 的情况，rest 直接作为符号
                            result.add("&rest-" + restName)
                            current = Nil
                        }
                        break
                    } else {
                        // 普通参数
                        result.add(s)

                        // 检查 cdr 是否为可变参数（点对情况）
                        if (let Symbol(restName) <- cell.cdr) {
                            // 如果 cdr 是符号，说明这是最后一个参数的 rest 参数
                            result.add("&rest-" + restName)
                            break
                        }

                        current = cell.cdr
                    }
                } else {
                    // 非符号，跳过
                    current = cell.cdr
                }
            } else {
                break
            }
        }

        result
    }

    /**
     * 处理向量解构 [x y & rest]
     */
    func processVectorDestructure(pattern: LispValue, value: LispValue, env: Environment): Unit {
        let elements = this.extractVectorElements(pattern)

        // 查找 & 符号的位置
        var restIndex = -1
        for (i in 0..elements.size) {
            match (elements[i]) {
                case Symbol("&") =>
                    restIndex = i
                    break
                case _ => ()
            }
        }

        if (restIndex >= 0) {
            // 有 & rest，绑定前面的元素和剩余部分
            var valueCurrent: LispValue = value

            // 绑定 & 之前的元素
            for (i in 0..restIndex) {
                match (elements[i]) {
                    case Symbol(name) =>
                        match (valueCurrent) {
                            case Cons(vCell) =>
                                env.define(name, vCell.car)
                                valueCurrent = vCell.cdr
                            case _ => env.define(name, Nil)
                        }
                    case Cons(subPattern) =>
                        match (valueCurrent) {
                            case Cons(vCell) =>
                                // 检查是否是嵌套向量解构
                                if (this.isVectorDestructurePattern(Cons(subPattern))) {
                                    this.processVectorDestructure(Cons(subPattern), vCell.car, env)
                                } else {
                                    this.destructurePattern(Cons(subPattern), vCell.car, env)
                                }
                                valueCurrent = vCell.cdr
                            case _ => ()
                        }
                    case _ => ()
                }
            }

            // 绑定 & 之后的 rest 变量
            if (restIndex + 1 < elements.size) {
                match (elements[restIndex + 1]) {
                    case Symbol(restName) =>
                        env.define(restName, valueCurrent)
                    case _ => ()
                }
            }
        } else {
            // 没有 &，逐个绑定元素
            var valueCurrent: LispValue = value
            for (elem in elements) {
                match (elem) {
                    case Symbol(name) =>
                        match (valueCurrent) {
                            case Cons(vCell) =>
                                env.define(name, vCell.car)
                                valueCurrent = vCell.cdr
                            case _ => env.define(name, Nil)
                        }
                    case Cons(subPattern) =>
                        match (valueCurrent) {
                            case Cons(vCell) =>
                                // 检查是否是嵌套向量解构
                                if (this.isVectorDestructurePattern(Cons(subPattern))) {
                                    this.processVectorDestructure(Cons(subPattern), vCell.car, env)
                                } else {
                                    this.destructurePattern(Cons(subPattern), vCell.car, env)
                                }
                                valueCurrent = vCell.cdr
                            case _ => ()
                        }
                    case _ => ()
                }
            }
        }
    }

    /**
     * 处理 let 绑定列表（新版本，支持向量）
     */
    func processBindingsNew(bindingsValue: LispValue, env: Environment) {
        // 检查 bindings 是否为向量
        if (this.isVectorDestructurePattern(bindingsValue)) {
            // bindings 是向量：[pattern1 value1 pattern2 value2 ...]
            let elements = this.extractVectorElements(bindingsValue)
            var i = 0
            while (i + 1 < elements.size) {
                let pattern = elements[i]
                let valueExpr = elements[i + 1]
                let value = this.eval(valueExpr)

                if (this.isVectorDestructurePattern(pattern)) {
                    // 嵌套向量解构
                    this.processVectorDestructure(pattern, value, env)
                } else {
                    match (pattern) {
                        case Symbol(name) => env.define(name, value)
                        case Cons(_) => this.destructurePattern(pattern, value, env)
                        case _ => ()
                    }
                }

                i = i + 2
            }
        } else {
            // bindings 是普通列表
            var current: LispValue = bindingsValue
            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    let binding = currCell.car

                    // 检查是否为向量解构模式
                    if (this.isVectorDestructurePattern(binding)) {
                        // 向量解构：binding 是 pattern，下一个元素是值表达式
                        if (let Cons(nextCell) <- currCell.cdr) {
                            let value = this.eval(nextCell.car)
                            this.processVectorDestructure(binding, value, env)
                        }
                        // 跳过下一个元素
                        current = if (let Cons(nextCell) <- currCell.cdr) { nextCell.cdr } else { currCell.cdr }
                    } else if (binding.isDottedPair()) {
                        // 传统点对绑定：((pattern . rest) value)
                        // binding 本身是一个点对
                        match (binding) {
                            case Cons(bindingCell) =>
                                let pattern = binding
                                let valueExpr = if (let Cons(nextCell) <- currCell.cdr) { nextCell.car } else { currCell.cdr }
                                let value = this.eval(valueExpr)

                                match (pattern) {
                                    case Symbol(name) => env.define(name, value)
                                    case Cons(_) => this.destructurePattern(pattern, value, env)
                                    case _ => ()
                                }
                            case _ => ()
                        }
                        current = currCell.cdr
                    } else {
                        // 普通绑定：(pattern value)
                        if (let Cons(bindingCell) <- binding) {
                            let pattern = bindingCell.car
                            let valueExpr = if (let Cons(cdrCell) <- bindingCell.cdr) { cdrCell.car } else { bindingCell.cdr }

                            let value = this.eval(valueExpr)

                            match (pattern) {
                                case Symbol(name) => env.define(name, value)
                                case Cons(_) => this.destructurePattern(pattern, value, env)
                                case _ => ()
                            }
                        }

                        current = currCell.cdr
                    }
                } else {
                    break
                }
            }
        }
    }

    /**
     * 处理 let 绑定列表
     */
    func processBindings(bindings: ConsCell, env: Environment) {
        this.processBindingsNew(Cons(bindings), env)
    }

    /**
     * 解构模式匹配
     */
    func destructurePattern(pattern: LispValue, value: LispValue, env: Environment): Unit {
        match (pattern) {
            case Cons(patternCell) =>
                match (value) {
                    case Cons(valueCell) =>
                        // 检查 pattern 是否是点对结构 (x . y)
                        if (!patternCell.isProperList()) {
                            // 点对语法: (x . y)
                            // patternCell.car 是第一个元素的绑定模式
                            // patternCell.cdr 是剩余部分的绑定模式
                            match (patternCell.car) {
                                case Symbol(first) =>
                                    env.define(first, valueCell.car)
                                case Cons(subPattern) =>
                                    this.destructurePattern(Cons(subPattern), valueCell.car, env)
                                case _ => ()
                            }

                            match (patternCell.cdr) {
                                case Symbol(restSym) =>
                                    env.define(restSym, valueCell.cdr)
                                case Cons(restPattern) =>
                                    this.destructurePattern(Cons(restPattern), valueCell.cdr, env)
                                case _ => ()
                            }
                        } else {
                            // 普通列表解构: (x y z)
                            match (patternCell.car) {
                                case Symbol(sym) => env.define(sym, valueCell.car)
                                case Cons(subPattern) => this.destructurePattern(Cons(subPattern), valueCell.car, env)
                                case _ => ()
                            }

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
                        }
                    case Nil => ()
                    case _ => ()
                }
            case _ => ()
        }
    }
}
