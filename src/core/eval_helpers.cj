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
        if (let LispValue.Cons(cell) <- value && let Symbol("cangjie::vector") <- cell.car) {
            true
        } else {
            false
        }
    }

    /**
     * 检查是否为 HashMap 解构模式 {:key1 var1 :key2 var2 ...}
     * 实际解析为：(hashmap (quote :key1) var1 (quote :key2) var2 ...)
     */
    func isHashMapDestructurePattern(value: LispValue): Bool {
        if (let Cons(cell) <- value && let Symbol("hashmap") <- cell.car) {
            true
        } else {
            false
        }
    }

    /**
     * 提取单个 quote 表达式中的符号
     * 输入：(quote :key)
     * 输出：Some("key") 或 None
     */
    private func extractQuotedSymbol(expr: LispValue): ?String {
        if (let LispValue.Cons(quoteCell) <- expr &&
            let (Symbol("quote"), Cons(keySymCell)) <- (quoteCell.car, quoteCell.cdr) &&
            let Symbol(keySym) <- keySymCell.car && 
            keySym.startsWith(":")) {
            return keySym.split(":")[1]
        }
        return None
    }

    /**
     * 从 HashMap 解构模式中提取键值对
     * 模式格式：{:key1 var1 :key2 var2 ...}
     * 实际解析为：(hashmap (quote :key1) var1 (quote :key2) var2 ...)
     * 返回：ArrayList<(:key1, var1), (:key2, var2), ...>  （key 已去掉冒号）
     */
    func extractHashMapBindings(pattern: LispValue): ArrayList<(String, LispValue)> {
        let result = ArrayList<(String, LispValue)>()
        if (let LispValue.Cons(cell) <- pattern) {
            var current = cell.cdr  // 跳过 hashmap 符号
            // 遍历 ((quote :key1) var1 (quote :key2) var2 ...)
            while (!current.isNil()) {
                if (let Cons(keyQuoteCell) <- current &&
                    let Some(key) <- this.extractQuotedSymbol(keyQuoteCell.car) && 
                    let Cons(varCell) <- keyQuoteCell.cdr) {
                        result.add((key, varCell.car))
                        // 移动到下一对
                        current = varCell.cdr
                } else {
                    break
                }
            }
        }
        result
    }


    /**
     * 提取向量元素列表
     */
    func extractVectorElements(pattern: LispValue): ArrayList<LispValue> {
        let result = ArrayList<LispValue>()
        if (let Cons(cell) <- pattern) {
            var current = cell.cdr
            while (!current.isNil()) {
                if (let Cons(currCell) <- current) {
                    result.add(currCell.car)
                    current = currCell.cdr
                } else {
                    break
                }
            }
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
        if (let Cons(cell) <- expr &&
            let Nil <- cell.car &&
            let Cons(cdrCell) <- cell.cdr &&
            let Symbol(restName) <- cdrCell.car &&
            cdrCell.cdr.isNil()) {
            // (nil all) 或 (nil . (all)) 的情况，all 是可变参数
            result.add("&rest-" + restName)
            return result
        }

        if (let Cons(cell) <- expr && let Nil <- cell.car && let Symbol(restName) <- cell.cdr) {
            // (nil . all) 的情况，all 是可变参数
            result.add("&rest-" + restName)
            return result
        }

        while (!current.isNil()) {
            if (let Cons(cell) <- current && let Symbol(s) <- cell.car) {
                // 检查是否为 . 或 &rest 标记
                if (s == "." || s == "&rest") {
                    // 跳到下一个参数，它应该是可变参数名
                    current = cell.cdr
                    if (let Cons(nextCell) <- current && let Symbol(restName) <- nextCell.car) {
                        // 为可变参数添加特殊前缀
                        result.add("&rest-" + restName)
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
                // 非符号，跳过 - cell 不可见，需要重新声明
                if (let Cons(c) <- current) {
                    current = c.cdr
                } else {
                    break
                }
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
            if (let LispValue.Symbol("&") <-  elements[i]) {
                restIndex = i
                break
            }
        }

        if (restIndex >= 0) {
            // 有 & rest，绑定前面的元素和剩余部分
            var valueCurrent: LispValue = value

            // 绑定 & 之前的元素
            for (i in 0..restIndex) {
                match (elements[i]) {
                    case Symbol(name) =>
                        if (let LispValue.Cons(vCell) <- valueCurrent) {
                            env.define(name, vCell.car)
                            valueCurrent = vCell.cdr
                        } else {
                            env.define(name, Nil)
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
                if (let LispValue.Symbol(restName) <- elements[restIndex + 1]) {
                    env.define(restName, valueCurrent)
                }
            }
        } else {
            // 没有 &，逐个绑定元素
            var valueCurrent: LispValue = value
            for (elem in elements) {
                match (elem) {
                    case Symbol(name) =>
                        if (let LispValue.Cons(vCell) <- valueCurrent) {
                            env.define(name, vCell.car)
                            valueCurrent = vCell.cdr
                        } else {
                            env.define(name, Nil)
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
     * 处理 HashMap 解构 {:key1 var1 :key2 var2 ...}
     */
    func processHashMapDestructure(pattern: LispValue, value: LispValue, env: Environment): Unit {
        match (value) {
            case HashMap(mapValue) =>
                // 从模式中提取键值对（key 已去掉冒号）
                let bindings = this.extractHashMapBindings(pattern)

                // 使用索引访问
                var i = 0
                while (i < bindings.size) {
                    let binding = bindings[i]
                    match (binding) {
                        case (key, pattern) =>
                            // pattern 是 LispValue，可能是变量或常量
                            // 在 let 中，总是绑定变量
                            match (pattern) {
                                case Symbol(varName) =>
                                    // 从 HashMap 中获取值并绑定
                                    match (mapValue.get(key)) {
                                        case Some(v) => env.define(varName, v)
                                        case None => env.define(varName, Nil)  // 键不存在时绑定 nil
                                    }
                                case _ => ()
                            }
                        case _ => ()
                    }
                    i = i + 1
                }
            case _ => ()
        }
    }

    /**
     * 处理 let 绑定列表（新版本，支持向量和 HashMap）
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
                } else if (this.isHashMapDestructurePattern(pattern)) {
                    // HashMap 解构
                    this.processHashMapDestructure(pattern, value, env)
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
                    if (this.isVectorDestructurePattern(binding) && let Cons(nextCell) <- currCell.cdr) {
                        // 向量解构：binding 是 pattern，下一个元素是值表达式
                        let value = this.eval(nextCell.car)
                        this.processVectorDestructure(binding, value, env)
                        // 跳过下一个元素
                        current = nextCell.cdr
                    } else if (this.isHashMapDestructurePattern(binding) && let Cons(nextCell) <- currCell.cdr) {
                        // HashMap 解构：binding 是 {:key1 var1 :key2 var2}
                        let value = this.eval(nextCell.car)
                        this.processHashMapDestructure(binding, value, env)
                        // 跳过下一个元素
                        current = nextCell.cdr
                    } else if (binding.isDottedPair()) {
                        // 传统点对绑定：((pattern . rest) value)
                        // binding 本身是一个点对
                        if (let Cons(_) <- binding) {
                            let pattern = binding
                            let valueExpr = if (let Cons(nextCell) <- currCell.cdr) { nextCell.car } else { currCell.cdr }
                            let value = this.eval(valueExpr)

                            match (pattern) {
                                case Symbol(name) => env.define(name, value)
                                case Cons(_) => this.destructurePattern(pattern, value, env)
                                case _ => ()
                            }
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
                                case Cons(_) =>
                                    // 检查是否为嵌套向量或 HashMap 解构
                                    if (this.isVectorDestructurePattern(pattern)) {
                                        this.processVectorDestructure(pattern, value, env)
                                    } else if (this.isHashMapDestructurePattern(pattern)) {
                                        this.processHashMapDestructure(pattern, value, env)
                                    } else {
                                        this.destructurePattern(pattern, value, env)
                                    }
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
