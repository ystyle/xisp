package ystyle::xisp.core

import std.collection.ArrayList
import std.collection.HashMap


/**
 * 环境（作用域）
 *
 * 用于存储变量绑定，支持词法作用域
 * Environment 不依赖 LispValue，先定义它
 */
public open class Environment {
    /**
     * 父环境（外层作用域）
     */
    public var parent: Option<Environment>

    /**
     * 变量绑定表
     */
    private var bindings: HashMap<String, LispValue>

    /**
     * 关键字别名映射表
     * 用于支持多语言关键字（如：定义 -> define）
     */
    private var keywordAliases: HashMap<String, String>

    /**
     * 默认构造函数
     */
    public init() {
        this.parent = None
        this.bindings = HashMap<String, LispValue>()
        this.keywordAliases = HashMap<String, String>()
    }

    /**
     * 带父环境的构造函数
     */
    public init(parent: Environment) {
        this.parent = Some(parent)
        this.bindings = HashMap<String, LispValue>()
        this.keywordAliases = HashMap<String, String>()
    }

    /**
     * 在当前环境中定义变量
     */
    public func define(name: String, value: LispValue) {
        this.bindings[name] = value
    }

    /**
     * 查找变量（在当前环境和所有父环境中查找）
     */
    public func lookup(name: String): LispValue {
        if (this.bindings.contains(name)) {
            let opt = this.bindings.get(name)
            match (opt) {
                case Some(v) => v
                case None => Nil
            }
        } else {
            match (this.parent) {
                case Some(env) => env.lookup(name)
                case None => Nil
            }
        }
    }

    /**
     * 修改已有变量
     */
    public func set(name: String, value: LispValue): Bool {
        if (this.bindings.contains(name)) {
            this.bindings[name] = value
            true
        } else {
            match (this.parent) {
                case Some(env) => env.set(name, value)
                case None => false
            }
        }
    }

    /**
     * 判断变量是否存在
     */
    public func contains(name: String): Bool {
        if (this.bindings.contains(name)) {
            true
        } else {
            match (this.parent) {
                case Some(env) => env.contains(name)
                case None => false
            }
        }
    }

    /**
     * 创建子环境
     */
    public func createChild(): Environment {
        let env = Environment()
        env.parent = Some(this)
        env
    }

    /**
     * 获取当前环境中的所有变量名
     */
    public func getKeys(): ArrayList<String> {
        let keys = ArrayList<String>()
        for ((key, _) in this.bindings) {
            keys.add(key)
        }
        keys
    }

    /**
     * 克隆环境（浅拷贝，复制绑定但共享父环境）
     */
    public func clone(): Environment {
        let env = Environment()
        env.parent = this.parent
        for ((key, value) in this.bindings) {
            env.bindings[key] = value
        }
        // 也复制关键字别名
        for ((alias, original) in this.keywordAliases) {
            env.keywordAliases[alias] = original
        }
        env
    }

    // ==================== 关键字别名管理 ====================

    /**
     * 注册关键字别名
     * @param alias 别名（如："定义"）
     * @param original 原始关键字（如："define"）
     */
    public func registerKeywordAlias(alias: String, original: String) {
        this.keywordAliases[alias] = original
    }

    /**
     * 查找关键字的实际名称
     * @param sym 符号名
     * @return 如果有别名映射，返回原始关键字；否则返回原符号
     *
     * 支持在父环境中查找别名
     */
    public func lookupKeyword(sym: String): String {
        // 先检查当前环境的别名映射
        if (this.keywordAliases.contains(sym)) {
            match (this.keywordAliases.get(sym)) {
                case Some(original) => return original
                case None => ()
            }
        }

        // 如果是父环境的别名，也要查找
        match (this.parent) {
            case Some(parent) => return parent.lookupKeyword(sym)
            case None => ()
        }

        // 没有别名，返回原符号
        sym
    }
}

/**
 * 链表单元格
 *
 * ConsCell 不依赖复杂的 LispValue，可以先定义
 */
public open class ConsCell {
    /**
     * 列表头部元素
     */
    public let car: LispValue

    /**
     * 列表剩余部分
     */
    public var cdr: LispValue

    /**
     * 构造函数
     */
    public init(car: LispValue, cdr: LispValue) {
        this.car = car
        this.cdr = cdr
    }

    /**
     * 判断是否为空列表
     */
    public func isNil(): Bool {
        match (this.car) {
            case Nil => match (this.cdr) {
                case Nil => true
                case _ => false
            }
            case _ => false
        }
    }

    /**
     * 判断是否为正规列表（proper list）
     * 正规列表的 cdr 必须是 Nil 或另一个 ConsCell
     */
    public func isProperList(): Bool {
        match (this.cdr) {
            case Nil => true
            case Cons(_) => true
            case _ => false
        }
    }

    /**
     * 转换为字符串表示
     */
    public func toString(): String {
        if (this.isNil()) {
            return "()"
        }
        "(" + this.car.toString() + this.formatCdr(this.cdr) + ")"
    }

    /**
     * 格式化 cdr 部分
     */
    private func formatCdr(cdr: LispValue): String {
        match (cdr) {
            case Nil => ""
            case Cons(cell) => " " + cell.car.toString() + this.formatCdr(cell.cdr)
            case _ => " . " + cdr.toString()
        }
    }

    /**
     * 获取列表长度
     */
    public func length(): Int64 {
        if (this.isNil()) {
            0
        } else {
            this.count(0)
        }
    }

    private func count(acc: Int64): Int64 {
        match (this.cdr) {
            case Nil => acc + 1
            case Cons(cell) => cell.count(acc + 1)
            case _ => acc + 1  // 不规则列表
        }
    }

    /**
     * 克隆链表（深拷贝）
     */
    public func clone(): ConsCell {
        ConsCell(this.car.cloneValue(), this.cdr.cloneValue())
    }
}

/**
 * Lisp 值类型枚举
 *
 * 支持递归定义，可以包含 ConsCell（它又包含 LispValue）
 */
public enum LispValue <: ToString {
    /**
     * 空值
     */
    | Nil

    /**
     * 布尔值
     */
    | Boolean(Bool)

    /**
     * 数字（使用 Float64 支持小数）
     */
    | Number(Float64)

    /**
     * 字符串（使用 Str 避免与仓颉 String 类型冲突）
     */
    | Str(String)

    /**
     * 符号（用于变量名、函数名等）
     */
    | Symbol(String)

    /**
     * 链表单元格
     */
    | Cons(ConsCell)

    /**
     * 过程/函数（闭包）
     *
     * 包含：参数列表、函数体、捕获的环境
     */
    | Procedure(ArrayList<String>, LispValue, Environment)

    /**
     * 宏（编译时元编程）
     *
     * 包含：参数列表、宏体、捕获的环境
     * 与 Procedure 不同，宏接收的是未求值的语法树
     */
    | Macro(ArrayList<String>, LispValue, Environment)

    /**
     * 原生函数（用仓颉实现的内置函数）
     *
     * 接收参数列表，返回结果
     */
    | NativeFunc((ArrayList<LispValue>) -> LispValue)

    /**
     * 判断是否为真值
     * 除了 nil 和 false，其他都是真
     */
    public func isTruthy(): Bool {
        match (this) {
            case Nil => false
            case Boolean(false) => false
            case _ => true
        }
    }

    /**
     * 判断是否为 Nil
     */
    public func isNil(): Bool {
        match (this) {
            case Nil => true
            case _ => false
        }
    }

    /**
     * 判断是否为数字
     */
    public func isNumber(): Bool {
        match (this) {
            case Number(_) => true
            case _ => false
        }
    }

    /**
     * 判断是否为字符串
     */
    public func isString(): Bool {
        match (this) {
            case Str(_) => true
            case _ => false
        }
    }

    /**
     * 判断是否为符号
     */
    public func isSymbol(): Bool {
        match (this) {
            case Symbol(_) => true
            case _ => false
        }
    }

    /**
     * 判断是否为列表（不包括点对）
     */
    public func isList(): Bool {
        match (this) {
            case Nil => true
            case Cons(cell) => cell.isProperList()
            case _ => false
        }
    }

    /**
     * 判断是否为点对结构
     * 点对是指 cdr 不是 Nil 且不是列表的 ConsCell
     */
    public func isDottedPair(): Bool {
        match (this) {
            case Cons(cell) => !cell.isProperList()
            case _ => false
        }
    }

    /**
     * 判断是否为过程
     */
    public func isProcedure(): Bool {
        match (this) {
            case Procedure(_, _, _) => true
            case Macro(_, _, _) => true
            case NativeFunc(_) => true
            case _ => false
        }
    }

    /**
     * 判断是否为宏
     */
    public func isMacro(): Bool {
        match (this) {
            case Macro(_, _, _) => true
            case _ => false
        }
    }

    /**
     * 转换为字符串表示
     */
    public func toString(): String {
        match (this) {
            case Nil => "nil"
            case Boolean(b) => if (b) { "true" } else { "false" }
            case Number(n) => n.toString()
            case Str(s) => "\"${s}\""
            case Symbol(s) => s
            case Cons(c) => c.toString()
            case Procedure(_, _, _) => "#<procedure>"
            case Macro(_, _, _) => "#<macro>"
            case NativeFunc(_) => "#<native-function>"
        }
    }

    /**
     * 深拷贝值
     */
    public func cloneValue(): LispValue {
        match (this) {
            case Nil => Nil
            case Boolean(b) => Boolean(b)
            case Number(n) => Number(n)
            case Str(s) => Str(s)
            case Symbol(s) => Symbol(s)
            case Cons(cell) => Cons(cell.clone())
            case Procedure(params, body, env) => Procedure(params, body, env.clone())
            case Macro(params, body, env) => Macro(params, body, env.clone())
            case NativeFunc(f) => NativeFunc(f)  // 原生函数不需要克隆
        }
    }
}

/**
 * 类型判断辅助函数
 */
public func isNil(value: LispValue): Bool {
    value.isNil()
}

public func isTruthy(value: LispValue): Bool {
    value.isTruthy()
}

public func isNumber(value: LispValue): Bool {
    value.isNumber()
}

public func isString(value: LispValue): Bool {
    value.isString()
}

public func isSymbol(value: LispValue): Bool {
    value.isSymbol()
}

public func isList(value: LispValue): Bool {
    value.isList()
}

public func isProcedure(value: LispValue): Bool {
    value.isProcedure()
}

/**
 * 值构造辅助函数
 */
public func nilValue(): LispValue {
    Nil
}

public func boolValue(b: Bool): LispValue {
    Boolean(b)
}

public func numberValue(n: Float64): LispValue {
    Number(n)
}

public func stringValue(s: String): LispValue {
    Str(s)
}

public func symbolValue(s: String): LispValue {
    Symbol(s)
}

public func consValue(car: LispValue, cdr: LispValue): LispValue {
    Cons(ConsCell(car, cdr))
}

public func procedureValue(params: ArrayList<String>, body: LispValue, env: Environment): LispValue {
    Procedure(params, body, env)
}

public func macroValue(params: ArrayList<String>, body: LispValue, env: Environment): LispValue {
    Macro(params, body, env)
}

/**
 * 列表构造辅助函数
 */
public func list(elements: ArrayList<LispValue>): LispValue {
    if (elements.size == 0) {
        Nil
    } else {
        let first = elements[0]
        let rest = ArrayList<LispValue>()
        for (i in 1..elements.size) {
            rest.add(elements[i])
        }
        consValue(first, list(rest))
    }
}

public func makeList(args: Array<LispValue>): LispValue {
    if (args.size == 0) {
        Nil
    } else {
        var result: LispValue = Nil
        var i = args.size - 1
        while (i >= 0) {
            result = consValue(args[i], result)
            i = i - 1
        }
        result
    }
}
