package ystyle::xisp.types

import std.collection.ArrayList
import std.collection.HashMap
import std.io.*
import std.collection.HashSet


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
     * 导出符号集合
     * 用于跟踪哪些符号被导出（用于包导入）
     */
    private var exportedSymbols: HashSet<String>

    /**
     * 默认构造函数
     */
    public init() {
        this.parent = None
        this.bindings = HashMap<String, LispValue>()
        this.keywordAliases = HashMap<String, String>()
        this.exportedSymbols = HashSet<String>()
    }

    /**
     * 带父环境的构造函数
     */
    public init(parent: Environment) {
        this.parent = Some(parent)
        this.bindings = HashMap<String, LispValue>()
        this.keywordAliases = HashMap<String, String>()
        this.exportedSymbols = HashSet<String>()
    }

    /**
     * 在当前环境中定义变量
     */
    public func define(name: String, value: LispValue) {
        this.bindings[name] = value
    }

    /**
     * 查找变量（在当前环境和所有父环境中查找）
     * 如果变量未定义，返回错误
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
                case None => LispValue.Error(XispError(ErrorType.UnboundVariable, "Undefined variable: '${name}'"))
            }
        }
    }

    /**
     * 检查变量是否存在（不包括错误）
     * 与 contains() 不同，这个方法不会误判 Error 类型的值
     */
    public func has(name: String): Bool {
        if (this.bindings.contains(name)) {
            true
        } else {
            match (this.parent) {
                case Some(env) => env.has(name)
                case None => false
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

    // ==================== 导出符号管理 ====================

    /**
     * 添加导出符号
     * @param symbolName 符号名称
     */
    public func addExportedSymbol(symbolName: String) {
        this.exportedSymbols.add(symbolName)
    }

    /**
     * 批量添加导出符号
     * @param symbolNames 符号名称列表
     */
    public func addExportedSymbols(symbolNames: ArrayList<String>) {
        for (name in symbolNames) {
            this.exportedSymbols.add(name)
        }
    }

    /**
     * 检查符号是否已导出
     * @param symbolName 符号名称
     * @return 如果符号已导出返回 true，否则返回 false
     */
    public func isExported(symbolName: String): Bool {
        this.exportedSymbols.contains(symbolName)
    }

    /**
     * 获取所有导出的符号名称
     * @return 导出符号名称列表
     */
    public func getExportedSymbols(): ArrayList<String> {
        let symbols = ArrayList<String>()
        for (sym in this.exportedSymbols) {
            symbols.add(sym)
        }
        symbols
    }

    /**
     * 获取导出符号的值（只获取已导出的符号）
     * @return 导出符号的键值对列表
     */
    public func getExportedBindings(): ArrayList<(String, LispValue)> {
        let bindings = ArrayList<(String, LispValue)>()
        for ((key, value) in this.bindings) {
            if (this.exportedSymbols.contains(key)) {
                bindings.add((key, value))
            }
        }
        bindings
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
 * 参数信息
 * 用于表示函数/宏的参数，包括是否为可变参数、是否为命名参数、默认值
 */
public class ParamInfo {
    public let name: String
    public let isRest: Bool
    public let isKey: Bool
    public let defaultValue: ?LispValue

    public init(name: String, isRest: Bool) {
        this.name = name
        this.isRest = isRest
        this.isKey = false
        this.defaultValue = None
    }

    public init(name: String, isRest: Bool, isKey: Bool, defaultValue: ?LispValue) {
        this.name = name
        this.isRest = isRest
        this.isKey = isKey
        this.defaultValue = defaultValue
    }
}

/**
 * 错误类型分类
 */
public enum ErrorType {
    | UnboundVariable      // 未定义变量
    | UndefinedFunction    // 未定义函数
    | TypeError           // 类型错误
    | ArityMismatch       // 参数数量不匹配
    | SyntaxError         // 语法错误
    | RuntimeError        // 运行时错误
    | IOError             // IO 错误
    | ModuleError         // 模块错误

    // 实现相等比较
    public operator func ==(right: ErrorType): Bool {
        match (this) {
            case UnboundVariable => match (right) {
                case UnboundVariable => true
                case _ => false
            }
            case UndefinedFunction => match (right) {
                case UndefinedFunction => true
                case _ => false
            }
            case TypeError => match (right) {
                case TypeError => true
                case _ => false
            }
            case ArityMismatch => match (right) {
                case ArityMismatch => true
                case _ => false
            }
            case SyntaxError => match (right) {
                case SyntaxError => true
                case _ => false
            }
            case RuntimeError => match (right) {
                case RuntimeError => true
                case _ => false
            }
            case IOError => match (right) {
                case IOError => true
                case _ => false
            }
            case ModuleError => match (right) {
                case ModuleError => true
                case _ => false
            }
        }
    }
}

/**
 * Xisp 错误类型
 */
public class XispError <: ToString {
    public let kind: ErrorType
    public let message: String
    public var stack: ArrayList<String>  // 调用栈

    public init(kind: ErrorType, message: String) {
        this.kind = kind
        this.message = message
        this.stack = ArrayList<String>()
    }

    public init(kind: ErrorType, message: String, stack: ArrayList<String>) {
        this.kind = kind
        this.message = message
        this.stack = stack
    }

    /**
     * 添加堆栈帧
     */
    public func pushFrame(frame: String) {
        this.stack.add(frame)
    }

    /**
     * 转换为字符串表示
     */
    public func toString(): String {
        let kindStr = match (this.kind) {
            case UnboundVariable => "UnboundVariable"
            case UndefinedFunction => "UndefinedFunction"
            case TypeError => "TypeError"
            case ArityMismatch => "ArityMismatch"
            case SyntaxError => "SyntaxError"
            case RuntimeError => "RuntimeError"
            case IOError => "IOError"
            case ModuleError => "ModuleError"
        }
        "[${kindStr}] ${this.message}"
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
     * 整数
     */
    | Int(Int64)

    /**
     * 浮点数
     */
    | Float(Float64)

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
     * 过程/函数（使用 ParamInfo，支持命名参数和默认值）
     *
     * 包含：参数信息列表、函数体、捕获的环境
     */
    | ProcedureFromParams(ArrayList<ParamInfo>, LispValue, Environment)

    /**
     * 宏（编译时元编程）
     *
     * 包含：参数列表、宏体、捕获的环境
     * 与 Procedure 不同，宏接收的是未求值的语法树
     */
    | Macro(ArrayList<String>, LispValue, Environment)

    /**
     * 宏（使用 ParamInfo，支持命名参数和默认值）
     *
     * 包含：参数信息列表、宏体、捕获的环境
     */
    | MacroFromParams(ArrayList<ParamInfo>, LispValue, Environment)

    /**
     * 原生函数（用仓颉实现的内置函数）
     *
     * 接收参数列表，返回结果
     */
    | NativeFunc((ArrayList<LispValue>) -> LispValue)

    /**
     * 哈希映射（字典）
     *
     * 键值对集合，键是字符串，值可以是任意 LispValue
     */
    | HashMap(HashMap<String, LispValue>)

    /**
     * 错误类型
     *
     * 用于表示运行时错误
     */
    | Error(XispError)

    /**
     * 判断是否为真值
     * 假值：nil、false、数字 0
     * 其他都是真
     */
    public func isTruthy(): Bool {
        match (this) {
            case Nil => false
            case Boolean(false) => false
            case Int(0) => false
            case Float(0.0) => false
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
     * 判断是否为数字（整数或浮点数）
     */
    public func isNumber(): Bool {
        match (this) {
            case Int(_) => true
            case Float(_) => true
            case _ => false
        }
    }

    /**
     * 判断是否为整数
     */
    public func isInteger(): Bool {
        match (this) {
            case Int(_) => true
            case _ => false
        }
    }

    /**
     * 判断是否为浮点数
     */
    public func isFloat(): Bool {
        match (this) {
            case Float(_) => true
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
            case ProcedureFromParams(_, _, _) => true
            case Macro(_, _, _) => true
            case MacroFromParams(_, _, _) => true
            case NativeFunc(_) => true
            case _ => false
        }
    }

    /**
     * 判断是否为哈希映射
     */
    public func isHashMap(): Bool {
        match (this) {
            case HashMap(_) => true
            case _ => false
        }
    }

    /**
     * 转换哈希映射为字符串表示
     */
    private func hashmapToString(map: HashMap<String, LispValue>): String {
        if (map.size == 0) {
            return "{}"
        }

        let sb = StringBuilder()
        sb.append("{")
        var first = true
        for ((key, value) in map) {
            if (!first) {
                sb.append(" ")
            }
            sb.append("${key} ${value.toString()}")
            first = false
        }
        sb.append("}")
        sb.toString()
    }

    /**
     * 判断是否为宏
     */
    public func isMacro(): Bool {
        match (this) {
            case Macro(_, _, _) => true
            case MacroFromParams(_, _, _) => true
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
            case Int(i) => i.toString()
            case Float(f) => f.toString()
            case Str(s) => "\"${s}\""
            case Symbol(s) => s
            case Cons(c) => c.toString()
            case Procedure(_, _, _) => "#<procedure>"
            case ProcedureFromParams(_, _, _) => "#<procedure>"
            case Macro(_, _, _) => "#<macro>"
            case MacroFromParams(_, _, _) => "#<macro>"
            case NativeFunc(_) => "#<native-function>"
            case HashMap(map) => hashmapToString(map)
            case Error(err) => err.toString()
        }
    }

    /**
     * 深拷贝值
     */
    public func cloneValue(): LispValue {
        match (this) {
            case Nil => Nil
            case Boolean(b) => Boolean(b)
            case Int(i) => Int(i)
            case Float(f) => Float(f)
            case Str(s) => Str(s)
            case Symbol(s) => Symbol(s)
            case Cons(cell) => Cons(cell.clone())
            case Procedure(params, body, env) => Procedure(params, body, env.clone())
            case ProcedureFromParams(params, body, env) => ProcedureFromParams(params, body, env.clone())
            case Macro(params, body, env) => Macro(params, body, env.clone())
            case MacroFromParams(params, body, env) => MacroFromParams(params, body, env.clone())
            case NativeFunc(f) => NativeFunc(f)  // 原生函数不需要克隆
            case HashMap(map) => HashMap(map)  // HashMap 是不可变的，直接复制引用
            case Error(err) => Error(err)  // 错误是不可变的，直接复制引用
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

public func isInteger(value: LispValue): Bool {
    value.isInteger()
}

public func isFloat(value: LispValue): Bool {
    value.isFloat()
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

public func isError(value: LispValue): Bool {
    match (value) {
        case Error(_) => true
        case _ => false
    }
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

public func intValue(i: Int64): LispValue {
    LispValue.Int(i)
}

public func floatValue(f: Float64): LispValue {
    LispValue.Float(f)
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

public func procedureFromParamsValue(params: ArrayList<ParamInfo>, body: LispValue, env: Environment): LispValue {
    ProcedureFromParams(params, body, env)
}

public func macroValue(params: ArrayList<String>, body: LispValue, env: Environment): LispValue {
    Macro(params, body, env)
}

public func macroFromParamsValue(params: ArrayList<ParamInfo>, body: LispValue, env: Environment): LispValue {
    MacroFromParams(params, body, env)
}

/**
 * 错误构造辅助函数
 */
public func errorValue(errorType: ErrorType, message: String): LispValue {
    LispValue.Error(XispError(errorType, message))
}

public func errorWithStack(errorType: ErrorType, message: String, stack: ArrayList<String>): LispValue {
    LispValue.Error(XispError(errorType, message, stack))
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
