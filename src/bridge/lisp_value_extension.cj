package ystyle::xisp.bridge

import ystyle::xisp.core.*
import ystyle::xisp.types.*
import std.collection.ArrayList
import std.collection.HashMap
import std.collection.HashSet

/**
 * Lisp 可转换接口
 * 所有可以转换为 LispValue 的类型都需要实现此接口
 *
 * 使用场景：
 * - 在桥接层将仓颉类型转换为 Lisp 值
 * - 在内置函数中接受仓颉类型参数
 * - 统一类型转换逻辑
 */
public interface LispConvertible {
    func toLisp(): LispValue
}

/**
 * 为基本类型实现 LispConvertible 接口
 */

// 整数类型
extend Int8 <: LispConvertible {
    public func toLisp(): LispValue {
        LispValue.Int(Int64(this))
    }
}

extend Int16 <: LispConvertible {
    public func toLisp(): LispValue {
        LispValue.Int(Int64(this))
    }
}

extend Int32 <: LispConvertible {
    public func toLisp(): LispValue {
        LispValue.Int(Int64(this))
    }
}

extend Int64 <: LispConvertible {
    public func toLisp(): LispValue {
        LispValue.Int(this)
    }
}

// 无符号整数
extend UInt8 <: LispConvertible {
    public func toLisp(): LispValue {
        LispValue.Int(Int64(this))
    }
}

extend UInt16 <: LispConvertible {
    public func toLisp(): LispValue {
        LispValue.Int(Int64(this))
    }
}

extend UInt32 <: LispConvertible {
    public func toLisp(): LispValue {
        LispValue.Int(Int64(this))
    }
}

extend UInt64 <: LispConvertible {
    public func toLisp(): LispValue {
        LispValue.Int(Int64(this))
    }
}

// 浮点类型
extend Float16 <: LispConvertible {
    public func toLisp(): LispValue {
        LispValue.Float(Float64(this))
    }
}

extend Float32 <: LispConvertible {
    public func toLisp(): LispValue {
        LispValue.Float(Float64(this))
    }
}

extend Float64 <: LispConvertible {
    public func toLisp(): LispValue {
        LispValue.Float(this)
    }
}

// 字符和字符串
extend Rune <: LispConvertible {
    public func toLisp(): LispValue {
        Str(String(this))
    }
}

extend String <: LispConvertible {
    public func toLisp(): LispValue {
        Str(this)
    }
}

// 布尔值
extend Bool <: LispConvertible {
    public func toLisp(): LispValue {
        Boolean(this)
    }
}

// 数组类型 - 使用泛型约束
extend<T> Array<T> <: LispConvertible where T <: LispConvertible {
    public func toLisp(): LispValue {
        // 先收集到 ArrayList，然后转换为 Lisp 列表
        let temp = ArrayList<LispValue>()
        for (item in this) {
            temp.add(item.toLisp())
        }
        // 使用内置的 list 函数
        list(temp)
    }
}

extend<T> ArrayList<T> <: LispConvertible where T <: LispConvertible {
    public func toLisp(): LispValue {
        // 先收集到 ArrayList，然后转换为 Lisp 列表
        let temp = ArrayList<LispValue>()
        for (item in this) {
            temp.add(item.toLisp())
        }
        // 使用内置的 list 函数
        list(temp)
    }
}

// Option 类型 - 使用泛型约束
extend<T> Option<T> <: LispConvertible where T <: LispConvertible {
    public func toLisp(): LispValue {
        match (this) {
            case Some(v) => v.toLisp()
            case None => Nil
        }
    }
}

// HashMap 类型 - 使用泛型约束（键需要能转换为字符串）
extend<K, V> HashMap<K, V> <: LispConvertible
    where K <: ToString & LispConvertible, V <: LispConvertible {
    public func toLisp(): LispValue {
        // 转换为 Lisp 列表：((key1 . value1) (key2 . value2) ...)
        let temp = ArrayList<LispValue>()
        for ((key, value) in this) {
            // 创建键值对 (key . value)
            let pair = consValue(key.toLisp(), consValue(value.toLisp(), Nil))
            temp.add(pair)
        }
        list(temp)
    }
}

// HashSet 类型 - 使用泛型约束
extend<T> HashSet<T> <: LispConvertible where T <: LispConvertible {
    public func toLisp(): LispValue {
        let temp = ArrayList<LispValue>()
        for (item in this) {
            temp.add(item.toLisp())
        }
        list(temp)
    }
}

/**
 * 类型转换器 - 简化类型转换操作
 *
 * 现在可以使用泛型约束来接受所有 LispConvertible 类型
 */
public class TypeConverter {
    /**
     * 将任意 LispConvertible 类型转换为 LispValue
     *
     * 使用示例：
     * ```
     * let values = ArrayList<Int64>()
     * let lispValues = toLispList(values)
     * ```
     */
    public static func from<T>(value: T): LispValue where T <: LispConvertible {
        value.toLisp()
    }

    /**
     * 将仓颉列表转换为 LispValue 列表
     *
     * 使用泛型约束，所有 LispConvertible 类型都可以使用
     */
    public static func fromList<T>(values: ArrayList<T>): LispValue
        where T <: LispConvertible {
        let temp = ArrayList<LispValue>()
        for (v in values) {
            temp.add(v.toLisp())
        }
        list(temp)
    }

    /**
     * 从 LispValue 中提取字符串（如果可能）
     */
    public static func asString(value: LispValue): ?String {
        match (value) {
            case Str(s) => Some(s)
            case Symbol(s) => Some(s)
            case _ => None
        }
    }

    /**
     * 从 LispValue 中提取浮点数（如果可能）
     */
    public static func asFloat(value: LispValue): ?Float64 {
        match (value) {
            case LispValue.Int(i) => Some(Float64(i))
            case LispValue.Float(f) => Some(f)
            case _ => None
        }
    }

    /**
     * 从 LispValue 中提取整数（如果可能）
     */
    public static func asInt64(value: LispValue): ?Int64 {
        match (value) {
            case LispValue.Int(i) => Some(i)
            case LispValue.Float(f) => Some(Int64(f))
            case _ => None
        }
    }

    /**
     * 从 LispValue 中提取布尔值（如果可能）
     */
    public static func asBool(value: LispValue): ?Bool {
        match (value) {
            case Boolean(b) => Some(b)
            case _ => None
        }
    }
}

/**
 * 辅助函数 - 创建 Lisp 列表的便捷方法
 */

/**
 * 将任意 LispConvertible 列表转换为 LispValue
 *
 * 使用示例：
 * ```
 * let numbers = ArrayList<Int64>([1, 2, 3])
 * let lispList = toLispList(numbers)  // => (1 2 3)
 * ```
 */
public func toLispList<T>(values: ArrayList<T>): LispValue
    where T <: LispConvertible {
    TypeConverter.fromList(values)
}

/**
 * 将单个值转换为 LispValue
 */
public func toLisp<T>(value: T): LispValue
    where T <: LispConvertible {
    TypeConverter.from(value)
}
