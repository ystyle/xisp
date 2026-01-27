package ystyle::xisp.bridge

import ystyle::xisp.types.*
import std.collection.ArrayList
import std.collection.HashMap

/**
 * LispValue 扩展接口
 * 定义从 LispValue 转换为仓颉类型的便捷方法
 */
public interface ExtendLispValue {
    func asInt(): ?Int64
    func asFloat(): ?Float64
    func asString(): ?String
    func asBool(): ?Bool
    func asCjValue<T>(): ?T where T <: LispDeserializable<T>
}

/**
 * Lisp 反序列化接口 - 从 LispValue 转换为仓颉类型
 *
 * 任何类型都可以实现此接口以支持从 LispValue 转换
 * 与 LispConvertible 接口成对：
 * - LispConvertible: 仓颉 → Lisp（序列化）
 * - LispDeserializable: Lisp → 仓颉（反序列化）
 *
 * 使用示例：
 * ```
 * class MyType {
 *     let data: String
 * }
 *
 * extend MyType <: LispDeserializable<MyType> {
 *     public static func fromLisp(value: LispValue): ?MyType {
 *         match (value) {
 *             case Str(s) => Some(MyType(s))
 *             case _ => None
 *         }
 *     }
 * }
 *
 * // 使用方式 1：直接调用静态方法
 * let lispValue = Str("hello")
 * let myValue = MyType.fromLisp(lispValue)
 *
 * // 使用方式 2：通过 LispValue.asCjValue<T>() 调用
 * let myValue2 = lispValue.asCjValue<MyType>()
 * ```
 */
public interface LispDeserializable<T> {
    static func fromLisp(value:LispValue): ?T
}

// ============================================================================
// 内置类型的 LispDeserializable 实现
// ============================================================================

/**
 * 为 Int64 实现 LispDeserializable
 * 支持从 LispValue.Int 和 LispValue.Float 转换
 */
extend Int64 <: LispDeserializable<Int64> {
    public static func fromLisp(value: LispValue): ?Int64 {
        match (value) {
            case LispValue.Int(i) => Some(i)
            case LispValue.Float(f) => Some(Int64(f))
            case _ => None
        }
    }
}

/**
 * 为 Float64 实现 LispDeserializable
 * 支持从 LispValue.Int 和 LispValue.Float 转换
 */
extend Float64 <: LispDeserializable<Float64> {
    public static func fromLisp(value: LispValue): ?Float64 {
        match (value) {
            case LispValue.Float(f) => Some(f)
            case LispValue.Int(i) => Some(Float64(i))
            case _ => None
        }
    }
}

/**
 * 为 String 实现 LispDeserializable
 * 支持从 LispValue.Str 和 LispValue.Symbol 转换
 */
extend String <: LispDeserializable<String> {
    public static func fromLisp(value: LispValue): ?String {
        match (value) {
            case Str(s) => Some(s)
            case Symbol(s) => Some(s)
            case _ => None
        }
    }
}

/**
 * 为 Bool 实现 LispDeserializable
 * 支持从 LispValue.Boolean 转换
 */
extend Bool <: LispDeserializable<Bool> {
    public static func fromLisp(value: LispValue): ?Bool {
        match (value) {
            case Boolean(b) => Some(b)
            case _ => None
        }
    }
}

// ============================================================================
// LispValue 扩展方法
// ============================================================================

/**
 * 为 LispValue 添加类型转换扩展方法
 */
extend LispValue <: ExtendLispValue {
    /**
     * 泛型转换方法 - 使用 LispDeserializable 接口转换
     *
     * 使用示例：
     * ```
     * let lispValue = LispValue.Int(42)
     * let i = lispValue.asCjValue<Int64>()  // Some(42)
     *
     * let s = lispValue.asCjValue<String>()  // None
     * ```
     */
    public func asCjValue<T>(): ?T where T <: LispDeserializable<T> {
        T.fromLisp(this)
    }

    /**
     * 便捷方法：转换为 Int64
     *
     * 使用示例：
     * ```
     * let lispValue = LispValue.Int(42)
     * if (let Some(i) <- lispValue.asInt()) {
     *     println(i)  // 42
     * }
     * ```
     */
    public func asInt(): ?Int64 {
        Int64.fromLisp(this)
    }

    /**
     * 便捷方法：转换为 Float64
     */
    public func asFloat(): ?Float64 {
        Float64.fromLisp(this)
    }

    /**
     * 便捷方法：转换为 String
     */
    public func asString(): ?String {
        String.fromLisp(this)
    }

    /**
     * 便捷方法：转换为 Bool
     */
    public func asBool(): ?Bool {
        Bool.fromLisp(this)
    }
}

// ============================================================================
// 集合类型的 LispDeserializable 实现
// ============================================================================

/**
 * 为 ArrayList 实现 LispDeserializable（泛型版本）
 */
extend<T> ArrayList<T> <: LispDeserializable<ArrayList<T>> where T <: LispDeserializable<T> {
    public static func fromLisp(value: LispValue): ?ArrayList<T> {
        match (value) {
            case Nil => Some(ArrayList<T>())
            case Cons(cell) =>
                let result = ArrayList<T>()
                var current: LispValue = value
                while (let Cons(c) <- current) {
                    match (T.fromLisp(c.car)) {
                        case Some(item) => result.add(item)
                        case None => return None
                    }
                    current = c.cdr
                }
                Some(result)
            case _ => None
        }
    }
}

/**
 * 为 HashMap<String, V> 实现 LispDeserializable（泛型版本）
 */
extend<V> HashMap<String, V> <: LispDeserializable<HashMap<String, V>>
    where V <: LispDeserializable<V> {
    public static func fromLisp(value: LispValue): ?HashMap<String, V> {
        match (value) {
            case HashMap(lispMap) =>
                let result = HashMap<String, V>()
                for ((key, lispValue) in lispMap) {
                    match (V.fromLisp(lispValue)) {
                        case Some(v) => result[key] = v
                        case None => return None
                    }
                }
                Some(result)
            case _ => None
        }
    }
}
