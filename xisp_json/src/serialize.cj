package ystyle::xisp_json

import ystyle::xisp.types.*

import std.collection.ArrayList
import std.convert.*
import std.convert.Formattable
import std.collection.HashMap as StdHashMap

/**
 * Lisp 值 → JSON 文本（`json-dump`）
 *
 * gjson 是**只读**库（没有 encode / marshal），所以序列化自己写 —— 结构简单，
 * 16 个变体里可序列化的只有 7 个，零依赖且完全可控。
 *
 * ## 可序列化
 *
 * | Lisp | JSON |
 * |---|---|
 * | `nil` | `null` |
 * | 布尔 | `true` / `false` |
 * | 整数 / 浮点 | 数字 |
 * | 字符串 | 字符串（转义） |
 * | 列表（proper） | 数组 |
 * | `HashMap` | 对象 |
 *
 * ## 不可序列化
 *
 * 过程 / 原生函数 / 宏 / 编译闭包 / 外部值 / 错误值 → `Error(TypeError)`。
 * 这些都没有 JSON 对应物，静默丢掉会让调用方拿到"看起来成功"的截断数据。
 *
 * ## 两条防坑
 *
 * - **浮点必须保留小数点**：`1.0` 若输出成 `1`，`json-parse` 回来就变整数，
 *   往返不一致（`(+ x 1)` 的行为都会变）。
 * - **深度上限**：`HashMap` 可以把自己装进去（`(hashmap-set! h "self" h)`），
 *   形成环。没有上限就是无限递归 —— 直接栈溢出而不是给个错误。
 */
const MAX_DEPTH: Int64 = 64

/** 序列化入口：返回 `Str` 或 `Error` */
func dumpJson(value: LispValue, pretty: Bool): LispValue {
    let sb = StringBuilder()
    match (writeValue(value, sb, 0, pretty)) {
        case None => LispValue.Str(sb.toString())
        case Some(err) => err
    }
}

/** 失败时返回错误值；成功返回 `None` */
func writeValue(value: LispValue, sb: StringBuilder, depth: Int64, pretty: Bool): ?LispValue {
    if (depth > MAX_DEPTH) {
        return Some(LispValue.Error(XispError(ErrorType.RuntimeError,
            "json-dump: nesting deeper than ${MAX_DEPTH} (circular structure?)")))
    }
    match (value) {
        case Nil => sb.append("null")
        case LispValue.Boolean(b) => sb.append(if (b) { "true" } else { "false" })
        case LispValue.Int(n) => sb.append(n.toString())
        case LispValue.Float(f) =>
            match (formatFloat(f)) {
                case Some(s) => sb.append(s)
                case None => return Some(LispValue.Error(XispError(ErrorType.TypeError,
                    "json-dump: cannot serialize ${f} (JSON has no NaN/Infinity)")))
            }
        case LispValue.Str(s) => writeString(s, sb)
        case LispValue.Cons(c) =>
            match (writeArray(c, sb, depth, pretty)) {
                case Some(err) => return Some(err)
                case None => ()
            }
        case LispValue.HashMap(m) =>
            match (writeObject(m, sb, depth, pretty)) {
                case Some(err) => return Some(err)
                case None => ()
            }
        case other =>
            return Some(LispValue.Error(XispError(ErrorType.TypeError,
                "json-dump: cannot serialize ${valueTypeName(value)}")))
    }
    None
}

/** 不可序列化变体的类型名（只用来拼错误信息） */
func valueTypeName(v: LispValue): String {
    match (v) {
        case Procedure(_, _, _) | ProcedureFromParams(_, _, _) => "a procedure"
        case NativeFunc(_) => "a native function"
        case Macro(_, _, _) | MacroFromParams(_, _, _) => "a macro"
        case Closure(_, _) => "a compiled closure"
        case External(_) => "an external value"
        case Error(_) => "an error value"
        case Symbol(_) => "a symbol"
        case _ => "this value"
    }
}

/**
 * 浮点格式化：**必须带小数点或指数**
 *
 * `1.0` 若给成 `1`，`json-parse` 回来就成整数了。
 */
func formatFloat(f: Float64): ?String {
    if (f.isNaN() || f.isInf()) {
        return None
    }
    // 不能用 `f.toString()` —— 仓颉内置的 Float64.toString 只给 **6 位小数**
    // （实测 1/3 → "0.333333"、π → "3.141593"），序列化会静默丢精度。
    // 仓颉的 format **不带 `%` 前缀**（写 "%.17g" 会抛 IllegalArgumentException，
    // 实测所有带 % 的都失败）。`.17g` 是双精度的可往返精度，g 格式自动去尾零。
    var s = f.format(".17g")
    if (!s.contains(".") && !s.contains("e") && !s.contains("E")) {
        s = s + ".0"
    }
    Some(s)
}

/** 字符串转义（按 Rune 遍历，非 ASCII 原样保留 UTF-8） */
func writeString(s: String, sb: StringBuilder): Unit {
    sb.append("\"")
    for (r in s.toRuneArray()) {
        if (r == r'"') {
            sb.append("\\\"")
        } else if (r == r'\\') {
            sb.append("\\\\")
        } else if (r == r'\n') {
            sb.append("\\n")
        } else if (r == r'\r') {
            sb.append("\\r")
        } else if (r == r'\t') {
            sb.append("\\t")
        } else if (UInt32(r) < 0x20) {
            // 其余控制字符走 \u00XX（JSON 规定必须转义 < 0x20）
            let hex = ["0", "1", "2", "3", "4", "5", "6", "7",
                       "8", "9", "A", "B", "C", "D", "E", "F"]
            let n = Int64(UInt32(r))
            sb.append("\\u00")
            sb.append(hex[n / 16])
            sb.append(hex[n % 16])
        } else {
            sb.append(r)
        }
    }
    sb.append("\"")
}

/** 数组（列表）；dotted pair 没有 JSON 对应物，直接报错 */
func writeArray(head: ConsCell, sb: StringBuilder, depth: Int64, pretty: Bool): ?LispValue {
    sb.append("[")
    var cursor: LispValue = Cons(head)
    var count: Int64 = 0
    while (true) {
        match (cursor) {
            case Cons(cell) =>
                if (count > 0) {
                    sb.append(",")
                }
                if (pretty) {
                    newlineIndent(sb, depth + 1)
                }
                match (writeValue(cell.car, sb, depth + 1, pretty)) {
                    case Some(err) => return Some(err)
                    case None => ()
                }
                count += 1
                cursor = cell.cdr
            case Nil =>
                break
            case other =>
                return Some(LispValue.Error(XispError(ErrorType.TypeError,
                    "json-dump: cannot serialize an improper list " +
                    "(tail is ${valueTypeName(other)})")))
        }
    }
    if (pretty && count > 0) {
        newlineIndent(sb, depth)
    }
    sb.append("]")
    None
}

/** 对象（HashMap）；键已是字符串（xisp 的 HashMap 键会归一化） */
func writeObject(m: StdHashMap<String, LispValue>, sb: StringBuilder,
                 depth: Int64, pretty: Bool): ?LispValue {
    sb.append("{")
    var count: Int64 = 0
    for ((k, v) in m) {
        if (count > 0) {
            sb.append(",")
        }
        if (pretty) {
            newlineIndent(sb, depth + 1)
        }
        writeString(k, sb)
        sb.append(if (pretty) { ": " } else { ":" })
        match (writeValue(v, sb, depth + 1, pretty)) {
            case Some(err) => return Some(err)
            case None => ()
        }
        count += 1
    }
    if (pretty && count > 0) {
        newlineIndent(sb, depth)
    }
    sb.append("}")
    None
}

/** pretty 模式的换行 + 缩进（2 空格/层） */
func newlineIndent(sb: StringBuilder, depth: Int64): Unit {
    sb.append("\n")
    for (_ in 0..depth) {
        sb.append("  ")
    }
}
