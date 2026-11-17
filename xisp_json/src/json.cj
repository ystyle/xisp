package ystyle::xisp_json

// gjson 的 `parse` / `get` 与 std.convert 的 `Parsable.parse` 等重名，
// 逐个改名引入，避免解析到错误的目标
import gjson.Result
import gjson.Kind
import gjson.get as gjsonGet
import gjson.getMany as gjsonGetMany
import gjson.parse as gjsonParse
import gjson.valid as gjsonValid
import ystyle::xisp.types.*
import ystyle::xisp.core.eval.*

import std.collection.ArrayList
import std.collection.HashMap
// Int64.parse / Float64.parse 来自 std.convert 的 Parsable
import std.convert.*

/**
 * JSON 能力（能力域 `json`）
 *
 * 设计见 `docs/plans/2026-11-17-json-design.md`。四个符号：
 *
 * | 符号 | 语义 |
 * |---|---|
 * | `cangjie::json-get` | `(json-get json path)` → 值 或 `nil`（路径不存在不是错误） |
 * | `cangjie::json-get-many` | `(json-get-many json paths)` → 值的列表 |
 * | `cangjie::json-parse` | `(json-parse json)` → `LispValue` 树 |
 * | `cangjie::json-valid?` | `(json-valid? json)` → `Bool` |
 *
 * 分两层不是冗余：`json-get` 走 gjson 的路径直达，**不把整棵树转成 LispValue**
 * （大响应省内存）；`json-parse` 才转树，便于遍历与模式匹配。
 */

/**
 * 数字的原始文本是否为整数形态（无小数点、无指数）
 *
 * JSON 本身不分整数与浮点，但 Lisp 用户分。判据只看**文本形态**，
 * 不看能否装进 `Int64` —— 后者是下一步的事（装不下就回落浮点）。
 */
func isIntegerForm(raw: String): Bool {
    for (b in raw) {
        if (b == b'.' || b == b'e' || b == b'E') {
            return false
        }
    }
    return true
}

/**
 * 数字映射：整数形态且装得下 `Int64` → `Int`，否则 `Float`
 *
 * 用 `raw`（原始文本）而不是 `num`（`Float64`）—— 后者超过 2^53 会丢精度。
 * 装不下 `Int64` 时回落浮点并在文档里标注为已知限制。
 */
func numberFromRaw(raw: String): LispValue {
    if (isIntegerForm(raw)) {
        var intValue: Int64 = 0
        var parsed = false
        try {
            intValue = Int64.parse(raw)
            parsed = true
        } catch (e: Exception) {
            parsed = false
        }
        if (parsed) {
            return LispValue.Int(intValue)
        }
    }
    try {
        return LispValue.Float(Float64.parse(raw))
    } catch (e: Exception) {
        // gjson 已保证 Kind.Number，走到这里说明文本异常；给 nil 而不是编造数值
        LispValue.Nil
    }
}

/**
 * `gjson.Result` → `LispValue`（递归）
 *
 * 映射契约（设计文档 §5）：
 * `null`→`Nil`、`true`/`false`→`Boolean`、`string`→`Str`、
 * `number`→`Int`/`Float`（见 `numberFromRaw`）、
 * `array`→`Cons` 列表、`object`→`HashMap`。
 *
 * **空数组与 `null` 都得到 `Nil`** —— xisp 里空列表就是 `nil`（`'()` = `nil`，
 * 见 `core/src/types/types.cj` 的 `list()`），这是语言既有的语义，
 * 本模块不为此破例；需要区分时看原始文本（`cangjie::json-valid?` 无法区分，
 * 用 `json-get` 取该字段的 `raw` 语义无法表达，故文档里建议：需要区分就别用 `json-parse`，
 * 直接用路径取到具体字段）。
 */
func resultToLisp(r: Result): LispValue {
    match (r.kind) {
        case Kind.Null => LispValue.Nil
        case Kind.False => LispValue.Boolean(false)
        case Kind.True => LispValue.Boolean(true)
        case Kind.String => LispValue.Str(r.str)
        case Kind.Number => numberFromRaw(r.raw)
        case Kind.JSON =>
            if (r.isArray()) {
                let items = ArrayList<LispValue>()
                for (item in r.array()) {
                    items.add(resultToLisp(item))
                }
                list(items)
            } else {
                let m = HashMap<String, LispValue>()
                for ((k, v) in r.map()) {
                    m[k] = resultToLisp(v)
                }
                LispValue.HashMap(m)
            }
    }
}

/**
 * JSON 文本 → `LispValue`（内部/测试用，不做语法校验）
 *
 * 对外入口（`cangjie::json-parse`）会先 `valid` 再走这里；测试直接调它，
 * 免去在测试文件里重复 gjson 的别名 import。
 */
func parseText(jsonText: String): LispValue {
    resultToLisp(gjsonParse(jsonText))
}

/** 取第 `idx` 个参数为字符串；不是字符串或越界时返回 `None` */
func stringArg(args: ArrayList<LispValue>, idx: Int64): ?String {
    if (idx >= args.size) {
        return None
    }
    match (args[idx]) {
        case LispValue.Str(s) => Some(s)
        case _ => None
    }
}

/** 取第 `idx` 个参数为字符串列表；不是列表或元素不是字符串时返回 `None` */
func stringListArg(args: ArrayList<LispValue>, idx: Int64): ?ArrayList<String> {
    if (idx >= args.size) {
        return None
    }
    let out = ArrayList<String>()
    var cursor = args[idx]
    while (let Cons(cell) <- cursor) {
        match (cell.car) {
            case LispValue.Str(s) => out.add(s)
            case _ => return None
        }
        cursor = cell.cdr
    }
    match (cursor) {
        case Nil => Some(out)
        // 非正规列表（点对）不接受
        case _ => None
    }
}

/**
 * 注册 JSON 能力域的符号。
 *
 * 参数与错误契约见设计文档 §6：
 * 语法非法 → `SyntaxError`；路径不存在 → `nil`；类型错 → `TypeError`；
 * 参数个数错 → `ArityMismatch`。**不新增 `ErrorType`**。
 */
public func registerJson(env: Environment): Unit {
    // (cangjie::json-get json path) → 值 或 nil
    env.define("cangjie::json-get", NativeFunc({ args =>
        if (args.size != 2) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "json-get expects 2 arguments: json path"))
        }
        match (stringArg(args, 0)) {
            case Some(jsonText) =>
                match (stringArg(args, 1)) {
                    case Some(path) =>
                        if (!gjsonValid(jsonText)) {
                            return LispValue.Error(XispError(ErrorType.SyntaxError,
                                "json-get: invalid JSON text"))
                        }
                        let r = gjsonGet(jsonText, path)
                        if (!r.exists()) {
                            return LispValue.Nil
                        }
                        resultToLisp(r)
                    case None =>
                        LispValue.Error(XispError(ErrorType.TypeError,
                            "json-get: second argument must be a string"))
                }
            case None =>
                LispValue.Error(XispError(ErrorType.TypeError,
                    "json-get: first argument must be a string"))
        }
    }))

    // (cangjie::json-get-many json paths) → 值的列表（顺序与 paths 一致）
    env.define("cangjie::json-get-many", NativeFunc({ args =>
        if (args.size != 2) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "json-get-many expects 2 arguments: json paths"))
        }
        match (stringArg(args, 0)) {
            case Some(jsonText) =>
                match (stringListArg(args, 1)) {
                    case Some(paths) =>
                        if (!gjsonValid(jsonText)) {
                            return LispValue.Error(XispError(ErrorType.SyntaxError,
                                "json-get-many: invalid JSON text"))
                        }
                        let out = ArrayList<LispValue>()
                        for (r in gjsonGetMany(jsonText, paths.toArray())) {
                            if (r.exists()) {
                                out.add(resultToLisp(r))
                            } else {
                                out.add(LispValue.Nil)
                            }
                        }
                        list(out)
                    case None =>
                        LispValue.Error(XispError(ErrorType.TypeError,
                            "json-get-many: second argument must be a list of strings"))
                }
            case None =>
                LispValue.Error(XispError(ErrorType.TypeError,
                    "json-get-many: first argument must be a string"))
        }
    }))

    // (cangjie::json-parse json) → LispValue 树
    env.define("cangjie::json-parse", NativeFunc({ args =>
        if (args.size != 1) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "json-parse expects 1 argument: json"))
        }
        match (stringArg(args, 0)) {
            case Some(jsonText) =>
                if (!gjsonValid(jsonText)) {
                    return LispValue.Error(XispError(ErrorType.SyntaxError,
                        "json-parse: invalid JSON text"))
                }
                resultToLisp(gjsonParse(jsonText))
            case None =>
                LispValue.Error(XispError(ErrorType.TypeError,
                    "json-parse: argument must be a string"))
        }
    }))

    // (cangjie::json-valid? json) → Bool
    // 谓词惯例：非字符串返回 false（与 `exists?` 等既有谓词一致），不报 TypeError ——
    // "不是字符串"本来就不是合法 JSON 文本，判断结果为假。
    env.define("cangjie::json-valid?", NativeFunc({ args =>
        if (args.size != 1) {
            return LispValue.Boolean(false)
        }
        match (args[0]) {
            case LispValue.Str(jsonText) => LispValue.Boolean(gjsonValid(jsonText))
            case _ => LispValue.Boolean(false)
        }
    }))
}

/**
 * JSON 能力域。
 *
 * 纯计算：不碰文件、不碰网络、不碰进程，因此 `escapable = false`、`hostPolicy = false`
 * —— 会被 `presetPure` 推导纳入。
 */
public func jsonDomain(): LispCapability {
    SimpleCapability("json",
        ["cangjie::json-get", "cangjie::json-get-many", "cangjie::json-parse", "cangjie::json-valid?"],
        { ctx => registerJson(ctx.env) },
        escapable: false, hostPolicy: false)
}
