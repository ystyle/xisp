package ystyle::xisp_json

import ystyle::xisp.types.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import std.collection.HashMap
import std.convert.Formattable

/**
 * JSON 映射规则的白盒测试（设计文档 §5）
 *
 * 这些是包内函数，黑盒测不到；映射是契约的一部分，必须逐条钉住。
 */
@Test
class JsonMappingTest {
    /** 整数形态判据只看文本形态 */
    @TestCase
    func testIntegerForm() {
        @Assert(isIntegerForm("42"))
        @Assert(isIntegerForm("-7"))
        @Assert(isIntegerForm("0"))
        @Assert(isIntegerForm("9223372036854775807"))
        // 有小数点 / 指数 → 不是整数形态
        @Assert(!isIntegerForm("42.0"))
        @Assert(!isIntegerForm("1e10"))
        @Assert(!isIntegerForm("1E10"))
        @Assert(!isIntegerForm("-0.5"))
    }

    /**
     * 数字映射：整数形态且装得下 Int64 → `Int`，否则 `Float`
     *
     * 关键用例是第一条 —— `42` 必须给 `Int` 而不是 `Float`，
     * 否则 `(+ n 1)` 会得到 `43.0`，Lisp 用户不能接受。
     */
    @TestCase
    func testNumberMapping() {
        match (numberFromRaw("42")) {
            case LispValue.Int(n) => @Assert(n == 42)
            case other => @Fail("42 应映射为 Int，实得 ${other.toString()}")
        }
        match (numberFromRaw("-7")) {
            case LispValue.Int(n) => @Assert(n == -7)
            case other => @Fail("-7 应映射为 Int，实得 ${other.toString()}")
        }
        match (numberFromRaw("42.0")) {
            case LispValue.Float(f) => @Assert(f == 42.0)
            case other => @Fail("42.0 应映射为 Float，实得 ${other.toString()}")
        }
        match (numberFromRaw("1e3")) {
            case LispValue.Float(f) => @Assert(f == 1000.0)
            case other => @Fail("1e3 应映射为 Float，实得 ${other.toString()}")
        }
        // Int64 边界：正好装得下 → Int
        match (numberFromRaw("9223372036854775807")) {
            case LispValue.Int(n) => @Assert(n == Int64.Max)
            case other => @Fail("Int64 上界应映射为 Int，实得 ${other.toString()}")
        }
        // 超出 Int64 → 回落 Float（已知限制，JSON 无 BigInt）
        match (numberFromRaw("9223372036854775808")) {
            case LispValue.Float(_) => ()
            case other => @Fail("超 Int64 应回落 Float，实得 ${other.toString()}")
        }
    }

    /** 六个变体的映射 */
    @TestCase
    func testScalarMapping() {
        @Assert(parseText("null").toString() == "nil")
        @Assert(parseText("true").toString() == "true")
        @Assert(parseText("false").toString() == "false")
        @Assert(parseText("\"hi\"").toString() == "\"hi\"")
        @Assert(parseText("42").toString() == "42")
        @Assert(parseText("42.5").toString() == "42.500000")
    }

    /** object → HashMap，可用 hashmap-get 取值 */
    @TestCase
    func testObjectMapping() {
        let v = parseText("{\"a\":1,\"b\":\"x\"}")
        match (v) {
            case LispValue.HashMap(m) =>
                @Assert(m.size == 2)
                @Assert(m["a"].toString() == "1")
                @Assert(m["b"].toString() == "\"x\"")
            case other => @Fail("object 应映射为 HashMap，实得 ${other.toString()}")
        }
    }

    /** array → Cons 列表（不加 vector 变体） */
    @TestCase
    func testArrayMapping() {
        let v = parseText("[1,2,3]")
        match (v) {
            case LispValue.Cons(_) => ()
            case other => @Fail("array 应映射为 Cons 列表，实得 ${other.toString()}")
        }
        @Assert(v.toString() == "(1 2 3)")
        // 空数组 → 空列表；而 xisp 里**空列表就是 Nil**（Lisp 传统：'() = nil），
        // 所以 `[]` 与 `null` 映射后不可区分。这是语言的既有语义，不是本模块的缺陷，
        // 已在用户文档里写明。
        @Assert(parseText("[]").toString() == "nil")
    }

    /** 嵌套结构与 null 值 */
    @TestCase
    func testNestedMapping() {
        let v = parseText("{\"list\":[{\"n\":1},{\"n\":2}],\"none\":null}")
        match (v) {
            case LispValue.HashMap(m) =>
                @Assert(m["none"].toString() == "nil")
                let inner = m["list"]
                match (inner) {
                    case LispValue.Cons(cell) =>
                        match (cell.car) {
                            case LispValue.HashMap(im) => @Assert(im["n"].toString() == "1")
                            case other => @Fail("嵌套元素应为 HashMap，实得 ${other.toString()}")
                        }
                    case other => @Fail("list 应为 Cons，实得 ${other.toString()}")
                }
            case other => @Fail("应映射为 HashMap，实得 ${other.toString()}")
        }
    }

    /** 参数辅助函数：越界 / 类型错都给 None */
    @TestCase
    func testArgHelpers() {
        let args = ArrayList<LispValue>()
        args.add(LispValue.Str("s"))
        args.add(LispValue.Int(1))
        @Assert(stringArg(args, 0) == Some("s"))
        @Assert(stringArg(args, 1).isNone())
        @Assert(stringArg(args, 5).isNone())

        let listArgs = ArrayList<LispValue>()
        let strs = ArrayList<LispValue>()
        strs.add(LispValue.Str("a"))
        strs.add(LispValue.Str("b"))
        listArgs.add(list(strs))
        match (stringListArg(listArgs, 0)) {
            case Some(out) => @Assert(out.size == 2 && out[0] == "a" && out[1] == "b")
            case None => @Fail("应为 Some 字符串列表")
        }
        // 非列表
        @Assert(stringListArg(args, 1).isNone())
        // 元素不是字符串
        let bad = ArrayList<LispValue>()
        let badStrs = ArrayList<LispValue>()
        badStrs.add(LispValue.Int(1))
        bad.add(list(badStrs))
        @Assert(stringListArg(bad, 0).isNone())
    }

    // ==================== json-dump（序列化） ====================

    /** 取字符串结果，非 Str 直接失败（序列化成功必然给 Str） */
    func expectStr(r: LispValue, what: String): String {
        match (r) {
            case LispValue.Str(s) => s
            case other => throw Exception("${what}: 期望 Str，实得 ${other.toString()}")
        }
    }

    /** 标量序列化 */
    @TestCase
    func testDumpScalars() {
        println("\n=== json-dump 标量 ===")
        @Assert(expectStr(dumpJson(Nil, false), "nil") == "null")
        @Assert(expectStr(dumpJson(LispValue.Boolean(true), false), "true") == "true")
        @Assert(expectStr(dumpJson(LispValue.Boolean(false), false), "false") == "false")
        @Assert(expectStr(dumpJson(LispValue.Int(42), false), "42") == "42")
        @Assert(expectStr(dumpJson(LispValue.Int(-7), false), "-7") == "-7")
        @Assert(expectStr(dumpJson(LispValue.Str("hi"), false), "hi") == "\"hi\"")
        println("✓ null / true / false / 整数 / 字符串")
    }

    /**
     * **浮点必须保留小数点**
     *
     * `1.0` 若输出成 `1`，json-parse 回来就成整数 —— 往返不一致，
     * 连 `(+ x 1)` 的行为都会变。
     */
    @TestCase
    func testDumpFloatKeepsDecimal() {
        println("\n=== json-dump 浮点保留小数点 ===")
        @Assert(expectStr(dumpJson(LispValue.Float(1.0), false), "1.0") == "1.0")
        @Assert(expectStr(dumpJson(LispValue.Float(1.5), false), "1.5") == "1.5")
        @Assert(expectStr(dumpJson(LispValue.Float(-0.25), false), "-0.25") == "-0.25")
        // 大数走指数形态也合法
        let big = expectStr(dumpJson(LispValue.Float(1.0e20), false), "1e20")
        @Assert(big.contains("e") || big.contains("E") || big.contains("."))
        println("✓ 1.0 → \"1.0\"（不是 \"1\"）")
    }

    /**
     * **序列化不能丢精度**
     *
     * 仓颉内置的 `Float64.toString()` 只给 6 位小数（`1/3` → `0.333333`、`π` →
     * `3.141593`），拿它写 JSON 会静默截断。必须用 `.17g`（可往返精度）。
     *
     * 这条用例钉住的就是「不能退回 toString」。
     */
    @TestCase
    func testDumpFloatKeepsPrecision() {
        println("\n=== json-dump 浮点不丢精度 ===")
        // 1/3 的 17 位有效数字表示
        let third = expectStr(dumpJson(LispValue.Float(1.0/3.0), false), "third")
        @Assert(third == "0.33333333333333331")
        // π
        let pi = expectStr(dumpJson(LispValue.Float(3.141592653589793), false), "pi")
        @Assert(pi == "3.1415926535897931")
        // 往返：写出去的文本 parse 回来还是浮点（不是整数、不丢位）
        let back = parseText("[" + third + ", " + pi + "]")
        match (back) {
            case LispValue.Cons(cell) =>
                match (cell.car) {
                    case LispValue.Float(f) =>
                        @Assert(f.toString() == "0.333333")
                    case other => @Fail("1/3 应解析回 Float，实得 ${other.toString()}")
                }
            case other => @Fail("往返解析失败：${other.toString()}")
        }
        println("✓ 1/3 与 π 都是 17 位有效数字（不是 toString 的 6 位）")
    }

    /** NaN / Infinity 没有 JSON 对应物 */
    @TestCase
    func testDumpRejectsSpecialFloats() {
        println("\n=== json-dump 拒绝 NaN/Inf ===")
        for (f in [Float64.NaN, Float64.Inf, -Float64.Inf]) {
            let r = dumpJson(LispValue.Float(f), false)
            match (r) {
                case LispValue.Error(e) =>
                    @Assert(e.kind == ErrorType.TypeError)
                    @Assert(e.message.contains("NaN"))
                case other => @Fail("应报 TypeError，实得 ${other.toString()}")
            }
        }
        println("✓ NaN / Infinity 报 TypeError")
    }

    /** 字符串转义 */
    @TestCase
    func testDumpStringEscaping() {
        println("\n=== json-dump 字符串转义 ===")
        // 引号、反斜杠、换行、制表
        @Assert(expectStr(dumpJson(LispValue.Str("a\"b"), false), "q") == "\"a\\\"b\"")
        @Assert(expectStr(dumpJson(LispValue.Str("a\\b"), false), "bs") == "\"a\\\\b\"")
        @Assert(expectStr(dumpJson(LispValue.Str("a\nb"), false), "nl") == "\"a\\nb\"")
        @Assert(expectStr(dumpJson(LispValue.Str("a\tb"), false), "tab") == "\"a\\tb\"")
        // 控制字符走 \u00XX
        @Assert(expectStr(dumpJson(LispValue.Str("a\u{0001}b"), false), "ctl") == "\"a\\u0001b\"")
        // 非 ASCII 原样保留（不转 \u）
        @Assert(expectStr(dumpJson(LispValue.Str("中文"), false), "cn") == "\"中文\"")
        println("✓ 引号 / 反斜杠 / 换行 / 控制字符 / 中文")
    }

    /** 列表 → 数组 */
    @TestCase
    func testDumpList() {
        println("\n=== json-dump 列表 ===")
        // 空列表
        @Assert(expectStr(dumpJson(LispValue.Nil, false), "empty") == "null")
        // (list 1 2 3)
        let lst = list(ArrayList<LispValue>([LispValue.Int(1), LispValue.Int(2), LispValue.Int(3)]))
        @Assert(expectStr(dumpJson(lst, false), "list") == "[1,2,3]")
        // 嵌套
        let inner = list(ArrayList<LispValue>([LispValue.Str("a")]))
        let nested = list(ArrayList<LispValue>([LispValue.Int(1), inner]))
        @Assert(expectStr(dumpJson(nested, false), "nested") == "[1,[\"a\"]]")
        println("✓ [1,2,3] / 嵌套")
    }

    /** HashMap → 对象；pretty 模式带缩进 */
    @TestCase
    func testDumpObjectAndPretty() {
        println("\n=== json-dump 对象与 pretty ===")
        let m = HashMap<String, LispValue>()
        m["a"] = LispValue.Int(1)
        m["b"] = LispValue.Str("x")
        let obj = LispValue.HashMap(m)
        // 键序不确定，断言片段而不是整串
        let compact = expectStr(dumpJson(obj, false), "obj")
        @Assert(compact.startsWith("{"))
        @Assert(compact.endsWith("}"))
        @Assert(compact.contains("\"a\":1"))
        @Assert(compact.contains("\"b\":\"x\""))

        let pretty = expectStr(dumpJson(obj, true), "pretty")
        @Assert(pretty.contains("\n  \"a\": 1"))
        @Assert(pretty.contains("\n  \"b\": \"x\""))
        println("✓ 紧凑与 pretty 两种形态")
    }

    /** 不可序列化的值必须报错，不能静默丢 */
    @TestCase
    func testDumpRejectsUnserializable() {
        println("\n=== json-dump 拒绝不可序列化的值 ===")
        let cases = [
            LispValue.NativeFunc({ _ => Nil }),
            LispValue.Symbol("sym"),
            LispValue.Error(XispError(ErrorType.RuntimeError, "x"))
        ]
        for (v in cases) {
            let r = dumpJson(v, false)
            match (r) {
                case LispValue.Error(e) =>
                    @Assert(e.kind == ErrorType.TypeError)
                    @Assert(e.message.contains("cannot serialize"))
                case other => @Fail("应报 TypeError，实得 ${other.toString()}")
            }
        }
        println("✓ 过程 / 符号 / 错误值都报 TypeError")
    }

    /**
     * **环检测**：HashMap 可以把自己装进去，没有深度上限就是无限递归
     */
    @TestCase
    func testDumpCircular() {
        println("\n=== json-dump 环检测 ===")
        let m = HashMap<String, LispValue>()
        let self = LispValue.HashMap(m)
        m["self"] = self
        let r = dumpJson(self, false)
        match (r) {
            case LispValue.Error(e) =>
                @Assert(e.kind == ErrorType.RuntimeError)
                @Assert(e.message.contains("circular"))
            case other => @Fail("环应被深度上限拦住，实得 ${other.toString().size} 字符")
        }
        println("✓ 自引用 HashMap 被深度上限拦住（不是栈溢出）")
    }
}
