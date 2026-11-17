package ystyle::xisp_json

import ystyle::xisp.types.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import std.collection.HashMap

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
}
