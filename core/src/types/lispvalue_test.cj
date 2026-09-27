package ystyle::xisp.types

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList

/**
 * LispValue 类型测试
 */
@Test
class LispValueTest {
    @TestCase
    func testNil() {
        let nilVal = Nil
        @Assert(nilVal.isNil())
        @Assert(!nilVal.isTruthy())
    }

    @TestCase
    func testBool() {
        let trueValue = LispValue.Boolean(true)
        let falseValue = LispValue.Boolean(false)

        @Assert(trueValue.isTruthy())
        @Assert(!falseValue.isTruthy())
        @Assert(trueValue.toString() == "true")
        @Assert(falseValue.toString() == "false")

        // 测试克隆
        @Assert(trueValue.cloneValue().isTruthy())
        @Assert(!falseValue.cloneValue().isTruthy())
    }

    /**
     * 测试 Number 类型
     */
    @TestCase
    func testNumber() {
        let intNum = LispValue.Int(42)
        let floatNum = LispValue.Float(3.5)

        @Assert(intNum.isNumber())
        @Assert(floatNum.isNumber())
        @Assert(intNum.isTruthy())
        @Assert(floatNum.isTruthy())

        // 测试克隆
        @Assert(intNum.cloneValue().isNumber())
    }

    /**
     * 测试 Str 类型
     */
    @TestCase
    func testStr() {
        let str = Str("hello world")

        @Assert(str.isString())
        @Assert(!str.isNumber())
        @Assert(str.isTruthy())
        @Assert(str.toString() == "\"hello world\"")

        // 测试克隆
        @Assert(str.cloneValue().isString())
    }

    /**
     * 测试 Symbol 类型
     */
    @TestCase
    func testSymbol() {
        let sym = Symbol("foo")

        @Assert(sym.isSymbol())
        @Assert(!sym.isString())
        @Assert(sym.isTruthy())
        @Assert(sym.toString() == "foo")

        // 测试克隆
        @Assert(sym.cloneValue().isSymbol())
    }
}

/**
 * ConsCell 和列表操作测试
 */
@Test
class ConsCellTest {
    /**
     * 测试空列表
     */
    @TestCase
    func testNilList() {
        let cell = ConsCell(Nil, Nil)

        @Assert(cell.isNil())
        @Assert(cell.toString() == "()")
        @Assert(cell.length() == 0)
    }

    /**
     * 测试简单列表
     */
    @TestCase
    func testSimpleList() {
        // 创建 (a b c)
        let list = ConsCell(Symbol("a"), consValue(Symbol("b"), consValue(Symbol("c"), Nil)))

        @Assert(!list.isNil())
        @Assert(list.toString() == "(a b c)")
        @Assert(list.length() == 3)
        @Assert(list.car.isSymbol())
        @Assert(list.car.toString() == "a")
    }

    /**
     * 测试不规则列表（点对）
     */
    @TestCase
    func testImproperList() {
        let list = ConsCell(Symbol("a"), Symbol("b"))

        @Assert(!list.isNil())
        @Assert(list.toString() == "(a . b)")
    }

    /**
     * 测试列表克隆
     */
    @TestCase
    func testListClone() {
        let original = ConsCell(LispValue.Int(1), consValue(LispValue.Int(2), Nil))
        let cloned = original.clone()

        @Assert(cloned.toString() == original.toString())
        @Assert(cloned.length() == original.length())
    }

    /**
     * 测试嵌套列表
     */
    @TestCase
    func testNestedList() {
        // 创建 (a (b c))
        let inner = consValue(Symbol("b"), consValue(Symbol("c"), Nil))
        let outer = consValue(Symbol("a"), consValue(inner, Nil))

        @Assert(outer.toString() == "(a (b c))")
    }
}

/**
 * 辅助函数测试
 */
@Test
class HelperFunctionsTest {
    /**
     * 测试值构造函数
     */
    @TestCase
    func testValueConstructors() {
        @Assert(nilValue().isNil())
        @Assert(boolValue(true).isTruthy())
        @Assert(floatValue(3.5).isNumber())
        @Assert(stringValue("hello").isString())
        @Assert(symbolValue("foo").isSymbol())
    }

    /**
     * 测试类型判断函数
     */
    @TestCase
    func testTypeCheckers() {
        let num = LispValue.Int(42)
        let str = Str("hello")
        let sym = Symbol("foo")
        let list = consValue(sym, Nil)

        @Assert(isNumber(num))
        @Assert(isString(str))
        @Assert(isSymbol(sym))
        @Assert(isList(list))
    }

    /**
     * 测试 cons 辅助函数
     */
    @TestCase
    func testConsHelper() {
        let list = consValue(Symbol("a"), consValue(Symbol("b"), Nil))

        @Assert(list.isList())
        @Assert(list.toString() == "(a b)")
    }

    /**
     * 测试 list 辅助函数
     */
    @TestCase
    func testListHelper() {
        let elements = ArrayList<LispValue>()
        elements.add(Symbol("a"))
        elements.add(Symbol("b"))
        elements.add(Symbol("c"))

        let list = list(elements)
        @Assert(list.toString() == "(a b c)")
    }

    /**
     * 测试空列表
     */
    @TestCase
    func testEmptyList() {
        let elements = ArrayList<LispValue>()
        let list = list(elements)

        @Assert(list.isNil())
        @Assert(list.toString() == "nil")
    }

    /**
     * 测试 makeList 函数
     */
    @TestCase
    func testMakeList() {
        let arr = ArrayList<LispValue>()
        arr.add(Symbol("a"))
        arr.add(Symbol("b"))
        arr.add(Symbol("c"))

        let list = list(arr)
        @Assert(list.toString() == "(a b c)")
    }
}

/**
 * 边界情况测试
 */
@Test
class EdgeCasesTest {
    /**
     * 测试零值
     */
    @TestCase
    func testZeroValues() {
        // 0 是假值（用作 false）
        @Assert(!LispValue.Int(0).isTruthy())
        // LispValue.Boolean(false) 是假值
        @Assert(!LispValue.Boolean(false).isTruthy())
        // 其他非零数字是真值
        @Assert(LispValue.Int(1).isTruthy())
        @Assert(LispValue.Int(-1).isTruthy())
    }

    /**
     * 测试空字符串
     */
    @TestCase
    func testEmptyString() {
        let empty = Str("")
        @Assert(empty.isString())
        @Assert(empty.toString() == "\"\"")
    }

    /**
     * 测试深层嵌套列表
     */
    @TestCase
    func testDeeplyNestedList() {
        // 创建 (((a)))- 使用 Symbol 避免浮点数精度问题
        let list = consValue(consValue(consValue(Symbol("a"), Nil), Nil), Nil)

        match (list) {
            case Cons(cell) => @Assert(cell.toString() == "(((a)))")
            case _ => @Fail("Expected Cons cell")
        }
    }

    /**
     * 测试长列表
     */
    @TestCase
    func testLongList() {
        let elements = ArrayList<LispValue>()
        for (i in 0..100) {
            elements.add(LispValue.Int(i))
        }

        let list = list(elements)
        @Assert(list.isList())
        // 验证长度
        match (list) {
            case Cons(cell) => @Assert(cell.length() == 100)
            case _ => @Fail("Expected Cons cell")
        }
    }
}
