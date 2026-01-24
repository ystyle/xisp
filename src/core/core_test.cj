package ystyle::xisp.core

import ystyle::xisp.types.*
import std.unittest.*
import std.collection.ArrayList

/**
 * LispValue 类型测试
 */
@Test
class LispValueTest {
    /**
     * 测试 Nil 类型
     */
    @TestCase
    func testNil() {
        let nilValue = Nil
        @Assert(nilValue.isNil())
        @Assert(!nilValue.isTruthy())
        @Assert(!nilValue.isNumber())
        @Assert(!nilValue.isString())
        @Assert(!nilValue.isSymbol())
        @Assert(nilValue.isList())
        @Assert(!nilValue.isProcedure())
        @Assert(nilValue.toString(), "nil")
    }

    /**
     * 测试 Bool 类型
     */
    @TestCase
    func testBool() {
        let trueValue = Boolean(true)
        let falseValue = Boolean(false)

        @Assert(trueValue.isTruthy())
        @Assert(!falseValue.isTruthy())
        @Assert(trueValue.toString(), "true")
        @Assert(falseValue.toString(), "false")

        // 测试克隆
        @Assert(trueValue.cloneValue().isTruthy())
        @Assert(!falseValue.cloneValue().isTruthy())
    }

    /**
     * 测试 Number 类型
     */
    @TestCase
    func testNumber() {
        let intNum = Number(42.0)
        let floatNum = Number(3.5)

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
        @Assert(str.toString(), "\"hello world\"")

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
        @Assert(sym.toString(), "foo")

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
        @Assert(cell.toString(), "()")
        @Assert(cell.length(), 0)
    }

    /**
     * 测试简单列表
     */
    @TestCase
    func testSimpleList() {
        // 创建 (a b c)
        let list = ConsCell(Symbol("a"), consValue(Symbol("b"), consValue(Symbol("c"), Nil)))

        @Assert(!list.isNil())
        @Assert(list.toString(), "(a b c)")
        @Assert(list.length(), 3)
        @Assert(list.car.isSymbol())
        @Assert(list.car.toString(), "a")
    }

    /**
     * 测试不规则列表（点对）
     */
    @TestCase
    func testImproperList() {
        let list = ConsCell(Symbol("a"), Symbol("b"))

        @Assert(!list.isNil())
        @Assert(list.toString(), "(a . b)")
    }

    /**
     * 测试列表克隆
     */
    @TestCase
    func testListClone() {
        let original = ConsCell(Number(1.0), consValue(Number(2.0), Nil))
        let cloned = original.clone()

        @Assert(cloned.toString(), original.toString())
        @Assert(cloned.length(), original.length())
    }

    /**
     * 测试嵌套列表
     */
    @TestCase
    func testNestedList() {
        // 创建 (a (b c))
        let inner = consValue(Symbol("b"), consValue(Symbol("c"), Nil))
        let outer = consValue(Symbol("a"), consValue(inner, Nil))

        @Assert(outer.toString(), "(a (b c))")
    }
}

/**
 * Environment 测试
 */
@Test
class EnvironmentTest {
    /**
     * 测试基本变量定义和查找
     */
    @TestCase
    func testBasicDefineAndLookup() {
        let env = Environment()

        env.define("x", Number(42.5))  // 使用能精确表示的值
        let result = env.lookup("x")

        @Assert(result.isNumber())
        match (result) {
            case Number(n) => @Assert(n > 40.0)  // 检查数值范围
            case _ => @Fail("Expected Number")
        }
    }

    /**
     * 测试变量修改
     */
    @TestCase
    func testSet() {
        let env = Environment()

        env.define("x", Number(1.5))
        match (env.lookup("x")) {
            case Number(n) => @Assert(n > 1.0)
            case _ => @Fail("Expected Number")
        }

        let success = env.set("x", Number(2.5))
        @Assert(success)
        match (env.lookup("x")) {
            case Number(n) => @Assert(n > 2.0)
            case _ => @Fail("Expected Number")
        }
    }

    /**
     * 测试父环境查找
     */
    @TestCase
    func testParentLookup() {
        let parent = Environment()
        let child = parent.createChild()

        parent.define("x", Number(10.5))
        match (child.lookup("x")) {
            case Number(n) => @Assert(n > 10.0)
            case _ => @Fail("Expected Number")
        }
    }

    /**
     * 测试变量遮蔽
     */
    @TestCase
    func testVariableShadowing() {
        let parent = Environment()
        let child = parent.createChild()

        parent.define("x", Number(10.5))
        child.define("x", Number(20.5))

        match (child.lookup("x")) {
            case Number(n) => @Assert(n > 20.0)
            case _ => @Fail("Expected Number")
        }
        match (parent.lookup("x")) {
            case Number(n) => @Assert(n > 10.0 && n < 15.0)
            case _ => @Fail("Expected Number")
        }
    }

    /**
     * 测试未定义变量
     */
    @TestCase
    func testUndefinedVariable() {
        let env = Environment()

        let result = env.lookup("undefined")
        @Assert(result.isNil())
    }

    /**
     * 测试 contains 方法
     */
    @TestCase
    func testContains() {
        let parent = Environment()
        let child = parent.createChild()

        parent.define("x", Number(1.0))
        child.define("y", Number(2.0))

        @Assert(child.contains("x"))
        @Assert(child.contains("y"))
        @Assert(!child.contains("z"))
    }

    /**
     * 测试环境克隆
     */
    @TestCase
    func testClone() {
        let env = Environment()
        env.define("x", Number(42.5))

        let cloned = env.clone()
        match (cloned.lookup("x")) {
            case Number(n) => @Assert(n > 40.0)
            case _ => @Fail("Expected Number")
        }

        // 修改克隆不应影响原环境
        cloned.set("x", Number(100.5))
        match (env.lookup("x")) {
            case Number(n) => @Assert(n > 40.0 && n < 50.0)
            case _ => @Fail("Expected Number")
        }
        match (cloned.lookup("x")) {
            case Number(n) => @Assert(n > 100.0)
            case _ => @Fail("Expected Number")
        }
    }

    /**
     * 测试 getKeys 方法
     */
    @TestCase
    func testGetKeys() {
        let env = Environment()
        env.define("x", Number(1.0))
        env.define("y", Number(2.0))
        env.define("z", Number(3.0))

        let keys = env.getKeys()
        @Assert(keys.size, 3)
    }
}

/**
 * Procedure 类型测试
 */
@Test
class ProcedureTest {
    /**
     * 测试过程构造
     */
    @TestCase
    func testProcedureCreation() {
        let env = Environment()
        let params = ArrayList<String>()
        params.add("x")
        params.add("y")
        let body = Symbol("x")

        let proc = Procedure(params, body, env)
        match (proc) {
            case Procedure(p, b, e) => @Assert(p.size, 2)
            case _ => @Fail("Expected Procedure")
        }
    }

    /**
     * 测试过程在 LispValue 中
     */
    @TestCase
    func testProcedureInLispValue() {
        let env = Environment()
        let params = ArrayList<String>()
        params.add("x")
        let body = Symbol("x")

        let procValue = Procedure(params, body, env)
        @Assert(procValue.isProcedure())
        @Assert(procValue.toString(), "#<procedure>")
    }

    /**
     * 测试过程克隆
     */
    @TestCase
    func testProcedureClone() {
        let env = Environment()
        env.define("x", Number(42.0))

        let params = ArrayList<String>()
        params.add("x")
        let body = Symbol("x")

        let procValue = Procedure(params, body, env)
        let cloned = procValue.cloneValue()

        @Assert(cloned.isProcedure())
    }

    /**
     * 测试简单函数调用
     */
    @TestCase
    func testSimpleFunctionCall() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // 定义一个函数：(lambda (x) (+ x 1))
        let params = ArrayList<String>()
        params.add("x")

        // 函数体：(+ x 1)，body 应该是包含表达式的列表
        let add1Cell = consValue(Symbol("x"), consValue(Number(1.0), Nil))
        let expr = consValue(Symbol("+"), add1Cell)
        let body = consValue(expr, Nil)  // body 是 ((+ x 1))

        let proc = Procedure(params, body, env)
        let args = ArrayList<LispValue>()
        args.add(Number(5.0))

        // 应用函数
        let eval = Evaluator(env)
        let result = eval.applyProcedure(proc, args)

        match (result) {
            case Number(n) =>
                println("testSimpleFunctionCall: result = ${n}")
                @Assert(n >= 5.9 && n <= 6.1)
            case _ =>
                println("testSimpleFunctionCall: result = ${result}, type = ${result.toString()}")
                @Fail("Expected Number result")
        }
    }

    /**
     * 测试多参数函数调用
     */
    @TestCase
    func testMultiArgumentFunctionCall() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)

        // 定义一个函数：(lambda (x y) (+ x y))
        let params = ArrayList<String>()
        params.add("x")
        params.add("y")

        // 函数体：(+ x y)，body 应该是包含表达式的列表
        let argsList = consValue(Symbol("y"), Nil)
        let expr = consValue(Symbol("+"), consValue(Symbol("x"), argsList))
        let body = consValue(expr, Nil)  // body 是 ((+ x y))

        let proc = Procedure(params, body, env)
        let args = ArrayList<LispValue>()
        args.add(Number(3.0))
        args.add(Number(4.0))

        let eval = Evaluator(env)
        let result = eval.applyProcedure(proc, args)

        match (result) {
            case Number(n) =>
                println("testMultiArgumentFunctionCall: result = ${n}")
                @Assert(n >= 6.9 && n <= 7.1)
            case _ =>
                println("testMultiArgumentFunctionCall: result = ${result}, type = ${result.toString()}")
                @Fail("Expected Number result")
        }
    }

    /**
     * 测试闭包（词法作用域）
     */
    @TestCase
    func testClosure() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        env.define("multiplier", Number(2.0))

        // 定义一个函数：(lambda (x) (* x multiplier))
        let params = ArrayList<String>()
        params.add("x")

        // 函数体：(* x multiplier)，body 应该是包含表达式的列表
        let multList = consValue(Symbol("multiplier"), Nil)
        let expr = consValue(Symbol("*"), consValue(Symbol("x"), multList))
        let body = consValue(expr, Nil)  // body 是 ((* x multiplier))

        let proc = Procedure(params, body, env)
        let args = ArrayList<LispValue>()
        args.add(Number(5.0))

        let eval = Evaluator(env)
        let result = eval.applyProcedure(proc, args)

        match (result) {
            case Number(n) =>
                println("testClosure: result = ${n}")
                @Assert(n >= 9.9 && n <= 10.1)
            case _ =>
                println("testClosure: result = ${result}, type = ${result.toString()}")
                @Fail("Expected Number result, closure failed")
        }
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
        @Assert(numberValue(3.5).isNumber())
        @Assert(stringValue("hello").isString())
        @Assert(symbolValue("foo").isSymbol())
    }

    /**
     * 测试类型判断函数
     */
    @TestCase
    func testTypeCheckers() {
        let num = Number(42.0)
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
        @Assert(list.toString(), "(a b)")
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
        @Assert(list.toString(), "(a b c)")
    }

    /**
     * 测试空列表
     */
    @TestCase
    func testEmptyList() {
        let elements = ArrayList<LispValue>()
        let list = list(elements)

        @Assert(list.isNil())
        @Assert(list.toString(), "nil")
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
        @Assert(list.toString(), "(a b c)")
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
        @Assert(!Number(0.0).isTruthy())
        // Boolean(false) 是假值
        @Assert(!Boolean(false).isTruthy())
        // 其他非零数字是真值
        @Assert(Number(1.0).isTruthy())
        @Assert(Number(-1.0).isTruthy())
    }

    /**
     * 测试空字符串
     */
    @TestCase
    func testEmptyString() {
        let empty = Str("")
        @Assert(empty.isString())
        @Assert(empty.toString(), "\"\"")
    }

    /**
     * 测试深层嵌套列表
     */
    @TestCase
    func testDeeplyNestedList() {
        // 创建 (((a))) - 使用 Symbol 避免浮点数精度问题
        let list = consValue(consValue(consValue(Symbol("a"), Nil), Nil), Nil)

        match (list) {
            case Cons(cell) => @Assert(cell.toString(), "(((a)))")
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
            elements.add(Number(Float64(i)))
        }

        let list = list(elements)
        @Assert(list.isList())
        // 验证长度
        match (list) {
            case Cons(cell) => @Assert(cell.length(), 100)
            case _ => @Fail("Expected Cons cell")
        }
    }
}
