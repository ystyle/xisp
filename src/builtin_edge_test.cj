package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*

/**
 * 边缘内置函数单元测试
 * 测试不常用但存在的内置函数
 */
@Test
class BuiltinEdgeTest {
    @TestCase
    func testDisplayFunction() {
        println("\n=== 测试 display 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(display \"Hello\")")
        let result = evaluator.eval(expr)
        @Assert(result.isNil())  // display 返回 nil
    }

    @TestCase
    func testPrincFunction() {
        println("\n=== 测试 princ 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(princ \"No newline\")")
        let result = evaluator.eval(expr)
        @Assert(result.isNil())  // princ 返回 nil
    }

    @TestCase
    func testNewlineFunction() {
        println("\n=== 测试 newline 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(newline)")
        let result = evaluator.eval(expr)
        @Assert(result.isNil())  // newline 返回 nil
    }

    @TestCase
    func testSecondFunction() {
        println("\n=== 测试 second 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(second (quote (1 2 3)))")
        let result = evaluator.eval(expr)
        match (result) {
            case LispValue.Int(i) => @Assert(i == 2)
            case LispValue.Float(f) => @Assert(f == 2.0)
            case _ => @Fail("second should return second element")
        }
    }

    @TestCase
    func testThirdFunction() {
        println("\n=== 测试 third 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(third (quote (1 2 3 4)))")
        let result = evaluator.eval(expr)
        match (result) {
            case LispValue.Int(i) => @Assert(i == 3)
            case LispValue.Float(f) => @Assert(f == 3.0)
            case _ => @Fail("third should return third element")
        }
    }

    @TestCase
    func testPrependFunction() {
        println("\n=== 测试 prepend 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(prepend 1 (quote (2 3)))")
        let result = evaluator.eval(expr)
        println("prepend result: ${result.toString()}")
        match (result) {
            case Cons(cell) =>
                println("prepend cell.car: ${cell.car.toString()}")
                @Assert(cell.car.toString() == "1")
            case _ => @Fail("prepend should work like cons")
        }
    }

    @TestCase
    func testMoreFunction() {
        println("\n=== 测试 more 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(more (quote (1 2 3)))")
        let result = evaluator.eval(expr)
        println("more result: ${result.toString()}")
        match (result) {
            case Cons(cell) =>
                println("more cell.car: ${cell.car.toString()}")
                @Assert(cell.car.toString() == "2")
            case _ => @Fail("more should work like cdr")
        }
    }

    @TestCase
    func testProductFunction() {
        println("\n=== 测试 product 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(product (quote (2 3 4)))")
        let result = evaluator.eval(expr)
        match (result) {
            case LispValue.Int(i) => @Assert(i == 24)  // 2 * 3 * 4
            case LispValue.Float(f) => @Assert(f == 24.0)
            case _ => @Fail("product should multiply all elements")
        }

        // 空列表
        let expr2 = Parser.parseSingleFromString("(product (quote ()))")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Int(i) => @Assert(i == 1)  // 空列表返回1
            case LispValue.Float(f) => @Assert(f == 1.0)
            case _ => @Fail("product of empty list should be 1")
        }
    }

    @TestCase
    func testReverseFunction() {
        println("\n=== 测试 reverse 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(reverse (quote (1 2 3)))")
        let result = evaluator.eval(expr)
        println("reverse result: ${result.toString()}")

        // 应该是 (3 2 1)
        match (result) {
            case Cons(cell) =>
                if (let Cons(secondCell) <- cell.cdr) {
                    if (let Cons(thirdCell) <- secondCell.cdr) {
                        println("thirdCell.car: ${thirdCell.car.toString()}")
                        @Assert(thirdCell.car.toString() == "1")
                    }
                }
            case _ => @Fail("reverse should reverse the list")
        }
    }

    @TestCase
    func testRangeFunction() {
        println("\n=== 测试 range 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 基本范围
        let expr1 = Parser.parseSingleFromString("(range 0 5)")
        let result1 = evaluator.eval(expr1)
        @Assert(result1.isList())

        // 带步长
        let expr2 = Parser.parseSingleFromString("(range 0 10 2)")
        let result2 = evaluator.eval(expr2)
        @Assert(result2.isList())

        // 验证元素个数
        let expr3 = Parser.parseSingleFromString("(length (range 0 5))")
        let result3 = evaluator.eval(expr3)
        match (result3) {
            case LispValue.Int(i) => @Assert(i == 5)
            case LispValue.Float(f) => @Assert(f == 5.0)
            case _ => @Fail("range 0 5 should have 5 elements")
        }

        // 验证步长
        let expr4 = Parser.parseSingleFromString("(length (range 0 10 2))")
        let result4 = evaluator.eval(expr4)
        match (result4) {
            case LispValue.Int(i) => @Assert(i == 5)  // 0, 2, 4, 6, 8
            case LispValue.Float(f) => @Assert(f == 5.0)
            case _ => @Fail("range 0 10 2 should have 5 elements")
        }
    }

    @TestCase
    func testSumFunction() {
        println("\n=== 测试 sum 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(sum (quote (1 2 3 4 5)))")
        let result = evaluator.eval(expr)
        match (result) {
            case LispValue.Int(i) => @Assert(i == 15)
            case LispValue.Float(f) => @Assert(f == 15.0)
            case _ => @Fail("sum should add all elements")
        }

        // 空列表
        let expr2 = Parser.parseSingleFromString("(sum (quote ()))")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Int(i) => @Assert(i == 0)
            case LispValue.Float(f) => @Assert(f == 0.0)
            case _ => @Fail("sum of empty list should be 0")
        }
    }

    @TestCase
    func testMaxFunction() {
        println("\n=== 测试 max 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(max (quote (1 5 3 9 2)))")
        let result = evaluator.eval(expr)
        match (result) {
            case LispValue.Int(i) => @Assert(i == 9)
            case LispValue.Float(f) => @Assert(f == 9.0)
            case _ => @Fail("max should return maximum value")
        }

        // 单元素
        let expr2 = Parser.parseSingleFromString("(max (quote (7)))")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Int(i) => @Assert(i == 7)
            case LispValue.Float(f) => @Assert(f == 7.0)
            case _ => @Fail("max of single element should return that element")
        }
    }

    @TestCase
    func testMinFunction() {
        println("\n=== 测试 min 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        let expr = Parser.parseSingleFromString("(min (quote (1 5 3 9 2)))")
        let result = evaluator.eval(expr)
        match (result) {
            case LispValue.Int(i) => @Assert(i == 1)
            case LispValue.Float(f) => @Assert(f == 1.0)
            case _ => @Fail("min should return minimum value")
        }

        // 单元素
        let expr2 = Parser.parseSingleFromString("(min (quote (7)))")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Int(i) => @Assert(i == 7)
            case LispValue.Float(f) => @Assert(f == 7.0)
            case _ => @Fail("min of single element should return that element")
        }
    }

    @TestCase
    func testStringFunction() {
        println("\n=== 测试 str 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 基本拼接
        let expr1 = Parser.parseSingleFromString("(str \"Hello\" \" \" \"World\")")
        let result1 = evaluator.eval(expr1)
        match (result1) {
            case Str(s) => @Assert(s == "Hello World")
            case _ => @Fail("str should concatenate strings")
        }

        // 数字自动转字符串
        let expr2 = Parser.parseSingleFromString("(str \"Value: \" 42)")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case Str(s) => @Assert(s.contains("42"))
            case _ => @Fail("str should convert number to string")
        }

        // nil 转字符串
        let expr3 = Parser.parseSingleFromString("(str \"nil: \" nil)")
        let result3 = evaluator.eval(expr3)
        match (result3) {
            case Str(s) => @Assert(s.contains("nil"))
            case _ => @Fail("str should convert nil to string")
        }
    }

    @TestCase
    func testPrintVsPrincVsDisplay() {
        println("\n=== 测试 print/princ/display 区别 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 这些函数都返回 nil，主要副作用是输出
        let expr1 = Parser.parseSingleFromString("(print \"A\")")
        let result1 = evaluator.eval(expr1)
        @Assert(result1.isNil())

        let expr2 = Parser.parseSingleFromString("(princ \"B\")")
        let result2 = evaluator.eval(expr2)
        @Assert(result2.isNil())

        let expr3 = Parser.parseSingleFromString("(display \"C\")")
        let result3 = evaluator.eval(expr3)
        @Assert(result3.isNil())
    }

    @TestCase
    func testModFunction() {
        println("\n=== 测试 mod 函数 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 正数取模
        let expr1 = Parser.parseSingleFromString("(mod 10 3)")
        let result1 = evaluator.eval(expr1)
        println("mod 10 3 result: ${result1.toString()}")
        match (result1) {
            case LispValue.Int(i) => @Assert(i == 1)
            case LispValue.Float(f) => @Assert(f == 1.0)
            case _ => @Fail("mod 10 3 should be 1")
        }

        // 负数取模 (C 风格：向零方向取整)
        let expr2 = Parser.parseSingleFromString("(mod -10 3)")
        let result2 = evaluator.eval(expr2)
        println("mod -10 3 result: ${result2.toString()}")
        match (result2) {
            case LispValue.Int(i) => @Assert(i == -1)  // fmod 风格：-10 = 3*(-3) - 1
            case LispValue.Float(f) => @Assert(f == -1.0)
            case _ => @Fail("mod -10 3 should be -1")
        }

        // 除零
        let expr3 = Parser.parseSingleFromString("(mod 5 0)")
        let result3 = evaluator.eval(expr3)
        @Assert(result3.isNil())  // 除零返回 nil
    }

    @TestCase
    func testZeroPredicate() {
        println("\n=== 测试 zero? 谓词 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 整数测试
        let expr1 = Parser.parseSingleFromString("(zero? 0)")
        let result1 = evaluator.eval(expr1)
        match (result1) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("zero? 0 should return true")
        }

        let expr2 = Parser.parseSingleFromString("(zero? 1)")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("zero? 1 should return false")
        }

        // 浮点数测试
        let expr3 = Parser.parseSingleFromString("(zero? 0.0)")
        let result3 = evaluator.eval(expr3)
        match (result3) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("zero? 0.0 should return true")
        }

        let expr4 = Parser.parseSingleFromString("(zero? 0.1)")
        let result4 = evaluator.eval(expr4)
        match (result4) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("zero? 0.1 should return false")
        }
    }

    @TestCase
    func testPositivePredicate() {
        println("\n=== 测试 positive? 谓词 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 正数测试
        let expr1 = Parser.parseSingleFromString("(positive? 5)")
        let result1 = evaluator.eval(expr1)
        match (result1) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("positive? 5 should return true")
        }

        let expr2 = Parser.parseSingleFromString("(positive? -5)")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("positive? -5 should return false")
        }

        let expr3 = Parser.parseSingleFromString("(positive? 0)")
        let result3 = evaluator.eval(expr3)
        match (result3) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("positive? 0 should return false")
        }

        // 浮点数测试
        let expr4 = Parser.parseSingleFromString("(positive? 3.14)")
        let result4 = evaluator.eval(expr4)
        match (result4) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("positive? 3.14 should return true")
        }
    }

    @TestCase
    func testNegativePredicate() {
        println("\n=== 测试 negative? 谓词 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 负数测试
        let expr1 = Parser.parseSingleFromString("(negative? -5)")
        let result1 = evaluator.eval(expr1)
        match (result1) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("negative? -5 should return true")
        }

        let expr2 = Parser.parseSingleFromString("(negative? 5)")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("negative? 5 should return false")
        }

        let expr3 = Parser.parseSingleFromString("(negative? 0)")
        let result3 = evaluator.eval(expr3)
        match (result3) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("negative? 0 should return false")
        }

        // 浮点数测试
        let expr4 = Parser.parseSingleFromString("(negative? -3.14)")
        let result4 = evaluator.eval(expr4)
        match (result4) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("negative? -3.14 should return true")
        }
    }

    @TestCase
    func testEvenPredicate() {
        println("\n=== 测试 even? 谓词 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 偶数测试
        let expr1 = Parser.parseSingleFromString("(even? 4)")
        let result1 = evaluator.eval(expr1)
        match (result1) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("even? 4 should return true")
        }

        let expr2 = Parser.parseSingleFromString("(even? 3)")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("even? 3 should return false")
        }

        let expr3 = Parser.parseSingleFromString("(even? 0)")
        let result3 = evaluator.eval(expr3)
        match (result3) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("even? 0 should return true")
        }

        // 负数测试
        let expr4 = Parser.parseSingleFromString("(even? -2)")
        let result4 = evaluator.eval(expr4)
        match (result4) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("even? -2 should return true")
        }

        // 非整数类型应该返回 false
        let expr5 = Parser.parseSingleFromString("(even? 3.14)")
        let result5 = evaluator.eval(expr5)
        match (result5) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("even? 3.14 should return false")
        }
    }

    @TestCase
    func testOddPredicate() {
        println("\n=== 测试 odd? 谓词 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 奇数测试
        let expr1 = Parser.parseSingleFromString("(odd? 3)")
        let result1 = evaluator.eval(expr1)
        match (result1) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("odd? 3 should return true")
        }

        let expr2 = Parser.parseSingleFromString("(odd? 4)")
        let result2 = evaluator.eval(expr2)
        match (result2) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("odd? 4 should return false")
        }

        let expr3 = Parser.parseSingleFromString("(odd? 1)")
        let result3 = evaluator.eval(expr3)
        match (result3) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("odd? 1 should return true")
        }

        // 负数测试
        let expr4 = Parser.parseSingleFromString("(odd? -3)")
        let result4 = evaluator.eval(expr4)
        match (result4) {
            case LispValue.Boolean(b) => @Assert(b)
            case _ => @Fail("odd? -3 should return true")
        }

        // 非整数类型应该返回 false
        let expr5 = Parser.parseSingleFromString("(odd? 3.14)")
        let result5 = evaluator.eval(expr5)
        match (result5) {
            case LispValue.Boolean(b) => @Assert(!b)
            case _ => @Fail("odd? 3.14 should return false")
        }
    }

    @TestCase
    func testPrognAlias() {
        println("\n=== 测试 progn 别名 ===")
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // progn 应该和 begin 行为相同
        let expr1 = Parser.parseSingleFromString("(progn (println \"a\") (println \"b\"))")
        let result1 = evaluator.eval(expr1)
        @Assert(result1.isNil())  // progn 返回 nil

        let expr2 = Parser.parseSingleFromString("(begin (println \"c\") (println \"d\"))")
        let result2 = evaluator.eval(expr2)
        @Assert(result2.isNil())  // begin 返回 nil
    }
}
