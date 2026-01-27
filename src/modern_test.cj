package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*

/**
 * 现代化函数测试项
 */
class ModernTestItem {
    ModernTestItem(let input: String, let expectedStr: String) {}
}

var modernTests = ArrayList<ModernTestItem>([
    // 现代别名测试
    ModernTestItem("(first '(1 2 3))", "1"),
    ModernTestItem("(rest '(1 2 3))", "(2"),  // 只检查开头
    ModernTestItem("(second '(a b c))", "b"),
    ModernTestItem("(third '(a b c))", "c"),
    ModernTestItem("(prepend 1 '(2 3))", "1"),  // 检查包含 1

    // 传统组合函数测试
    ModernTestItem("(cadr '(1 2 3))", "2"),     // cadr = second
    ModernTestItem("(caddr '(1 2 3))", "3"),    // caddr = third
    ModernTestItem("(cddr '(1 2 3 4 5))", "(3"), // cddr = (rest (rest lst))
    ModernTestItem("(caar '((a b) (c d)))", "a"), // caar = (car (car lst))
    ModernTestItem("(cdar '((a b) (c d)))", "(b"), // cdar = (cdr (car lst))

    // 高阶函数测试（使用原生函数）
    ModernTestItem("(sum '(1 2 3 4 5))", "15"),
    ModernTestItem("(product '(2 3 4))", "24"),
    ModernTestItem("(reverse '(1 2 3))", "3"),  // 检查包含 3
    ModernTestItem("(range 0 5)", "0"),  // 检查包含 0

    // reduce 测试（使用原生函数）
    ModernTestItem("(reduce + 0 '(1 2 3))", "6"),
])

/**
 * 现代化函数测试
 */
@Test
class ModernTest {
    @TestCase
    func testModernFunctions() {
        for (i in 0..modernTests.size) {
            let test = modernTests[i]
            println("[${i}] Testing: ${test.input}")

            let env = Environment()
            BuiltinFunctions.registerAll(env)
            let evaluator = Evaluator(env)

            let exprs = Parser.parseFromString(test.input)
            var result: LispValue = Nil

            for (j in 0..exprs.size) {
                result = evaluator.eval(exprs[j])
            }

            let resultStr = result.toString()
            println("[${i}] result: ${resultStr}, expected contains: ${test.expectedStr}")

            if (!resultStr.contains(test.expectedStr)) {
                @Fail("[${i}] Expected '${resultStr}' to contain '${test.expectedStr}'")
            }
        }
    }

    @TestCase
    func testMapWithLambda() {
        // 测试 map 与 lambda 结合
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 定义一个平方函数
        let def = Parser.parseSingleFromString("(define square (lambda (x) (* x x)))")
        evaluator.eval(def)

        // 使用 map
        let mapExpr = Parser.parseSingleFromString("(map square '(1 2 3 4 5))")
        let result = evaluator.eval(mapExpr)

        println("Map result: ${result.toString()}")
        @Assert(result.isList())
    }

    @TestCase
    func testChainedOperations() {
        // 测试链式操作（使用原生函数）
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // (reverse (range 1 6)) => (5 4 3 2 1)
        let expr = Parser.parseSingleFromString("(reverse (range 1 6))")
        let result = evaluator.eval(expr)

        println("Chained result: ${result.toString()}")
        @Assert(result.isList())

        // 验证结果
        let resultStr = result.toString()
        @Assert(resultStr == "(5 4 3 2 1)")
    }

    @TestCase
    func testTypeOf() {
        // 测试 type-of 函数
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 测试整数
        let intExpr = Parser.parseSingleFromString("(type-of 42)")
        let intResult = evaluator.eval(intExpr)
        println("type-of 42: ${intResult.toString()}")
        @Assert(intResult.toString() == "\"integer\"")

        // 测试浮点数
        let floatExpr = Parser.parseSingleFromString("(type-of 3.14)")
        let floatResult = evaluator.eval(floatExpr)
        println("type-of 3.14: ${floatResult.toString()}")
        @Assert(floatResult.toString() == "\"float\"")

        // 测试字符串
        let strExpr = Parser.parseSingleFromString("(type-of \"hello\")")
        let strResult = evaluator.eval(strExpr)
        println("type-of \"hello\": ${strResult.toString()}")
        @Assert(strResult.toString() == "\"string\"")

        // 测试符号
        let symExpr = Parser.parseSingleFromString("(type-of 'foo)")
        let symResult = evaluator.eval(symExpr)
        println("type-of 'foo: ${symResult.toString()}")
        @Assert(symResult.toString() == "\"symbol\"")

        // 测试列表
        let listExpr = Parser.parseSingleFromString("(type-of '(1 2 3))")
        let listResult = evaluator.eval(listExpr)
        println("type-of '(1 2 3): ${listResult.toString()}")
        @Assert(listResult.toString() == "\"list\"")

        // 测试空列表（在 Lisp 中，空列表和 nil 是同一个东西）
        let emptyListExpr = Parser.parseSingleFromString("(type-of '())")
        let emptyListResult = evaluator.eval(emptyListExpr)
        println("type-of '(): ${emptyListResult.toString()}")
        @Assert(emptyListResult.toString() == "\"nil\"")

        // 测试 nil
        let nilExpr = Parser.parseSingleFromString("(type-of nil)")
        let nilResult = evaluator.eval(nilExpr)
        println("type-of nil: ${nilResult.toString()}")
        @Assert(nilResult.toString() == "\"nil\"")

        // 测试布尔值
        let boolExpr = Parser.parseSingleFromString("(type-of #t)")
        let boolResult = evaluator.eval(boolExpr)
        println("type-of #t: ${boolResult.toString()}")
        @Assert(boolResult.toString() == "\"boolean\"")

        // 测试过程
        let procExpr = Parser.parseSingleFromString("(type-of +)")
        let procResult = evaluator.eval(procExpr)
        println("type-of +: ${procResult.toString()}")
        @Assert(procResult.toString() == "\"procedure\"")
    }

    @TestCase
    func testEqualityFunctions() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        // 测试 eq? 函数 - 符号比较
        println("\n=== 测试 eq? 符号比较 ===")
        let eqSymbol1 = Parser.parseSingleFromString("(eq? :ok :ok)")
        let eqSymbol1Result = evaluator.eval(eqSymbol1)
        println("(eq? :ok :ok) = ${eqSymbol1Result.toString()}")
        @Assert(eqSymbol1Result.toString() == "true")

        let eqSymbol2 = Parser.parseSingleFromString("(eq? :ok :error)")
        let eqSymbol2Result = evaluator.eval(eqSymbol2)
        println("(eq? :ok :error) = ${eqSymbol2Result.toString()}")
        @Assert(eqSymbol2Result.toString() == "false")

        // 测试 eq? 函数 - 字符串比较
        println("\n=== 测试 eq? 字符串比较 ===")
        let eqStr1 = Parser.parseSingleFromString("(eq? \"a\" \"a\")")
        let eqStr1Result = evaluator.eval(eqStr1)
        println("(eq? \"a\" \"a\") = ${eqStr1Result.toString()}")
        @Assert(eqStr1Result.toString() == "true")

        let eqStr2 = Parser.parseSingleFromString("(eq? \"a\" \"b\")")
        let eqStr2Result = evaluator.eval(eqStr2)
        println("(eq? \"a\" \"b\") = ${eqStr2Result.toString()}")
        @Assert(eqStr2Result.toString() == "false")

        // 测试 eq? 函数 - 整数比较
        println("\n=== 测试 eq? 整数比较 ===")
        let eqInt1 = Parser.parseSingleFromString("(eq? 42 42)")
        let eqInt1Result = evaluator.eval(eqInt1)
        println("(eq? 42 42) = ${eqInt1Result.toString()}")
        @Assert(eqInt1Result.toString() == "true")

        let eqInt2 = Parser.parseSingleFromString("(eq? 42 43)")
        let eqInt2Result = evaluator.eval(eqInt2)
        println("(eq? 42 43) = ${eqInt2Result.toString()}")
        @Assert(eqInt2Result.toString() == "false")

        // 测试 eq? 函数 - nil 比较
        println("\n=== 测试 eq? nil 比较 ===")
        let eqNil = Parser.parseSingleFromString("(eq? nil nil)")
        let eqNilResult = evaluator.eval(eqNil)
        println("(eq? nil nil) = ${eqNilResult.toString()}")
        @Assert(eqNilResult.toString() == "true")

        // 测试 eq? 函数 - 布尔值比较
        println("\n=== 测试 eq? 布尔值比较 ===")
        let eqBool1 = Parser.parseSingleFromString("(eq? #t #t)")
        let eqBool1Result = evaluator.eval(eqBool1)
        println("(eq? #t #t) = ${eqBool1Result.toString()}")
        @Assert(eqBool1Result.toString() == "true")

        let eqBool2 = Parser.parseSingleFromString("(eq? #t #f)")
        let eqBool2Result = evaluator.eval(eqBool2)
        println("(eq? #t #f) = ${eqBool2Result.toString()}")
        @Assert(eqBool2Result.toString() == "false")

        // 测试 eq? 函数 - 不同类型比较
        println("\n=== 测试 eq? 不同类型比较 ===")
        let eqDiff = Parser.parseSingleFromString("(eq? :ok \"ok\")")
        let eqDiffResult = evaluator.eval(eqDiff)
        println("(eq? :ok \"ok\") = ${eqDiffResult.toString()}")
        @Assert(eqDiffResult.toString() == "false")

        // 测试 string< 函数
        println("\n=== 测试 string< ===")
        let strLess1 = Parser.parseSingleFromString("(string< \"a\" \"b\")")
        let strLess1Result = evaluator.eval(strLess1)
        println("(string< \"a\" \"b\") = ${strLess1Result.toString()}")
        @Assert(strLess1Result.toString() == "true")

        let strLess2 = Parser.parseSingleFromString("(string< \"b\" \"a\")")
        let strLess2Result = evaluator.eval(strLess2)
        println("(string< \"b\" \"a\") = ${strLess2Result.toString()}")
        @Assert(strLess2Result.toString() == "false")

        let strLess3 = Parser.parseSingleFromString("(string< \"abc\" \"abc\")")
        let strLess3Result = evaluator.eval(strLess3)
        println("(string< \"abc\" \"abc\") = ${strLess3Result.toString()}")
        @Assert(strLess3Result.toString() == "false")

        // 测试 string> 函数
        println("\n=== 测试 string> ===")
        let strGreater1 = Parser.parseSingleFromString("(string> \"b\" \"a\")")
        let strGreater1Result = evaluator.eval(strGreater1)
        println("(string> \"b\" \"a\") = ${strGreater1Result.toString()}")
        @Assert(strGreater1Result.toString() == "true")

        let strGreater2 = Parser.parseSingleFromString("(string> \"a\" \"b\")")
        let strGreater2Result = evaluator.eval(strGreater2)
        println("(string> \"a\" \"b\") = ${strGreater2Result.toString()}")
        @Assert(strGreater2Result.toString() == "false")

        let strGreater3 = Parser.parseSingleFromString("(string> \"abc\" \"abc\")")
        let strGreater3Result = evaluator.eval(strGreater3)
        println("(string> \"abc\" \"abc\") = ${strGreater3Result.toString()}")
        @Assert(strGreater3Result.toString() == "false")

        // 测试 string=? 函数
        println("\n=== 测试 string=? ===")
        let strEq1 = Parser.parseSingleFromString("(string=? \"abc\" \"abc\")")
        let strEq1Result = evaluator.eval(strEq1)
        println("(string=? \"abc\" \"abc\") = ${strEq1Result.toString()}")
        @Assert(strEq1Result.toString() == "true")

        let strEq2 = Parser.parseSingleFromString("(string=? \"abc\" \"def\")")
        let strEq2Result = evaluator.eval(strEq2)
        println("(string=? \"abc\" \"def\") = ${strEq2Result.toString()}")
        @Assert(strEq2Result.toString() == "false")
    }

    @TestCase
    func testRestParameters() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        println("\n=== 测试可变参数 ===")

        // 测试 lambda 可变参数 (x y . rest)
        let lambdaCode = Parser.parseSingleFromString("(define f (lambda (x y . rest) (list x y rest)))")
        evaluator.eval(lambdaCode)

        let call1 = Parser.parseSingleFromString("(f 1 2 3 4)")
        let result1 = evaluator.eval(call1)
        println("(f 1 2 3 4) = ${result1.toString()}")
        @Assert(result1.toString() == "(1 2 (3 4))")

        let call2 = Parser.parseSingleFromString("(f 1 2)")
        let result2 = evaluator.eval(call2)
        println("(f 1 2) = ${result2.toString()}")
        @Assert(result2.toString() == "(1 2 nil)")
    }

    @TestCase
    func testOnlyRestParameter() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        println("\n=== 测试纯可变参数 ===")

        // 测试纯可变参数 (. all)
        let lambdaCode = Parser.parseSingleFromString("(define g (lambda (. all) all))")
        evaluator.eval(lambdaCode)

        let call1 = Parser.parseSingleFromString("(g 'a 'b 'c)")
        let result1 = evaluator.eval(call1)
        println("(g 'a 'b 'c) = ${result1.toString()}")
        @Assert(result1.toString() == "(a b c)")

        let call2 = Parser.parseSingleFromString("(g)")
        let result2 = evaluator.eval(call2)
        println("(g) = ${result2.toString()}")
        @Assert(result2.toString() == "nil")
    }

    @TestCase
    func testCommaAtSplice() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        println("\n=== 测试 ,@ 拼接 ===")

        // 测试 backquote 中的 ,@
        let defineList = Parser.parseSingleFromString("(define lst '(a b c))")
        evaluator.eval(defineList)

        let spliceCode = Parser.parseSingleFromString("`(x y z ,@lst)")
        let result = evaluator.eval(spliceCode)
        println("`(x y z ,@lst) = ${result.toString()}")
        @Assert(result.toString() == "(x y z a b c)")

        // 测试中间拼接
        let spliceCode2 = Parser.parseSingleFromString("`(1 2 ,@lst 4 5)")
        let result2 = evaluator.eval(spliceCode2)
        println("`(1 2 ,@lst 4 5) = ${result2.toString()}")
        @Assert(result2.toString() == "(1 2 a b c 4 5)")

        // 测试开头拼接
        let spliceCode3 = Parser.parseSingleFromString("`(,@lst)")
        let result3 = evaluator.eval(spliceCode3)
        println("`(,@lst) = ${result3.toString()}")
        @Assert(result3.toString() == "(a b c)")
    }

    @TestCase
    func testMacroExpandMyLet() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        println("\n=== 测试 my-let 宏展开 ===")

        // 定义 my-let 宏
        let defmacro = Parser.parseSingleFromString("(defmacro my-let (bindings . body) `((lambda ,(map car bindings) ,@body) ,@(map cadr bindings)))")
        evaluator.eval(defmacro)

        // 测试 macroexpand
        let expanded = evaluator.eval(Parser.parseSingleFromString("(macroexpand '(my-let ((x 10) (y 20)) (+ x y)))"))
        println("expanded = ${expanded.toString()}")

        // 验证展开后的结果
        @Assert(expanded.toString() == "((lambda (x y) (+ x y)) 10 20)")

        // 测试执行展开后的表达式
        let manualExpanded = Parser.parseSingleFromString("((lambda (x y) (+ x y)) 10 20)")
        let result = evaluator.eval(manualExpanded)
        println("result = ${result.toString()}")
        @Assert(result.toString() == "30")
    }

    @TestCase
    func testDefineRestParameters() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        println("\n=== 测试普通 define 函数的可变参数 ===")

        // 测试 1: 普通 define 函数的可变参数
        let defineFunc1 = Parser.parseSingleFromString("(define (test-func x y . rest) (list \"x=\" x \"y=\" y \"rest=\" rest))")
        evaluator.eval(defineFunc1)

        let call1 = Parser.parseSingleFromString("(test-func 1 2 3 4 5)")
        let result1 = evaluator.eval(call1)
        println("(test-func 1 2 3 4 5) = ${result1.toString()}")
        @Assert(result1.toString() == "(\"x=\" 1 \"y=\" 2 \"rest=\" (3 4 5))")

        let call2 = Parser.parseSingleFromString("(test-func 10 20)")
        let result2 = evaluator.eval(call2)
        println("(test-func 10 20) = ${result2.toString()}")
        @Assert(result2.toString() == "(\"x=\" 10 \"y=\" 20 \"rest=\" nil)")

        // 测试 2: 纯可变参数
        let defineFunc2 = Parser.parseSingleFromString("(define (test-all . args) args)")
        evaluator.eval(defineFunc2)

        let call3 = Parser.parseSingleFromString("(test-all 'a 'b 'c)")
        let result3 = evaluator.eval(call3)
        println("(test-all 'a 'b 'c) = ${result3.toString()}")
        @Assert(result3.toString() == "(a b c)")

        let call4 = Parser.parseSingleFromString("(test-all)")
        let result4 = evaluator.eval(call4)
        println("(test-all) = ${result4.toString()}")
        @Assert(result4.toString() == "nil")

        // 测试 3: 对比 lambda 和 define
        let defineLambda = Parser.parseSingleFromString("(define test-lambda (lambda (x y . rest) (list \"L-x=\" x \"L-y=\" y \"L-rest=\" rest)))")
        evaluator.eval(defineLambda)

        let call5 = Parser.parseSingleFromString("(test-lambda 1 2 3 4)")
        let result5 = evaluator.eval(call5)
        println("(test-lambda 1 2 3 4) = ${result5.toString()}")
        @Assert(result5.toString() == "(\"L-x=\" 1 \"L-y=\" 2 \"L-rest=\" (3 4))")
    }

    @TestCase
    func testEval() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        println("\n=== 测试 eval 函数 ===")

        // 测试 eval 整数
        let evalInt = Parser.parseSingleFromString("(eval 42)")
        let intResult = evaluator.eval(evalInt)
        println("(eval 42) = ${intResult.toString()}")
        @Assert(intResult.toString() == "42")

        // 测试 eval 符号
        evaluator.eval(Parser.parseSingleFromString("(define x 100)"))
        let evalSymbol = Parser.parseSingleFromString("(eval (quote x))")
        let symbolResult = evaluator.eval(evalSymbol)
        println("(eval (quote x)) = ${symbolResult.toString()}")
        @Assert(symbolResult.toString() == "100")

        // 测试 eval quoted list
        let evalList = Parser.parseSingleFromString("(eval (quote ((lambda (x y) (+ x y)) 10 20)))")
        let listResult = evaluator.eval(evalList)
        println("(eval (quote ((lambda (x y) (+ x y)) 10 20))) = ${listResult.toString()}")
        @Assert(listResult.toString() == "30")

        // 测试 eval 动态构造的表达式
        evaluator.eval(Parser.parseSingleFromString("(define code (quote (+ 1 2 3)))"))
        let evalDynamic = Parser.parseSingleFromString("(eval code)")
        let dynamicResult = evaluator.eval(evalDynamic)
        println("(eval code) where code = (+ 1 2 3) = ${dynamicResult.toString()}")
        @Assert(dynamicResult.toString() == "6")

        // 测试 eval 嵌套调用
        let evalNested = Parser.parseSingleFromString("(eval (list (quote +) 5 10))")
        let nestedResult = evaluator.eval(evalNested)
        println("(eval (list (quote +) 5 10)) = ${nestedResult.toString()}")
        @Assert(nestedResult.toString() == "15")

        // 测试 eval 字符串
        let evalStr = Parser.parseSingleFromString("(eval (quote \"hello\"))")
        let strResult = evaluator.eval(evalStr)
        println("(eval (quote \"hello\")) = ${strResult.toString()}")
        @Assert(strResult.toString() == "\"hello\"")

        // 测试 eval nil
        let evalNil = Parser.parseSingleFromString("(eval (quote nil))")
        let nilResult = evaluator.eval(evalNil)
        println("(eval (quote nil)) = ${nilResult.toString()}")
        @Assert(nilResult.toString() == "nil")
    }

    @TestCase
    func testMacroRestParameters() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        println("\n=== 测试宏的纯可变参数 ===")

        // 测试1: 纯可变参数宏 - 返回 list
        evaluator.eval(Parser.parseSingleFromString("(defmacro list-all (. args) `(list ,@args))"))
        let result1 = evaluator.eval(Parser.parseSingleFromString("(list-all 1 2 3)"))
        println("(list-all 1 2 3) = ${result1.toString()}")
        @Assert(result1.toString() == "(1 2 3)")

        // 测试2: 纯可变参数宏 - 返回 quoted 列表
        evaluator.eval(Parser.parseSingleFromString("(defmacro quote-all (. args) `(quote ,args))"))
        let result2 = evaluator.eval(Parser.parseSingleFromString("(quote-all a b c)"))
        println("(quote-all a b c) = ${result2.toString()}")
        @Assert(result2.toString() == "(a b c)")

        // 测试3: 纯可变参数宏 - 使用 map 和 ,@
        evaluator.eval(Parser.parseSingleFromString("(defmacro print-all (. args) `(begin ,@(map (lambda (x) `(println ,x)) args)))"))
        let result3 = evaluator.eval(Parser.parseSingleFromString("(print-all \"Hello\" \"World\")"))
        println("(print-all \"Hello\" \"World\") = ${result3.toString()}")
        @Assert(result3.toString() == "nil")  // begin 返回 nil

        // 测试4: 混合参数宏 (固定参数 + 可变参数)
        evaluator.eval(Parser.parseSingleFromString("(defmacro mixed-macro (x y . rest) `(list 'x= ,x 'y= ,y 'rest= (quote ,rest)))"))
        let result4 = evaluator.eval(Parser.parseSingleFromString("(mixed-macro 1 2 3 4)"))
        println("(mixed-macro 1 2 3 4) = ${result4.toString()}")
        @Assert(result4.toString() == "(x= 1 y= 2 rest= (3 4))")

        // 测试5: 空可变参数
        evaluator.eval(Parser.parseSingleFromString("(defmacro only-rest (. rest) `(quote ,rest))"))
        let result5 = evaluator.eval(Parser.parseSingleFromString("(only-rest)"))
        println("(only-rest) = ${result5.toString()}")
        @Assert(result5.toString() == "nil")
    }

    @TestCase
    func testHashMapDestructuring() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        println("\n=== 测试 HashMap 解构 ===")

        // 测试1: 基本 HashMap 解构 - 使用返回值而不是 define
        println("\n测试1: 基本 HashMap 解构")
        evaluator.eval(Parser.parseSingleFromString("(define config {:host \"localhost\" :port 8080})"))
        let result1 = evaluator.eval(Parser.parseSingleFromString("(let [{:host h :port p} config] (list h p))"))
        println("result = ${result1.toString()}")
        @Assert(result1.toString() == "(\"localhost\" 8080)")

        // 测试2: 部分键解构
        println("\n测试2: 部分键解构")
        evaluator.eval(Parser.parseSingleFromString("(define data {:name \"Alice\" :age 30 :city \"Beijing\"})"))
        let result2 = evaluator.eval(Parser.parseSingleFromString("(let [{:name n :age a} data] (list n a))"))
        println("result = ${result2.toString()}")
        @Assert(result2.toString() == "(\"Alice\" 30)")

        // 测试3: 单键解构
        println("\n测试3: 单键解构")
        evaluator.eval(Parser.parseSingleFromString("(define single {:value 42})"))
        let result3 = evaluator.eval(Parser.parseSingleFromString("(let [{:value v} single] v)"))
        println("result = ${result3.toString()}")
        @Assert(result3.toString() == "42")

        // 测试4: 空HashMap 解构
        println("\n测试4: 空 HashMap 解构")
        evaluator.eval(Parser.parseSingleFromString("(define empty-map {})"))
        let result4 = evaluator.eval(Parser.parseSingleFromString("(let [{:x x} empty-map] x)"))
        println("result = ${result4.toString()}")
        @Assert(result4.toString() == "nil")

        // 测试5: 不存在的键
        println("\n测试5: 不存在的键")
        let result5 = evaluator.eval(Parser.parseSingleFromString("(let [{:nonexistent x} {:value 100}] x)"))
        println("result = ${result5.toString()}")
        @Assert(result5.toString() == "nil")

        // 测试6: 嵌套解构（向量 + HashMap）- 分步测试
        println("\n测试6: 嵌套 HashMap 解构")
        // 先定义变量，再解构
        evaluator.eval(Parser.parseSingleFromString("(define user1 {:name \"Bob\" :email \"bob@example.com\"})"))
        evaluator.eval(Parser.parseSingleFromString("(define user2 {:name \"Carol\" :email \"carol@example.com\"})"))
        let result6a = evaluator.eval(Parser.parseSingleFromString("(let [{:name n1} user1] (let [{:name n2} user2] (list n1 n2))))"))
        println("嵌套 HashMap 解构结果 = ${result6a.toString()}")
        @Assert(result6a.toString() == "(\"Bob\" \"Carol\")")

        // 测试7: 直接在 let 中创建 HashMap 并解构
        println("\n测试7: 直接 HashMap 解构")
        let result7 = evaluator.eval(Parser.parseSingleFromString("(let [{:name n :email e} {:name \"Dave\" :email \"dave@example.com\"}] (list n e)))"))
        println("result = ${result7.toString()}")
        @Assert(result7.toString() == "(\"Dave\" \"dave@example.com\")")

        // 测试8: 多个 HashMap 解构
        println("\n测试8: 多个 HashMap 解构")
        let result8 = evaluator.eval(Parser.parseSingleFromString("(let [{:name n1} {:name \"Alice\"} {:name n2} {:name \"Bob\"}] (list n1 n2))"))
        println("result = ${result8.toString()}")
        @Assert(result8.toString() == "(\"Alice\" \"Bob\")")
    }

    @TestCase
    func testMatchHashMapPattern() {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)

        println("\n=== 测试 match HashMap 模式 ===")

        // 测试1: 基本 HashMap 匹配
        println("\n测试1: 基本 HashMap 匹配")
        let result1 = evaluator.eval(Parser.parseSingleFromString("(match {:name \"Alice\" :age 30} {:name n :age a} (list n a) _ \"not matched\")"))
        println("result = ${result1.toString()}")
        @Assert(result1.toString() == "(\"Alice\" 30)")

        // 测试2: 部分键匹配
        println("\n测试2: 部分键匹配")
        let result2 = evaluator.eval(Parser.parseSingleFromString("(match {:name \"Bob\" :age 25} {:name n} n _ \"not matched\")"))
        println("result = ${result2.toString()}")
        @Assert(result2.toString() == "\"Bob\"")

        // 测试3: 不匹配的情况，跳到下一个分支
        println("\n测试3: 不匹配跳到下一个分支")
        let result3 = evaluator.eval(Parser.parseSingleFromString("(match {:name \"Charlie\"} {:name n :age a} \"should not match\" {:name n} (list \"matched\" n))"))
        println("result = ${result3.toString()}")
        @Assert(result3.toString() == "(\"matched\" \"Charlie\")")

        // 测试4: 多个分支
        println("\n测试4: 多个分支 - 变量匹配")
        let result4 = evaluator.eval(Parser.parseSingleFromString("(match {:type \"user\" :name \"Dave\"} {:type t :name n} (list t n) {:type \"admin\"} \"admin\" _ \"unknown\")"))
        println("result = ${result4.toString()}")
        @Assert(result4.toString() == "(\"user\" \"Dave\")")

        // 测试5: 通配符 _
        println("\n测试5: 通配符 _")
        let result5 = evaluator.eval(Parser.parseSingleFromString("(match {:x 1 :y 2} _ \"wildcard\")"))
        println("result = ${result5.toString()}")
        @Assert(result5.toString() == "\"wildcard\"")

        // 测试6: 单键匹配
        println("\n测试6: 单键匹配")
        let result6 = evaluator.eval(Parser.parseSingleFromString("(match {:value 42} {:value v} v)"))
        println("result = ${result6.toString()}")
        @Assert(result6.toString() == "42")

        // 测试7: 值类型匹配
        println("\n测试7: 值类型匹配")
        let result7 = evaluator.eval(Parser.parseSingleFromString("(match {:count 42} {:count c} c)"))
        println("result = ${result7.toString()}")
        @Assert(result7.toString() == "42")
    }
}
