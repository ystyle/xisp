package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*

/**
 * 字节码 VM / 编译器测试
 *
 * 覆盖：
 * 1. 直接函数调用走帧切换（修复前经 Str(name) 堕入 AST）
 * 2. 闭包值经寄存器传递的调用路径
 * 3. globalRefs 实时解析（重新 define 全局变量后取新值）
 * 4. 宏展开后的表达式可再编译进 VM（子求值器继承编译能力）
 * 5. 嵌套 VM 执行（AST 回退后重入 VM）
 * 6. 命名 let 不静默编译（回退 AST）
 */
@Test
class BytecodeTest {
    /**
     * 用字节码模式求值一串表达式，返回最后一个表达式的结果
     */
    func evalWithBytecode(code: String): LispValue {
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let evaluator = Evaluator(env)
        let compiler = BytecodeCompiler()
        let vm = BytecodeVM()
        vm.setCompiler(compiler)
        evaluator.setCompiler(compiler)
        evaluator.setVM(vm)
        var result: LispValue = Nil
        for (expr in Parser.parseFromString(code)) {
            result = evaluator.eval(expr)
        }
        result
    }

    @TestCase
    func testDirectCallFrameSwitch() {
        // 直接调用（函数名常量 → env.lookup → Closure → 帧切换）
        let result = this.evalWithBytecode("(define (f x) (+ x 1)) (f 41)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 42)
            case _ => @Fail("expected 42, got ${result}")
        }
    }

    @TestCase
    func testDirectRecursiveCall() {
        // 递归直接调用：fib 的递归全部经帧切换在 VM 中执行
        let result = this.evalWithBytecode(
            "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 55)
            case _ => @Fail("expected 55, got ${result}")
        }
    }

    @TestCase
    func testClosureValueCall() {
        // 闭包经局部变量/参数传递后调用（寄存器中的函数值 → 帧切换）
        let result = this.evalWithBytecode(
            "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (define (run-it f) (f 10)) (run-it fib)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 55)
            case _ => @Fail("expected 55, got ${result}")
        }
    }

    @TestCase
    func testGlobalRefReload() {
        // globalRefs 实时解析：重新 define 全局变量后，函数读到新值（不缓存旧值）
        let result = this.evalWithBytecode("(define g 1) (define (f) g) (f) (define g 2) (f)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 2)
            case _ => @Fail("expected 2, got ${result}")
        }
    }

    @TestCase
    func testGlobalRefCrossEnv() {
        // 两个不同环境编译出的同名结构函数各自解析自己的全局引用
        let result = this.evalWithBytecode(
            "(define g 10) (define (f) (+ g 1)) (let ((g 100)) (f))")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 11)
            case _ => @Fail("expected 11, got ${result}")
        }
    }

    @TestCase
    func testMacroExpansionRecompiles() {
        // 宏展开后的表达式由子求值器（继承 compiler/vm）再编译进 VM
        let result = this.evalWithBytecode(
            "(defmacro two () 2) (define (add-two x) (+ x (two))) (add-two 40)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 42)
            case _ => @Fail("expected 42, got ${result}")
        }
    }

    @TestCase
    func testNestedVMReentry() {
        // AST 回退（lambda 不支持编译）的函数体内，调用已编译函数时经
        // 子求值器再编译进 VM：AST 边缘自动回到 VM
        let result = this.evalWithBytecode(
            "(define (id x) x) (define (pass f) (f (id 9))) (pass id)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 9)
            case _ => @Fail("expected 9, got ${result}")
        }
    }

    @TestCase
    func testNamedLetFallsBack() {
        // 命名 let 不受编译支持：编译器必须返回 None（回退 AST），
        // 而不是静默产出语义错误的字节码
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let compiler = BytecodeCompiler()
        let namedLet = Parser.parseSingleFromString("(let loop ((i 3)) loop)")
        match (compiler.compile(namedLet)) {
            case None => @Assert(true)
            case Some(_) => @Fail("named let must not compile; should fallback to AST")
        }
    }

    @TestCase
    func testLetCompiles() {
        // 普通 let 必须照常编译（回归：不要误伤）
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let compiler = BytecodeCompiler()
        let letExpr = Parser.parseSingleFromString("(let ((x 1) (y 2)) (+ x y))")
        match (compiler.compile(letExpr)) {
            case Some(_) => @Assert(true)
            case None => @Fail("ordinary let should compile")
        }
    }

    @TestCase
    func testEmptyLetCompiles() {
        // (let () body) 合法且应编译
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let compiler = BytecodeCompiler()
        let letExpr = Parser.parseSingleFromString("(let () 42)")
        match (compiler.compile(letExpr)) {
            case Some(_) => @Assert(true)
            case None => @Fail("empty let should compile")
        }
    }

    @TestCase
    func testSetGlobalVisible() {
        // 编译模式下 set! 全局变量对后续帧切换可见（实时解析全局引用）
        let result = this.evalWithBytecode(
            "(define g 1) (define (f) g) (set! g 2) (f)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 2)
            case _ => @Fail("expected 2, got ${result}")
        }
    }

    @TestCase
    func testLambdaCompiledClosureCapture() {
        // lambda 编译 + 闭包捕获外层参数（帧槽 → 捕获环境）
        let result = this.evalWithBytecode(
            "(define (adder x) (lambda (y) (+ x y))) (define add5 (adder 5)) (add5 10)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 15)
            case _ => @Fail("expected 15, got ${result}")
        }
    }

    @TestCase
    func testLambdaLocalCapture() {
        // let 局部变量的闭包捕获
        let result = this.evalWithBytecode("((let ((x 7)) (lambda (y) (+ x y))) 3)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 10)
            case _ => @Fail("expected 10, got ${result}")
        }
    }

    @TestCase
    func testSetCapturedVar() {
        // 闭包内 set! 捕获变量（counter 模式做 3 次）
        let result = this.evalWithBytecode(
            "(define (counter) (let ((n 0)) (lambda () (set! n (+ n 1)) n))) (define c (counter)) (c) (c) (c)")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 3)
            case _ => @Fail("expected 3, got ${result}")
        }
    }

    @TestCase
    func testQuasiquoteBasic() {
        let result = this.evalWithBytecode("(define x 42) `(a ,x b)")
        match (result) {
            case Cons(cell) =>
                // (a 42 b)
                let (val, ok) = this.listAsStrings(cell)
                @Assert(ok && val.size == 3 && val[0] == "a" && val[1] == "42" && val[2] == "b")
            case _ => @Fail("expected list, got ${result}")
        }
    }

    @TestCase
    func testQuasiquoteSplice() {
        let result = this.evalWithBytecode("`(a ,@(list 1 2 3) b)")
        match (result) {
            case Cons(cell) =>
                let (val, ok) = this.listAsStrings(cell)
                @Assert(ok && val.size == 5 && val[0] == "a" && val[1] == "1" && val[2] == "2" && val[3] == "3" && val[4] == "b")
            case _ => @Fail("expected list, got ${result}")
        }
    }

    // 辅助：把列表转成字符串数组（用于断言）
    func listAsStrings(cell: ConsCell): (ArrayList<String>, Bool) {
        let result = ArrayList<String>()
        var current: LispValue = Cons(cell)
        var ok = true
        while (let Cons(c) <- current) {
            result.add(c.car.toString())
            current = c.cdr
        }
        if (!current.isNil()) { ok = false }
        (result, ok)
    }

    @TestCase
    func testIfLetCompiled() {
        let result = this.evalWithBytecode(
            "(define v (list 1 2)) (if-let (val v) val v)")
        match (result) {
            case Cons(cell) =>
                let (val, ok) = this.listAsStrings(cell)
                @Assert(ok && val.size == 2 && val[0] == "1" && val[1] == "2")
            case _ => @Fail("expected (1 2), got ${result}")
        }
    }

    @TestCase
    func testWhenLetStarCompiled() {
        let result = this.evalWithBytecode(
            "(when-let* ((a 1) (b (+ a 1))) (+ a b))")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 3)
            case _ => @Fail("expected 3, got ${result}")
        }
    }

    @TestCase
    func testThreadFirstCompiled() {
        let result = this.evalWithBytecode("(-> 5 (+ 1) (* 2))")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 12)
            case _ => @Fail("expected 12, got ${result}")
        }
    }

    @TestCase
    func testApplyCompiled() {
        let result = this.evalWithBytecode("(apply + 1 2 (list 3 4))")
        match (result) {
            case LispValue.Int(i) => @Assert(i == 10)
            case _ => @Fail("expected 10, got ${result}")
        }
    }

    @TestCase
    func testMapClosureCallback() {
        // 高阶函数回调 Closure（lambda 编译产物）
        let result = this.evalWithBytecode(
            "(map (lambda (x) (* x 2)) (list 1 2 3))")
        match (result) {
            case Cons(cell) =>
                let (val, ok) = this.listAsStrings(cell)
                @Assert(ok && val.size == 3 && val[0] == "2" && val[1] == "4" && val[2] == "6")
            case _ => @Fail("expected (2 4 6), got ${result}")
        }
    }

    @TestCase
    func testZeroIsFalsy() {
        // isTruthy 语义：0 为假（与 AST 一致）
        let result = this.evalWithBytecode("(if 0 \"truthy\" \"falsy\")")
        match (result) {
            case Str(s) => @Assert(s == "falsy")
            case _ => @Fail("expected falsy, got ${result}")
        }
    }

    @TestCase
    func testRestParamsFallsBack() {
        // &rest 参数不支持编译：编译返回 None（回退 AST 保证语义）
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let compiler = BytecodeCompiler()
        let restFn = Parser.parseSingleFromString("(define (f a &rest r) r)")
        match (compiler.compile(restFn)) {
            case None => @Assert(true)
            case Some(_) => @Fail("&rest params must not compile; should fallback to AST")
        }
    }

    @TestCase
    func testRestParamsSemantics() {
        // 回退后 &rest 语义正确（经 AST）：(f 1 2 3 4) → a=1, r=(2 3 4)
        let result = this.evalWithBytecode(
            "(define (f a &rest r) (list a (length r))) (f 1 2 3 4)")
        match (result) {
            case Cons(cell) =>
                let (val, ok) = this.listAsStrings(cell)
                @Assert(ok && val.size == 2 && val[0] == "1" && val[1] == "3")
            case _ => @Fail("expected (1 3), got ${result}")
        }
    }

    @TestCase
    func testMatchSymbolPatternBinding() {
        // match Symbol 模式：总是匹配 + 绑定到 body（与 AST matchPattern 语义一致）
        let result = this.evalWithBytecode(
            "(match 42 (x (list x x)) (else \"nope\"))")
        match (result) {
            case Cons(cell) =>
                let (val, ok) = this.listAsStrings(cell)
                @Assert(ok && val.size == 2 && val[0] == "42" && val[1] == "42")
            case _ => @Fail("expected (42 42), got ${result}")
        }
    }

    @TestCase
    func testMatchLiteralAndElse() {
        let result = this.evalWithBytecode(
            "(match 10 (1000 \"thousand\") (100 \"hundred\") (10 \"ten\") (else \"other\"))")
        match (result) {
            case Str(s) => @Assert(s == "ten")
            case _ => @Fail("expected ten, got ${result}")
        }
    }

    @TestCase
    func testMatchGuardFallsBack() {
        // guard 子句（pattern when guard ...）不支持编译：编译返回 None（回退 AST 保证语义）
        let env = Environment()
        BuiltinFunctions.registerAll(env)
        let compiler = BytecodeCompiler()
        let guardMatch = Parser.parseSingleFromString("(match 5 (x when (> x 3) \"big\") (else \"small\"))")
        match (compiler.compile(guardMatch)) {
            case None => @Assert(true)
            case Some(_) => @Fail("guard clauses must not compile; should fallback to AST")
        }
    }
}
