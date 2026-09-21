package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*

/**
 * 纯函数子集（BuiltinFunctions.registerPure）单元测试。
 *
 * 关注两件事：纯计算能力是否齐备，以及文件/模块等能力是否真的不在环境里。
 * 后者是重点——权限检查（setFuncChecker）对特殊形式无效，且函数值可被当作
 * 参数传递，因此「注册后过滤」不可靠，「不注册」才是可靠的边界表达。
 */
@Test
class PureEnvTest {

    func evalPure(code: String): LispValue {
        let env = Environment()
        BuiltinFunctions.registerPure(env)
        let ev = Evaluator(env)
        ev.eval(Parser.parseSingleFromString(code))
    }

    func isError(v: LispValue): Bool {
        match (v) {
            case Error(_) => true
            case _ => false
        }
    }

    func joinAll(parts: ArrayList<String>): String {
        let sb = StringBuilder()
        for (p in parts) {
            sb.append(p)
        }
        sb.toString()
    }

    // ==================== 纯计算能力 ====================

    @TestCase
    func testArithmetic() {
        match (evalPure("(+ 1 2 3)")) {
            case Int(n) => @Assert(n == 6)
            case _ => @Fail("arithmetic should work in pure env")
        }
    }

    @TestCase
    func testStringOps() {
        // str 原本与 print 同在一个注册函数里，拆分后必须仍属纯函数子集
        match (evalPure("(str \"a\" 1 \"b\")")) {
            case Str(s) => @Assert(s == "a1b")
            case _ => @Fail("str should be available in pure env")
        }
    }

    @TestCase
    func testStringCompare() {
        match (evalPure("(string-contains? \"hello world\" \"world\")")) {
            case Boolean(b) => @Assert(b)
            case _ => @Fail("string-contains? should be available")
        }
    }

    @TestCase
    func testModernAliases() {
        // first / rest 是 car / cdr 的别名
        match (evalPure("(first '(1 2 3))")) {
            case Int(n) => @Assert(n == 1)
            case _ => @Fail("first should be available")
        }
    }

    @TestCase
    func testHigherOrder() {
        // map / lambda 由求值器内建驱动，不需要环境里有东西
        let v = evalPure("(map (lambda (x) (* x 2)) '(1 2 3))")
        @Assert(!isError(v))
    }

    @TestCase
    func testHashmapOps() {
        match (evalPure("(hashmap-get {:a 1} \"a\")")) {
            case Int(n) => @Assert(n == 1)
            case _ => @Fail("hashmap-get should work")
        }
    }

    @TestCase
    func testErrorHelper() {
        let v = evalPure("(error \"boom\")")
        match (v) {
            case Error(e) =>
                match (e.kind) {
                    case ErrorType.RuntimeError => @Assert(true)
                    case _ => @Fail("error should produce RuntimeError")
                }
            case _ => @Fail("error helper should be available")
        }
    }

    // ==================== 危险能力必须不在环境里 ====================

    @TestCase
    func testNoFileAccess() {
        @Assert(isError(evalPure("(cangjie::read-file \"/etc/passwd\")")))
        @Assert(isError(evalPure("(cangjie::write-file \"/tmp/x\" \"y\")")))
        @Assert(isError(evalPure("(cangjie::append-file \"/tmp/x\" \"y\")")))
        @Assert(isError(evalPure("(cangjie::list-dir \"/\")")))
        @Assert(isError(evalPure("(cangjie::exists? \"/etc/passwd\")")))
    }

    @TestCase
    func testNoOutputWithoutSink() {
        // 未传入 output 时不注册输出函数
        @Assert(isError(evalPure("(println \"x\")")))
        @Assert(isError(evalPure("(print \"x\")")))
        @Assert(isError(evalPure("(display \"x\")")))
        @Assert(isError(evalPure("(princ \"x\")")))
    }

    @TestCase
    func testEvalCannotEscape() {
        // eval 是特殊形式，先于权限检查执行且无法移除；
        // 但环境里没有文件能力，因此「逃逸」无从发生——这正是选「不注册」而非「过滤」的原因
        @Assert(isError(evalPure("(eval '(cangjie::read-file \"/etc/passwd\"))")))
    }

    @TestCase
    func testApplyCannotReachFile() {
        // apply 可绕过函数白名单（实测：白名单判断对 apply 无效），
        // 但被绕过的函数必须先在环境里存在。环境里没有时，
        // evalApply 的 `case _ => Nil` 落空返回 nil——不执行任何东西即安全。
        match (evalPure("(apply cangjie::read-file '(\"/etc/passwd\"))")) {
            case Nil => @Assert(true)
            case _ => @Fail("apply on an absent function should fall through to nil")
        }
    }

    @TestCase
    func testModuleImportUnavailable() {
        @Assert(isError(evalPure("(import foo::bar)")))
    }

    // ==================== 输出通道 ====================

    @TestCase
    func testOutputSinkReceivesText() {
        // 字符串沿用 toString 的带引号形式（与 registerPrint 的既有行为一致）
        let buf = ArrayList<String>()
        let env = Environment()
        BuiltinFunctions.registerPure(env, output: { s => buf.add(s) })
        let ev = Evaluator(env)
        ev.eval(Parser.parseSingleFromString("(println \"hello\" \"world\")"))
        @Assert(joinAll(buf) == "\"hello\" \"world\"\n")
    }

    @TestCase
    func testOutputSinkForDisplayAndNewline() {
        let buf = ArrayList<String>()
        let env = Environment()
        BuiltinFunctions.registerPure(env, output: { s => buf.add(s) })
        let ev = Evaluator(env)
        ev.eval(Parser.parseSingleFromString("(display \"a\")"))
        ev.eval(Parser.parseSingleFromString("(newline)"))
        ev.eval(Parser.parseSingleFromString("(princ \"b\")"))
        @Assert(joinAll(buf) == "\"a\"\n\"b\"")
    }

    @TestCase
    func testOutputFunctionsCallableWithSink() {
        // 传入 sink 后输出函数必须可调用（不报错），且输出都落进 sink
        let buf = ArrayList<String>()
        let env = Environment()
        BuiltinFunctions.registerPure(env, output: { s => buf.add(s) })
        let ev = Evaluator(env)
        let v = ev.eval(Parser.parseSingleFromString("(println \"x\")"))
        @Assert(!isError(v))
        @Assert(buf.size > 0)
    }
}
