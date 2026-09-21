package ystyle::xisp

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.*
import ystyle::xisp.parser.*
import ystyle::xisp.types.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*

/**
 * 严格解析（Parser.parseChecked）单元测试。
 *
 * 宽松解析会静默补全不完整输入，宿主执行模型生成的代码时这意味着
 * 残缺程序照常运行；严格解析把「输入不完整」显式报告出来。
 * 同时验证宽松路径的既有行为未被改动。
 */
@Test
class ParseCheckedTest {

    func hasError(input: String): Bool {
        Parser.parseChecked(input).error.isSome()
    }

    func errorMessage(input: String): String {
        match (Parser.parseChecked(input).error) {
            case Some(e) => e.message
            case None => ""
        }
    }

    // ==================== 不完整输入必须报错 ====================

    @TestCase
    func testUnclosedList() {
        @Assert(hasError("(+ 1 "))
        @Assert(hasError("(+ 1"))
        @Assert(hasError("("))
    }

    @TestCase
    func testUnclosedNestedList() {
        @Assert(hasError("(foo (bar"))
    }

    @TestCase
    func testUnclosedVector() {
        @Assert(hasError("[1 2 3"))
    }

    @TestCase
    func testUnclosedHashMap() {
        @Assert(hasError("{:a 1"))
    }

    @TestCase
    func testUnclosedString() {
        @Assert(hasError("\"unterminated"))
    }

    @TestCase
    func testErrorMessageIsSpecific() {
        // 错误消息要能让模型据此自我修正
        @Assert(errorMessage("(+ 1 ").contains("')'"))
        @Assert(errorMessage("[1 2").contains("']'"))
        @Assert(errorMessage("{:a 1").contains("'}'"))
        @Assert(errorMessage("\"abc").contains("unterminated"))
    }

    // ==================== 完整输入不得误报 ====================

    @TestCase
    func testCompleteFormsHaveNoError() {
        @Assert(!hasError("(+ 1 2)"))
        @Assert(!hasError("(define (f x) (* x 2))"))
        @Assert(!hasError("(let [x 1 y 2] (+ x y))"))
        @Assert(!hasError("[1 2 3]"))
        @Assert(!hasError("{:a 1 :b 2}"))
        @Assert(!hasError("\"closed\""))
        @Assert(!hasError("(map (lambda (x) (* x 2)) (range 1 10))"))
    }

    @TestCase
    func testMultipleExpressions() {
        let outcome = Parser.parseChecked("(+ 1 2) (+ 3 4)")
        @Assert(outcome.error.isNone())
        @Assert(outcome.exprs.size == 2)
    }

    @TestCase
    func testEmptyInput() {
        let outcome = Parser.parseChecked("")
        @Assert(outcome.error.isNone())
        @Assert(outcome.exprs.size == 0)
    }

    @TestCase
    func testCommentOnly() {
        let outcome = Parser.parseChecked("; just a comment")
        @Assert(outcome.error.isNone())
        @Assert(outcome.exprs.size == 0)
    }

    // ==================== 出错时的结果语义 ====================

    @TestCase
    func testErrorKeepsEarlierExpressions() {
        // 出错前的完整表达式保留在 exprs 中，但 error 非空即要求调用方整体拒绝
        let outcome = Parser.parseChecked("(+ 1 2) (foo")
        @Assert(outcome.error.isSome())
        @Assert(outcome.exprs.size >= 1)
    }

    @TestCase
    func testErrorIsSyntaxErrorKind() {
        match (Parser.parseChecked("(+ 1 ").error) {
            case Some(e) =>
                match (e.kind) {
                    case ErrorType.SyntaxError => @Assert(true)
                    case _ => @Fail("parse error should be SyntaxError kind")
                }
            case None => @Fail("unclosed input should report an error")
        }
    }

    // ==================== 宽松路径行为不变 ====================

    @TestCase
    func testLenientPathStillCompletes() {
        // 既有行为：未闭合括号被补全，未闭合字符串被接受。
        // REPL 的连续输入依赖它，因此严格模式必须是 opt-in。
        let exprs = Parser.parseFromString("(+ 1 ")
        @Assert(exprs.size == 1)
        @Assert(exprs[0].toString() == "(+ 1)")

        let strExprs = Parser.parseFromString("\"unterminated")
        @Assert(strExprs.size == 1)
    }

    @TestCase
    func testLenientParserHasNoRecordedError() {
        let lexer = Lexer("(+ 1 ")
        let parser = Parser(lexer.tokenize())
        parser.parse()
        // 未启用严格模式时不记录错误
        @Assert(parser.getSyntaxError().isNone())
    }

    // ==================== 与求值器配合 ====================

    @TestCase
    func testEvalOfStrictlyParsedProgram() {
        // 宿主拿到 exprs 后逐个求值；完整输入的路径必须与宽松解析一致
        let outcome = Parser.parseChecked("(define (double x) (* x 2)) (double 21)")
        @Assert(outcome.error.isNone())
        let env = Environment()
        BuiltinFunctions.registerPure(env)
        let ev = Evaluator(env)
        var last: LispValue = Nil
        for (expr in outcome.exprs) {
            last = ev.eval(expr)
        }
        match (last) {
            case Int(n) => @Assert(n == 42)
            case _ => @Fail("strictly parsed program should evaluate to 42")
        }
    }
}
