package ystyle::xisp.repl

import ystyle::xisp.types.*
import std.collection.HashMap
import std.collection.ArrayList
import std.unittest.*
import std.unittest.testmacro.*

func containsStr(haystack: String, needle: String): Bool {
    match (haystack.indexOf(needle)) {
        case Some(_) => true
        case None => false
    }
}

@Test
class HighlighterTest {
    @TestCase
    func testHighlightInt() {
        let h = Highlighter()
        let result = h.highlight(LispValue.Int(42))
        @Assert(result, "\u{001b}[33m42\u{001b}[0m")
    }

    @TestCase
    func testHighlightFloat() {
        let h = Highlighter()
        let result = h.highlight(LispValue.Float(3.5))
        @Assert(containsStr(result, "\u{001b}[33m"))
        @Assert(containsStr(result, "3"))
        @Assert(containsStr(result, "5"))
        @Assert(containsStr(result, "\u{001b}[0m"))
    }

    @TestCase
    func testHighlightString() {
        let h = Highlighter()
        let result = h.highlight(Str("hello"))
        @Assert(result, "\u{001b}[32m\"hello\"\u{001b}[0m")
    }

    @TestCase
    func testHighlightBool() {
        let h = Highlighter()
        @Assert(h.highlight(LispValue.Boolean(true)), "\u{001b}[36mtrue\u{001b}[0m")
        @Assert(h.highlight(LispValue.Boolean(false)), "\u{001b}[36mfalse\u{001b}[0m")
    }

    @TestCase
    func testHighlightSymbol() {
        let h = Highlighter()
        @Assert(h.highlight(Symbol("foo")), "\u{001b}[34mfoo\u{001b}[0m")
    }

    @TestCase
    func testHighlightNil() {
        let h = Highlighter()
        @Assert(h.highlight(Nil), "\u{001b}[90mnil\u{001b}[0m")
    }

    @TestCase
    func testHighlightCons() {
        let h = Highlighter()
        let list = consValue(LispValue.Int(1), consValue(LispValue.Int(2), consValue(LispValue.Int(3), Nil)))
        @Assert(h.highlight(list), "(\u{001b}[33m1\u{001b}[0m \u{001b}[33m2\u{001b}[0m \u{001b}[33m3\u{001b}[0m)")
    }

    @TestCase
    func testHighlightConsNested() {
        let h = Highlighter()
        let inner = consValue(Symbol("b"), Nil)
        let outer = consValue(Symbol("a"), consValue(inner, Nil))
        @Assert(h.highlight(outer), "(\u{001b}[34ma\u{001b}[0m (\u{001b}[34mb\u{001b}[0m))")
    }

    @TestCase
    func testHighlightConsDotted() {
        let h = Highlighter()
        let pair = Cons(ConsCell(LispValue.Int(1), Symbol("b")))
        @Assert(h.highlight(pair), "(\u{001b}[33m1\u{001b}[0m . \u{001b}[34mb\u{001b}[0m)")
    }

    @TestCase
    func testHighlightHashMap() {
        let h = Highlighter()
        let map = HashMap<String, LispValue>()
        map.add("x", LispValue.Int(42))
        map.add("y", Str("hello"))
        let hm = LispValue.HashMap(map)
        let result = h.highlight(hm)
        @Assert(containsStr(result, "\u{001b}[33m42\u{001b}[0m"))
        @Assert(containsStr(result, "\u{001b}[32m"))
    }

    @TestCase
    func testHighlightHashMapEmpty() {
        let h = Highlighter()
        let map = HashMap<String, LispValue>()
        let hm = LispValue.HashMap(map)
        @Assert(h.highlight(hm), "{}")
    }

    @TestCase
    func testErrorKind() {
        let h = Highlighter()
        let err = XispError(TypeError, "type mismatch")
        let result = h.highlightError(err)
        @Assert(result, "\u{001b}[1;31m[TypeError]\u{001b}[0m type mismatch")
    }

    @TestCase
    func testErrorWithStack() {
        let h = Highlighter()
        let stack = ArrayList<String>()
        stack.add("inner-func")
        stack.add("outer-func")
        let err = XispError(RuntimeError, "something wrong", stack)
        let result = h.highlightError(err)
        @Assert(containsStr(result, "[RuntimeError]"))
        @Assert(containsStr(result, "outer-func"))
        @Assert(containsStr(result, "inner-func"))
    }

    @TestCase
    func testErrorKindAll() {
        let h = Highlighter()
        let err1 = XispError(UnboundVariable, "x not found")
        @Assert(containsStr(h.highlightError(err1), "[UnboundVariable]"))

        let err2 = XispError(UndefinedFunction, "foo not defined")
        @Assert(containsStr(h.highlightError(err2), "[UndefinedFunction]"))

        let err3 = XispError(ArityMismatch, "wrong arg count")
        @Assert(containsStr(h.highlightError(err3), "[ArityMismatch]"))

        let err4 = XispError(SyntaxError, "unexpected token")
        @Assert(containsStr(h.highlightError(err4), "[SyntaxError]"))

        let err5 = XispError(IOError, "file not found")
        @Assert(containsStr(h.highlightError(err5), "[IOError]"))

        let err6 = XispError(ModuleError, "module load failed")
        @Assert(containsStr(h.highlightError(err6), "[ModuleError]"))
    }

    @TestCase
    func testHighlightValueInError() {
        let h = Highlighter()
        let err = XispError(TypeError, "expected number, got string")
        let value = LispValue.Error(err)
        let result = h.highlight(value)
        @Assert(containsStr(result, "[TypeError]"))
        @Assert(containsStr(result, "expected number"))
    }

    @TestCase
    func testHighlightProcedure() {
        let h = Highlighter()
        let proc = Procedure(ArrayList<String>(), Nil, Environment())
        @Assert(h.highlight(proc), "#<procedure>")
    }

    @TestCase
    func testHighlightNativeFunc() {
        let h = Highlighter()
        let native = NativeFunc({ args => args[0] })
        @Assert(h.highlight(native), "#<native-function>")
    }

    @TestCase
    func testErrorNoStack() {
        let h = Highlighter()
        let err = XispError(RuntimeError, "simple error")
        let result = h.highlightError(err)
        @Assert(result, "\u{001b}[1;31m[RuntimeError]\u{001b}[0m simple error")
    }

    @TestCase
    func testErrorStackOrder() {
        let h = Highlighter()
        let stack = ArrayList<String>()
        stack.add("bottom")
        stack.add("middle")
        stack.add("top")
        let err = XispError(RuntimeError, "stack order", stack)
        let result = h.highlightError(err)
        let lines = result.split("\n")
        @Assert(lines.size, 4)
        @Assert(lines[0], "\u{001b}[1;31m[RuntimeError]\u{001b}[0m stack order")
        @Assert(containsStr(lines[1], "top"))
        @Assert(containsStr(lines[2], "middle"))
        @Assert(containsStr(lines[3], "bottom"))
    }
}
