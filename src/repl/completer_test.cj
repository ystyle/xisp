package ystyle::xisp.repl

import ystyle::xisp.core.*
import ystyle::xisp.types.*
import std.collection.ArrayList
import std.unittest.*
import std.unittest.testmacro.*

@Test
class CompleterTest {
    @TestCase
    func testCompleteWithPrefix() {
        let evaluator = Evaluator(Environment())
        let completer = Completer(evaluator)
        let result = completer.complete("de")
        @Assert(result.size == 1)
        @Assert(result[0], "define")
    }

    @TestCase
    func testCompleteCaseInsensitive() {
        let evaluator = Evaluator(Environment())
        let completer = Completer(evaluator)
        let result = completer.complete("DE")
        @Assert(result.size == 1)
        @Assert(result[0], "define")
    }

    @TestCase
    func testCompleteEmpty() {
        let evaluator = Evaluator(Environment())
        let completer = Completer(evaluator)
        let result = completer.complete("")
        @Assert(result.size > 0)
    }

    @TestCase
    func testCompleteNoMatch() {
        let evaluator = Evaluator(Environment())
        let completer = Completer(evaluator)
        let result = completer.complete("xyzzy")
        @Assert(result.size == 0)
    }

    @TestCase
    func testCommonPrefix() {
        let evaluator = Evaluator(Environment())
        let completer = Completer(evaluator)
        let candidates = ArrayList<String>()
        candidates.add("define")
        candidates.add("defun")
        candidates.add("defmacro")
        let prefix = completer.getCommonPrefix(candidates)
        @Assert(prefix, "def")
    }

    @TestCase
    func testCycleIndex() {
        let evaluator = Evaluator(Environment())
        let completer = Completer(evaluator)
        @Assert(completer.getCycleIndex(), 0)
        completer.setCycleIndex(3)
        @Assert(completer.getCycleIndex(), 3)
    }

    @TestCase
    func testCompleteMultiple() {
        let evaluator = Evaluator(Environment())
        let completer = Completer(evaluator)
        let result = completer.complete("l")
        @Assert(result.size >= 3)
        @Assert(result.contains("lambda"))
        @Assert(result.contains("let"))
        @Assert(result.contains("list"))
    }

    @TestCase
    func testCompleteSymbolFromEnv() {
        let env = Environment()
        env.define("myvar", LispValue.Int(42))
        let evaluator = Evaluator(env)
        let completer = Completer(evaluator)
        let result = completer.complete("myv")
        @Assert(result.size == 1)
        @Assert(result[0], "myvar")
    }
}
