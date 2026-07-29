package ystyle::xisp.repl

import std.collection.ArrayList
import std.unittest.*
import std.unittest.testmacro.*
import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.terminal.*

class TestHelper {
    static func keyChar(c: Rune): KeyEvent {
        KeyEvent(KeyCode.Char(c), KeyModifiers())
    }

    static func keyCtrl(c: Rune): KeyEvent {
        KeyEvent(KeyCode.Char(c), KeyModifiers(KeyModifiers.CONTROL))
    }

    static func keyAltEnter(): KeyEvent {
        KeyEvent(KeyCode.Enter, KeyModifiers(KeyModifiers.ALT))
    }

    static func keyEnter(): KeyEvent {
        KeyEvent(KeyCode.Enter, KeyModifiers())
    }

    static func keyLeft(): KeyEvent { KeyEvent(KeyCode.Left, KeyModifiers()) }
    static func keyRight(): KeyEvent { KeyEvent(KeyCode.Right, KeyModifiers()) }
    static func keyUp(): KeyEvent { KeyEvent(KeyCode.Up, KeyModifiers()) }
    static func keyDown(): KeyEvent { KeyEvent(KeyCode.Down, KeyModifiers()) }
    static func keyHome(): KeyEvent { KeyEvent(KeyCode.Home, KeyModifiers()) }
    static func keyEnd(): KeyEvent { KeyEvent(KeyCode.End, KeyModifiers()) }
    static func keyBackspace(): KeyEvent { KeyEvent(KeyCode.Backspace, KeyModifiers()) }
    static func keyDelete(): KeyEvent { KeyEvent(KeyCode.Delete, KeyModifiers()) }
    static func keyTab(): KeyEvent { KeyEvent(KeyCode.Tab, KeyModifiers()) }
    static func keyEsc(): KeyEvent { KeyEvent(KeyCode.Esc, KeyModifiers()) }

    static func makeEditor(): LineEditor {
        let history = HistoryManager(maxEntries: 100)
        let evaluator = Evaluator(Environment())
        let completer = Completer(evaluator)
        LineEditor(history, completer)
    }

    static func isAction(result: EditorAction, expected: EditorAction): Bool {
        match (result) {
            case EditorAction.Continue =>
                match (expected) { case EditorAction.Continue => true; case _ => false }
            case EditorAction.Exit =>
                match (expected) { case EditorAction.Exit => true; case _ => false }
            case EditorAction.Cancel =>
                match (expected) { case EditorAction.Cancel => true; case _ => false }
            case EditorAction.Submit(_) =>
                match (expected) { case EditorAction.Submit(_) => true; case _ => false }
            case EditorAction.Complete(_) =>
                match (expected) { case EditorAction.Complete(_) => true; case _ => false }
        }
    }
}

@Test
public class LineEditorTest {
    @TestCase
    public func testInsertChar() {
        let ed = TestHelper.makeEditor()
        @Assert(TestHelper.isAction(ed.processKey(TestHelper.keyChar(r'a')), EditorAction.Continue), true)
        @Assert(TestHelper.isAction(ed.processKey(TestHelper.keyChar(r'b')), EditorAction.Continue), true)
        @Assert(TestHelper.isAction(ed.processKey(TestHelper.keyChar(r'c')), EditorAction.Continue), true)
        @Assert(ed.getBuffer(), "abc")
        @Assert(ed.getCursorCol(), 3)
    }

    @TestCase
    public func testBackspace() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'x'))
        ed.processKey(TestHelper.keyChar(r'y'))
        ed.processKey(TestHelper.keyChar(r'z'))
        ed.processKey(TestHelper.keyBackspace())
        @Assert(ed.getBuffer(), "xy")
        @Assert(ed.getCursorCol(), 2)
    }

    @TestCase
    public func testBackspaceAtStart() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'a'))
        ed.processKey(TestHelper.keyHome())
        ed.processKey(TestHelper.keyBackspace())
        @Assert(ed.getBuffer(), "a")
        @Assert(ed.getCursorCol(), 0)
    }

    @TestCase
    public func testDelete() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'a'))
        ed.processKey(TestHelper.keyChar(r'b'))
        ed.processKey(TestHelper.keyChar(r'c'))
        ed.processKey(TestHelper.keyHome())
        ed.processKey(TestHelper.keyDelete())
        @Assert(ed.getBuffer(), "bc")
        @Assert(ed.getCursorCol(), 0)
    }

    @TestCase
    public func testCursorMovement() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'a'))
        ed.processKey(TestHelper.keyChar(r'b'))
        ed.processKey(TestHelper.keyChar(r'c'))
        ed.processKey(TestHelper.keyLeft())
        @Assert(ed.getCursorCol(), 2)
        ed.processKey(TestHelper.keyLeft())
        @Assert(ed.getCursorCol(), 1)
        ed.processKey(TestHelper.keyRight())
        @Assert(ed.getCursorCol(), 2)
    }

    @TestCase
    public func testHomeAndEnd() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'a'))
        ed.processKey(TestHelper.keyChar(r'b'))
        ed.processKey(TestHelper.keyHome())
        @Assert(ed.getCursorCol(), 0)
        ed.processKey(TestHelper.keyEnd())
        @Assert(ed.getCursorCol(), 2)
    }

    @TestCase
    public func testCtrlAandCtrlE() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'a'))
        ed.processKey(TestHelper.keyChar(r'b'))
        ed.processKey(TestHelper.keyCtrl(r'a'))
        @Assert(ed.getCursorCol(), 0)
        ed.processKey(TestHelper.keyCtrl(r'e'))
        @Assert(ed.getCursorCol(), 2)
    }

    @TestCase
    public func testCtrlKDeleteToEnd() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'a'))
        ed.processKey(TestHelper.keyChar(r'b'))
        ed.processKey(TestHelper.keyChar(r'c'))
        ed.processKey(TestHelper.keyHome())
        ed.processKey(TestHelper.keyRight())
        ed.processKey(TestHelper.keyCtrl(r'k'))
        @Assert(ed.getBuffer(), "a")
        @Assert(ed.getCursorCol(), 1)
    }

    @TestCase
    public func testCtrlUDeleteLine() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'a'))
        ed.processKey(TestHelper.keyChar(r'b'))
        ed.processKey(TestHelper.keyCtrl(r'u'))
        @Assert(ed.getBuffer(), "")
        @Assert(ed.getCursorCol(), 0)
    }

    @TestCase
    public func testCtrlEnterSubmit() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'x'))
        let result = ed.processKey(TestHelper.keyCtrl(r'j'))
        @Assert(TestHelper.isAction(result, EditorAction.Submit("")), true)
    }

    @TestCase
    public func testAltEnterSubmit() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'y'))
        let result = ed.processKey(TestHelper.keyAltEnter())
        @Assert(TestHelper.isAction(result, EditorAction.Submit("")), true)
    }

    @TestCase
    public func testCtrlC() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'a'))
        let result = ed.processKey(TestHelper.keyCtrl(r'c'))
        @Assert(TestHelper.isAction(result, EditorAction.Cancel), true)
    }

    @TestCase
    public func testCtrlDEmptyBufferExit() {
        let ed = TestHelper.makeEditor()
        let result = ed.processKey(TestHelper.keyCtrl(r'd'))
        @Assert(TestHelper.isAction(result, EditorAction.Exit), true)
    }

    @TestCase
    public func testInsertMiddle() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'a'))
        ed.processKey(TestHelper.keyChar(r'c'))
        ed.processKey(TestHelper.keyLeft())
        ed.processKey(TestHelper.keyChar(r'b'))
        @Assert(ed.getBuffer(), "abc")
        @Assert(ed.getCursorCol(), 2)
    }

    @TestCase
    public func testInsertMultipleLines() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'('))
        ed.processKey(TestHelper.keyEnter())
        ed.processKey(TestHelper.keyChar(r'b'))
        @Assert(ed.getBuffer(), "(\nb")
        @Assert(ed.getCursorRow(), 1)
        @Assert(ed.getCursorCol(), 1)
    }

    @TestCase
    public func testMoveBetweenLines() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'('))
        ed.processKey(TestHelper.keyEnter())
        ed.processKey(TestHelper.keyChar(r'b'))
        ed.processKey(TestHelper.keyChar(r'c'))
        @Assert(ed.getBuffer(), "(\nbc")
        ed.processKey(TestHelper.keyUp())
        @Assert(ed.getCursorRow(), 0)
        @Assert(ed.getCursorCol(), 1)
        ed.processKey(TestHelper.keyDown())
        @Assert(ed.getCursorRow(), 1)
        @Assert(ed.getCursorCol(), 1)
    }

    @TestCase
    public func testBackspaceJoinLines() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'('))
        ed.processKey(TestHelper.keyEnter())
        ed.processKey(TestHelper.keyChar(r'b'))
        ed.processKey(TestHelper.keyHome())
        ed.processKey(TestHelper.keyBackspace())
        @Assert(ed.getBuffer(), "(b")
        @Assert(ed.getCursorRow(), 0)
        @Assert(ed.getCursorCol(), 1)
    }

    @TestCase
    public func testDeleteJoinLines() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'('))
        ed.processKey(TestHelper.keyEnter())
        ed.processKey(TestHelper.keyChar(r'b'))
        ed.processKey(TestHelper.keyHome())
        ed.processKey(TestHelper.keyUp())
        ed.processKey(TestHelper.keyEnd())
        ed.processKey(TestHelper.keyDelete())
        @Assert(ed.getBuffer(), "(b")
    }

    @TestCase
    public func testEscOnNonEmptyBuffer() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'a'))
        let result = ed.processKey(TestHelper.keyEsc())
        @Assert(TestHelper.isAction(result, EditorAction.Continue), true)
        @Assert(ed.getBuffer(), "")
    }

    @TestCase
    public func testEscOnEmptyBuffer() {
        let ed = TestHelper.makeEditor()
        let result = ed.processKey(TestHelper.keyEsc())
        @Assert(TestHelper.isAction(result, EditorAction.Exit), true)
    }

    @TestCase
    public func testInsertCharInMiddleOfLine() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'a'))
        ed.processKey(TestHelper.keyChar(r'c'))
        ed.processKey(TestHelper.keyLeft())
        ed.processKey(TestHelper.keyChar(r'b'))
        @Assert(ed.getLine(0), "abc")
        @Assert(ed.getCursorCol(), 2)
    }

    @TestCase
    public func testParenBalance() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'('))
        @Assert(ed.isBalanced(), false)
        ed.processKey(TestHelper.keyChar(r')'))
        @Assert(ed.isBalanced(), true)
    }

    @TestCase
    public func testBracketBalance() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'['))
        @Assert(ed.isBalanced(), false)
        ed.processKey(TestHelper.keyChar(r']'))
        @Assert(ed.isBalanced(), true)
    }

    @TestCase
    public func testNestedParens() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'('))
        ed.processKey(TestHelper.keyChar(r'('))
        ed.processKey(TestHelper.keyChar(r')'))
        @Assert(ed.isBalanced(), false)
        ed.processKey(TestHelper.keyChar(r')'))
        @Assert(ed.isBalanced(), true)
    }

    @TestCase
    public func testSubmitWithHistory() {
        let ed = TestHelper.makeEditor()
        for (c in "(+ 1 2)".toRuneArray()) { ed.processKey(TestHelper.keyChar(c)) }
        ed.processKey(TestHelper.keyCtrl(r'j'))
        @Assert(ed.getBuffer(), "")
        ed.processKey(TestHelper.keyUp())
        @Assert(ed.getBuffer(), "(+ 1 2)")
    }

    @TestCase
    public func testHistoryNavigation() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'a'))
        ed.processKey(TestHelper.keyCtrl(r'j'))
        ed.processKey(TestHelper.keyChar(r'b'))
        ed.processKey(TestHelper.keyCtrl(r'j'))
        ed.processKey(TestHelper.keyUp())
        @Assert(ed.getBuffer(), "b")
        ed.processKey(TestHelper.keyUp())
        @Assert(ed.getBuffer(), "a")
        ed.processKey(TestHelper.keyDown())
        @Assert(ed.getBuffer(), "b")
    }

    @TestCase
    public func testTabCompletion() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'd'))
        ed.processKey(TestHelper.keyChar(r'e'))
        ed.processKey(TestHelper.keyTab())
        @Assert(ed.getBuffer(), "define")
    }

    @TestCase
    public func testLineCountSingleLine() {
        let ed = TestHelper.makeEditor()
        @Assert(ed.getLineCount(), 1)
    }

    @TestCase
    public func testLineCountMultiLine() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'('))
        ed.processKey(TestHelper.keyEnter())
        @Assert(ed.getLineCount(), 2)
    }

    @TestCase
    public func testDisplayLines() {
        let ed = TestHelper.makeEditor()
        ed.processKey(TestHelper.keyChar(r'('))
        ed.processKey(TestHelper.keyEnter())
        ed.processKey(TestHelper.keyChar(r'b'))
        let lines = ed.getDisplayLines()
        @Assert(lines.size, 2)
    }
}
