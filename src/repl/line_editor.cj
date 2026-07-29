package ystyle::xisp.repl

import std.collection.ArrayList
import ystyle::xisp.terminal.*

public enum EditorAction {
    | Submit(String)
    | Exit
    | Continue
    | Cancel
    | Complete(ArrayList<String>)
}

public class LineEditor {
    private var buffer: ArrayList<String> = ArrayList()
    private var cursorRow: Int64 = 0
    private var cursorCol: Int64 = 0
    private var history: HistoryManager
    private var completer: Completer
    private var completionCandidates: ArrayList<String> = ArrayList()
    private var completionCycle: Int64 = 0
    private var parenCount: Int64 = 0
    private var inString: Bool = false
    private let prompt: String = "xisp> "
    private let contPrompt: String = "    ... "

    public init(history: HistoryManager, completer: Completer) {
        this.history = history
        this.completer = completer
        this.buffer.add("")
    }

    public func processKey(event: KeyEvent): EditorAction {
        match (event.code) {
            case Char(c) =>
                if (event.modifiers.contains(KeyModifiers.CONTROL)) {
                    processControlKey(c)
                } else if (event.modifiers.contains(KeyModifiers.ALT)) {
                    Continue
                } else {
                    insertChar(c)
                    updateParenCount(c)
                    Continue
                }
            case Enter =>
                if (event.modifiers.contains(KeyModifiers.CONTROL) || event.modifiers.contains(KeyModifiers.ALT)) {
                    submitBuffer()
                } else {
                    this.handleEnter()
                }
            case Backspace =>
                deleteBeforeCursor()
                Continue
            case Delete =>
                deleteAtCursor()
                Continue
            case Left => cursorLeft(); Continue
            case Right => cursorRight(); Continue
            case Up => cursorUp(); Continue
            case Down => cursorDown(); Continue
            case Home => cursorHome(); Continue
            case End => cursorEnd(); Continue
            case Tab => this.triggerCompletion()
            case Esc => if (isEmptyBuffer()) { Exit } else { clear(); Continue }
            case _ => Continue
        }
    }

    private func processControlKey(c: Rune): EditorAction {
        let ch = UInt32(c)
        let ctrlCode = if (ch >= 0x61 && ch <= 0x7A) { ch - 96 }
            else if (ch >= 0x41 && ch <= 0x5A) { ch - 64 }
            else { ch }
        if (ctrlCode == 13 || ctrlCode == 10) {
            return submitBuffer()
        } else if (ctrlCode == 3) {
            return Cancel
        } else if (ctrlCode == 4) {
            if (isEmptyBuffer()) { return Exit }
            return Continue
        } else if (ctrlCode == 1) {
            cursorHome(); Continue
        } else if (ctrlCode == 5) {
            cursorEnd(); Continue
        } else if (ctrlCode == 11) {
            deleteToEndOfLine(); Continue
        } else if (ctrlCode == 21) {
            deleteLine(); Continue
        } else if (ctrlCode == 12) {
            Continue
        } else if (ctrlCode == 16) {
            historyBack(); Continue
        } else if (ctrlCode == 14) {
            historyForward(); Continue
        } else {
            Continue
        }
    }

    private func submitBuffer(): EditorAction {
        let text = getBuffer()
        if (text.trimAscii().size > 0) {
            history.add(text)
        }
        clear()
        EditorAction.Submit(text)
    }

    public func handleEnter(): EditorAction {
        if (this.parenCount <= 0) {
            let buf = getBuffer().trimAscii()
            if (buf.size > 0 && buf.toRuneArray()[0] == r',') {
                return submitBuffer()
            }
            if (buf.size > 0) {
                return submitBuffer()
            }
        }
        insertNewLine()
        EditorAction.Continue
    }

    private func insertChar(c: Rune): Unit {
        let line = buffer[cursorRow].toRuneArray()
        let newLine = ArrayList<Rune>()
        for (i in 0..cursorCol) { newLine.add(line[i]) }
        newLine.add(c)
        for (i in cursorCol..line.size) { newLine.add(line[i]) }
        buffer[cursorRow] = String(newLine)
        cursorCol = cursorCol + 1
    }

    private func insertNewLine(): Unit {
        let line = buffer[cursorRow]
        let chars = line.toRuneArray()
        let before = ArrayList<Rune>()
        let after = ArrayList<Rune>()
        for (i in 0..cursorCol) { before.add(chars[i]) }
        for (i in cursorCol..chars.size) { after.add(chars[i]) }
        buffer[cursorRow] = String(before)
        let newBuffer = ArrayList<String>()
        for (i in 0..(cursorRow + 1)) { newBuffer.add(buffer[i]) }
        newBuffer.add(String(after))
        for (i in (cursorRow + 1)..buffer.size) { newBuffer.add(buffer[i]) }
        buffer = newBuffer
        cursorRow = cursorRow + 1
        cursorCol = 0
    }

    private func deleteBeforeCursor(): Unit {
        if (cursorCol > 0) {
            let line = buffer[cursorRow].toRuneArray()
            let newLine = ArrayList<Rune>()
            for (i in 0..(cursorCol - 1)) { newLine.add(line[i]) }
            for (i in cursorCol..line.size) { newLine.add(line[i]) }
            buffer[cursorRow] = String(newLine)
            cursorCol = cursorCol - 1
        } else if (cursorRow > 0) {
            let prevLineLen = buffer[cursorRow - 1].toRuneArray().size
            let currentLine = buffer[cursorRow]
            buffer[cursorRow - 1] = buffer[cursorRow - 1] + currentLine
            buffer.remove(at: cursorRow)
            cursorRow = cursorRow - 1
            cursorCol = prevLineLen
        }
    }

    private func deleteAtCursor(): Unit {
        let line = buffer[cursorRow].toRuneArray()
        if (cursorCol < line.size) {
            let newLine = ArrayList<Rune>()
            for (i in 0..cursorCol) { newLine.add(line[i]) }
            for (i in (cursorCol + 1)..line.size) { newLine.add(line[i]) }
            buffer[cursorRow] = String(newLine)
        } else if (cursorRow < buffer.size - 1) {
            let nextLine = buffer[cursorRow + 1]
            buffer[cursorRow] = buffer[cursorRow] + nextLine
            buffer.remove(at: cursorRow + 1)
        }
    }

    private func cursorLeft(): Unit {
        if (cursorCol > 0) {
            cursorCol = cursorCol - 1
        } else if (cursorRow > 0) {
            cursorRow = cursorRow - 1
            cursorCol = buffer[cursorRow].toRuneArray().size
        }
    }

    private func cursorRight(): Unit {
        let line = buffer[cursorRow].toRuneArray()
        if (cursorCol < line.size) {
            cursorCol = cursorCol + 1
        } else if (cursorRow < buffer.size - 1) {
            cursorRow = cursorRow + 1
            cursorCol = 0
        }
    }

    private func cursorUp(): Unit {
        if (cursorRow > 0) {
            cursorRow = cursorRow - 1
            let lineLen = buffer[cursorRow].toRuneArray().size
            if (cursorCol > lineLen) { cursorCol = lineLen }
        } else {
            historyBack()
        }
    }

    private func cursorDown(): Unit {
        if (cursorRow < buffer.size - 1) {
            cursorRow = cursorRow + 1
            let lineLen = buffer[cursorRow].toRuneArray().size
            if (cursorCol > lineLen) { cursorCol = lineLen }
        } else {
            historyForward()
        }
    }

    private func cursorHome(): Unit { cursorCol = 0 }

    private func cursorEnd(): Unit {
        cursorCol = buffer[cursorRow].toRuneArray().size
    }

    private func deleteToEndOfLine(): Unit {
        let line = buffer[cursorRow].toRuneArray()
        if (cursorCol < line.size) {
            let newLine = ArrayList<Rune>()
            for (i in 0..cursorCol) { newLine.add(line[i]) }
            buffer[cursorRow] = String(newLine)
        }
    }

    private func deleteLine(): Unit {
        buffer[cursorRow] = ""
        cursorCol = 0
    }

    private func historyBack(): Unit {
        match (history.navigate(-1)) {
            case Some(entry) =>
                setBufferFromHistory(entry)
            case None => ()
        }
    }

    private func historyForward(): Unit {
        match (history.navigate(1)) {
            case Some(entry) =>
                setBufferFromHistory(entry)
            case None => ()
        }
    }

    private func setBufferFromHistory(entry: String): Unit {
        let lines = entry.split("\n")
        buffer = ArrayList<String>()
        for (line in lines) { buffer.add(line) }
        cursorRow = buffer.size - 1
        cursorCol = buffer[cursorRow].toRuneArray().size
    }

    private func triggerCompletion(): EditorAction {
        let currentLine = buffer[cursorRow]
        let prefix = getCurrentPrefix(currentLine)
        let candidates = completer.complete(prefix)
        if (candidates.size == 1) {
            let remaining = candidates[0].toRuneArray()
            let remSize = remaining.size
            let resultRunes = ArrayList<Rune>()
            for (i in prefix.size..remSize) { resultRunes.add(remaining[i]) }
            for (r in resultRunes) { insertChar(r) }
            Continue
        } else if (candidates.size > 1) {
            let common = completer.getCommonPrefix(candidates)
            if (common.size > prefix.size) {
                let remaining = common.toRuneArray()
                let remSize = remaining.size
                let resultRunes = ArrayList<Rune>()
                for (i in prefix.size..remSize) { resultRunes.add(remaining[i]) }
                for (r in resultRunes) { insertChar(r) }
                Continue
            } else {
                EditorAction.Complete(candidates)
            }
        } else {
            Continue
        }
    }

    private func getCurrentPrefix(line: String): String {
        let chars = line.toRuneArray()
        var end = cursorCol
        while (end > 0) {
            let c = chars[end - 1]
            if (c == r' ' || c == r'(' || c == r')' || c == r'\t' || c == r'\n') {
                break
            }
            end = end - 1
        }
        var result = ArrayList<Rune>()
        for (i in end..cursorCol) { result.add(chars[i]) }
        String(result)
    }

    public func getCompletionCandidates(): ArrayList<String> { completionCandidates }

    public func clearCompletionCandidates(): Unit {
        completionCandidates = ArrayList()
    }

    public func isEmptyBuffer(): Bool {
        for (line in buffer) {
            if (line.trimAscii().size > 0) { return false }
        }
        true
    }

    private func updateParenCount(c: Rune): Unit {
        if (c == r'"') { inString = !inString }
        if (!inString) {
            if (c == r'(' || c == r'[') { parenCount = parenCount + 1 }
            if (c == r')' || c == r']') { parenCount = parenCount - 1 }
        }
    }

    public func isBalanced(): Bool { parenCount <= 0 }

    public func getBuffer(): String {
        var result = ""
        for (i in 0..buffer.size) {
            if (i > 0) { result = result + "\n" }
            result = result + buffer[i]
        }
        result
    }

    public func getCursorRow(): Int64 { cursorRow }
    public func getCursorCol(): Int64 { cursorCol }
    public func getLineCount(): Int64 { buffer.size }
    public func getLine(index: Int64): String { buffer[index] }
    public func getPrompt(): String {
        if (cursorRow == 0 && buffer[0].size == 0) { prompt }
        else if (cursorRow == 0) { prompt }
        else { contPrompt }
    }

    public func getDisplayLines(): ArrayList<String> {
        let result = ArrayList<String>()
        for (i in 0..buffer.size) {
            result.add(buffer[i])
        }
        result
    }

    public func clear(): Unit {
        buffer = ArrayList<String>()
        buffer.add("")
        cursorRow = 0
        cursorCol = 0
        parenCount = 0
        inString = false
        completionCandidates = ArrayList()
    }

    public func reset(entry: String): Unit {
        clear()
        if (entry.size > 0) {
            let lines = entry.split("\n")
            buffer = ArrayList<String>()
            for (line in lines) { buffer.add(line) }
        }
    }
}
