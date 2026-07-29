package ystyle::xisp.repl

import ystyle::xisp.types.*
import std.collection.HashMap

public class Highlighter {
    public static let RED: String = "\u{001b}[31m"
    public static let GREEN: String = "\u{001b}[32m"
    public static let YELLOW: String = "\u{001b}[33m"
    public static let BLUE: String = "\u{001b}[34m"
    public static let MAGENTA: String = "\u{001b}[35m"
    public static let CYAN: String = "\u{001b}[36m"
    public static let GRAY: String = "\u{001b}[90m"
    public static let BOLD: String = "\u{001b}[1m"
    public static let RESET: String = "\u{001b}[0m"

    public func highlight(value: LispValue): String {
        highlightValue(value)
    }

    public func highlightError(err: XispError): String {
        let kindStr = match (err.kind) {
            case UnboundVariable => "UnboundVariable"
            case UndefinedFunction => "UndefinedFunction"
            case TypeError => "TypeError"
            case ArityMismatch => "ArityMismatch"
            case SyntaxError => "SyntaxError"
            case RuntimeError => "RuntimeError"
            case IOError => "IOError"
            case ModuleError => "ModuleError"
        }
        var result = "\u{001b}[1;31m[${kindStr}]\u{001b}[0m ${err.message}"
        if (!err.stack.isEmpty()) {
            for (i in 0..err.stack.size) {
                let frame = err.stack[err.stack.size - 1 - i]
                result = "${result}\n${GRAY}  ${i + 1}. ${frame}${RESET}"
            }
        }
        result
    }

    private func highlightValue(value: LispValue): String {
        match (value) {
            case Nil => "${GRAY}nil${RESET}"
            case Boolean(b) => "${CYAN}${if (b) { "true" } else { "false" }}${RESET}"
            case Int(i) => "${YELLOW}${i.toString()}${RESET}"
            case Float(f) => "${YELLOW}${f.toString()}${RESET}"
            case Str(s) => "${GREEN}\"${s}\"${RESET}"
            case Symbol(s) => "${BLUE}${s}${RESET}"
            case Cons(cell) => highlightCons(cell)
            case HashMap(map) => highlightHashMap(map)
            case Error(err) => highlightError(err)
            case _ => value.toString()
        }
    }

    private func highlightCons(cell: ConsCell): String {
        if (cell.isNil()) {
            "()"
        } else if (cell.isProperList()) {
            let sb = StringBuilder()
            sb.append("(")
            sb.append(highlightValue(cell.car))
            var current = cell.cdr
            while (true) {
                match (current) {
                    case Nil => break
                    case Cons(nextCell) =>
                        sb.append(" ")
                        sb.append(highlightValue(nextCell.car))
                        current = nextCell.cdr
                    case _ => break
                }
            }
            sb.append(")")
            sb.toString()
        } else {
            "(${highlightValue(cell.car)} . ${highlightValue(cell.cdr)})"
        }
    }

    private func highlightHashMap(map: HashMap<String, LispValue>): String {
        if (map.size == 0) {
            "{}"
        } else {
            let sb = StringBuilder()
            sb.append("{")
            var first = true
            for ((key, value) in map) {
                if (!first) {
                    sb.append(" ")
                }
                sb.append("${key} ${highlightValue(value)}")
                first = false
            }
            sb.append("}")
            sb.toString()
        }
    }

    public func highlightInput(line: String): String {
        let chars = line.toRuneArray()
        let sb = StringBuilder()
        var i = 0
        let n = chars.size
        let keywords = ["define", "lambda", "let", "if", "cond", "match",
            "import", "export", "begin", "quote", "when", "unless",
            "and", "or", "not", "nil", "true", "false", "car", "cdr",
            "cons", "list", "map", "filter", "reduce", "println",
            "print", "first", "rest", "prepend", "length", "append"]

        while (i < n) {
            let c = chars[i]
            if (c == r'"') {
                var j = i + 1
                while (j < n) {
                    if (chars[j] == r'"') { j = j + 1; break }
                    j = j + 1
                }
                sb.append(GREEN)
                for (k in i..j) { sb.append(chars[k]) }
                sb.append(RESET)
                i = j
            } else if (c == r';') {
                sb.append(GRAY)
                for (k in i..n) { sb.append(chars[k]) }
                sb.append(RESET)
                i = n
            } else if (c >= r'0' && c <= r'9') {
                var j = i + 1
                while (j < n && chars[j] >= r'0' && chars[j] <= r'9') { j = j + 1 }
                sb.append(YELLOW)
                for (k in i..j) { sb.append(chars[k]) }
                sb.append(RESET)
                i = j
            } else if ((c >= r'a' && c <= r'z') || (c >= r'A' && c <= r'Z') || c == r'_') {
                var j = i + 1
                while (j < n && ((chars[j] >= r'a' && chars[j] <= r'z') || (chars[j] >= r'A' && chars[j] <= r'Z') || (chars[j] >= r'0' && chars[j] <= r'9') || chars[j] == r'_' || chars[j] == r'-' || chars[j] == r'?')) { j = j + 1 }
                let word = String(chars[i..j])
                if (keywords.contains(word)) {
                    sb.append(BLUE)
                    for (k in i..j) { sb.append(chars[k]) }
                    sb.append(RESET)
                } else {
                    for (k in i..j) { sb.append(chars[k]) }
                }
                i = j
            } else {
                sb.append(c)
                i = i + 1
            }
        }
        sb.toString()
    }
}
