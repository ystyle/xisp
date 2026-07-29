package ystyle::xisp.repl

import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*
import ystyle::xisp.core.module.*
import ystyle::xisp.types.*
import std.collection.ArrayList
import std.sort.*

public class Completer {
    private var evaluator: Evaluator
    private var cycleIndex: Int64 = 0

    public init(evaluator: Evaluator) {
        this.evaluator = evaluator
    }

    public func complete(prefix: String): ArrayList<String> {
        let candidates = ArrayList<String>()

        for (key in this.evaluator.env.getKeys()) {
            candidates.add(key)
        }

        for (kw in this.getKeywords()) {
            if (!candidates.contains(kw)) {
                candidates.add(kw)
            }
        }

        if (prefix.isEmpty()) {
            sort(candidates)
            return candidates
        }

        let lowerPrefix = prefix.toAsciiLower()
        let matched = ArrayList<String>()
        for (c in candidates) {
            if (c.toAsciiLower().startsWith(lowerPrefix)) {
                matched.add(c)
            }
        }
        sort(matched)
        matched
    }

    private func getKeywords(): ArrayList<String> {
        let kws = ArrayList<String>()
        kws.add("define")
        kws.add("lambda")
        kws.add("let")
        kws.add("if")
        kws.add("cond")
        kws.add("match")
        kws.add("import")
        kws.add("export")
        kws.add("begin")
        kws.add("quote")
        kws.add("macro")
        kws.add("and")
        kws.add("or")
        kws.add("not")
        kws.add("car")
        kws.add("cdr")
        kws.add("cons")
        kws.add("list")
        kws.add("map")
        kws.add("filter")
        kws.add("reduce")
        kws.add("println")
        kws.add("print")
        kws.add("first")
        kws.add("rest")
        kws.add("prepend")
        kws
    }

    public func getCommonPrefix(candidates: ArrayList<String>): String {
        if (candidates.isEmpty()) {
            return ""
        }
        if (candidates.size == 1) {
            return candidates[0]
        }

        let first = candidates[0]
        var prefixLen = first.size
        for (i in 1..candidates.size) {
            let s = candidates[i]
            var j = 0
            let limit = if (prefixLen < s.size) { prefixLen } else { s.size }
            while (j < limit) {
                if (first[j] != s[j]) {
                    break
                }
                j = j + 1
            }
            if (j < prefixLen) {
                prefixLen = j
            }
        }
        first[0..prefixLen]
    }

    public func setCycleIndex(idx: Int64): Unit {
        this.cycleIndex = idx
    }

    public func getCycleIndex(): Int64 {
        this.cycleIndex
    }
}
