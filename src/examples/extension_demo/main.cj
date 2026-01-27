package ystyle::xisp.examples.extension_demo

import ystyle::xisp.bridge.*
import ystyle::xisp.core.*
import ystyle::xisp.types.*
import std.collection.ArrayList
import std.collection.HashMap
import std.collection.HashSet

/**
 * 扩展功能演示程序
 * 展示双向类型转换：仓颉 ↔ Lisp
 */
main(): Int64 {
    println("╔═══════════════════════════════════════════════════════════╗")
    println("║        仓颉扩展功能演示 - 双向类型转换                    ║")
    println("║  LispConvertible: 仓颉 → Lisp                            ║")
    println("║  LispDeserializable: Lisp → 仓颉                         ║")
    println("╚═══════════════════════════════════════════════════════════╝")

    // ========== 基本类型：仓颉 → Lisp ==========
    println("")
    println("【1】基本类型转换 (仓颉 → Lisp):")

    let num: Int64 = 42
    print("  Int64 42.toLisp() = ")
    println(num.toLisp().toString())

    let str: String = "Hello"
    print("  String \"Hello\".toLisp() = ")
    println(str.toLisp().toString())

    let bool: Bool = true
    print("  Bool true.toLisp() = ")
    println(bool.toLisp().toString())

    // ========== 基本类型：Lisp → 仓颉 ==========
    println("")
    println("【2】基本类型转换 (Lisp → 仓颉):")

    let lispInt = LispValue.Int(100)
    if (let Some(i) <- lispInt.asInt()) {
        println("  LispValue.Int(100).asInt() = ${i}")
    }

    let lispFloat = LispValue.Float(3.14)
    if (let Some(f) <- lispFloat.asFloat()) {
        println("  LispValue.Float(3.14).asFloat() = ${f}")
    }

    let lispStr = LispValue.Str("World")
    if (let Some(s) <- lispStr.asString()) {
        println("  LispValue.Str(\"World\").asString() = \"${s}\"")
    }

    // ========== ArrayList：双向转换 ==========
    println("")
    println("【3】ArrayList 双向转换:")

    // 仓颉 → Lisp
    let numbers = ArrayList<Int64>()
    numbers.add(1)
    numbers.add(2)
    numbers.add(3)

    let lispList = numbers.toLisp()
    print("  ArrayList<Int64>[1,2,3].toLisp() = ")
    println(lispList.toString())

    // Lisp → 仓颉
    if (let Some(convertedList) <- ArrayList<Int64>.fromLisp(lispList)) {
        print("  Lisp → ArrayList: [")
        for (i in convertedList) {
            print(" ${i}")
        }
        println(" ]")
    }

    // ========== HashMap：双向转换 ==========
    println("")
    println("【4】HashMap 双向转换:")

    // 仓颉 → Lisp
    let map = HashMap<String, Int64>()
    map["apple"] = 10
    map["banana"] = 20
    map["cherry"] = 30

    let lispHashMap = map.toLisp()
    print("  HashMap.toLisp() = ")
    println(lispHashMap.toString())

    // Lisp → 仓颉
    if (let Some(convertedMap) <- HashMap<String, Int64>.fromLisp(lispHashMap)) {
        println("  Lisp → HashMap:")
        for ((key, value) in convertedMap) {
            println("    \"${key}\" => ${value}")
        }
    }

    // ========== 类型自动转换 ==========
    println("")
    println("【5】类型自动转换:")

    // Float → Int (自动截断)
    let lispFloat2 = LispValue.Float(3.99)
    if (let Some(i) <- lispFloat2.asInt()) {
        println("  Float(3.99).asInt() = ${i} (自动截断)")
    }

    // Int → Float (自动提升)
    let lispInt2 = LispValue.Int(42)
    if (let Some(f) <- lispInt2.asFloat()) {
        println("  Int(42).asFloat() = ${f} (自动提升)")
    }

    // Symbol → String
    let lispSymbol = Symbol("test-symbol")
    if (let Some(s) <- lispSymbol.asString()) {
        println("  Symbol(\"test-symbol\").asString() = \"${s}\"")
    }

    // ========== 类型不匹配处理 ==========
    println("")
    println("【6】类型不匹配处理:")

    let lispStringValue = LispValue.Str("hello")

    if (let Some(i) <- lispStringValue.asInt()) {
        println("  Str.asInt() = ${i}")
    } else {
        println("  Str.asInt() = None (类型不匹配)")
    }

    // ========== HashSet 转换 ==========
    println("")
    println("【7】HashSet 转换:")

    let set = HashSet<Int64>()
    set.add(10)
    set.add(20)
    set.add(30)

    let lispSet = set.toLisp()
    print("  HashSet.toLisp() = ")
    println(lispSet.toString())

    // ========== Option 转换 ==========
    println("")
    println("【8】Option 转换:")

    let someValue: Option<Int64> = Some(42)
    let noneValue: Option<Int64> = None

    print("  Some(42).toLisp() = ")
    println(someValue.toLisp().toString())
    print("  None.toLisp() = ")
    println(noneValue.toLisp().toString())

    println("")
    println("╔═══════════════════════════════════════════════════════════╗")
    println("║              扩展功能演示完成！                        ║")
    println("║                                                        ║")
    println("║  双向转换能力：                                         ║")
    println("║  ✓ LispConvertible: 仓颉类型 → LispValue              ║")
    println("║  ✓ LispDeserializable: LispValue → 仓颉类型           ║")
    println("║  ✓ 自动类型提升和截断 (Int ↔ Float)                   ║")
    println("║  ✓ 类型安全的 Option 返回                             ║")
    println("╚═══════════════════════════════════════════════════════════╝")

    return 0
}
