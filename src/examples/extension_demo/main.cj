package ystyle::xisp.examples.extension_demo

import ystyle::xisp.bridge.*
import ystyle::xisp.core.*
import std.collection.ArrayList
import std.collection.HashMap
import std.collection.HashSet

/**
 * 扩展功能演示程序
 * 展示如何使用 LispConvertible 接口和扩展功能
 */
main(): Int64 {
    println("╔═══════════════════════════════════════════════════════════╗")
    println("║      仓颉扩展功能演示 - LispConvertible 接口              ║")
    println("╚═══════════════════════════════════════════════════════════╝")

    // ========== 基本类型转换 ==========
    println("")
    println(">>> 基本类型转换:")

    // 整数
    let num: Int64 = 42
    print("  Int64 42.toLisp() = ")
    println(num.toLisp().toString())

    // 字符串
    let str: String = "Hello"
    print("  String \"Hello\".toLisp() = ")
    println(str.toLisp().toString())

    // 布尔值
    let bool: Bool = true
    print("  Bool true.toLisp() = ")
    println(bool.toLisp().toString())

    // ========== 列表转换 ==========
    println("")
    println(">>> 列表转换:")

    let numbers = ArrayList<Int64>()
    numbers.add(1)
    numbers.add(2)
    numbers.add(3)

    let lispList = TypeConverter.fromList(numbers)
    print("  ArrayList<Int64>([1, 2, 3]).toLisp() = ")
    println(lispList.toString())

    // ========== HashMap 转换 ==========
    println("")
    println(">>> HashMap 转换:")

    let map = HashMap<String, Int64>()
    map["a"] = 1
    map["b"] = 2
    map["c"] = 3

    let lispMap = map.toLisp()
    print("  HashMap.toLisp() = ")
    println(lispMap.toString())

    // ========== HashSet 转换 ==========
    println("")
    println(">>> HashSet 转换:")

    let set = HashSet<Int64>()
    set.add(10)
    set.add(20)
    set.add(30)

    let lispSet = set.toLisp()
    print("  HashSet.toLisp() = ")
    println(lispSet.toString())

    // ========== Option 转换 ==========
    println("")
    println(">>> Option 转换:")

    let someValue: Option<Int64> = Some(42)
    let noneValue: Option<Int64> = None

    print("  Some(42).toLisp() = ")
    println(someValue.toLisp().toString())
    print("  None.toLisp() = ")
    println(noneValue.toLisp().toString())

    // ========== 便捷函数 ==========
    println("")
    println(">>> 便捷函数:")

    let nums = ArrayList<Int64>()
    nums.add(10)
    nums.add(20)
    nums.add(30)

    print("  toLispList([10, 20, 30]) = ")
    println(toLispList(nums).toString())

    let single: Int64 = 99
    print("  toLisp(99) = ")
    println(toLisp(single).toString())

    // ========== 嵌套集合 ==========
    println("")
    println(">>> 嵌套集合:")

    let nested = ArrayList<ArrayList<Int64>>()
    nested.add(ArrayList<Int64>())
    let inner1 = nested[0]
    inner1.add(1)
    inner1.add(2)
    nested.add(ArrayList<Int64>())
    let inner2 = nested[1]
    inner2.add(3)
    inner2.add(4)

    let lispNested = nested.toLisp()
    print("  嵌套列表.toLisp() = ")
    println(lispNested.toString())

    println("")
    println("╔═══════════════════════════════════════════════════════════╗")
    println("║              扩展功能演示完成！                        ║")
    println("║                                                        ║")
    println("║  现在所有实现了 LispConvertible 的类型都可以：          ║")
    println("║  - 调用 .toLisp() 方法转换为 LispValue                   ║")
    println("║  - 作为泛型约束在函数中使用                          ║")
    println("║  - 使用 toLisp() 和 toLispList() 便捷函数               ║")
    println("╚═══════════════════════════════════════════════════════════╝")

    return 0
}
