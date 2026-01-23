package ystyle::xisp.examples.modern_syntax

import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.bridge.*

/**
 * 现代化语法演示程序
 *
 * 展示 Xisp 的现代化语法特性：
 * - 向量字面量 []
 * - 哈希映射字面量 {}
 * - 哈希集合字面量 #{}
 */
main(): Int64 {
    println("╔═══════════════════════════════════════════════════════════╗")
    println("║          Xisp 现代化语法演示                             ║")
    println("╚═══════════════════════════════════════════════════════════╝")
    println("")

    // 创建解释器环境
    let env = Environment()
    let evaluator = Evaluator(env)

    // 注册桥接函数
    Bridge.registerStdCollection(env)

    // 测试向量字面量
    println("=" * 60)
    println("1. 向量字面量 []")
    println("=" * 60)
    println("代码: [1 2 3 4 5]")
    let vecCode = "[1 2 3 4 5]"
    let vecExpr = Parser.parseSingleFromString(vecCode)
    println("解析结果: ${vecExpr.toString()}")
    println("求值结果: ${evaluator.eval(vecExpr).toString()}")
    println("")

    // 测试哈希映射字面量
    println("=" * 60)
    println("2. 哈希映射字面量 {}")
    println("=" * 60)
    println("代码: {:name \"张三\" :age 25}")
    let mapCode = "{:name \"张三\" :age 25}"
    let mapExpr = Parser.parseSingleFromString(mapCode)
    println("解析结果: ${mapExpr.toString()}")
    println("求值结果: ${evaluator.eval(mapExpr).toString()}")
    println("")

    // 测试哈希集合字面量
    println("=" * 60)
    println("3. 哈希集合字面量 #{}")
    println("=" * 60)
    println("代码: #{1 2 3 4 5}")
    let setCode = "#{1 2 3 4 5}"
    let setExpr = Parser.parseSingleFromString(setCode)
    println("解析结果: ${setExpr.toString()}")
    println("求值结果: ${evaluator.eval(setExpr).toString()}")
    println("")

    // 测试嵌套使用
    println("=" * 60)
    println("4. 嵌套使用")
    println("=" * 60)
    println("代码: [1 2 {:a 100 :b 200}]")
    let nestedCode = "[1 2 {:a 100 :b 200}]"
    let nestedExpr = Parser.parseSingleFromString(nestedCode)
    println("解析结果: ${nestedExpr.toString()}")
    println("求值结果: ${evaluator.eval(nestedExpr).toString()}")
    println("")

    // 与传统语法对比
    println("=" * 60)
    println("5. 语法对比")
    println("=" * 60)
    println("传统语法: (list 1 2 3)")
    println("现代语法: [1 2 3]")
    println("")
    println("传统语法: (list (list :a 1) (list :b 2))")
    println("现代语法: {:a 1 :b 2}")
    println("")

    // 设计说明
    println("=" * 60)
    println("设计说明")
    println("=" * 60)
    println("""
• 向量字面量 [1 2 3] 被转换为 (cangjie:vector 1 2 3)
  - 当前实现：返回列表形式
  - 未来：可扩展为真正的向量（随机访问 O(1)）

• 哈希映射字面量 {:a 1 :b 2} 被转换为 (cangjie:hashmap :a 1 :b 2)
  - 当前实现：返回关联列表 ((:a . 1) (:b . 2))
  - 未来：可扩展为真正的 HashMap

• 哈希集合字面量 #{1 2 3} 被转换为 (cangjie:hashset 1 2 3)
  - 当前实现：返回列表形式
  - 未来：可扩展为真正的 HashSet（自动去重）

• 这些语法与 Racket、Clojure 等现代 Lisp 方言保持一致
""")

    println("=" * 60)
    println("演示完成！")
    println("=" * 60)
    println("")
    println("提示：你可以在 REPL 中尝试这些语法：")
    println("  xisp> [1 2 3]")
    println("  xisp> {:a 1 :b 2}")
    println("  xisp> #{1 2 3}")
    println("")

    0
}
