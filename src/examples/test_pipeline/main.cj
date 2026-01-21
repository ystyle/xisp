package ystyle::xisp.examples.test_pipeline

import ystyle::xisp.core.*
import ystyle::xisp.parser.*

/**
 * 管道操作符测试程序
 */
main(): Int64 {
    println("╔═══════════════════════════════════════════════════════════╗")
    println("║              管道操作符 -> 测试                          ║")
    println("╚═══════════════════════════════════════════════════════════╝")
    println()

    // 创建环境和求值器
    let env = Environment()
    BuiltinFunctions.registerAll(env)
    let evaluator = Evaluator(env)

    // 辅助函数：求值字符串表达式
    func evalCode(code: String): LispValue {
        try {
            let lexer = Lexer(code)
            let tokens = lexer.tokenize()
            let parser = Parser(tokens)
            let exprs = parser.parse()
            if (exprs.size > 0) {
                evaluator.eval(exprs[0])
            } else {
                Nil
            }
        } catch (e: Exception) {
            println("Error: ${e.message}")
            Nil
        }
    }

    // 测试 1：基础管道
    println("=== 测试 1：基础管道 ===")
    println("代码: (-> 5 (+ 3))")
    println("期望: 8")
    let result1 = evalCode("(-> 5 (+ 3))")
    print("结果: ")
    println(result1.toString())
    println()

    // 测试 2：多步管道
    println("=== 测试 2：多步管道 ===")
    println("代码: (-> 5 (+ 3) (* 2))")
    println("期望: 16")
    let result2 = evalCode("(-> 5 (+ 3) (* 2))")
    print("结果: ")
    println(result2.toString())
    println()

    // 测试 3：符号形式（单参数函数）
    println("=== 测试 3：符号形式 ===")
    println("代码: (-> -5 -)")
    println("期望: 5 (单参数 - 表示取负)")
    let result3 = evalCode("(-> -5 -)")
    print("结果: ")
    println(result3.toString())
    println()

    // 测试 4：带额外参数
    println("=== 测试 4：带额外参数 ===")
    println("代码: (-> 10 (- 3))")
    println("期望: 7")
    let result4 = evalCode("(-> 10 (- 3))")
    print("结果: ")
    println(result4.toString())
    println()

    // 测试 5：复杂管道
    println("=== 测试 5：复杂管道 ===")
    println("代码: (-> 100 (/ 10) (+ 5) (* 2))")
    println("期望: 30")
    println("说明: 100 / 10 = 10, 10 + 5 = 15, 15 * 2 = 30")
    let result5 = evalCode("(-> 100 (/ 10) (+ 5) (* 2))")
    print("结果: ")
    println(result5.toString())
    println()

    // 测试 6：与 list 结合
    println("=== 测试 6：与 list 结合 ===")
    println("代码: (-> '(1 2 3) length (+ 1))")
    println("期望: 4")
    let result6 = evalCode("(-> (quote (1 2 3)) length (+ 1))")
    print("结果: ")
    println(result6.toString())
    println()

    // 测试 7：嵌套管道（在 lambda 中）
    println("=== 测试 7：在 lambda 中使用 ===")
    println("代码: ((lambda (x) (-> x (+ 10) (* 2)) 5))")
    println("期望: 30")
    let result7 = evalCode("((lambda (x) (-> x (+ 10) (* 2))) 5)")
    print("结果: ")
    println(result7.toString())
    println()

    println("╔═══════════════════════════════════════════════════════════╗")
    println("║              测试完成！                                  ║")
    println("╚═══════════════════════════════════════════════════════════╝")

    return 0
}
