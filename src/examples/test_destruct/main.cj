package ystyle::xisp.examples.test_destruct

import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*

/**
 * 解构绑定测试程序 - 展示现代向量语法和传统点对语法
 */
main(): Int64 {
    println("╔═══════════════════════════════════════════════════════════╗")
    println("║         解构绑定功能测试（现代 + 传统语法）             ║")
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

    println("═══════════════════════════════════════════════════════════")
    println("现代向量语法（推荐）")
    println("═══════════════════════════════════════════════════════════")
    println()

    // 测试 1：现代向量解构 - [x y]
    println("测试 1：现代向量解构 [x y]")
    println("代码: (let [[x y] '(1 2 3)] x)")
    println("期望: 1")
    let result1 = evalCode("(let [[x y] '(1 2 3)] x)")
    println("结果: ${result1.toString()}")
    println()

    // 测试 2：& rest 收集剩余元素
    println("测试 2：& rest 收集剩余元素")
    println("代码: (let [[x y & rest] '(1 2 3 4 5)] rest)")
    println("期望: (3 4 5)")
    let result2 = evalCode("(let [[x y & rest] '(1 2 3 4 5)] rest)")
    println("结果: ${result2.toString()}")
    println()

    // 测试 3：嵌套向量解构 - [[a b] c]
    println("测试 3：嵌套向量解构 [[a b] c]")
    println("代码: (let [[[a b] c] '((1 2) 3)] (list a b c))")
    println("期望: (1 2 3)")
    let result3 = evalCode("(let [[[a b] c] '((1 2) 3)] (list a b c))")
    println("结果: ${result3.toString()}")
    println()

    println("═══════════════════════════════════════════════════════════")
    println("传统点对语法（向后兼容）")
    println("═══════════════════════════════════════════════════════════")
    println()

    // 测试 4：传统点对解构 - (x . y)
    println("测试 4：传统点对解构 (x . y)")
    println("代码: (let ((x . y) '(1 2 3)) x)")
    println("期望: 1")
    let result4 = evalCode("(let ((x . y) '(1 2 3)) x)")
    println("结果: ${result4.toString()}")
    println()

    // 测试 5：传统嵌套点对解构
    println("测试 5：传统嵌套点对解构")
    println("代码: (let (((x . y) . z) '((1 2) 3 4)) (list x y z))")
    println("期望: (1 (2) (3 4))")
    let result5 = evalCode("(let (((x . y) . z) '((1 2) 3 4)) (list x y z))")
    println("结果: ${result5.toString()}")
    println()

    println("═══════════════════════════════════════════════════════════")
    println("对比示例：现代 vs 传统")
    println("═══════════════════════════════════════════════════════════")
    println()

    // 测试 6：对比
    println("测试 6：现代 [x y & rest] vs 传统 (x . y)")
    println("现代语法: [x y & rest] - 更清晰直观")
    let result6Modern = evalCode("(let [[x y & rest] '(1 2 3 4 5)] (list x y rest))")
    println("结果: ${result6Modern.toString()}")

    println("传统语法: (x . y) - 向后兼容")
    let result6Traditional = evalCode("(let ((x . y) '(1 2 3 4 5)) (list x y))")
    println("结果: ${result6Traditional.toString()}")
    println()

    println("═══════════════════════════════════════════════════════════")
    println("总结")
    println("═══════════════════════════════════════════════════════════")
    println("✅ 推荐使用现代向量语法：[x y & rest]")
    println("✅ 传统点对语法继续有效：(x . y)")
    println("✅ 两种语法可以共存，根据需要选择")
    println()

    println("╔═══════════════════════════════════════════════════════════╗")
    println("║              测试完成！                                  ║")
    println("╚═══════════════════════════════════════════════════════════╝")

    return 0
}
