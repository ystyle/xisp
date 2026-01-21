package ystyle::xisp.examples.test_destruct

import ystyle::xisp.core.*
import ystyle::xisp.parser.*

/**
 * 解构绑定测试程序
 */
main(): Int64 {
    println("╔═══════════════════════════════════════════════════════════╗")
    println("║              解构绑定功能测试                             ║")
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

    // 测试 1：简单解构 - (let ((x . y) '(1 2 3)) x)
    println("=== 测试 1：简单解构 ===")
    println("代码: (let ((x . y) '(1 2 3)) x)")
    println("期望: 1 (列表的第一个元素)")
    let result1 = evalCode("(let ((x . y) '(1 2 3)) x)")
    print("结果: ")
    println(result1.toString())
    println()

    // 测试 2：绑定剩余部分 - (let ((x . y) '(1 2 3)) y)
    println("=== 测试 2：绑定剩余部分 ===")
    println("代码: (let ((x . y) '(1 2 3)) y)")
    println("期望: (2 3) (列表的剩余部分)")
    let result2 = evalCode("(let ((x . y) '(1 2 3)) y)")
    print("结果: ")
    println(result2.toString())
    println()

    // 测试 3：普通绑定（对比）- (let ((z 10)) z)
    println("=== 测试 3：普通绑定（对比） ===")
    println("代码: (let ((z 10)) z)")
    println("期望: 10")
    let result3 = evalCode("(let ((z 10)) z)")
    print("结果: ")
    println(result3.toString())
    println()

    // 测试 4：嵌套解构 - (let (((x . y) . z) '((1 2) 3 4)) x)
    println("=== 测试 4：嵌套解构 ===")
    println("代码: (let (((x . y) . z) '((1 2) 3 4)) x)")
    println("期望: 1")
    let result4 = evalCode("(let (((x . y) . z) '((1 2) 3 4)) x)")
    print("结果: ")
    println(result4.toString())
    println()

    // 测试 5：多个普通绑定 - (let ((a 1) (b 2)) (+ a b))
    println("=== 测试 5：多个普通绑定 ===")
    println("代码: (let ((a 1) (b 2)) (+ a b))")
    println("期望: 3")
    let result5 = evalCode("(let ((a 1) (b 2)) (+ a b))")
    print("结果: ")
    println(result5.toString())
    println()

    println("╔═══════════════════════════════════════════════════════════╗")
    println("║              测试完成！                                  ║")
    println("╚═══════════════════════════════════════════════════════════╝")

    return 0
}
