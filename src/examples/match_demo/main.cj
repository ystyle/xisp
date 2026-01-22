package ystyle::xisp.examples.match_demo

import ystyle::xisp.*

main() {
    println("=== Xisp 模式匹配演示 ===\n")

    // ==================== 测试 1: 常量匹配 ====================
    println("1. 常量匹配")
    let test1 = LispInterpreter([withQuietMode()])

    let code1 = "(match 5 1 \"one\" 2 \"two\" 5 \"five\" _ \"other\")"
    println("代码: ${code1}")
    let result1 = test1.eval(code1)
    println("结果: ${result1}")
    println("期望: \"five\"")
    println()

    // ==================== 测试 2: 变量绑定 ====================
    println("2. 变量绑定")
    let test2 = LispInterpreter([withQuietMode()])

    let code2 = "(match 42 x (str \"Value is: \" x))"
    println("代码: ${code2}")
    let result2 = test2.eval(code2)
    println("结果: ${result2}")
    println("期望: \"Value is: 42.000000\"")
    println()

    // ==================== 测试 3: 列表模式匹配 ====================
    println("3. 列表模式匹配")
    let test3 = LispInterpreter([withQuietMode()])

    let code3 = "(match (quote (1 2 3)) (x y z) (str \"x=\" x \", y=\" y \", z=\" z) _ \"not a list of 3\")"
    println("代码: ${code3}")
    let result3 = test3.eval(code3)
    println("结果: ${result3}")
    println("期望: 解构绑定 x=1, y=2, z=3")
    println()

    // ==================== 测试 4: 通配符 ====================
    println("4. 通配符")
    let test4 = LispInterpreter([withQuietMode()])

    let code4 = "(match \"hello\" _ \"matched anything\")"
    println("代码: ${code4}")
    let result4 = test4.eval(code4)
    println("结果: ${result4}")
    println("期望: \"matched anything\"")
    println()

    // ==================== 测试 5: 守卫条件 ====================
    println("5. 守卫条件")
    let test5 = LispInterpreter([withQuietMode()])

    let code5 = "(match 15 (x when (> x 10) (str \"large: \" x)) (x when (< x 10) (str \"small: \" x)) _ \"medium\")"
    println("代码: ${code5}")
    let result5 = test5.eval(code5)
    println("结果: ${result5}")
    println("期望: \"large: 15.000000\"")
    println()

    // ==================== 测试 6: 嵌套列表匹配 ====================
    println("6. 嵌套列表匹配")
    let test6 = LispInterpreter([withQuietMode()])

    let code6 = "(match (quote ((1 2) (3 4))) ((a b) (c d)) (str \"a=\" a \", b=\" b \", c=\" c \", d=\" d) _ \"no match\")"
    println("代码: 嵌套列表")
    let result6 = test6.eval(code6)
    println("结果: ${result6}")
    println("期望: 解构嵌套列表")
    println()

    // ==================== 测试 7: 向量模式匹配 ====================
    println("7. 向量模式匹配")
    let test7 = LispInterpreter([withQuietMode()])

    let code7a = "(match [1 2 3] [x y z] (str \"vec: \" x \", \" y \", \" z) _ \"not a vec\")"
    println("代码: ${code7a}")
    let result7a = test7.eval(code7a)
    println("结果: ${result7a}")
    println("期望: 匹配向量")
    println()

    // ==================== 测试 8: rest 参数 ====================
    // 先测试简单的 rest 参数
    println("8.1. rest 参数 - 基础测试")
    let test8_1 = LispInterpreter([withQuietMode()])
    let code8_1 = "(match (quote (1)) (x & r) r _ \"no match\")"
    println("代码: ${code8_1}")
    let result8_1 = test8_1.eval(code8_1)
    println("结果: ${result8_1}")
    println("期望: nil (单元素列表的rest为空)")
    println()

    println("8.2. rest 参数 - 使用rest变量名")
    let test8_2 = LispInterpreter([withQuietMode()])
    let code8_2 = "(match (quote (1)) (x & rest) rest _ \"no match\")"
    println("代码: ${code8_2}")
    let result8_2 = test8_2.eval(code8_2)
    println("结果: ${result8_2}")
    println("期望: nil")
    println()

    println("8.3. rest 参数 - 在字符串中使用")
    let test8_3 = LispInterpreter([withQuietMode()])
    let code8_3 = "(match (quote (1)) (x & r) (str \"x=\" x \", r=\" r) _ \"no match\")"
    println("代码: ${code8_3}")
    let result8_3 = test8_3.eval(code8_3)
    println("结果: ${result8_3}")
    println("期望: \"x=1.000000, r=nil\"")
    println()

    println("8.4. rest 参数 - 使用rest变量名在字符串中")
    let test8_4 = LispInterpreter([withQuietMode()])
    let code8_4 = "(match (quote (1)) (x & rest) (str \"x=\" x \", rest=\" rest) _ \"no match\")"
    println("代码: ${code8_4}")
    let result8_4 = test8_4.eval(code8_4)
    println("结果: ${result8_4}")
    println("期望: \"x=1.000000, rest=nil\"")
    println()

    println("8.5. 测试：不使用rest，直接用str")
    let test8_5 = LispInterpreter([withQuietMode()])
    let code8_5 = "(match (quote (1)) x (str \"x=\" x) _ \"no match\")"
    println("代码: ${code8_5}")
    let result8_5 = test8_5.eval(code8_5)
    println("结果: ${result8_5}")
    println("期望: \"x=1.000000\"")
    println()

    println("8.6. 测试：两个变量的模式匹配")
    let test8_6 = LispInterpreter([withQuietMode()])
    let code8_6 = "(match (quote (1 2)) (x y) (str \"x=\" x \", y=\" y) _ \"no match\")"
    println("代码: ${code8_6}")
    let result8_6 = test8_6.eval(code8_6)
    println("结果: ${result8_6}")
    println("期望: \"x=1.000000, y=2.000000\"")
    println()

    println("8.7. 测试：rest参数，直接返回常量")
    let test8_7 = LispInterpreter([withQuietMode()])
    let code8_7 = "(match (quote (1)) (x & r) \"matched\" _ \"no match\")"
    println("代码: ${code8_7}")
    let result8_7 = test8_7.eval(code8_7)
    println("结果: ${result8_7}")
    println("期望: \"matched\"")
    println()

    println("8.8. 测试：rest参数，返回x")
    let test8_8 = LispInterpreter([withQuietMode()])
    let code8_8 = "(match (quote (1)) (x & r) x _ \"no match\")"
    println("代码: ${code8_8}")
    let result8_8 = test8_8.eval(code8_8)
    println("结果: ${result8_8}")
    println("期望: 1.000000")
    println()

    println("8.9. 测试：rest参数，两个元素")
    let test8_9 = LispInterpreter([withQuietMode()])
    let code8_9 = "(match (quote (1 2)) (x & r) (str \"x=\" x) _ \"no match\")"
    println("代码: ${code8_9}")
    let result8_9 = test8_9.eval(code8_9)
    println("结果: ${result8_9}")
    println("期望: \"x=1.000000\"")
    println()

    println("8.10. 测试：define和match结合")
    let test8_10 = LispInterpreter([withQuietMode()])
    let code8_10 = "(define (f x) (str \"x=\" x)) (f 1)"
    println("代码: ${code8_10}")
    let result8_10 = test8_10.eval(code8_10)
    println("结果: ${result8_10}")
    println("期望: \"x=1.000000\"")
    println()

    println("8.11. 测试：match在define中")
    let test8_11 = LispInterpreter([withQuietMode()])
    let code8_11 = "(define (g lst) (match lst (x y) (str \"pair\") _ \"other\")) (g (quote (1 2)))"
    println("代码: ${code8_11}")
    let result8_11 = test8_11.eval(code8_11)
    println("结果: ${result8_11}")
    println("期望: \"pair\"")
    println()

    println("8.12. 测试：简化的describe-data")
    let test8_12 = LispInterpreter([withQuietMode()])
    let code8_12 = "(define (d data) (match data (x y) (str \"pair\") _ \"other\")) (d (quote (1 2)))"
    println("代码: ${code8_12}")
    let result8_12 = test8_12.eval(code8_12)
    println("结果: ${result8_12}")
    println("期望: \"pair\"")
    println()

    println("8.13. 测试：match带多个模式")
    let test8_13 = LispInterpreter([withQuietMode()])
    let code8_13 = "(match (quote (1 2)) 0 \"zero\" 1 \"one\" (x y) (str \"pair\") _ \"other\")"
    println("代码: ${code8_13}")
    let result8_13 = test8_13.eval(code8_13)
    println("结果: ${result8_13}")
    println("期望: \"pair\"")
    println()

    println("8.14. 测试：带列表模式的match")
    let test8_14 = LispInterpreter([withQuietMode()])
    let code8_14 = "(match (quote (1 2)) (x y) (str \"pair\") _ \"other\")"
    println("代码: ${code8_14}")
    let result8_14 = test8_14.eval(code8_14)
    println("结果: ${result8_14}")
    println("期望: \"pair\"")
    println()

    println("8.15. 测试：向量模式")
    let test8_15 = LispInterpreter([withQuietMode()])
    let code8_15 = "(match [1 2 3] [x y z] (str \"vec\") _ \"other\")"
    println("代码: ${code8_15}")
    let result8_15 = test8_15.eval(code8_15)
    println("结果: ${result8_15}")
    println("期望: \"vec\"")
    println()

    println("8.16. 测试：带向量模式的define")
    let test8_16 = LispInterpreter([withQuietMode()])
    let code8_16 = "(define (h v) (match v [x y z] (str \"vec\") _ \"other\")) (h [1 2 3])"
    println("代码: ${code8_16}")
    let result8_16 = test8_16.eval(code8_16)
    println("结果: ${result8_16}")
    println("期望: \"vec\"")
    println()

    // ==================== 测试 9: 复杂模式 ====================
    println("9. 复杂模式：多种模式组合")
    let test9 = LispInterpreter([withQuietMode()])

    // 使用仓颉多行文本字面量
    let code9 = """
(define (describe-data data)
  (match data
    0 "zero"
    1 "one"
    (x y) (str "Pair: " x ", " y)
    _ "unknown"))
(describe-data (quote (1 2)))
"""
    println("代码: ${code9}")
    let result9 = test9.eval(code9)
    println("结果: ${result9}")
    println("期望: \"Pair: 1.000000, 2.000000\"")
    println()

    // ==================== 测试 10: 模式匹配在递归中的使用 ====================
    println("10. 模式匹配在递归中：计算列表长度")
    let test10 = LispInterpreter([withQuietMode()])

    let code10 = """
        (define (my-length lst)
          (match lst
            () 0
            (head & tail) (+ 1 (my-length tail))))
        (my-length (quote (1 2 3 4 5)))
    """
    println("代码: 用模式匹配实现 length")
    let result10 = test10.eval(code10)
    println("结果: ${result10}")
    println("期望: 5.000000")
    println()

    println("=== 演示完成 ===")
    println("\n模式匹配已成功实现以下功能：")
    println("✅ 常量匹配（数字、字符串、布尔值）")
    println("✅ 变量绑定")
    println("✅ 通配符 _")
    println("✅ 列表结构匹配 (x y z)")
    println("✅ 向量结构匹配 [x y z]")
    println("✅ 嵌套模式匹配")
    println("✅ rest 参数 (& rest) - 完全支持，包括单元素列表")
    println("✅ 在 define 中使用 match")
    println("✅ 多模式组合")
    println("✅ 守卫条件 (when guard)")
    println("✅ 在递归函数中使用（my-length 示例）")
}
