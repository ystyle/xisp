package ystyle::xisp.examples.quick_test

import ystyle::xisp.*

main() {
    println("=== 快速功能测试 ===")

    let interpreter = LispInterpreter([withQuietMode()])

    // 测试 1: max/min
    println("\n--- 测试 max/min ---")
    let r1 = interpreter.eval("(max (quote (1 5 3 9 2)))")
    println("(max (1 5 3 9 2)) = ${r1}")

    let r2 = interpreter.eval("(min (quote (5 1 9 3 2)))")
    println("(min (5 1 9 3 2)) = ${r2}")

    // 测试 2: 守卫条件
    println("\n--- 测试守卫条件 ---")
    let r3 = interpreter.eval("(match 15 (x when (> x 10) (str \"large: \" x)) _ \"medium\")")
    println("(match 15 (x when (> x 10) (str \"large: \" x)) _ \"medium\") = ${r3}")

    // 测试 3: 字符串插值
    println("\n--- 测试字符串插值 ---")
    let r4 = interpreter.eval("#\"Value: {+ 1 2}\"")
    println("#\"Value: {+ 1 2}\" = ${r4}")

    // 测试 4: 管道操作符
    println("\n--- 测试管道操作符 ---")
    let r5 = interpreter.eval("(-> 10 (+ 5) (* 2))")
    println("(-> 10 (+ 5) (* 2)) = ${r5}")

    // 测试 5: 解构绑定
    println("\n--- 测试解构绑定 ---")
    let r6 = interpreter.eval("(let [(x y z) (quote (1 2 3))] (list x y z))")
    println("(let [(x y z) (1 2 3)] (list x y z)) = ${r6}")

    println("\n=== 测试完成 ===")
}
