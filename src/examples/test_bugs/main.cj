package ystyle::xisp.examples.test_bugs

import ystyle::xisp.*

main() {
    println("=== 测试报告的 Bug ===\n")

    let interp = LispInterpreter()

    // Bug 1: apply_demo.lisp - max/min
    println("1. 测试 max/min:")
    let result1 = interp.eval("(apply max (quote (1 2 3 4 5)))")
    println("   (apply max '(1 2 3 4 5)) = ${result1}")

    let result2 = interp.eval("(apply min (quote (1 2 3 4 5)))")
    println("   (apply min '(1 2 3 4 5)) = ${result2}")
    println()

    // Bug 2: apply_test.lisp - apply cons
    println("2. 测试 apply cons:")
    let result3 = interp.eval("(apply cons 1 (quote (2 3 4)))")
    println("   (apply cons 1 '(2 3 4)) = ${result3}")
    println()

    // Bug 3: pattern_matching.lisp - 守卫条件
    println("3. 测试守卫条件:")
    let result4 = interp.eval("(match 15 (x when (> x 10) \"large\") _ \"small\")")
    println("   (match 15 (x when (> x 10) \"large\") _ \"small\") = ${result4}")
    println()

    // Bug 4: modern_syntax.lisp - 字符串插值
    println("4. 测试字符串插值:")
    let result5 = interp.eval("#\"Value: #{(+ 1 2)}\"")
    println("   #\"Value: #{(+ 1 2)}\" = ${result5}")
    println()

    // Bug 5: modern_syntax.lisp - 管道操作符
    println("5. 测试管道操作符:")
    let result6 = interp.eval("(-> 10 (+ 5) (* 2))")
    println("   (-> 10 (+ 5) (* 2)) = ${result6}")
    println()

    // Bug 6: destructuring.lisp
    println("6. 测试解构绑定:")
    let result7 = interp.eval("(let [(x y) (quote (1 2))] (str \"x=\" x))")
    println("   (let [(x y) (quote (1 2))] (str \"x=\" x)) = ${result7}")
    println()

    println("=== 测试完成 ===")
}
