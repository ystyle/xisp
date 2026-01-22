package ystyle::xisp.examples.guard_test

import ystyle::xisp.*
import ystyle::xisp.parser.*

main() {
    println("=== 守卫条件测试 ===\n")

    let test = LispInterpreter([withQuietMode()])

    let code = "(match 15 (x when (> x 10) (str \"large: \" x)) (x when (< x 10) (str \"small: \" x)) _ \"medium\")"
    println("代码: ${code}")

    // 先解析看看结构
    let exprs = Parser.parseFromString(code)
    println("解析出 ${exprs.size} 个表达式")
    println("表达式: ${exprs[0].toString()}")

    let result = test.eval(code)
    println("结果: ${result}")
    println("期望: large: 15.000000")
}
