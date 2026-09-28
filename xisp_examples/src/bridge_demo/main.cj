package ystyle::xisp_examples.bridge_demo

import ystyle::xisp.*
import ystyle::xisp.types.*
import ystyle::xisp.bridge.*
import std.collection.ArrayList

/**
 * 桥接 + 反向调用示例
 *
 * 本文件与 `docs-site/integration/bridge.md` 的「完整示例」一一对应。
 * 存在的意义是**让文档里的 API 名字受编译校验**：文档示例中出现不存在的
 * 成员（错误类型名、方法名写错）时，这里会先编译失败，而不是等使用者踩到。
 *
 * 因此：改动本文件时同步改文档；文档示例改动时同步改本文件。
 */
main() {
    println("=== 桥接 + 反向调用示例 ===")

    let interpreter = LispInterpreter([withStdLib(), withQuietMode()])

    // 1. 注册仓颉函数到 Lisp（命名空间符号为 ns::name，双冒号）
    interpreter.registerBridgeFunctionWithNS("math", "double", { args =>
        if (args.size > 0 && let LispValue.Int(n) <- args[0]) {
            LispValue.Int(n * 2)
        } else {
            LispValue.Error(XispError(ErrorType.TypeError, "double: expected number"))
        }
    })

    // 2. Lisp 中使用桥接函数
    interpreter.eval("(define x (math::double 21))")
    let result = interpreter.eval("(* x 2)")
    if (let Some(i) <- result.asInt()) {
        println("(math::double 21) * 2 = ${i}")   // 84
    }

    // 3. 反向调用 Lisp 函数：列表实参按值注入
    interpreter.eval("(define (sum-list lst) (reduce + 0 lst))")
    let total = interpreter.call("sum-list", [ArrayList<Int64>([1, 2, 3, 4])])
    if (let Some(i) <- total.asInt()) {
        println("(sum-list (1 2 3 4)) = ${i}")   // 10
    }

    // 4. 列表 + 标量的异构实参：装箱成 LispValue 后走 Array<LispValue>
    interpreter.eval("(define (len-plus lst n) (+ (length lst) n))")
    let lst: LispValue = ArrayList<Int64>([1, 2, 3]).toLisp()
    let mixed = interpreter.call("len-plus", [lst, LispValue.Int(7)])
    if (let Some(i) <- mixed.asInt()) {
        println("(len-plus (1 2 3) 7) = ${i}")   // 10
    }

    // 5. 标准库桥接：双冒号命名空间
    println("(cangjie::exists? \"cjpm.toml\") = ${interpreter.eval("(cangjie::exists? \"cjpm.toml\")")}")

    println("=== 示例完成 ===")
}
