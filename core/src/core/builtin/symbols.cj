package ystyle::xisp.core.builtin

import ystyle::xisp.types.*
import std.collection.ArrayList
import std.sync.*

/**
 * 符号工具
 *
 * 目前提供 `gensym` —— 非卫生宏（`defmacro`）生成"不会撞上用户变量"的临时符号的手段。
 *
 * 背景：xisp 的 `defmacro` 是 Common Lisp 式的**非卫生宏**——宏体里出现的符号
 * 与调用点的符号在同一个符号空间里解析，因此宏内部引入的临时变量会**捕获**用户
 * 同名变量（经典例子见 `docs/core.md` 与 `xisp_tests/src/gensym_test.cj`）。
 * 卫生宏（R7RS `syntax-rules`）由语言机制解决该问题；非卫生宏的通行解法就是
 * 让宏在展开期向运行时索要一个"保证唯一"的符号，也就是 `gensym`。
 */
public let GENSYM_PREFIX = "#:g"

/**
 * 全局唯一计数器
 *
 * `gensym` 天然是"全局名字源"（Common Lisp 亦如此），且 `NativeFunc` 的 handler
 * 只接收参数列表、拿不到求值器（见能力域设计 §3.1 的接缝 A），因此计数器必须
 * 自带而不是挂在 `Evaluator` 上。用原子量是为了并发安全：沙箱模式在独立线程里
 * 求值，宿主也可能并发调用。
 */
private let gensymCounter = AtomicInt64(0)

extend BuiltinFunctions {
    /**
     * 注册符号相关内建函数
     */
    static func registerSymbols(env: Environment): Unit {
        // (gensym) / (gensym "prefix")
        //   返回一个全新的符号，形如 #:g12；带前缀时形如 #:g12-<prefix>。
        //   前缀只用于可读性，唯一性由计数器保证。
        env.define("gensym", NativeFunc({ args =>
            let n = gensymCounter.fetchAdd(1) + 1
            match (args.size) {
                case 0 => Symbol("${GENSYM_PREFIX}${n}")
                case _ =>
                    // 前缀拼在末尾：符号须以 #:gN 开头，便于识别与审计。
                    // 字符串前缀取其内容（toString 会带引号）；其它类型用其文本形式。
                    let label = match (args[0]) {
                        case Str(s) => s
                        case other => other.toString()
                    }
                    Symbol("${GENSYM_PREFIX}${n}-${label}")
            }
        }))
    }
}
