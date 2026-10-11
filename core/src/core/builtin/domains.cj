package ystyle::xisp.core.builtin

import ystyle::xisp.types.*
import ystyle::xisp.core.eval.*
import std.collection.ArrayList

/**
 * core 内置能力域（能力域设计 §4.6 A）
 *
 * 把现有 `registerXxx` 包装成 `LispCapability`，使"装哪些符号"从两个硬编码函数
 * （`registerAll` / `registerPure`）变成**可组合的域集合**：
 *
 * - `PRESET_FULL` = 所有域的并集
 * - `PRESET_PURE` = 按 `escapable` / `hostPolicy` **推导**的子集
 *   （不再维护第二份平行清单，消除隐式依赖）
 *
 * ## 属性判据
 *
 * - `escapable`：本域是否提供能**绕过权限检查**的东西。按 `registerPure` 的既有论证，
 *   `eval` / `apply` / `map` / `filter` / `reduce` / `for-each` 会先于检查执行，
 *   故这些域标 `true`；**其余默认也标 `true`**（保守），只有明确安全的才 `false`。
 * - `hostPolicy`：是否触及宿主资源（输出通道即算）。
 *
 * 注意 `and` / `or` / `condb` / `if-let` / `let*` / `when-let*` 现已降为**宏**，
 * 归 `macros.core` 域；`map` / `filter` / `reduce` / `apply` / `eval` / `->` / `for-each`
 * 仍由求值器拦截（②类），本文件不注册它们（它们不在环境里）。
 */
extend BuiltinFunctions {
    /** base：`nil` 字面量等最小基座 */
    public static func domainBase(): LispCapability {
        SimpleCapability("base", ["nil"], { ctx => ctx.env.define("nil", Nil) },
            escapable: false, hostPolicy: false)
    }

    /** 算术：`+ - * / mod abs min max` 等 */
    public static func domainArithmetic(): LispCapability {
        SimpleCapability("arithmetic",
            ["+", "-", "*", "/", "mod", "abs", "min", "max", "sum", "product"],
            { ctx => registerArithmetic(ctx.env); registerModulo(ctx.env); registerMath(ctx.env) },
            escapable: false, hostPolicy: false)
    }

    /** 比较：`= < > <= >=` */
    public static func domainComparison(): LispCapability {
        SimpleCapability("comparison", ["=", "<", ">", "<=", ">="],
            { ctx => registerComparison(ctx.env) },
            escapable: false, hostPolicy: false)
    }

    /** 列表：`cons car cdr list length append reverse` 等 */
    public static func domainList(): LispCapability {
        SimpleCapability("list",
            ["cons", "car", "cdr", "list", "length", "append", "reverse", "range",
             "car", "cdr", "first", "rest"],
            { ctx => registerListOps(ctx.env); registerHigherOrder(ctx.env) },
            escapable: false, hostPolicy: false, after: ["base"])
    }

    /** 逻辑与谓词：`not eq? null? number?` 等（`and`/`or` 已降为宏，不在此域） */
    public static func domainLogic(): LispCapability {
        SimpleCapability("logic",
            ["not", "eq?", "null?", "number?", "string?", "symbol?", "list?",
             "procedure?", "integer?", "float?", "zero?", "positive?", "negative?"],
            { ctx => registerLogic(ctx.env); registerPredicates(ctx.env) },
            escapable: false, hostPolicy: false)
    }

    /** 哈希表：`hashmap-*` 与 Redis 风格别名 */
    public static func domainHashMap(): LispCapability {
        SimpleCapability("hashmap", ["hashmap", "hashmap-get", "hashmap-set!", "hashmap-keys"],
            { ctx =>
                registerHashMap(ctx.env)
                registerRedisAliases(ctx.env)
            },
            escapable: false, hostPolicy: false)
    }

    /** 输出：`str print println princ newline display`（触及宿主通道，故 hostPolicy） */
    public static func domainPrint(): LispCapability {
        SimpleCapability("print",
            ["str", "print", "println", "princ", "newline", "display"],
            { ctx => registerPrint(ctx.env) },
            escapable: false, hostPolicy: true)
    }

    /** 符号工具：`gensym` */
    public static func domainSymbols(): LispCapability {
        SimpleCapability("symbols", ["gensym"],
            { ctx => registerSymbols(ctx.env) },
            escapable: false, hostPolicy: false)
    }

    /** 内建宏包：`when` `unless` `incf` `swap` `do` `defun` `and` `or` `condb` `if-let` `let*` `when-let*` … */
    public static func domainMacrosCore(): LispCapability {
        SimpleCapability("macros.core",
            ["when", "unless", "incf", "decf", "swap", "push", "pop", "negate",
             "do", "defun", "and", "or", "condb", "if-let", "let*", "when-let*"],
            { ctx => BuiltinMacros.registerAll(ctx.env) },
            // 宏展开本身不逃逸：它产出的是**代码**，最终仍受环境的权限检查约束。
            // 但 `and`/`or` 等展开后可能引用高阶形式，故保守标 true。
            escapable: true, hostPolicy: false,
            after: ["base", "list", "logic"])
    }

    /** 错误辅助：`error` `raise` `assert` */
    public static func domainErrorHelpers(): LispCapability {
        SimpleCapability("error", ["error", "raise", "assert"],
            { ctx => registerErrorHelpers(ctx.env) },
            escapable: false, hostPolicy: false)
    }

    /** 全部 core 内置域（安装顺序即数组顺序） */
    public static func coreDomains(): Array<LispCapability> {
        [
            domainBase(),
            domainArithmetic(),
            domainComparison(),
            domainList(),
            domainLogic(),
            domainHashMap(),
            domainSymbols(),
            domainErrorHelpers(),
            domainMacrosCore(),
        ]
    }
}
