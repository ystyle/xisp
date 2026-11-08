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

    /**
     * 算术：`+ - * / mod round min max sum product`
     *
     * 原先声明里还有个 `abs`，但**它从未被实现**（全仓无 `env.define("abs")`，
     * docs-site 的算术 API 清单里也没有）。域声明的 `symbols` 是对外契约
     * （沙箱审计 / 文档生成 / 重名检出都读它），多声明一个不存在的符号会让
     * 上层以为可用 —— 已从声明移除；`min` / `max` / `sum` / `product` 由
     * `registerHigherOrder` 提供，不在 `registerMath` 里。
     */
    public static func domainArithmetic(): LispCapability {
        SimpleCapability("arithmetic",
            ["+", "-", "*", "/", "mod", "round", "min", "max", "sum", "product"],
            { ctx =>
                registerArithmetic(ctx.env)
                registerModulo(ctx.env)
                registerMath(ctx.env)
                // 聚合归约（sum / product / max / min）：语义上是算术，此前却随
                // `registerHigherOrder` 挂在 list 域，导致"只装 arithmetic 没有 sum"。
                registerAggregates(ctx.env)
            },
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
            ["cons", "car", "cdr", "list", "length", "nth", "append", "reverse", "range"],
            { ctx => registerListOps(ctx.env); registerListExtras(ctx.env) },
            escapable: false, hostPolicy: false, after: ["base"])
    }

    /**
     * 谓词：`number?` `string?` `null?` `procedure?` `type-of` `external?` 等
     *
     * 与 `logic`（`not` / `eq?`）分开：宿主可能只要布尔运算、不要这一堆类型判断
     * （谓词是"自省能力"，最小嵌入选它没有意义）。此前两者同挤在一个域。
     */
    public static func domainPredicates(): LispCapability {
        SimpleCapability("predicates",
            // 注意不含 `hashmap?`：它由 `registerHashMap` 注册（属 hashmap 域）。
            // 域声明必须与"谁来注册"一致，否则只装 predicates 时它并不存在。
            ["number?", "string?", "symbol?", "list?", "procedure?", "integer?", "float?",
             "zero?", "positive?", "negative?", "even?", "odd?", "null?",
             "external?", "external-type", "type-of"],
            { ctx => registerPredicates(ctx.env) },
            escapable: false, hostPolicy: false, after: ["base"])
    }

    /** 逻辑：`not` `eq?`（`and`/`or` 已降为宏，不在此域；类型谓词见 domainPredicates） */
    public static func domainLogic(): LispCapability {
        SimpleCapability("logic", ["not", "eq?"],
            { ctx => registerLogic(ctx.env) },
            escapable: false, hostPolicy: false)
    }

    /** 哈希表：`hashmap-*` `hashmap?` 与 Redis 风格别名 */
    public static func domainHashMap(): LispCapability {
        SimpleCapability("hashmap",
            ["hashmap", "hashmap-get", "hashmap-set!", "hashmap-remove!", "hashmap-contains?",
             "hashmap-keys", "hashmap-values", "hashmap-size", "hashmap?",
             "hget", "hset", "hdel", "hexists", "contains?", "hkeys", "hvals", "hlen", "hgetall"],
            { ctx =>
                registerHashMap(ctx.env)
                registerRedisAliases(ctx.env)
            },
            escapable: false, hostPolicy: false)
    }

    /**
     * 字符串计算：`str` `string-length` `string=?` `string<` `string>` `string-contains?`
     *
     * **纯计算、不碰输出通道**，因此与 `print` 分成两个域 —— 代码里本来就是
     * 两个注册器（`registerStringOps` / `registerOutput`）。并进 `hostPolicy` 域的话，
     * `presetPure` 按属性推导时会把它们一起排除，而纯函数子集一直需要 `str`。
     */
    public static func domainString(): LispCapability {
        SimpleCapability("string",
            ["str", "string-length", "string=?", "string<", "string>", "string-contains?"],
            { ctx => registerStringOps(ctx.env) },
            escapable: false, hostPolicy: false)
    }

    /** 输出：`print println princ newline display`（触及宿主通道，故 hostPolicy） */
    public static func domainPrint(): LispCapability {
        SimpleCapability("print",
            ["print", "println", "princ", "newline", "display"],
            { ctx => registerOutput(ctx.env, { s => print(s) }) },
            escapable: false, hostPolicy: true)
    }

    /**
     * 现代别名与传统组合函数：`first` `rest` `second` … `setq` `progn` `string-append`
     *
     * **顺序敏感**：`registerModernAliases` 用 `lookup` 取 `car` / `cdr` / `cons` / `str`
     * 并复用其值，所以必须排在 `list` 与 `string` 之后 —— 这正是 `after` 要显式化的
     * 隐式约束（此前只靠 `registerAll` 里的"最后注册"注释）。
     *
     * 此前它**不属任何域**：独占装配（`replace: true`）时这些别名集体消失，
     * 而 `domainList` 的 symbols 却声明了 `first` / `rest` —— 声明与安装脱节。
     */
    public static func domainAliases(): LispCapability {
        SimpleCapability("aliases",
            ["first", "rest", "second", "third", "fourth", "prepend", "more",
             "cadr", "caddr", "cadddr", "cddr", "cdar", "caar",
             "setq", "progn", "string-append"],
            { ctx => registerModernAliases(ctx.env) },
            escapable: false, hostPolicy: false,
            after: ["base", "list", "string"])
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
            BuiltinMacros.allSymbols(),
            { ctx => BuiltinMacros.registerAll(ctx.env) },
            // **不逃逸**：宏是代码变换器，展开产物仍是普通表达式，照样要过
            // `funcChecker` / 沙箱检查 —— 它不提供绕过权限检查的手段
            // （对照 `escapable` 的定义：能绕过权限检查的东西才算）。
            //
            // 曾经保守标 `true`，后果是 `presetPure` 按属性推导把宏包整个排除，
            // 于是"纯函数子集"里连 `(when #t 1)` 都不认 —— 与 `registerPure`
            // （一直包含 ④ 层宏包）不一致，两档装配无法互相替代。
            escapable: false, hostPolicy: false,
            // `swap` / `or` 展开时会调 `gensym`，故依赖 symbols 域
            after: ["base", "list", "logic", "symbols"])
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
            domainPredicates(),
            domainHashMap(),
            domainString(),
            domainPrint(),
            domainSymbols(),
            domainAliases(),
            domainErrorHelpers(),
            domainMacrosCore(),
        ]
    }
}
