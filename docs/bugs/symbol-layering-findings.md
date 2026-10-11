# 符号分层实测：求值器原语 / 半原语 / 注册函数

**调查日期**: 2026-10-11
**触发**: 评审"lisp 必需符号的划分是否正确"时，逐个符号实测默认环境行为
**结论**: 现有三个真值来源（`docs/core.md` 清单、`trySpecialForm` 硬编码表、`registerXxx` 注册表）
互不一致；「注册集合 = 能力边界」这一前提**只对三分之一的符号成立**

---

## 1. 测量方法

对每个候选符号，用**独立解释器**（避免互相污染）测三件事：

| 维度 | 测法 | 含义 |
|---|---|---|
| R 注册为值 | `eval("符号名")` 是否为 `Error` | 环境中真的有这个绑定 |
| P 一等值 | `eval("(procedure? 符号名)")` | 能作为值传递 |
| S define 覆盖 | `eval("(define 符号名 42)")` 后调用它的行为 | 硬编码是否优先于注册表 |

`procedure?` 本身在 C 类中；A 类符号连求值都会报 `UnboundVariable`。

---

## 2. 实测结果：符号分三类

### A. 求值器原语（硬编码，从未注册）

`if` `quote` `backquote` `comma` `comma-at` `define` `defmacro` `set!` `lambda`
`begin` `let` `let*` `when-let*` `if-let` `condb` `match` `macroexpand`
`macroexpand-all` `import` `export`

| 维度 | 结果 |
|---|---|
| R | ❌ 未注册（`(eval "if")` → `UnboundVariable`） |
| P | ❌ 非一等值（同上） |
| S | ❌ `(define if 42)` 后 `(if 1 2)` **仍返回 2** |

实现位置：`core/src/core/eval/core.cj:120` 的 `trySpecialForm`，共 **33 条 `case`**。

### B. 半原语（既注册、又硬编码；硬编码优先）

| 子类 | 符号 | R | P | 覆盖后行为 |
|---|---|---|---|---|
| 算术/比较快速路径 | `+ - * / < > =` | ✅ | ✅ | `(define + (lambda (a b) 999))` → `(+ 1 2)` 报 `TypeError: Attempt to call a non-function value: +` |
| 控制流 / 高阶 | `and` `or` `apply` `eval` `map` `filter` `reduce` `for-each` `->` | ❌（`and`/`or` 除外） | ❌ | `(define map 42)` → `map` 求值为 42，但 `(map f lst)` 仍照常工作 |

即：**同一个符号，`define` 能改它的"值"，改不了它的"行为"。**

`+ - * / < > =` 的实现是 `arithmetic_fast.cj` 的 `evalAdd` / `evalArithBin` 等快速路径；
`isNativeArith()`（`arithmetic_fast.cj:280`）决定走快速路径还是回退普通调用。

### C. 纯注册函数（真靠注册表）

`cons` `car` `cdr` `list` `length` `append` `null?` `not` `eq?` `number?` `string?`
`symbol?` `list?` `procedure?` `mod` `max` `min` `sum` `reverse` `range` `str`
`print` `println` `and`→见 B …

| 维度 | 结果 |
|---|---|
| R | ✅ |
| P | ✅ |
| S | ✅ `(define car 42)` 后 `(car '(1 2))` → `TypeError: Attempt to call a non-function value: car`（符合预期：值被换掉了） |

---

## 3. 由此暴露的实现问题（三处）

### 3.1 `funcChecker` 对 A/B 类失效 → `withSandbox()` 的核心意图失效

权限检查只挂在普通调用路径 `core/src/core/eval/core.cj:233`；A 类在 `trySpecialForm` 早退，
B 类走快速路径（`checkNative` 只看 `env.lookup` 是不是 `NativeFunc`，**不看 `funcChecker`**）。

实测（默认环境 + 拉黑单个符号）：

| 符号 | 类别 | 拉黑后 |
|---|---|---|
| `length` | C | ✅ `[UndefinedFunction] Function 'length' is not allowed` |
| `and` / `or` | B | ❌ 照常返回 `true`（AST 下会报错，见 3.3） |
| `apply` / `map` / `eval` | B | ❌ 照常工作 |
| `for-each` | C | ✅ 被拦住 |

`options.cj:222` 的 `withSandbox()` 明确写着 `blockFunctions(["eval", "apply"])` —— **两者都拦不住**。

### 3.2 `and` / `or` 在 AST 求值器下不短路

它们被注册成普通 `NativeFunc`（`core/src/core/builtin/logic.cj:11`），实参先全部求值：

```
(and 0 (undefined-fn-xyz))  →  [UndefinedFunction]   ← 本该短路
(or  1 (undefined-fn-xyz))  →  [UndefinedFunction]
```

在 Lisp 里 `and`/`or` 必须短路，否则 `(and (not (null? x)) (car x))` 这类惯用法不成立。

### 3.3 两个后端语义不一致（AST vs 字节码）

`xisp_bc/src/compiler.cj:163-164` 把 `and`/`or` 当**真特殊形式**（`compileAnd`/`compileOr` 短路编译），
与 AST 求值器的"普通函数"语义相冲突：

| 表达式 | AST（默认） | 字节码 |
|---|---|---|
| `(and 0 (undefined-fn-xyz))` | `[UndefinedFunction]` | `false` |
| `(or 1 (undefined-fn-xyz))` | `[UndefinedFunction]` | `1` |
| `(apply 42 '(1 2))` | `nil`（静默吞错） | `[RuntimeError] Bytecode: apply expected function (got: 42)` |
| 拉黑 `length` 后 `(length '(1 2))` | `[UndefinedFunction]`（拦住） | `2`（**未拦住**） |

最后一行是**沙箱绕过**：`xisp_bc` 里没有任何 `funcChecker` 的引用
（只有 `interruptChecker`），VM 执行时不查宿主函数权限。

### 3.4 附带：静默吞错

- `(apply 42 '(1 2))` → `nil`
- `(map 非函数 '(1 2))` → `(nil nil)`
- `(reduce 非函数 0 '(1 2))` → `0`

类型错误被吞成 `nil`，调用方拿不到任何信号。

---

## 4. 对设计的含义（重要）

`docs/plans/2026-10-11-capability-domains-design.md` §3.4 的硬约束
（"只能用『不注册』来表达禁止"）**只对 C 类成立**：

- A 类**从未注册**，无从"不注册"——它们由编译进去的求值器提供，**任何域装配都关不掉**；
- B 类**注册了也关不掉**（硬编码优先）。

而沙箱最想收紧的 `eval` / `apply` / `map` / `filter` / `reduce` **恰好全在 A/B 类**。
所以能力域要成立，前提是先做**符号归类收敛**（见设计文档 §3.6）。

**注意**：A 类原语并非一律不可配置 —— `import` / `export` 虽是 A 类，
但已接入宿主策略（`moduleLoadChecker`），实测：

```
withNoModuleLoad()  →  (import "…") = [ModuleError] Module loading is not allowed in this sandbox
```

说明"A 类"与"可配置"并不互斥：**硬编码的是语义，宿主策略可以挂在语义内部**。
这为"必需原语 + 可选能力"的分层提供了既有先例。

---

## 4.5 补充实测：可替换性（2026-10-11 追加）

目标从"可裁剪"推进到**可替换**后，又测了一轮"用户重定义同名符号，谁生效"：

| 符号 | 类别 | 用户重定义后 | 可替换 |
|---|---|---|---|
| `car` `length` `not` `+` `<` | ③ | 用户的实现生效 | ✅ |
| `when` | ④宏 | 用户的宏生效 | ✅ |
| `and` | ②(伪) | 用户的实现生效 | ✅ | 
| `map` `filter` `reduce` | ② | `define` 成功但**调用仍走 core 实现** | ❌ |
| `let` `if` `quote` `begin` | ① | 完全无效 | ❌ |

**`and` 的特殊性**：它在**空环境**里是 `[UndefinedFunction]`，说明它不是真特殊形式；
`map`/`filter`/`reduce` 在空环境里**仍可用**（求值器直接拦截），所以能被 `define` 改值、改不了行为。

### 4.5.1 `for-each` 只认 `NativeFunc` 回调（新增问题）

`core/src/core/builtin/builtin.cj:113`：

```cangjie
if (let NativeFunc(proc) <- args[0]) { … }   // Lisp 闭包被静默跳过
```

实测 `(for-each (lambda (x) x) '(1 2 3))` → `nil`，**什么都没发生且无任何错误**。
对照 `map`（求值器实现）能正常调闭包。

**后果**：这是"用 Lisp 自行实现同能力函数"的硬门槛——Lisp 层要复刻 `map`/`for-each`，
必须能从注册函数调用**任意 Lisp 函数值**，而 `applyProcedure` 是 `internal`
（`core/src/core/eval/higher_order.cj:323`），桥接层拿不到。

> 纯 Lisp 自己实现 `map` 是可行的（实测 `my-map` 递归版行为与 core 一致），
> 因为它由求值器执行、天然能调闭包。**门槛只卡在"仓颉注册的同能力函数"上。**

### 4.5.2 由此新增两道接缝（设计侧）

| # | 接缝 | 作用 |
|---|---|---|
| A | Lisp 函数值调用入口（`applyProcedure` 公开化 / `CapabilityContext.invoke`） | 让仓颉注册函数也能调 Lisp 闭包；顺带修掉 `for-each` 的静默失败 |
| B | ②类符号"让位"机制（已由用户定义则不走硬编码） | 让 `map`/`filter`/`reduce` 可被用户实现替换 |

---

## 5. 未做的部分

本次只做**测量与记录**，未改动任何实现。修复项与优先级见设计文档 §3.6 与 §9。
