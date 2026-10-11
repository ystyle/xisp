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

## 5. 未做的部分

本次只做**测量与记录**，未改动任何实现。修复项与优先级见设计文档 §3.6 与 §9。
