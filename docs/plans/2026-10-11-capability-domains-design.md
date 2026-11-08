# Xisp 能力分层与扩展接缝设计

> **状态**：**已实施**（接缝 0 / 0a~0e、1、1b、2、3、4、6 均已完成，见 §8.1、§11、§12）；
> 当前位于分支 `feat/r5rs-symbol-layering`，**未合并 master、未发版**
> **目标版本**：待定（0.5.0 已发布，本设计的破坏性变更须并入下一个破坏性版本；
> 当前在分支上，合入时按 lockstep 推进 0.6.0）
> **前置**：工作区化（模块划分）已完成并发布，见
> [`2026-09-27-workspace-modularization.md`](2026-09-27-workspace-modularization.md)
> **下游规划**：科学计算族（`xisp_array` / `linalg` / `stats` / `signal` / `io` / `sci`）见
> [`2026-10-11-sci-family-design.md`](2026-10-11-sci-family-design.md)（**规划中**，最后做）
> **实测记录**：本文的测量细节与原始数据见 [`../bugs/symbol-layering-findings.md`](../bugs/symbol-layering-findings.md)

---

## 1. 首要目标：可替换

### 1.1 目标阶梯

本设计要解决的不是"能不能不装"，而是**"能不能换成我自己的"**：

| 层级 | 含义 | 判据 | 地位 |
|---|---|---|---|
| 可不要 | core 带了，宿主可以不要 | "不注册"是否生效 | 基础要求 |
| **可替换** | core 带了，**别人可以不要、直接用宏自行做一个** | "core 的实现会不会被顶掉" | **首要目标** |

> 用户原话："除了最基本的，所有其它的都可以不要，而且同能力（或者符号）我们 core 带了，
> 别人可以不要，直接用宏自行做一个，就是 lisp 大佬说的那样，**lisp 实现 lisp 所有**。"

对应到语言设计史：**①不可替换的那部分，正是"最基本的"**——与 McCarthy 的
`quote`/`cond`/`lambda`/`label` + 五个初等函数（`ATOM` `EQ` `CAR` `CDR` `CONS`）一脉相承，
后经 Scheme 收敛为 6 个语义原语、R5RS 定为 9 原语 + 14 派生形式。
**派生形式之所以能"派生"，就是因为它们可由原语 + 宏长出来**——这正是本设计要保证的性质。

### 1.2 可替换性实测（2026-10-11）

方法：默认环境里先重定义同名符号，再用**原语法**调用，看谁生效。

| 符号 | 层 | 用户重定义后 | 可替换 |
|---|---|---|---|
| `car` `length` `not` `cons` `str` … | ③ 注册函数 | 用户的实现生效 | ✅ |
| `+` `-` `*` `/` `<` `>` `=` | ③ 注册函数（带快速路径） | **用户的实现生效**（`(+ 1 2)` → `999`） | ✅ |
| `and` `or` | ③ 注册函数 | 用户的实现生效 | ✅ |
| `when` `unless` `incf` `swap` `do` `defun` | ④ Lisp 层宏 | 用户的宏生效 | ✅ |
| **`map` `filter` `reduce`** | ② 求值器拦截 | `define` 成功，但**调用仍走 core 的实现** | ❌ **顶不掉** |
| **`apply` `eval` `->`** | ② 求值器拦截 | 同上 | ❌ |
| `let` `if` `quote` `begin` `match` | ① 语言原语 | 完全无效 | ❌ |

**关键更正**（同日复测，见实测记录 §4.5）：算术符号**已具备让位能力**——
`isNativeArith()`（`core/src/core/eval/arithmetic_fast.cj:280`）先查 `env.lookup(op)`
是否为 `NativeFunc`，不是则**回退普通调用**（`evalAdd` 开头即
`if (!this.isNativeArith("+")) { return this.evalFunctionCall(…) }`）。
且该函数**内含 `funcChecker` 检查**，故拉黑算术符号有效：
`blockFunctions(["+"])` → `(+ 1 2)` 报 `Function '+' is not allowed`。

**所以真正顶不掉的只有"求值器直接拦截"的那批 + ①类。**

> **2026-11-06 补**：上表是 2026-10-11 的实测快照。让位机制（§3.2）与一等值化（§11.4）
> 实施后，②类也能被用户 `define` 顶掉（`(define map (lambda (f l) …))` 后调用走用户实现），
> 只剩①类仍不可替换。见 §8.1 接缝 0b / 0e 与 §12。

### 1.3 判别方法：空环境探测

判断一个符号属哪层，最简办法是看它在**空环境**（`Environment()` + `Evaluator`，不注册任何东西）里：

| 空环境里 | 含义 | 例 |
|---|---|---|
| 报 `UndefinedFunction` | 只是注册物（或"注册后才生效的快速路径"）→ **可替换** | `+ - * / < > =`、`and` `or`、`->`、`for-each`、`cons` `car` |
| **仍可用** | 求值器直接拦截 → **顶不掉** | `map` `filter` `reduce` `apply` `eval`、`let` `if` `quote` `begin` `match` |

### 1.4 现状的两个缺口

**缺口一：②类顶不掉。** `map`/`filter`/`reduce`/`apply`/`eval`/`->` 是"能纯 Lisp 写出来"的高阶函数
（实测 `my-map` 纯 Lisp 递归实现、行为与 core 一致），却被求值器硬编码占住了名字。

**缺口二：`for-each` 只认 `NativeFunc` 回调**（用仓颉注册同能力函数的技术门槛）：

```cangjie
// core/src/core/builtin/builtin.cj:113
if (let NativeFunc(proc) <- args[0]) { … }   // Lisp 闭包被静默跳过
```

实测 `(for-each (lambda (x) x) '(1 2 3))` → `nil`，**什么都没发生且不报错**。
对照 `map`（求值器实现，走内部 `applyProcedure`）能正常调闭包。

> **关键**：Lisp 层要复刻 `map`/`for-each`，必须能从注册函数调用**任意 Lisp 函数值**，
> 而 `applyProcedure` 是 `internal`（`core/src/core/eval/higher_order.cj:323`）。
> 纯 Lisp 实现没这个问题（由求值器执行、天然能调闭包）——**门槛只卡在"仓颉注册的同能力函数"上**。

---

## 2. 四层模型

### 2.1 四层（本文唯一的符号分类法）

| 层 | 符号（现状） | 实现位置 | 一等值 | 可拦 | 可不要 | 可替换 |
|---|---|---|---|---|---|---|
| **① 语言原语** | `if` `quote` `backquote` `comma` `define` `defmacro` `set!` `lambda` `begin` `let` `let*` `when-let*` `if-let` `condb` `match` `macroexpand` `import` `export` | 求值器硬编码（`trySpecialForm`，33 条 `case`） | ❌ | ❌ | ❌ | ❌ |
| **② 求值器拦截** | `map` `filter` `reduce` `apply` `eval` `->` | 求值器硬编码 | ❌ | ❌ | ❌ | ❌ **（目标 ✅）** |

> **2026-10-11 进展**：`and` / `or` 已从③（注册函数）**降为④（宏）**，两个后端的短路语义随之统一。
> 详见 §2.5。`let` / `let*` / `condb` / `if-let` / `when-let*` / `->` 的降级可行性已实测（§2.5），
> 待实施。
>
> **2026-11-06 订正**（表内"现状"列已过时，按实际记录）：②类已**一等值化 + 可拦 + 可替换**
> —— `(procedure? map)` → `true`、用户 `define` 同名即让位（§8.1 接缝 0b/0e、§11.4、§12）；
> `let*` / `condb` / `if-let` / `when-let*` 已在 `core/src/core/builtin/macros.cj` 以 `defmacro`
> 提供、`core.cj` 里不再有对应 `case`（即已属④层）；
> `for-each` 已从③**移入②**（求值器实现，`core/src/core/eval/for_each.cj`，见 §12）。
| **③ 注册函数** | `+ - * / < > =`、`and` `or`、`cons` `car` `cdr` `list` `length` `append` `not` `eq?` `number?` `str` `print` `for-each` … | 仓颉 `registerXxx`（部分带快速路径） | ✅ | ✅※ | ✅ | ✅ |
| **④ Lisp 层宏** | `when` `unless` `incf` `decf` `swap` `push` `pop` `negate` `do` `defun` | **Lisp 源码**（`BuiltinMacros`，启动时 `eval`） | ❌（是 Macro） | — | ✅ | ✅ |

※ `+ - * / < > =` 与 `and`/`or` 被拉黑时行为已验证有效；③类的可拦性来自"注册物 + 回退普通调用"。

**只有①不可替换——那就是"最基本的"。** ②被现状错误地钉在了①那一侧，这是本设计要修的核心。

### 2.2 ①的边界就是「lisp 实现 lisp」的边界

①类由编译进去的求值器提供，**任何域装配都关不掉**：实测 `(define if 42)` 后
`(if 1 2)` 仍返回 `2`，`(define let 42)` 后 `(let ((x 1)) x)` 仍返回 `1`。

这与"lisp 实现 lisp 所有"并不矛盾——**这条论断本来就是"除最基本的外"**。
① 就是那"最基本的几个"，②③④ 都应可替换。

### 2.3 ①也可携带宿主策略

**①不可"移除"，但可以携带宿主策略开关。** 既有先例：`import` / `export` 是①类原语，
但内部已接 `moduleLoadChecker`，实测：

```
withNoModuleLoad()  →  (import "…") = [ModuleError] Module loading is not allowed in this sandbox
```

所以"必有的原语"与"可配置的能力"不互斥：**硬编码的是语义，宿主策略挂在语义内部**。
这条为"①类能力也纳入域装配框架"提供了依据（如"给不给脚本模块加载能力"）。

### 2.4 目标态

| | 现状 | 目标 |
|---|---|---|
| ① | 不可替换、不可拦 | 保持不可替换；**补上宿主策略入口**（按 §2.3 模式） |
| ② | 不可替换、不可拦、非一等值 | **可替换 + 可拦 + 一等值**（`(procedure? map)` 为真、可作值传递） |
| ③ | 已可替换、可拦 | 保持；**修 `for-each` 回调限制**（§1.4 缺口二） |
| ④ | 已可替换 | 保持；**宏包化**（§4.2），使"用宏顶掉 core 实现"有正式形态 |

---

### 2.5 降级的可行性实测（2026-10-11）

「把派生形式降为宏」曾被认为会毁掉字节码后端 —— **该顾虑已被证伪**。
原因是宏展开的时机：修复前宏在**运行时**展开，任何含宏的表达式都会让整棵树回退 AST
（实测递归函数体内含一个宏：字节码 10767us vs 不含宏 566us，差 19 倍）；
修复后宏在**编译前**展开（§3.4），展开产物本身就是可编译的普通代码。

| 形态 | AST 模式 | 字节码模式 |
|---|---|---|
| 内建 `let`（特殊形式） | 36978us | 1011us |
| **宏版 `let`**（展开成 lambda 应用） | **24671us（更快）** | **1043us（≈持平）** |

即：**降级为宏不再有性能代价，AST 下甚至更快**。

#### 已完成的降级

| 形式 | 原归属 | 现归属 | 理由 |
|---|---|---|---|
| `and` / `or` | ③注册函数 | **④宏** | 原为 `NativeFunc`（实参先全求值，不短路），而字节码按短路编译 → 实测 4 处语义分歧。改宏后两后端走同一展开产物，分歧归零 |

#### 待降级对象的等价性约束（逐个已核）

| 形式 | 降级难点 | 结论 |
|---|---|---|
| `condb` | 含 `:let` 绑定子句 + `(条件 结果 …)` 对 + `else` | 可降：`:let` 归约为 `let`，子句归约为嵌套 `if` |
| `if-let` | 无（现实现本身就是"展开为 `let`+`if`"） | 可降，直接宏化 |
| `let*` | 现实现本身是"嵌套 `let`" | 可降，直接宏化 |
| `when-let*` | 顺序绑定 + 末值真值判断 + 真则求 body | 可降：归约为嵌套 `let` + `if` |
| `->` | **参数重排**：`map`/`filter`/`reduce`/`apply` 要把管道值插到**第二位**（实测 examples 依赖此行为）；且列表值需 `quote` 防二次求值 | 可降，但宏体需带这张特殊参数序表 |
| `let` | 支持**解构**（点对 / 向量 / HashMap 模式），且是**并行绑定**（`(let ((x 1) (y x)) y)` → `x` 未定义） | 可降但复杂：宏展开需处理解构模式；或保留为①的"编译可见形式" |

#### ① 的收敛目标

| 归属 | 成员 | 判据 |
|---|---|---|
| **① 核心原语** | `quote` `lambda` `if` `set!` `define` `defmacro` | 理论不可省（与 Scheme 的 6 语义原语一致） |
| **① 编译可见形式** | `let`（含解构）、`match`、`import` / `export` | 有专用编译路径或宿主语义；降级收益不足 |
| **④ 宏** | `and` `or` `condb` `if-let` `let*` `when-let*` `->` + 既有 10 个内建宏 | 纯派生，可完全用宏表达 |

> `match` 理论可派生（`if` + `car`/`cdr`），但工程上留①更合理（`compileMatch` 有专门实现）。

---

## 3. 接缝设计（达成可替换）

### 3.1 接缝 A：Lisp 函数值调用入口

**目的**：让仓颉注册的函数也能调用任意 Lisp 函数值——这是"用户用仓颉复刻 `map`/`for-each`"的前提，
同时修掉 §1.4 缺口二。

```cangjie
// 方案一：公开求值器入口（applyProcedure 现为 internal）
public func apply(procValue: LispValue, args: ArrayList<LispValue>): LispValue

// 方案二：经 CapabilityContext 暴露（推荐，与域装配同源，见 §3.3）
public let invoke: (LispValue, ArrayList<LispValue>) -> LispValue
```

需要覆盖的三类函数值（`applyProcedure` 自身已在处理，公开即可）：
`Procedure` / `ProcedureFromParams` / `Closure`（编译闭包，经 `ExecutionBackend.callClosure`）。

> 顺带收益：`for-each` 改成用 `invoke` 之后，"只认 `NativeFunc`"的静默失败自然消失。

### 3.2 接缝 B：②类「让位」机制

**目的**：②类符号若已由用户定义，求值器不再走硬编码。

**算术符号已有可复用的实现模式**（`+ - * / < > =`）：

```cangjie
// core/src/core/eval/arithmetic_fast.cj:26
func evalAdd(rest: LispValue): LispValue {
    if (!this.isNativeArith("+")) {
        return this.evalFunctionCall(Symbol("+"), rest)   // ← 让位：回退普通调用
    }
    …
}
```

`isNativeArith` 用 **`env.arithVersion` 版本号缓存**做守卫（环境变了才重算），
所以热路径没有额外开销。②类照此模式补即可：

| 符号 | 让位判据 | 备注 |
|---|---|---|
| `map` `filter` `reduce` `apply` `eval` | 环境里是否有用户绑定（复用版本号缓存） | 目前完全不查环境 |
| `->` | 同上 | 目前无条件展开 |

而"一等值"要求（`(procedure? map)` 为真）比"让位"更进一步：
需要求值器**取到函数值再决定如何调用**，而不是把符号名硬编码进分派。

### 3.3 接缝 C：`CapabilityContext`

域装配与外部模块需要的不止解释器，还需要中断检查器、类型声明通道、函数值调用入口：

```cangjie
// ystyle::xisp（core 侧，草案）
public class CapabilityContext {
    public let env: Environment
    public let isInterrupted: () -> Bool                                  // 长循环里周期性调用
    public let register: (String, (ArrayList<LispValue>) -> LispValue) -> Unit
    public let registerType: (String) -> Unit                             // 声明本域产生的外部类型名，供沙箱审计
    public let invoke: (LispValue, ArrayList<LispValue>) -> LispValue     // 接缝 A
    public let evalSource: (String) -> Unit                               // 以 Lisp 源码安装（§4.2）
}
```

**中断契约**（写进模块 README 与域声明）：

> 外部模块中任何 O(元素数) 以上的循环，必须周期性调用 `ctx.isInterrupted()`，
> 为真时以 `Error(RuntimeError, INTERRUPTED_MESSAGE)` 中止。

core 已有 `setInterruptChecker`（`evaluator.cj`），`INTERRUPTED_MESSAGE` 也已是 public。
但沙箱 `timeout` 与宿主取消**只能打断求值热路径**——域模块里一个跑 30 秒的循环若不主动查中断，
宿主就**杀不掉它**。

---

### 3.4 接缝 D：宏改为**编译前**展开（0/0b 的前提）

**这是本轮最关键的修正。** 宏本是语法层的东西，却一直放在**运行时**展开
（AST 在 `evalListInternal` 遇到宏才展开；VM 用 `callMacro` 每次调用重新展开）：

- **性能**：宏调用被编译成"调宏指令"，每次执行都展开一次并起子求值器。
  实测递归函数体内含一个宏：字节码 10767us vs 不含宏 566us（**差 19 倍**）；
- **语义**：编译路径下宏参数**已被求值为值**，VM 只能把值 `quote` 回去冒充语法
  （`vm.cj` 的 `buildCallExpr` 注释自陈"常见宏…与 AST 完全等价"）——
  对"把参数当语法分析"的宏就**不等价**，是静默错误。

改法：`evalInner` 分派后端**之前**，若表达式含宏则先展开，再用展开产物走正常编译。
新增 `expandForCompile`（尊重求值边界的展开）与零分配探测 `containsMacroCall`。

> **不能复用 `macroExpandAll`**：它是 `(macroexpand-all …)` 的语义，递归展开一切，
> 包括 `quote` 内部（实测 `(quote (quote (when …)))` 会被展平）——那是**数据**。
> `expandForCompile` 换判据为"只展开会被求值的位置"，遇 `quote`/`quasiquote`/`defmacro`
> 即停；而 `lambda` 与 `define` 的函数体**要**展开（函数体是会被求值的代码）。

效果：含宏递归 10767us → **971us**；宏展开次数由"每次调用 1 次"变为
**定义时 1 次、此后恒定**；AST 与字节码语义一致。

### 3.5 接缝 E：`gensym`

`defmacro` 与调用点共享符号空间（非卫生宏），宏体里的临时变量会捕获用户的同名变量
（实测 `(defmacro my-or (a b) …(quote tmp)…)` 在 `(let ((tmp 5)) (my-or #f tmp))`
下返回 `false` 而非 `5`）。卫生宏由语言机制解决；非卫生宏的通行解法是
`gensym`：`(gensym)` → `#:g12`，`(gensym "prefix")` → `#:g12-prefix`。

计数器用 `AtomicInt64`：`gensym` 天然是全局名字源（CL 亦如此），且 `NativeFunc`
的 handler 拿不到求值器（正是接缝 0a 要解决的事）。

---

## 4. 域装配模型（core 内置域 + 宿主自定义域）

### 4.1 一个域 = 五要素

1. **注册集合**（哪些顶层符号）—— 靠"不注册"表达禁止（边界见 §5.3）；
2. **实现位置**（core 内 / 独立模块）；
3. **默认开关状态**（解释器默认给不给）；
4. **属性**：`escapable` / `hostPolicy` —— 决定它能否进 `PRESET_PURE`（§4.4）；
5. **安装顺序约束**（`after`）—— 别名域必须晚于被别名域。

**`LispCapability` 是 L1（core 内置域）与 L2（宿主自定义域）共用的同一个抽象**：

```cangjie
public interface LispCapability {
    prop name: String                     // 域标识，如 "net"
    prop symbols: Array<String>           // 本域注册的顶层符号，供沙箱审计与自省
    prop escapable: Bool                  // 能否绕过权限检查（默认 true = 保守）
    prop hostPolicy: Bool                 // 是否触及宿主资源（文件/网络/进程/输出）
    prop after: Array<String>             // 安装顺序约束（如别名域 after ["arith"]）
    func install(ctx: CapabilityContext): Unit
}

public func withDomains(caps: Array<LispCapability>): InterpreterOption
public func withPreset(preset: Array<LispCapability>): InterpreterOption
```

对 `xisp_net` 这类外部域：它依赖 core 并提供 `NetCapability`，**core 完全不认识 net** ——
依赖方向正确，且对字节码 / JIT 后端同样适用（可与工作区化文档 §3.2 的执行后端接缝统一）。

### 4.2 install 的两种形态（对应能力的两种来源）

| 形态 | 用在哪 | 例 |
|---|---|---|
| `ctx.register(name, handler)` | 需要性能或宿主能力时（仓颉实现） | `car` / `+` / `read-file` |
| `ctx.evalSource(source)` | 纯语法糖、可用纯 Lisp 表达的能力 | **④层宏包** |

④层内建宏现在就是这么实现的——`when` / `unless` / `incf` / `decf` / `swap` / `push` /
`pop` / `negate` / `do` / `defun` **全是 Lisp 源码字符串**，启动时 `Parser.parseFromString`
+ `evaluator.eval` 装进环境（`core/src/core/builtin/macros.cj`）。

**所以"宏包即域"**：

```cangjie
public class MacroPackCapability <: LispCapability {
    public prop name = "macros.core"
    public prop symbols = ["when", "unless", "incf", "decf", "swap", "push", "pop", "negate", "do", "defun"]
    public prop escapable = false          // 纯语法糖，不逃逸
    public prop hostPolicy = false
    public prop after = ["base", "list"]   // 展开结果依赖 if/begin/cons
    public func install(ctx: CapabilityContext): Unit {
        ctx.evalSource(SOURCE)
    }
}
```

**用户想用宏顶掉 core 的③类实现，就是提供同名域**（`after` 保证顺序、`symbols` 用于重名检出）。

#### 4.2.1 实现记录（2026-10-11）

宏包已落地：16 个内建宏合并为**一份 Lisp 源码** `BUILTIN_MACROS_SOURCE`
（`core/src/core/builtin/macros.cj`，约 90 行），由 `evalSource` 一次装进环境。
原先它们是 16 个仓颉函数、各带同样的样板，文件 286 行 —— 合并后约 150 行。

顺带修掉一个卫生性隐患：`swap` 原用固定符号 `temp` 作临时变量，
非卫生宏下会被调用点同名变量捕获，现改用 `gensym`。

`BuiltinMacros.allSymbols()` 是宏包符号清单的**唯一来源**，能力域 `symbols` 复用它，
避免清单与实现漂移。

### 4.3 预置组合（preset）

`registerAll` / `registerPure` 重新表述为**预置组合**，而非两个函数：

```cangjie
// core 内置域（实际 13 个，见 core/src/core/builtin/domains.cj）
// **每个域与一个注册器一一对应**，`symbols` 声明即"该域安装后确实可用"的符号集。
BuiltinFunctions.domainBase()          // nil
BuiltinFunctions.domainArithmetic()    // + - * / mod round sum product max min
BuiltinFunctions.domainComparison()    // = < > <= >=
BuiltinFunctions.domainList()          // cons car cdr list length nth append reverse range
BuiltinFunctions.domainLogic()         // not eq?
BuiltinFunctions.domainPredicates()    // number? string? null? procedure? type-of external? …
BuiltinFunctions.domainHashMap()       // hashmap-* hashmap? 与 Redis 风格别名
BuiltinFunctions.domainString()        // str / string-length / string=? …（纯计算）
BuiltinFunctions.domainPrint()         // print / println / princ / newline / display（hostPolicy）
BuiltinFunctions.domainSymbols()       // gensym
BuiltinFunctions.domainAliases()       // first / rest / second … setq / progn / string-append
BuiltinFunctions.domainErrorHelpers()  // error / raise / assert
BuiltinFunctions.domainMacrosCore()    // ④层宏包（16 个，evalSource 形态）

// 预置组合（命名 + 域列表；宿主可自造）
BuiltinFunctions.presetFull()   // = coreDomains()
BuiltinFunctions.presetPure()   // 按 !escapable && !hostPolicy 推导（§4.4）
withDomains([domainArithmetic(), domainList(), MyOwnDomain])   // 宿主自定义组合
```

> **域边界 = 注册器边界**（2026-11-08 对齐）：曾经 `arithmetic` 捆绑 3 个注册器、
> `list` 还捎带 `sum/product/max/min`、`logic` 捆绑全部谓词，导致"声明 ≠ 安装"
> ——`sum` 声明在算术域却只在 `list` 域装得上。现在一个域装一个（或一组同语义的）
> 注册器，`symbols` 声明即事实；逐域护栏见 `testEachDomainDeclaresTruthfully`。
>
> **两档装配不再各有清单**：`registerAll` = `installDomains(env, presetFull())`，
> `registerPure` = `installDomains(env, presetPure())` + 可选输出接管。
> 此前是两份平行清单（`registerAll` 逐个调注册器、域集合另列一份），
> 漂移不会被任何测试发现 —— 实测漏掉 `print` 域与 `aliases` 域共 20+ 个符号，
> 详见 §12.4。现由 `testPresetFullCoversRegisterAll` / `testPresetPureCoversRegisterPure`
> 反向钉住等价性。

- 组合语义是**并集**：逐域 `install`（顺序 = 声明顺序，受 `after` 约束）；
- **顺序敏感**：别名域必须在被别名域之后（现有 `registerModernAliases` 已依赖这个事实）；
- **减法只对 core 预定义的组合成立**（在 core 内部按属性推导）。
  宿主若想从全量里减，只能自己列出要装的域——因为对已安装的环境做"删符号"违背 §5.3
  （逃逸域可能已执行过）。

### 4.4 域属性 → `PRESET_PURE` 由推导得出

| 属性 | 含义 | 例 |
|---|---|---|
| `escapable` | 该域能否绕过权限检查（`eval` / `apply` / 高阶回调） | `higher` = true |
| `hostPolicy` | 该域是否触及宿主资源（文件 / 网络 / 进程 / 输出） | `io` / `fs` / `net` = true |

规则：`PRESET_PURE` = 所有 `!escapable && !hostPolicy` 的域。
**新增域默认 `escapable = true`（保守），须显式声明才进 pure** —— 默认安全。

这样 pure 不再是手写清单，而是**推导结果**——顺带消除 §5.2 的"两份平行清单 + 隐式依赖"问题。

### 4.5 与 `InterpreterOption` 的关系

宿主侧保留既有风格（`withXxx`），域只是一个新的选项类型：

```cangjie
public func withDomains(caps: Array<LispCapability>): InterpreterOption
public func withPreset(preset: Array<LispCapability>): InterpreterOption
```

不动现有 31 个 `withXxx` 的心智模型，只把"装哪些符号"从两个硬编码函数扩展成
"可选组合 + 自定义域"。

### 4.6 域清单与现状

**A. core 域（编译进 core；目前大多无开关，建议按 §4.1 补上按域装配）**

| 域 | 符号示例 | 层 | 现注册点 |
|---|---|---|---|
| base | `nil`、关键字别名 | ③/① | `registerAll` 头部 / `env.registerKeywordAlias` |
| arithmetic | `+ - * / mod abs min max` | ③ | `registerArithmetic` / `registerModulo` / `registerMath` |
| comparison | `= < > <= >=` | ③ | `registerComparison` |
| list | `car` / `cdr` / `cons` / `length` | ③ | `registerListOps` |
| higher-order | `map` / `filter` / `reduce` / `apply` / `for-each` | **②** | `registerHigherOrder` |
| logic | `and` / `or` / `not` | ③ | `registerLogic` |
| predicates | `null?` / `number?` | ③ | `registerPredicates` |
| hashmap | `hashmap-*` | ③ | `registerHashMap` / `registerRedisAliases` |
| macros | 内建宏（10 个） | **④** | `BuiltinMacros.registerAll` |
| aliases | 现代 / Redis 别名 | ③ | `registerModernAliases` |
| string & output | `str` / `print` / `println` | ③ | `registerStringOps` / `registerPrint` / `registerOutput` |

> 注意 `higher-order` 域横跨②③：`map`/`filter`/`reduce`/`apply` 是②（求值器拦截），
> `for-each` 是③（注册的，但回调受限）。**域化时必须拆开**，否则无法按层施加不同策略。

**B. 宿主策略域（core 内，运行期开关，已实现）**

| 域 | 现开关 |
|---|---|
| stdio 桥接 | `withStdIO()` |
| fs 桥接 | `withStdFS()` |
| collection 桥接 | `withStdCollection()` |
| 模块加载 | `withNoModuleLoad()` / `withAllowedModulePaths()` / `withModuleMode()` |
| 文件写 | `withNoFileWrite()` / `withAllowedPaths()` |
| 函数黑/白名单 | `withBlockedFunctions()` / `withAllowedFunctions()` |
| 输出通道 | `withOutputFn()` |
| 纯函数子集 | `BuiltinFunctions.registerPure(env, output!)`（§4.3 建议收敛为 preset） |

**C. 只靠拆模块能解决的域（有外部依赖 / 平台特化）**

| 域 | 模块 | 外部依赖 | 状态 |
|---|---|---|---|
| bytecode + jit | `xisp_bc` | 无外部依赖；含 `rwx`（JIT，仅 Linux x86-64） | ✅ 已拆出并发布 |
| net（http / websocket / tls，见 §6.2） | `xisp_net` | stdx.net.* / stdx.crypto | 规划 |
| data（compress / serialization / json） | `xisp_data` | stdx.compress.* / stdx.serialization | 规划 |
| async | 见 §6.3 | 可能零外部依赖 | 规划 |

### 4.7 命名与符号冲突

- **模块名不能含连字符**（cjpm 要求合法标识符）：仓库可以叫 `xisp-net`，但模块必须是
  `xisp_net`，包名 `ystyle::xisp_net`；
- 扩展域会引入大量顶层符号（`matmul` / `mean` / `fft` / `shape`），与 core 内建**重名风险高**。
  建议：域注册时声明命名空间或统一前缀，并**把符号清单作为域声明的一部分**
  （§4.1 的 `LispCapability.symbols`）。好处有三：沙箱可审计、文档可自动生成、
  **重名能在装载时被检出**，而不是运行时静默覆盖。
- 这条对**用户自定义域同样适用**：用户用宏顶掉 core 的③类实现时，靠 `symbols` 冲突检测
  才能发现"我少写了一个符号、旧实现还在"（§4.2）。

---

## 5. register 收敛

### 5.1 现状：三条路径，三种粒度

| 路径 | 粒度 | 缺口 |
|---|---|---|
| `BuiltinFunctions.registerAll(env)` | **全量**（14 个子注册函数一次全上） | 只有 all / pure 两档，中间粒度缺席 |
| `BuiltinFunctions.registerPure(env, output!)` | **纯函数子集**（面向最小权限宿主） | 与 `registerAll` 是**两份平行清单**，靠人肉同步 |
| `Bridge.registerFunc` / `registerFuncWithNS` / `registerStdIO` / `registerStdFS` / `registerStdCollection` | **零散函数 / 桥接组** | 无域标识，沙箱审计拿不到清单 |

外加 `InterpreterOption` 上的若干开关（见 §4.6 B）。

### 5.2 `registerAll` 与 `registerPure` 的差异（实测，2026-10-11）

两份清单不是"包含关系"，而是**两个不同的集合**：

| 子注册函数 | `registerAll` | `registerPure` |
|---|---|---|
| `nil` 字面量 | ✅ | ✅ |
| arithmetic / modulo / math / comparison | ✅ | ✅ |
| listOps / higherOrder / logic / predicates | ✅ | ✅ |
| hashMap / 内建宏 / Redis 别名 / 现代别名 / errorHelpers | ✅ | ✅ |
| **`registerPrint`**（print / println / princ / newline / display） | ✅ | ❌（改为按 `output` 参数注册 `registerOutput`） |
| **`registerStringOps`**（`str` 等） | ✅（经 `registerPrint` **间接**） | ✅（**显式调用**） |

> `str` 在两条路径上都可用，但**依赖关系是隐式的**：谁调整 `registerPrint` 的内部组成，
> `registerAll` 就静默丢掉 `str`，而 `registerPure` 不受影响。
> 这类"平行清单 + 隐式依赖"正是要收敛的对象（§4.4 的推导式 `PRESET_PURE` 即是解）。

### 5.3 硬约束及其边界

`registerPure` 的文档注释已经写死了作用点，本设计必须继承：

> 权限检查对特殊形式无效 —— `eval` / `apply` / `map` / `filter` / `reduce` / `for-each`
> 会先于检查执行，且函数值可被当作参数传递，因此「注册后过滤」不可靠，
> **只能用「不注册」来表达禁止**。

即：**运行期装配的作用点是 `env.define` 的那一刻，不是调用那一刻。**

但**注意其边界**（§1.2、§2.1 的实测）：这条约束**只对③类有效**——
① 从未注册（"不注册"无从谈起），② 注册了也关不掉（求值器拦截）。
所以：

- ③类：靠"不注册"即可（现状已成立）；
- ②类：**必须先在 §3.2 做"让位"**，否则"不注册"对它无效；
- ①类：不可移除，只能按 §2.3 携带宿主策略。

**这条边界是本设计相对早期草案的重要修正**：早期以为"能力域 = 注册集合"可以覆盖全部符号，
实测表明它只覆盖 1/3。

### 5.4 `funcChecker` 的覆盖缺口（实现问题，见实测记录）

| 符号层 | 拉黑后是否生效 | 原因 |
|---|---|---|
| ③ 注册函数（含算术、`and`/`or`、`for-each`、`length`） | ✅ | 走普通调用路径，`funcChecker` 生效 |
| ② 求值器拦截（`map` `filter` `reduce` `apply` `eval` `->`） | ❌ | 求值器早退，不到检查点 |
| ① 语言原语 | ❌ | 同上（`trySpecialForm` 早退） |

`withSandbox()` 明确写着 `blockFunctions(["eval","apply"])` —— 实测**两者都拦不住**。

---

## 6. 外部依赖域

### 6.1 stdx 只能走二进制依赖，及其真实代价

**结论：stdx 的三种"源码依赖"名义上存在，实际不可用。**（评审反馈 + 工作区 20+ 项目一致实践）

| 方式 | 官方文档说法 | 实际 |
|---|---|---|
| git / 本地 / 中心仓源码依赖 | 支持 | 需把 stdx 连同它依赖的第三方 C 库一起编译（python / cmake(≥3.16.5,<4) / ninja / openssl≥3 / clang 15.x / Windows 下 mingw-w64）；中心仓方式还得不到可静态依赖的产物，部署复杂度直接上台阶 |
| **二进制依赖（`CANGJIE_STDX_PATH`）** | — | **唯一好用**，对齐 SDK 自带的预编译产物 |

所以 stdx 一律按二进制依赖处理：

```toml
[target.x86_64-unknown-linux-gnu.bin-dependencies]
  path-option = ["${CANGJIE_STDX_PATH}"]
# … 各平台各列一遍（参考 cordis-cj/cjpm.toml）
```

#### 实测一：`bin-dependencies` **会传递给消费者**（好消息）

独立模块 `probe_app3` 依赖另一个独立模块 `probe_lib`（后者 `import stdx.crypto.digest`
并声明了 `bin-dependencies` + `link-option`），而 `probe_app3` 自己**什么都不声明**：

| 宿主构建环境 | 结果 |
|---|---|
| 有 `CANGJIE_STDX_PATH` | ✅ `cjpm build success` |
| 无 `CANGJIE_STDX_PATH` | ❌ `root package 'ystyle::probe_lib' imports package 'stdx.crypto.digest' ... but it is not added as a dependency in cjpm.toml` |

即：**宿主不必抄 stdx 的样板，但环境变量必须在**。
（前提是**被依赖的库自己声明了** —— 制品里的 `cjpm.toml` 带着它，见工作区化文档 §5.2 实测二。）
并且那条报错**指向错误方向**（说"cjpm.toml 里没加依赖"，实际缺的是环境变量），诊断成本高。

#### 实测二：环境变量的路径写法因人而异（本设计要正面处理的问题）

`cjvs stdx-env zsh` 只导出**一个**变量，路径由三段拼成：

```
CANGJIE_STDX_PATH=/home/<user>/.config/cjvs/stdx/<stdx-version>/<platform-triple>/static/stdx
```

而这三段在不同安装方式下都不一样：cjvs 管理 / SDK 手装 / DevEco 自带，路径深度各不相同；
cjvs 下 `CANGJIE_HOME` 甚至是**每次会话变化的临时目录**。于是"写死一个路径"在团队内不可行，
只能依赖变量——而变量在 CI / 容器 / 新机器上默认不存在。

**设计对策**：

1. **把 stdx 关进能力域模块，core 与其它成员一律不得 `import stdx.*`。**
   这样"不需要网络/压缩的宿主"**完全不接触这个变量**——这是拆模块最实际的收益；
2. **由用到 stdx 的成员各自完整声明** `bin-dependencies` / `link-option`（全平台各一条）。
   **不要只写在 workspace 根**：根那份只对"在根目录跑本地构建"生效，
   在成员目录内 `bundle` / `publish` 时**不读根**（实测，见工作区化文档 §5.2 实测二）；
3. **能力域模块的 README 必须写明**：构建环境需先 `eval "$(cjvs stdx-env zsh)"`（或等价配置），
   并**明确警告 cjpm 的报错会指向 cjpm.toml、掩盖环境变量缺失**；
4. 选做：提供探测/体检脚本（`CANGJIE_STDX_PATH` 存在且目录内有 `libstdx.*` 才放行），
   接进 CI，避免拿 cjpm 那条误导性报错去排查。

**FFI 链接选项同样不外泄**：用到带 FFI 的 stdx 子库（`stdx.net.tls`、`stdx.crypto.x509` 等）
需按官方文档补 `-lstdx.net.tlsFFI -lcangjie-dynamicLoader-opensslFFI` 之类 —— 关在域模块内。

### 6.2 网络域的分层选型（决定要不要外部依赖）

网络能力**不是非 stdx 不可**，按协议层次分三档：

| 档 | 能力 | 依赖 | 归属 |
|---|---|---|---|
| 传输层 | TCP / UDP / UDS socket | **`std.net`（标准库，零外部依赖）** | 可留 core 或零依赖模块，运行期装配 |
| 应用层 | HTTP / WebSocket / TLS | `stdx.net.*` + `stdx.crypto`（FFI + 环境变量） | **必须拆 `xisp_net`** |
| 备选 | 第三方纯仓颉 HTTP 库（中心仓已有若干，如 `soulsoft_net_http` 等） | 待确认其自身是否依赖 stdx | **待调研**，判据：该库是否纯 `std.net` 实现 —— 若是，网络域也能做到零环境变量 |

（已查证：`std.net` 只有传输层 UDP/TCP/UDS，HTTP 与 TLS 都在 stdx。）

### 6.3 异步域的定位（不急于拆）

仓颉的 `async` / `await` 是**语言级**能力（std.core），不引入外部依赖，因此：

- **脚本语法层的 async 求值语义** → 属 core（需要 `Evaluator` 支持，不是模块问题）；
- **与宿主事件循环 / 线程池对接** → 属**宿主策略域**（运行期注入运行时），也不需要拆模块；
- **若采用 `stdx.actors`** → 转为**外部依赖域**，拆 `xisp_async` 模块。

即：异步的归属**取决于实现选型**，建议在选型确定前不预设模块，先用运行期装配表达。

---

## 7. 扩展接缝：外部值（`External` 变体 + 协议族）

> 这是整份设计中**唯一"只有 core 能给"**的东西：外部模块（NDArray、分布对象、句柄…）
> 要把自己的值交给 Lisp 层，就必须有一个 core 承认的值形态。
> 服务对象是下游的科学计算族（见 sci 文档），与 §1 的"可替换"目标正交。

### 7.1 硬约束：现在的 core 无法承载"外部对象"这种值

`LispValue` 是**封闭枚举**（`types/types.cj`），共 15 个变体：

```
Nil / Boolean / Int / Float / Str / Symbol / Cons
Procedure / ProcedureFromParams / Macro / MacroFromParams
NativeFunc / Closure / HashMap / Error
```

**没有任何"外部 / 宿主对象"变体。** 而 `bridge` 里的 `LispConvertible` / `LispDeserializable` /
`ExtendLispValue` 是**转换协议**——它们把仓颉类型转成上面 15 种之一（或从其一转回），
**不能新增变体**。

后果：外部模块的值**只能**退化成 `HashMap` 或 `Cons`（逐元素一个 `LispValue`）：

| | 想要 | 退化的后果 |
|---|---|---|
| 内存 | 连续缓冲区 + shape / strides | 1e6 元素 = 1e6 个装箱 `LispValue` |
| 视图 / 切片 | 零拷贝 view | 只能整份复制 |
| FFI 互操作 | 把指针直接交给 C | 无法表达 |
| 性能 | 与 numpy 同级的目标 | 数量级退化 |

### 7.2 接缝形态：为什么必须是「core 加一个变体 + 一个协议」

**先回答"是不是直接做到 core 里"：是，但 core 里只增加两样东西——**

1. `LispValue` 的**一个**变体：`External(LispExternal)`；
2. **一个**协议接口（约 20~40 行）。

外部对象的内存布局、算法、外部库调用，**一行都不进 core**。core 保持"不认识它们"。

**为什么绕不开**（逐个排除替代方案）：

| 方案 | 结论 |
|---|---|
| 用 `HashMap` / `Cons` 表示外部对象 | ❌ 1e6 元素 = 1e6 个装箱 `LispValue`，连续缓冲与原生身份全丢 |
| 用 `LispConvertible` / `LispDeserializable` | ❌ 它们是"映射到现有 15 个变体"，**不能新增变体** |
| 把 `LispValue` 从 `enum` 改成 `interface` | ❌ 全量重构 + 每个值一次虚分派，比现在更慢 |
| 用 `Any` 关联值（变体 `Host(Any)`） | ⚠️ 仍要加变体，但丢类型安全（任何值都能塞）；协议方案严格更优 |
| 把外部类型的实现做进 core | ❌ core 被具体领域绑架，与"嵌入式精简核心"直接矛盾 |
| **变体 `External(LispExternal)`** | ✅ 唯一同时满足**类型安全 / core 零依赖 / 零拷贝**的形态 |

**语言层实测**（cjpm 1.1.3，最小工程，已清理）：

- 接口可以作为枚举关联值 ✅；
- `non-exhaustive patterns` 是**编译错误**（不是警告）→ 加变体必然被编译器逐处定位，不会漏改（好事）；
- **踩到的语法细节**：枚举构造器名**不能与接口名同名** —— `| Ext(Ext)` 会被解析成类型模式，
  报 `enum pattern's parameters size is wrong`。所以变体名与协议名要区分（`External` / `LispExternal` 没问题）。

### 7.3 协议设计：最小必需 + 能力接口（可组合）

```cangjie
// ystyle::xisp.types（core 侧，草案）

/** 必需：core 的通用逻辑（print / type-of / 错误信息）只依赖这两项 */
public interface LispExternal {
    prop typeName: String        // "ndarray"，供 type-of / 谓词 / 错误信息
    func display(): String       // repr；**实现方负责截断**（见 §7.4 第 6 条）
}

/** 以下按需实现，core 内建按"是否实现"分派，互不牵连 */
public interface LispExternalEquatable <: LispExternal {
    func equals(other: LispExternal): Bool
}
public interface LispExternalIndexable <: LispExternal {
    func length(): Int64
    func at(index: Int64): LispValue      // 供 (length x) / (nth x i) 等通用内建
}
public interface LispExternalComparable <: LispExternal {
    func compare(other: LispExternal): Int64
}
public interface LispExternalCallable <: LispExternal {
    func invoke(args: ArrayList<LispValue>): LispValue
}
```

设计原则：**core 只强制最小集**，其余能力用接口探测（`is LispExternalIndexable`）渐进支持。
扩展方负担最小；将来新增能力接口也不会破坏已有实现（它们只是不实现新接口）。

### 7.4 语义决策（现在就定，别留到实现时随手定）

| # | 议题 | 建议 | 理由 |
|---|---|---|---|
| 1 | `=` 对 External 的语义 | **同一性 + 结构浅比较**（shape / dtype / 共享缓冲），**不做逐元素**；逐元素比较用显式 `array-equal?` | `=` 在 Lisp 里返回 Bool，藏在 `(if (= a b) …)` 里的 O(n) 比较是昂贵意外 |
| 2 | truthy | **External 恒为真**（对象存在即真） | 避免"空数组是假"这类跨域分歧；判空用显式谓词 |
| 3 | 可变 External 作 `HashMap` 键 | **禁止**（未实现 `LispExternalEquatable` 就不能作键） | 可变对象哈希不稳定，会静默破坏 `HashMap` |
| 4 | FFI 分配的资源 | 实现方自行实现 `Resource`；**core 不引入 `close` 语义** | core 不该知道谁持有外部内存 |
| 5 | 序列化 | External **不参与** JSON / 模块序列化；需要时实现 `LispConvertible.toLisp()` 显式转换 | 保持"序列化只处理内建变体"的封闭性 |
| 6 | `display()` 长度 | 由实现方截断 | core 若统一截断，想让实现方打全量就没法表达 |

### 7.5 破坏面实测：不是"全生态大改"，而是 **~24 处补分支**

> **更正**：本节此前写"core 内 133 处 match / 470 个分支"——那是**涉及 `LispValue` 的 match 总数**，
> 不是**需要修改的数量**（绝大多数已有 `case _` 兜底，或匹配的其实是 `Option` / `Token` 等）。
> 下面是按"模式里出现 LispValue 变体、且块内无 `case _`"重新统计的结果：

| 范围 | 未兜底的 LispValue match | 位置 |
|---|---|---|
| core | **12** | `core/eval/module.cj` 7 · `types/types.cj` 2 · `core/eval/macro.cj` 1 · `pattern_match.cj` 1 · `higher_order.cj` 1 |
| 下游生产代码 | **0** | atelier / cordis_xisp 的 `runtime.cj`、`json_bridge.cj` 均不受影响 |
| 下游测试代码 | **12** | 全部集中在 `atelier/src/ptc/json_bridge_test.cj` |
| **合计** | **~24** | 每处补一个 `case External(_) => …` |

而且 `non-exhaustive patterns` 是**编译错误**（实测），编译器会逐处指出，**不会漏改**。

**结论：加变体的成本远低于最初估计**。注意 0.5.0 **已经发布**，本变更须并入下一个破坏性版本
（原"并入 0.5.0"的建议已过期）。

---

## 8. 接缝清单与实施顺序

### 8.1 接缝清单

| # | 接缝 | 设计位置 | 状态 | 服务于 |
|---|---|---|---|---|
| **0** | 符号归类落成代码事实（①收敛：33 → 10 条） | §2 | **✅ 已完成**（2026-10-11）：`and` `or` `condb` `if-let` `let*` `when-let*` 降为宏 | 可替换（§1） |
| **0e** | ②类**一等值化**（`(procedure? map)` 为真、可作值传递） | §3.2 | **✅ 已完成**（2026-10-11）：双轨——保留求值器快速路径，另注册等价 NativeFunc | 可替换（完整） |
| **0a** | Lisp 函数值调用入口（`Evaluator.applyFunction` / `isCallableValue`） | §3.1 | **✅ 已完成**（2026-10-11）；顺带修掉 `for-each` 只认 `NativeFunc` 的静默失败 | 可替换 |
| **0b** | ②类"让位"机制（7 个符号：`map` `filter` `reduce` `apply` `eval` `->` `for-each`） | §3.2 | **✅ 已完成**（2026-10-11），双后端一致 | 可替换 |
| **0c** | 宏改为**编译前**展开 | §3.4 | **✅ 已完成**（2026-10-11）；含宏递归 10767us → 971us | 0/0b 的前提 |
| **0d** | `gensym`（非卫生宏的唯一符号来源） | §3.5 | **✅ 已完成**（2026-10-11） | 宏包的前提 |
| 1 | 域抽象 `LispCapability` + preset + `withDomains` | §4 | **✅ 已完成**（2026-10-11）：core 内置域（现 12 个，见 §12.4 的订正）、`presetFull`/`presetPure`（后者按属性推导）、`orderDomains`（after 拓扑序）、`replace` 独占装配 | 可不要 + 可替换 + 可定制 |
| 2 | `CapabilityContext`（中断注入、类型声明、`invoke`、`evalSource`） | §3.3 | **✅ 已完成**（2026-10-11） | 域装配 |
| 1b | **宏包即域**（④层用 `evalSource` 形态安装） | §4.2 | **✅ 已完成**（2026-10-11）：16 个内建宏合并为单一 Lisp 源码 | 可替换（④层） |
| 3 | `External` 变体 + `LispExternal` 协议族 | §7 | **✅ 已完成**：变体 + 5 个能力接口（2026-10-11）；5 个接口**全部接通分派**于 2026-11-06 补齐（`=`/`!=` → Equatable、`<`/`>`/`<=`/`>=` → Comparable、`length`/`nth` → Indexable、调用形式 → Callable），见 §12.3 | 下游 sci 族 |
| 4 | `funcChecker` 覆盖可逃逸特殊形式（②类） | §5.4 | **✅ 已完成**（2026-10-11）：`withSandbox()` 的 `blockFunctions(["eval","apply"])` 终于生效 | 沙箱正确性 |
| 6 | **AST ↔ 字节码差分测试**（91 个语料逐表达式比对） | §5.5 | **✅ 已完成**（2026-10-11）：从 726 处差异降到 **0**，修复 7 处真不兼容 | 兼容性 |
| 5 | 外部模块的**中断义务** | §3.3 | **✅ 已完成**（2026-11-08）：契约写进 `core/README.md`「扩展模块的中断义务」——`CapabilityContext.isInterrupted` 的调用时机、中止形式（错误值 + `INTERRUPTED_MESSAGE`）、粒度建议 | 宿主可控性 |

### 8.2 实施顺序

1. ~~**接缝 0 / 0a / 0b / 0c / 0d（可替换）**~~ —— **✅ 已完成**（2026-10-11）：
   - 0c 宏改为编译前展开（含宏递归 10767us → 971us，且消除 AST/BC 语义偏差）
   - 0d `gensym`（非卫生宏的唯一符号来源）
   - 0a `applyFunction` / `isCallableValue`；顺带修 `for-each` 只认 `NativeFunc` 的静默失败
   - 0b 让位机制（7 个符号，双后端一致；**让位判断必须先于编译**）
   - 0 六个派生形式降为宏；`and`/`or` 的 4 处 AST/BC 语义分歧归零
   **效果**：core 自带的同能力实现全部可被用户顶掉 —— "可替换"目标达成。

   **0e（原文记作"仍欠一项"）已随 2026-10-11 实施完成**：`(procedure? map)` 为真 / 可作值传递，
   见 §11.4：core 把 `map` 等**注册成值**（一等值），
   同时保留求值器快速路径，守卫判据为"绑定是否仍为 core 注册的那份"，用户替换后自动让位。
   实测 `(procedure? map)` → `true`、`(define f map) (f (lambda (x) (* x 2)) (list 1 2 3))` → `(2 4 6)`。
2. ~~**接缝 1 / 2（域装配）**~~ —— **✅ 已完成**（2026-10-11）：
   `LispCapability` + `CapabilityContext` + core 内置域（现 12 个）；
   `presetFull` / `presetPure`（后者按 `!escapable && !hostPolicy` **推导**）；
   `orderDomains`（`after` 拓扑序）；`withDomains` / `withPreset`（含 `replace` 独占装配）；
   宏包以 `evalSource` 形态安装（16 个内建宏合并为单一 Lisp 源码）。

   **边界如实记录**（并有测试锁定）：即便独占装配，①语言原语（`if`/`let`/`lambda`）
   与②求值器拦截（`map`/`eval`）**仍然可用** —— 它们不在环境里，域装配动不了。
   域装配只覆盖③注册函数。
3. ~~**接缝 4（`funcChecker` 覆盖）**~~ —— **✅ 已完成**（2026-10-11）：①②类接上权限检查，见 §11.3。
4. ~~**接缝 3（`External` 变体）**~~ —— **✅ 已完成**（变体与协议族 2026-10-11；能力接口分派 2026-11-06 补齐，见 §12.3）：
   `types` 加协议族 + 变体，编译驱动修 core 内非穷尽 match（实测只有 2 处，其余已有 `case _` 兜底）；
   core 通用语义按能力接口分派（`print` / `type-of` / `external?` / `=` / `<` / `length` / `nth` / 调用）。
5. ~~**用一个测试用假扩展端到端验证**~~ —— **✅ 已完成**（2026-10-11）：
   `xisp_tests/src/external_value_test.cj` 的 `CounterBox` / `Doubler`
   —— 注册域 → 产生 External → `print` / `=` / `<` / `length` / `nth` / 调用全部走通。
6. **之后才开科学计算族的地基模块**（见下游文档）。

---

## 9. 风险与未决问题

### 9.1 首要目标（可替换 / 分层）

| # | 项 | 说明 |
|---|---|---|
| C11 | ②类符号收敛的具体形态 | 目标是"真一等值"，有两条路：① 求值器保留原语语义、额外注册一个一等值（值即能力）；② 改为"取函数值再调用"（把控制流语义移进函数）。后者更干净但需改求值器核心，前者改动小但两套语义仍在。**待定** —— **✅ 已决（2026-10-11 实施）：取①「双轨」**，保留求值器快速路径，另注册等价 `NativeFunc` 作一等值（把实参拼成调用式交回原路径，不重复实现）。依据 `core/src/core/eval/first_class_values.cj` 头注释「## 做法：**双轨**」：调用位置 `(map f lst)` 走快速路径（性能与栈深不变），值位置 `(procedure? map)` / `(go map)` 从环境取那个 `NativeFunc`。未选②的原因见 §11.4 的三条实测约束 |
| C13 | **可替换性的边界** | ①明确不可替换；②③④应可替换。需定：用户顶掉 core 实现时**静默生效还是需显式声明**（如 `withOverride("map", …)`）？静默容易被误覆盖，显式则更啰嗦。**待定** —— **✅ 已决（2026-10-11 实施）：静默生效**，用户 `define` 同名符号即顶掉 core 实现，无需显式声明。依据 `core/src/core/eval/defer_special_form.cj`：`shouldDeferToUser`（第 58 行）用 `Environment.defineVersion` 版本号守卫查缓存，`hasUserBinding`（第 94 行）判据为"环境里有该名字的绑定、且不是 core 自己安装的那份"。实测 `(define map (lambda (f l) 'mine))` 后 `(map f lst)` 返回 `mine`。注意这条**只覆盖让位**（调用位置交给普通调用路径），与 C11 的"双轨"是两件事 |
| C14 | ②类"让位"的性能代价 | **算术部分已解决**：`+ - * / < > =` 用 `env.arithVersion` 版本号缓存做守卫，热路径无额外开销。②类照此模式即可，**不要每次调用都查环境** |
| C16 | `for-each` 的语义定位 | 它现在是③（注册），但语义上属"高阶函数"。修好 `invoke` 后：留下（用 `invoke`）还是并入 `map` 家族？**待定** —— **✅ 已决（2026-10-11 实施）：并入 `map` 家族（②类一等值）**，实现从注册函数移到求值器（`core/src/core/eval/for_each.cj`），与 `map`/`filter`/`reduce` 走同一条内部应用路径（能调闭包），同时进可让位集合与一等值集合。实测 `(procedure? for-each)` → `true`；`(for-each (lambda (x) (print x)) (list 1 2))` → 打印 `12`（不再静默跳过闭包） |

### 9.2 域装配模型

| # | 项 | 说明 |
|---|---|---|
| C1 | 能力域粒度 | core 域的按域装配是否需要（如"只要算术+表，不要高阶函数"），需真实宿主诉求来定 —— **✅ 已对齐（2026-11-08）**：先修了 5 处"声明 ≠ 安装"的边界错位（域 12 → 13），逐域护栏 `testEachDomainDeclaresTruthfully` 钉住；原始例子「不要高阶函数」**做不到**，因为 `map/filter/reduce/apply/eval/->/for-each` 是②类、不属任何域（§5.3 的边界，非粒度问题）。更细的拆分（`arithmetic` 再拆 `math`、`predicates` 按族拆）**等真实诉求**，见 §12.5 |
| C2 | 域装配与执行后端接缝是否合并 | §4.1 的 `LispCapability` 与工作区化文档 §3.2 的 `ExecutionBackend` 形态相近，成熟后宜统一 |
| C9 | `registerPure` 的归宿 | 收敛为 preset（§4.3）还是保留函数形态？改动会影响 atelier 的手工装配路径（§5.1） —— **✅ 已收口（2026-11-08）：保留函数签名、内部改为 `installDomains(env, presetPure())` 的委托**。调用方（含 atelier 式手工装配）无需改动；两档装配从此只有一份装配逻辑，等价性由 `testPresetPureCoversRegisterPure` / `testPresetFullCoversRegisterAll` 反向钉住 |
| C12 | `PRESET_PURE` 用减法还是显式列表 | core 内部按 `escapable`/`hostPolicy` **推导**（推荐，默认安全）；宿主做减只能自己列域。需确认"新增域默认 `escapable=true`"不会意外排除现有域 —— **✅ 已确认（2026-11-08）**：`SimpleCapability` 的 `escapable!` 默认值确为 `true`（`capability.cj:127`），新增域默认落在 pure 之外（默认安全）。同时发现推导本身把 3 个纯域误判为不纯（`print` 域混装字符串计算、`macros.core` 误标 escapable），已订正，见 §12.4 |
| C15 | `evalSource` 安装形态的沙箱影响 | 宏包用 `eval` 安装源码；若宿主禁用了 `eval`，宏包能否安装？（安装是宿主行为、非脚本行为，**应允许**——需在实现时明确区分"宿主安装期"与"脚本运行期"） —— **✅ 已收口（2026-11-08）**：`makeContext` 里 `evalSource` 直接用内部 `Evaluator.eval`，不经过脚本侧权限检查；用例 `testEvalSourceInstallSurvivesSandbox` 钉住"`withSandbox()` 下 `(eval …)` 被拦、而 `when`/`unless`/`incf` 正常" |
| C17 | 域 `after` 的完整性 | 现有 `registerModernAliases` 依赖"最后注册"，域化后这个隐式约束必须显式化。需确认所有域的实际依赖序 —— **✅ 已收口（2026-11-08）**：新增 `domainAliases()`（`after: ["base","list","string"]`），`macros.core` 的 `after` 补 `symbols`（`swap`/`or` 展开调 `gensym`）。此前 `registerModernAliases` **不属任何域**，独占装配时 16 个别名集体消失，见 §12.4 |

### 9.3 外部依赖与外部值接缝

| # | 项 | 说明 |
|---|---|---|
| C3 | `External` 变体的归属 | 只加 `External`，还是同时加专用变体（见 sci 文档的比选）；**待拍板** |
| C4 | 变体与协议命名 | 建议变体 `External` + 协议 `LispExternal`（二者不能同名，§7.2） |
| C5 | 符号命名策略 | 命名空间（`(import sci.linalg)`）还是前缀（`linalg-`）？待定，与 `LispCapability.symbols` 配套 |
| C6 | 是否把"`LispValue` 的 match 必须有外部值分支"写成 lint | **✅ 已落（2026-11-08）**：不改 lint（静态判定"哪些 match 作用在 LispValue 上"不可靠），改用**编译期强制**——`xisp_tests/src/lispvalue_matrix_test.cj` 的 `variantTag` 对 16 个变体**穷尽匹配、无 `case _`**，新增变体时该文件编译失败，作者必须先登记并回答横切语义。同轮它还立即查出 `type-of` 对 `Error` 变体报 `"unknown"`（已修，报 `"error"`）。另附三条横切契约：显示非空、克隆保形、`type-of` 不得为 `unknown` |
| C7 | stdx 环境变量 | `CANGJIE_STDX_PATH` 路径三段随安装方式而变，CI/容器/新机器默认不存在；对策见 §6.1 —— **不适用于本仓**（2026-11-08 核实：xisp 全工作区 `cjpm.toml` 无任何 stdx 依赖），保留给将来引入 stdx 的扩展模块 |
| C8 | 多成员样板重复 | `bin-dependencies` / `link-option` 必须在每个要发布的成员里各写全份（cordis-cj 实测 42 处重复），cjpm-ws 目前不同步这类字段。缓解：约定"只有能力域成员才允许出现 stdx 配置" + CI 校验各成员该段一致 —— **✅ 已落（2026-11-08）**：新增 `scripts/check-members.sh` 并接进 CI（`Member config gate`）。检查 `cjpm ws check` **管不到**的部分：`version` 与 `workspace.toml` 一致、`cjc-version` 全体一致、库成员禁 `[replace]`、平台相关配置（`bin-dependencies`/`standard-library`/`link-option`）必须在 `NATIVE_ALLOWED` 白名单内（当前为空）、发布成员的 `include` 显式且**锚定**、中心仓必需元数据齐全、目录内有 `README.md`。已用两处**故意破坏**验证确实会拦（未锚定 `include` / 库里加 `[replace]`） |
| C10 | 网络域走哪条路 | 应用层走 stdx 还是第三方纯仓颉库，见 §6.2，需按"是否引入 stdx"这一判据调研后再定 |

---

## 11. 兼容性与沙箱：实施记录（2026-10-11）

### 11.1 AST ↔ 字节码差分测试（新增回归网）

`xisp_tests/src/backend_diff_test.cj`：同一段代码在两个后端下**逐表达式**比对结果，
语料为仓库现有 91 个 `.lisp`。它一上线就暴露 **726 处差异** —— 说明"两个后端语义一致"
此前**从未被验证过**，只是被假设。

32 个可比对文件现已**全部一致**（其余跳过原因写在测试注释里：需模块搜索路径 /
真实文件系统交互，非后端差异）。

### 11.2 差分测试挖出的 7 处真不兼容

| # | 问题 | 性质 | 修法 |
|---|---|---|---|
| 1 | `println` 吞掉实参错误 | **静默算错** | VM `callFunction` 补实参错误短路（AST 本就短路，VM 不查） |
| 2 | 关键字别名（`setq`/`progn`）在 BC 下失效 | **静默不生效** | 编译前在每个求值位置解析别名（`alias_for_compile.cj`） |
| 3 | `+ - * / mod round` 类型错误静默返 `nil` | 静默 | 统一为明确 `TypeError` |
| 4 | `(= 2 2.0)` AST true / BC false | 语义分歧 | VM `equalValue` 支持数值跨类型 |
| 5 | `(procedure? <编译闭包>)` false、`type-of` "unknown" | 语义分歧 | 补 `Closure` 分支；统一显示名为 `#<procedure>` |
| 6 | 裸关键字符号 `:ok` BC 下 `UnboundVariable` | 语义分歧 | 编译器补关键字符号自求值 |
| 7 | `(match 1 (1 "one") …)` 字面量模式被误判为列表模式 | 语义分歧 | 新增 `isLiteralPattern` 判定 |

另修一处**语料自身**的 bug：`combination_functions_test.lisp` 缺右括号，
导致下一个 `(if …)` 被吞进 `define` 成为第三个参数。

### 11.3 沙箱：`funcChecker` 的覆盖缺口已补（接缝 4）

修复前后对比（实测）：

| 符号 | 类别 | 修复前拉黑后 | 修复后 |
|---|---|---|---|
| `length` | ③注册函数 | ✅ 被拦 | ✅ |
| `eval` / `apply` | ② | ❌ **照常执行** | ✅ 被拦 |
| `map` `filter` `reduce` `for-each` `->` | ② | ❌ 照常执行 | ✅ 被拦 |
| `if` `define` `lambda` `let` | ① | — | 刻意**不拦**（语法结构，拦掉语言不可用） |

`withSandbox()` 的 `blockFunctions(["eval","apply"])` 此前形同虚设，现已生效。

### 11.4 ②类一等值化（接缝 0e）

三条**实测约束**决定了不能简单把 `map` 等改成普通函数：

1. 纯 Lisp 版 `map` **500 元素就爆栈**（默认栈深 1000）；
2. `apply` / `eval` 需要"展开实参列表"/"对数据求值"，纯 Lisp 无法表达；
3. 求值器对高阶函数有专门迭代实现，改函数会退化。

故采用**双轨**：调用位置走求值器快速路径，值位置取另注册的等价 `NativeFunc`
（把实参拼成调用式交回原路径，不重复实现）。`Environment` 增 core 安装标记，
使让位判据不把自己的注册误判成用户实现；`define` 会清除标记，用户覆盖后仍正常让位。

---

## 12. 本轮实施记录（2026-11-06）

### 12.1 错误传播语义统一（AST / BC / JIT 三后端一致）

`LispValue.Error` 是**值**而非异常，故"错误发生后要不要中止"过去由各处分发代码各自决定，
三后端因此分道扬镳（典型：`(begin (error "b") 42)` AST 返回 `42`、BC 报错；
`(define (f) (assert 假) 42)` 在 AST 下 `assert` 形同虚设）。

现统一为：**错误一旦产生即中止当前求值链**，一路返回到最外层；**绑定初值出错时不建立绑定**
（不把错误值塞进变量）。三后端共用同一套判定，用例见 `xisp_tests/src/error_propagation_test.cj`
（`assertErrorAllBackends` / `assertSameResultAllBackends`）。

### 12.2 三后端差分闸门

`xisp_tests/src/backend_diff_test.cj` 从"AST ↔ 字节码"两后端扩为 **AST / BC / JIT 三后端**，
并分**两层**：`testFileDifferential`（逐文件，覆盖相对导入、模块搜索路径、副作用序列）
与 `testCorpusDifferential`（逐表达式，能定位到"第 N 步不一致"）。
比较时错误只比**种类**、不比文案（两侧由不同代码路径产出）；当前**零语义差异**。
差分网继续挖出并修掉了四处：VM 中函数值出错时实参不再求值（求值顺序与 AST 一致）、
函数体内头位置未绑定归一为 `UndefinedFunction`、字节码补 `OP_CHECK_ERR`（实参求值前先查函数值）、
以及差分测试自身的输出捕获（此前 Lisp 的 `println` 直接写 stdout，输出根本没参与比对）。

### 12.3 `LispExternal*` 五个能力接口全部接通分派

接缝 3 首轮只接通了 `length`（`LispExternalIndexable`）。本轮补完其余四个消费点：

| 能力接口 | 分派点 | 效果 |
|---|---|---|
| `LispExternalEquatable` | `core/src/core/builtin/comparison.cj:50`（`=`） | 两侧都实现才谈相等，否则恒不等；`!=` 经 `=` 反向得值 |
| `LispExternalComparable` | `comparison.cj:108/167/226/285`（`<` `>` `<=` `>=`） | 未实现则该顺序比较恒为 `false` |
| `LispExternalIndexable` | `core/src/core/builtin/list.cj:60`（`length`）、`:89`（`nth`） | 未实现时 `length` 按既有约定返回 `0`、`nth` 返回 `nil` |
| `LispExternalCallable` | `core/src/core/eval/apply_function.cj:54`（`applyFunction`）、`core.cj:366`（调用位置）；`isCallableValue`（`apply_function.cj:80`） | 可被调用；不可调用时明确报 `TypeError`，不静默返回 `nil` |

验证：`xisp_tests/src/external_value_test.cj` 的 `CounterBox`（Indexable + Equatable + Comparable）
与 `Doubler`（Callable）走通全链路。

### 12.4 域装配的 5 处缺陷（2026-11-08 订正）

审"这次重构范围内还有什么没做"时，对着 `coreDomains()` 与 `registerAll` 逐符号比对，
发现**域集合本身不完整**——而且这些缺陷全都只在 `replace: true`（"只留所选域"，
也就是"可裁剪"这条唯一路径）下才暴露，默认追加装配会把它们掩盖掉。

| # | 缺陷 | 后果 |
|---|---|---|
| 1 | `domainPrint` 不在 `coreDomains()` 里 | `presetFull` 独占装配下 `println` / `print` / `display` / `newline` / `princ` 全缺——"预设**全量**"不含输出域 |
| 2 | `registerStringOps` 与 `registerOutput` 同挤在 `print` 域（`hostPolicy`） | `presetPure` 按属性推导把它们一起排除 → 纯函数子集没有 `str` / `string-length`，而 `registerPure` 一直有 |
| 3 | `registerModernAliases` **不属任何域** | `first` / `rest` / `second` / `third` / `fourth` / `prepend` / `more` / `cadr`… `setq` / `progn` / `string-append` 共 16 个全缺；而 `domainList.symbols` 却声明了 `first` / `rest`（**声明与安装脱节**） |
| 4 | `macros.core` 标了 `escapable: true` | `presetPure` 排除整个宏包 → 纯环境里连 `(when #t 1)` 都不认，与 `registerPure`（一直含 ④ 层宏包）不一致。**宏是代码变换器，展开产物仍过权限检查，不提供逃逸手段**，应为 `false` |
| 5 | `domainArithmetic.symbols` 声明了 `abs` | `abs` **从未实现**（全仓无 `env.define("abs")`，docs-site 算术清单也没有）——域声明是对外契约（沙箱审计/文档生成/重名检出），多声明等于谎报 |

修法：

- 拆出 `domainString()`（纯计算）与 `domainPrint()`（`hostPolicy`）；
- 新增 `domainAliases()`，`after: ["base", "list", "string"]` —— 把原先只写在
  `registerAll` 注释里的"别名最后注册"这一隐式约束，显式化为 `after`（即 C17 的要求）；
- `macros.core` 改 `escapable: false`，`after` 补 `symbols`（`swap` / `or` 展开时用 `gensym`）；
- 从 `domainArithmetic.symbols` 移除 `abs`，并把 `round` 补上（它确实由 `registerModulo` 注册）；
- **`registerAll` / `registerPure` 改为预置组合的委托**，消除两份平行清单（C9 收口）。

新增 4 条用例（`capability_domain_test.cj`），都是"反向钉住"式的：

| 用例 | 钉住什么 |
|---|---|
| `testPresetFullCoversRegisterAll` | 独占装配 `presetFull` 后，`registerAll` 的**每一个符号**都必须存在 |
| `testPresetPureCoversRegisterPure` | 同上，针对 pure；且宏包可用 |
| `testDeclaredSymbolsActuallyInstalled` | 每个域 `symbols` 声明里的符号都必须真的装上（防"声明撒谎"） |
| `testAliasesAvailableUnderExclusiveAssembly` | 12 条别名在两种独占装配下逐个可用（含 `setq` / `progn` 关键字别名） |

顺带把一处测试里"看起来更安全"的错误断言纠正了：原先断言 `presetPure` **排除** `macros.core`
（注释写"保守判定"），实际是语义错误——纯环境里 `when`/`unless` 全都不可用。

**同轮收口**：接缝 **5**（外部模块的中断义务）——契约已写进 `core/README.md`
「扩展模块的中断义务」：`CapabilityContext.isInterrupted` 的调用时机、中止形式
（错误值 + `INTERRUPTED_MESSAGE`，不是抛异常）、粒度建议，以及"不查中断宿主就杀不掉"
这一后果。至此 §8.1 的 12 条接缝全部完成。

同一轮还收口了 §9 的四条：**C9**（`registerPure` 内部改为 preset 委托）、
**C12**（确认 `escapable!` 默认 `true`，新增域默认落在 pure 之外；同时发现推导把
3 个纯域误判，见上）、**C15**（`evalSource` 属安装期、不受脚本 `eval` 权限约束，有用例）、
**C17**（`after` 显式化，新增 `domainAliases()`）；**C7**（stdx 环境变量）经核实
**不适用**——全工作区无 stdx 依赖。

**范围内仍未做**（均为"需外部输入"，不阻塞本次重构）：

- **C1** 能力域粒度 —— 边界已对齐（§12.5），「更细要不要拆」需真实宿主诉求；
- **C2** 域装配与 `ExecutionBackend` 接缝是否合并 —— "成熟后宜统一"，未动；
- **C10** 网络域走 stdx 还是第三方纯仓颉库 —— 待调研；
- **C3 / C5** `External` 专用变体归属、符号命名策略 —— 下游 sci 族决策，见 sci 文档 §8。

### 12.5 C1「域边界对齐注册器」（2026-11-08）

结论先说：**C1 之前不是"待宿主诉求"，而是根本还没开始** —— 域边界与注册器边界不一致，
导致"声明 ≠ 安装"，粒度无从谈起。实测（只装单个域，不是 `presetFull`）：

```
只装 arithmetic 域：
  (sum '(1 2))      => [UndefinedFunction]   ← 但 domainArithmetic.symbols 声明了 sum
  (max 1 2)         => [UndefinedFunction]
只装 list 域：
  (sum '(1 2))      => 3                     ← sum 实际挂在这里
```

| # | 错位 | 原状 | 对齐后 |
|---|---|---|---|
| 1 | `sum` / `product` / `max` / `min` | 声明在 `arithmetic`，由 `registerHigherOrder` 注册、随 `list` 域安装 | 从 `registerHigherOrder` 拆出 `registerAggregates`，归 `arithmetic` 域 |
| 2 | `reverse` / `range` | 与聚合函数混在 `registerHigherOrder` | 拆出 `registerListExtras`，归 `list` 域 |
| 3 | `not` / `eq?` 与 14 个类型谓词 | 同挤在 `logic` 域 | 拆成 `logic` 与新的 `predicates` 域 |
| 4 | `first` / `rest` | 声明在 `list` 域，实际由 `aliases` 域的 `registerModernAliases` 注册 | 从 `list` 声明中移除（`aliases` 域已声明） |
| 5 | `hashmap?` | 由 `registerHashMap` 注册，却不属任何域的声明 | 归 `hashmap` 域声明；同时把该域漏声明的 9 个 Redis 别名补齐 |

域数 **12 → 13**（新增 `predicates`）。护栏是逐域用例 `testEachDomainDeclaresTruthfully`：
**每个域单独安装（连同其 `after` 依赖的传递闭包）后，其 `symbols` 声明必须全部可用**。
它上来就抓出了第 5 条。此前只有"全装 `presetFull`"的弱检查，A 域声明、B 域注册的错位
在那里面照样通过。

#### C1 现在能被回答的部分

- **粒度只作用在③类注册函数**（§5.3 的边界，不是缺陷）：①语言原语与②求值器拦截的
  `map` / `filter` / `reduce` / `apply` / `eval` / `->` / `for-each` **不属任何域**，
  域装配动不了它们。所以"只要算术+表、不要高阶函数"这个原始例子**做不到**，
  原因在②类，不在粒度。
- **能做的部分现在成立了**：宿主可以只装 `arithmetic` 而没有 `sum/max`，只装 `list`
  而没有聚合，只装 `logic` 而没有谓词 —— 每个域的 `symbols` 就是它给环境带来的全部符号。
- **真实诉求现状**（2026-11-08 核实）：下游 `atelier`（`ptc/runtime.cj:88`）与
  `cordis_xisp`（`runtime.cj:184`）都只用 `registerPure(env, output:)` 一档，
  没有宿主在挑域。所以"要不要更细"仍无需求驱动；但两档装配已等价（§12.4），
  将来换 `withPreset(presetPure()` 不会改变行为。

**仍待外部诉求**：是否需要把 `arithmetic` 再拆成 `arithmetic`（`+ - * /`）与 `math`
（`mod` / `round` / 聚合），或把 `predicates` 按类型族细分。没有宿主诉求前不预拆
——每多一个域，宿主就要多记一个名字。
