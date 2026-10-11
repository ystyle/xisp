# Xisp 能力域与扩展接缝设计

> **状态**：机制设计，**未实施**（§3 的 register 分层为待收敛项；§7、§8 的接缝待做）
> **目标版本**：待定（0.5.0 已发布，本设计的破坏性变更须并入下一个破坏性版本）
> **前置**：工作区化（模块划分）已完成并发布，见
> [`2026-09-27-workspace-modularization.md`](2026-09-27-workspace-modularization.md)
> **下游规划**：科学计算族（`xisp_array` / `linalg` / `stats` / `signal` / `io` / `sci`）见
> [`2026-10-11-sci-family-design.md`](2026-10-11-sci-family-design.md)（**规划中**，最后做）

---

## 1. 问题

工作区化解决了**编译期依赖图**（谁依赖谁、哪些代码被打进产物），但没有回答：

1. **运行期给脚本哪些能力**——同一个二进制里，宿主给不给脚本文件 I/O、网络、模块加载？
2. **宿主不要的扩展怎么不进产物**——`xisp_array` 这类外部模块，宿主不依赖就完全不存在；
   但 core 必须为它留出一道**接缝**，否则它接不上来。

这两件事是**两条正交的轴**（见 §2），本设计分别处理。

---

## 2. 两条正交的轴

| 轴 | 手段 | 决定什么 | 本文章节 |
|---|---|---|---|
| **仓颉侧（编译期）** | 拆 cjpm 模块 | 产物里**有没有**这段代码 | 工作区化文档 §3 |
| **Lisp 侧（运行期）** | `InterpreterOption` 符号装配 | 同一个二进制里，**宿主给不给脚本这个能力** | §3–§6 |

一个能力域的完整边界 = 两条轴上的共同决定。这也是本设计的主线：

> **Lisp 内建函数的"按需加载"不需要拆 cjpm 模块**——它是运行期符号装配，与仓颉侧的编译期依赖图正交。
> **能靠运行期装配解决的问题，不要靠拆模块解决；拆模块只用于"依赖图必须变"的场景**
> （独立外部依赖 / 平台特化 / 独立演进节奏）。

落到操作上：**域在 core 内时只用运行期装配；域有外部依赖或平台特化时才两侧一起做**
（独立模块 + 装配选项）。

---

## 3. register 分层：三条注册路径需要收敛

### 3.1 现状：三条路径，三种粒度

| 路径 | 粒度 | 缺口 |
|---|---|---|
| `BuiltinFunctions.registerAll(env)` | **全量**（14 个子注册函数一次全上） | 只有 all / pure 两档，中间粒度缺席 |
| `BuiltinFunctions.registerPure(env, output!)` | **纯函数子集**（面向最小权限宿主） | 与 `registerAll` 是**两份平行清单**，靠人肉同步 |
| `Bridge.registerFunc` / `registerFuncWithNS` / `registerStdIO` / `registerStdFS` / `registerStdCollection` | **零散函数 / 桥接组** | 无域标识，沙箱审计拿不到清单 |

外加 `InterpreterOption` 上的若干开关（`withStdIO` / `withStdFS` / `withStdCollection` /
`withNoModuleLoad` / `withNoFileWrite` / `withBlockedFunctions` / `withAllowedFunctions` / `withOutputFn`）。

### 3.2 `registerAll` 与 `registerPure` 的差异（实测，2026-10-11）

两份清单不是"包含关系"，而是**两个不同的集合**：

| 子注册函数 | `registerAll` | `registerPure` |
|---|---|---|
| `nil` 字面量 | ✅ | ✅ |
| arithmetic / modulo / math / comparison | ✅ | ✅ |
| listOps / higherOrder / logic / predicates | ✅ | ✅ |
| hashMap / 内建宏 / Redis 别名 / 现代别名 / errorHelpers | ✅ | ✅ |
| **`registerPrint`**（print / println / princ / newline / display） | ✅ | ❌（改为按 `output` 参数注册 `registerOutput`） |
| **`registerStringOps`**（`str` 等） | ✅（经 `registerPrint` 间接） | ✅（**显式调用**） |

> 值得注意：`registerPure` **显式调用 `registerStringOps`**，而 `registerAll` 是通过
> `registerPrint(env)` → `registerStringOps(env)` **间接**得到它。也就是说 `str` 在两条路径上都可用，
> 但**依赖关系是隐式的**：谁要是调整 `registerPrint` 的内部组成，`registerAll` 就会静默丢掉 `str`，
> 而 `registerPure` 不受影响 —— 这类"平行清单 + 隐式依赖"正是要收敛的对象。

### 3.3 收敛方向（草案）

1. **单一清单，按域声明**：把 14 个子注册函数提升为**域**（每个域 = 名字 + 符号清单 + 安装函数），
   `registerAll` = 所有域的并集，`registerPure` = 显式子集。**不再维护第二份平行清单。**
2. **`registerPure` 是否收敛为 `withPureEnv()`**：纯函数子集是**宿主策略**（"给脚本多小的权限"），
   与 `withNoFileWrite` / `withBlockedFunctions` 同类，放在 `InterpreterOption` 上比放在
   `BuiltinFunctions` 上更符合既有分层。待定项：是否需要"在已有全量环境上做减法"的语义
   （**注意**：按 §3.4 的约束，减法不可靠，只能"一开始就不注册"）。
3. **分层主轴：能力 + 可逃逸性**（倾向）。域的划分依据不该只是"函数主题"（算术 / 列表 / 哈希表），
   而是两个正交属性：
   - **能力**：本域提供什么（算术、文件、网络、模块加载…）；
   - **是否可逃逸**：域内的函数能不能**绕过权限检查**（`eval` / `apply` / `map` / `filter` / `reduce` /
     `for-each` / `load` 能先于检查执行，见 §3.4）。
   这样"纯函数子集"就不再是一张手写清单，而是"**排除所有可逃逸域 + 排除所有宿主策略域**"的推导结果，
   新增域时默认**不入 pure**，需要显式声明才进 —— 默认安全。
4. **域的符号清单进声明**（与 `LispCapability.symbols` 同一机制，见 §5.2）：沙箱审计、文档生成、
   重名检出都依赖它。

### 3.4 硬约束：域的粒度必须是「注册集合」，不能是「调用拦截」

`registerPure` 的文档注释已经把这条写死了，本设计必须继承：

> 权限检查对特殊形式无效 —— `eval` / `apply` / `map` / `filter` / `reduce` / `for-each`
> 会先于检查执行，且函数值可被当作参数传递，因此「注册后过滤」不可靠，
> **只能用「不注册」来表达禁止**。

这条约束决定了：**运行期装配的作用点是 `env.define` 的那一刻，不是调用那一刻。**
任何"先全注册、再按名单拦截"的设计都不成立。

### 3.5 ⚠️ 修正：这条约束只覆盖了符号的一半（2026-10-11 实测）

上文（承自 `registerPure` 注释）默认"符号都靠注册"，但**实测不成立**。按"默认环境里
`env.lookup` 是否命中"逐个符号测下来，现存符号是**三类**，而 §3.4 的约束只对第三类有效：

| 类 | 符号 | 注册为值 | 一等值（`procedure?`） | 「不注册」能否禁止 |
|---|---|---|---|---|
| **A 求值器原语** | `if` `quote` `define` `lambda` `let` `let*` `begin` `set!` `defmacro` `condb` `if-let` `when-let*` `match` `backquote` `comma` `import` `export` | ❌ | ❌ | **❌ 根本做不到**：从未注册，由求值器硬编码（`trySpecialForm` 33 条 `case`） |
| **B 半原语** | `+ - * / < > =`、`and or`、`apply eval map filter reduce for-each ->` | ✅/部分 | ❌/✅ 混合 | **❌ 注册了也关不掉**：硬编码优先于注册表 |
| **C 纯注册函数** | `cons car cdr list length null? not eq? number? str print…` | ✅ | ✅ | ✅ 这就是 §3.4 说的那类 |

**实测证据**（默认环境）：

```
(define map 42)        →  map 求值为 42，但 (map f lst) 仍照常工作   ← B 类：值可改、行为不可改
(define if 42)         →  (if 1 2) 仍返回 2                          ← A 类：define 完全无效
(procedure? map)       →  [UnboundVariable] Undefined variable: 'map' ← B 类：不是一等值
(procedure? car)       →  true                                        ← C 类：是
(define (go f) (f …)) (go map) → [UnboundVariable] 'map'              ← B 类无法作为值传递
(apply 42 '(1 2))      →  nil（静默吞掉类型错误）
```

**两个衍生问题**（另见实现问题记录）：

1. **`funcChecker` 对 A/B 类失效**：权限检查只在普通调用路径（`core.cj:233`），
   特殊形式在 `trySpecialForm` 早退。`withSandbox()` 明确 `blockFunctions(["eval","apply"])`，
   实测**两者都拦不住**；`length`（C 类）则正常报 `Function 'length' is not allowed`。
2. **`and` / `or` 在 AST 求值器下不短路**（注册成普通 `NativeFunc`，实参先全求值），
   而字节码编译器把它们当**真特殊形式**（`compileAnd`/`compileOr`）→ **两个后端语义不一致**：
   `(and 0 (undefined-fn-xyz))` AST 报 `UndefinedFunction`、BC 得 `false`。
   同一现象还有 `(apply 42 …)`（AST `nil` / BC `RuntimeError`）、以及
   **BC 下 `length` 被拉黑仍返回 2**（VM 不查 `funcChecker`）。

**对能力域设计的影响**（重要）：

- §3.4 的"注册集合"模型**只覆盖 C 类**；A/B 类由编译进去的求值器提供，**任何域装配都关不掉**。
  而沙箱最想收紧的 `eval` / `apply` / `map` / `filter` / `reduce` 恰恰全在 A/B 类。
- 因此能力域要真正成立，前提是先做**符号归类收敛**：A 类明确为"语言原语，域装配不管"，
  B 类要么降为 C 类（真注册 + 一等值 + 求值器按需取函数值），要么升为 A 类（明确不可配置）。
  这条比 `External` 变体更靠前（见 §9 实施顺序）。

---

### 3.6 符号归类：必需原语 vs 可选能力（本设计的模型基础）

§3.5 的实测说明"必需 / 可选"不能用"注册 / 不注册"来分。按**能否用函数替代**重分：

| 归类 | 判据 | 符号（现状） | 域装配能否关掉 |
|---|---|---|---|
| **① 语言原语**（不可省、不可拦、非一等值） | 语法结构；缺了就不是这门语言 | `if` `quote` `lambda` `define` `set!` `let` `begin` `defmacro` `match` `import` `export` | ❌ 关不掉（但**语义内部可以带宿主策略**，见下） |
| **② 控制流原语**（需延迟/控制求值，且应为**一等值**） | 必须能操作"未求值的实参"，或必须能作为值传递 | `and` `or`（短路）、`apply` `eval` `map` `filter` `reduce` `for-each` `->` | ❌ 关不掉（现状）；**应收敛为一等值** |
| **③ 纯函数**（真注册、可省、可拦、可作值） | 实参先求值，无控制流 | `+ - * / < > =` `cons car cdr list length append` `not eq? number? string? symbol? list? procedure?` `str print println` … | ✅ 只有这类能靠"不注册"表达禁止 |

**①/② 与"可配置"并不互斥**——既有先例：`import` / `export` 是①类原语，
但内部已接宿主策略（`moduleLoadChecker`），`withNoModuleLoad()` 实测能拦住。
所以正确的模型是：**原语不可"移除"，但可以携带宿主策略开关**。

**②类的目标形态**（收敛方向，不是现状）：让它们成为**真一等值**——
`(procedure? map)` 为真、`(go map)` 能把 `map` 当参数传。
只要求值器能"取到函数值再决定如何调用"（而非把符号名硬编码进分派），
② 就同时获得了一等值与可配置性。这也是字节码/AST 两个后端语义一致的唯一办法（§3.5 问题 3.3）。

---

### 3.7 两级装配模型（core 内置域 + 宿主自定义域）

本设计的装配分**两层**，粒度与作用者都不同：

| 层 | 作用者 | 粒度 | 现状 |
|---|---|---|---|
| **L1 core 内置域** | xisp 自己 | 一个"域"= 一组顶层符号（算术 / 列表 / 高阶 / 哈希表 / 输出…） | `registerAll`（全部）/ `registerPure`（子集）两档 |
| **L2 宿主自定义域** | 宿主 | 同上，但由宿主声明并安装 | 已有 `Bridge.registerFunc*` 的一函数级入口，无"域"抽象 |

**关键结论：`LispCapability` 应该是这两层共用的同一个抽象**——core 内置域把它当作
"自描述的分组"，宿主自定义域把它当作"扩展点"。这样：

- L1 的各域天然可组合（`registerAll` = 域并集，`registerPure` = 显式子集，**不再是两份平行清单**，见 §3.3）；
- L2 复用同一套 `install(ctx)` / `symbols` 声明；
- 沙箱审计、文档生成、重名检出对**两层**统一生效（都读 `symbols` 清单）。

### 3.7.1 预置组合（preset）与自定义组合

`registerAll` / `registerPure` 的定位应重新表述为**预置组合（preset）**，而不是两个函数：

```cangjie
// 草案：域集合（core 内置）
public let DOMAIN_BASE:      LispCapability   // nil / 关键字别名
public let DOMAIN_ARITH:     LispCapability
public let DOMAIN_LIST:      LispCapability
public let DOMAIN_HIGHER:    LispCapability   // map / filter / reduce …
public let DOMAIN_HASHMAP:   LispCapability
public let DOMAIN_OUTPUT:    LispCapability   // 需注入 output 通道，见下

// 预置组合（命名组合，可由 core 提供，也可由宿主自造）
public let PRESET_FULL: Array<LispCapability>   // 全量
public let PRESET_PURE: Array<LispCapability>   // = FULL 减去 可逃逸域 + 策略域
// 宿主可自造：withDomains([DOMAIN_ARITH, DOMAIN_LIST, MyOwnDomain])
```

- **预置组合是"命名 + 域列表"**，宿主可以定义自己的组合，而不只是二选一；
- 组合的语义是**并集**：`withDomains([...])` 逐域 `install`；
- **顺序敏感**：别名域必须在被别名域之后（现有 `registerModernAliases` 已依赖这个次序），
  故 `install` 顺序即声明顺序、且需要一条依赖声明（`after: ["arith"]`）。
- 关于"减法"：**只有 `PRESET_PURE` 这类"由 core 预先定义好的组合"才可能用减法表达**
  （在 core 内部按域属性推导）；宿主若想从全量里减，只能自己列出要装的域
  —— 因为对已安装的环境做"删符号"违背 §3.4（逃逸域已执行过）。

### 3.7.2 与 `InterpreterOption` 的关系（L2 定制的落点）

宿主侧保留既有的 `InterpreterOption` 风格（`withXxx`），域只是其中一个新的选项类型：

```cangjie
public func withDomains(caps: Array<LispCapability>): InterpreterOption
public func withPreset(preset: Array<LispCapability>): InterpreterOption
```

这样不动现有 31 个 `withXxx` 的心智模型，只是把"装哪些符号"从两个硬编码函数
扩展成"可选组合 + 自定义域"。

### 3.7.3 域的属性（决定它能进哪个 preset）

建议每个域声明两个布尔属性，`PRESET_PURE` 由**推导**得出而非手写清单：

| 属性 | 含义 | 例 |
|---|---|---|
| `escapable` | 该域能否绕过权限检查（`eval`/`apply`/`map`/高阶回调） | `higher` = true |
| `hostPolicy` | 该域是否触及宿主资源（文件/网络/进程/输出） | `io`/`fs`/`net` = true |

规则：`PRESET_PURE` = 所有 `!escapable && !hostPolicy` 的域。
**新增域默认 `escapable = true`（保守），须显式声明才进 pure** —— 默认安全。

---

### 3.8 分层要解决的真问题：**可替换**（不只是可裁剪）

> 用户原话："除了最基本的，所有其它的都可以不要，而且同能力（或者符号）我们 core 带了，
> 别人可以不要，直接用宏自行做一个，就是 lisp 大佬说的那样，lisp 实现 lisp 所有。"

这把 §3.6 的"三类/四类"推进成一个更硬的目标：**裁剪是"不给"，替换是"给一个我自己的"。**
两者判据完全不同——可裁剪只看"不注册是否生效"，**可替换还要看"core 的实现会不会被顶掉"**。

#### 3.8.1 实测：可替换性极不均匀（2026-10-11）

在默认环境里先重定义、再用原语法调用，看谁生效：

| 符号 | 类别 | 用户重定义后 | 可替换？ |
|---|---|---|---|
| `car` `length` `not` `+` `<` | ③纯函数 | **用户的实现生效** | ✅ |
| `and` | ② | **用户的实现生效**（`(defmacro my-and …)` + `define`） | ✅（见下注） |
| `when` | ④宏 | **用户的宏生效** | ✅ |
| `map` `filter` `reduce` | ② | `redef=#<procedure>` 但 **probe 仍是 core 的实现** | ❌ **顶不掉** |
| `let` | ① | `(let ((x 1)) x)` 仍返回 `1`（走硬编码） | ❌ |
| `if` `quote` `begin` | ① | 完全无效 | ❌ |

**注**：`and` 之所以能替换，是因为它在**空环境里根本不存在**（`[UndefinedFunction]`），
即它并非真特殊形式；`define` 一个同名宏即可接管。而 `map`/`filter`/`reduce` 在空环境下**仍然可用**
（求值器直接拦截），所以 `define` 只能改"值"、改不了"行为"。

**结论**：现状下，**③④类可替换、①②类不可替换**。而用户要的"除最基本的都能用宏自行实现"，
要求的是**②类也可替换**——它们是"能纯 Lisp 写出来"的高阶函数（`my-map` 实测可用纯 Lisp 实现且行为一致），
却被求值器硬编码占住了名字。

#### 3.8.2 决定性事实：`for-each` 暴露了真正的技术门槛

`for-each` 是③类（纯注册函数），但它**无法把 Lisp 闭包当回调**：

```cangjie
// core/src/core/builtin/builtin.cj:113
if (let NativeFunc(proc) <- args[0]) { … }   // 只认 NativeFunc，Lisp 闭包被静默跳过
```

实测：`(for-each (lambda (x) x) '(1 2 3))` → `nil`（**什么都没发生，静默失败**）。
而 `map`（求值器实现）能正常调用 Lisp 闭包，因为它走内部的 `applyProcedure`。

**这就是"用宏自行实现"的硬门槛**：Lisp 层要自己实现 `map`/`filter`/`reduce`，
就必须能**从 Lisp 调用任意 Lisp 函数值**——而 `applyProcedure` 是 `internal`
（`core/src/core/eval/higher_order.cj:323`），桥接层与注册函数都拿不到。

> 注意：纯 Lisp 自己实现 `map` **是可行的**（实测 `my-map` 递归版行为与 core 一致），
> 因为它由求值器执行、天然能调闭包。**门槛只卡在"仓颉注册的同能力函数"上。**

#### 3.8.3 推论：可实现替换需要两道接缝（本设计新增）

| # | 接缝 | 作用 | 现状 |
|---|---|---|---|
| **A** | **Lisp 函数值调用入口**（`applyProcedure` 公开化，或经 `CapabilityContext.invoke`） | 让仓颉注册函数也能调 Lisp 闭包 → 用户可在④层复刻 `map`/`for-each` | ❌ `internal`，需新增 |
| **B** | **②类符号的"让位"机制** | 用户提供同名实现时，求值器不再走硬编码快速路径 | ❌ 现状硬编码优先 |

**A 的最小形态**（供注册函数与域使用）：

```cangjie
public class CapabilityContext {
    public let env: Environment
    public let isInterrupted: () -> Bool
    public let register: (String, (ArrayList<LispValue>) -> LispValue) -> Unit
    public let registerType: (String) -> Unit
    public let invoke: (LispValue, ArrayList<LispValue>) -> LispValue   // ← 新增：调用任意 Lisp 函数值
}
```

有了 `invoke`，④层宏包/域就能用仓颉或 Lisp 自由复刻 `map`/`filter`/`for-each`，
而 `for-each` 现在那个"只认 NativeFunc"的静默失败也顺带修掉。

**B 的判据**（与 §3.6 的收敛方向一致）：②类符号若**已由用户定义**，求值器应走用户的实现
（即把硬编码降级为"默认实现"而非"抢占名字"）。这与 §3.6 的"②类收敛为一等值"是同一件事的两面：
一等值化之后，"名字被谁占"就可以由环境决定。

#### 3.8.4 分层模型最终形态（四层 × 可替换性）

| 层 | 符号 | 实现 | 可裁剪 | 可替换（目标） |
|---|---|---|---|---|
| ① 语言原语 | `if` `quote` `lambda` `define` `set!` `let` `begin` `defmacro` `match` `import` | 求值器硬编码 | ❌ | ❌（但**可携带宿主策略**，如 `import`+`moduleLoadChecker`） |
| ② 控制流原语 | `and` `or` `apply` `eval` `map` `filter` `reduce` `->`、`+ - * / < > =` | 硬编码 + 注册（双份） | ❌ | **✅ 目标**（接缝 B） |
| ③ 纯注册函数 | `cons` `car` `list` `length` `not` `str`… | 仓颉 `registerXxx` | ✅ | ✅ 现状即可 |
| ④ Lisp 层宏/派生语法 | `when` `unless` `incf` `swap` `push` `pop` `negate` `do` `defun` | **Lisp 源码**（`BuiltinMacros`，10 个） | ✅ | ✅ 现状即可 |

**①的边界就是"lisp 实现 lisp"的边界**：① 不可替换，其余全部应可替换。
用户要的"最基本的那几个"，正对应①（并且与 McCarthy 的 `quote`/`cond`/`lambda`/`label` +
五个初等函数一脉相承，见 §3.6）。**②类被现状错误地钉死在①那一侧**，这是要修的核心。

#### 3.8.5 ④层的落地形态：宏包即域

`BuiltinMacros.registerAll` 现在被 `registerAll` 与 `registerPure` **各调一次**（捆绑）。
域化之后，④层天然就是一个 `LispCapability`——`install` 的形态是**执行 Lisp 源码**而非 `register`：

```cangjie
public class MacroPackCapability <: LispCapability {
    public prop name = "macros.core"
    public prop symbols = ["when", "unless", "incf", "decf", "swap", "push", "pop", "negate", "do", "defun"]
    public prop escapable = false          // 纯语法糖，不逃逸
    public prop hostPolicy = false
    public prop after = ["base", "list"]   // 展开结果依赖 list/cons/if
    public func install(ctx: CapabilityContext): Unit {
        ctx.evalSource(SOURCE)             // ← 与 register 并列的第二种安装形态
    }
}
```

于是域的 `install` 有**两种形态**，正好对应"能力"的两种来源：
- `register`：仓颉实现（需要性能或宿主能力时）；
- `evalSource`：Lisp 源码（语法糖、可用纯 Lisp 表达的能力）。

**用户替换 core 的③类实现时，用自己的宏包顶掉同名域即可**（`after` 保证顺序、`symbols` 用于重名检出）。

---

## 4. 现状事实（装配骨架已存在）

### 4.1 `InterpreterOption` 骨架

- `type InterpreterOption = (LispInterpreter) -> Unit`，已有 31 个 `withXxx` 选项；
- `LispInterpreter.init()` 默认装配：`BuiltinFunctions.registerAll` + `bridge.registerStdIO` +
  `registerStdFS` + `Bridge.registerStdCollection`；
- `init(opts)` 在其后逐个应用选项。

**缺的不是机制，而是"域"这个抽象**：现在只有 all / pure 两档，外加若干零散开关（§3.1）。

### 4.2 真实消费者的实际用法

| 消费者 | 实际用法 | 说明 |
|---|---|---|
| `atelier`（`atelier/src/ptc/runtime.cj`） | 自建 `Environment` + `BuiltinFunctions.registerPure(env, output!)` + `Bridge.registerFunc` + `Evaluator`，**不用 `LispInterpreter`** | 只要纯函数子集 + 自己的工具绑定；输出走 `output` 回调收集 |
| `cordis_xisp` | `types` / `parser` / `core.eval` / `core.builtin.BuiltinFunctions` / `bridge.Bridge` | 同上，小而确定的表面 |

结论：**已有消费者根本不用 `registerAll`**，它们手工拼最小环境。
这既证明"域装配"是真需求，也说明收敛 `registerPure` 时要**保持这条手工路径可用**
（`Environment` + 域安装函数），不能把它藏进 `LispInterpreter` 里。

---

## 5. 域模型

### 5.1 一个域 = 五个要素

1. **注册集合**（哪些顶层符号）—— 靠"不注册"表达禁止（§3.4，但注意 §3.5 对它只覆盖 C 类）；
2. **实现位置**（core 内 / 独立模块）；
3. **默认开关状态**（解释器默认给不给）；
4. **属性**：`escapable`（能否绕过权限检查）、`hostPolicy`（是否触及宿主资源）—— 决定它能否进 `PRESET_PURE`（§3.7.3）；
5. **安装顺序约束**（`after`）—— 别名域必须晚于被别名域（§3.7.1）。

**`LispCapability` 是 L1（core 内置域）与 L2（宿主自定义域）共用的同一个抽象**（§3.7）：

```cangjie
// ystyle::xisp（core 侧，草案）
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

> 早期草案的 `install(interp: LispInterpreter)` 已改为 `install(ctx: CapabilityContext)`：
> 独立模块需要的不止解释器，还需要中断检查器与类型声明通道（§8.1）。

`xisp_net` 依赖 core 并提供 `NetCapability`，core **完全不认识 net** —— 依赖方向正确，
且这套机制对字节码 / JIT 后端同样适用（可与工作区化文档 §3.2 的执行后端接缝统一为同一套宿主接缝）。

**域在 core 内部时**（L1）也走同一接口：把现有 `registerXxx` 包装成域，
`PRESET_FULL` = 所有域的并集、`PRESET_PURE` = 按属性推导的子集（§3.7.3），
**不再维护第二份平行清单**（消除 §3.2 的隐式依赖问题）。

### 5.2 域清单与现状

**A. core 域（编译进 core；目前大多无开关，建议按 §3.3 补上按域装配）**

| 域 | 符号示例 | 现注册点 |
|---|---|---|
| base | `nil`、关键字别名 | `registerAll` 头部 / `env.registerKeywordAlias` |
| arithmetic | `+ - * / mod abs min max` | `registerArithmetic` / `registerModulo` / `registerMath` |
| comparison | `= < > <= >=` | `registerComparison` |
| list | `car` / `cdr` / `cons` / `length` | `registerListOps` |
| higher-order | `map` / `filter` / `reduce` / `apply` / `for-each` | `registerHigherOrder` |
| logic | `and` / `or` / `not` | `registerLogic` |
| predicates | `null?` / `number?` | `registerPredicates` |
| hashmap | `hashmap-*` | `registerHashMap` / `registerRedisAliases` |
| macros | 内建宏 | `BuiltinMacros.registerAll` |
| aliases | 现代 / Redis 别名 | `registerModernAliases` |
| string & output | `str` / `print` / `println` | `registerStringOps` / `registerPrint` / `registerOutput` |

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
| 纯函数子集 | `BuiltinFunctions.registerPure(env, output!)`（§3.3 建议收敛为 `withPureEnv()`） |

**C. 只靠拆模块能解决的域（有外部依赖 / 平台特化）**

| 域 | 模块 | 外部依赖 | 状态 |
|---|---|---|---|
| bytecode + jit | `xisp_bc` | 无外部依赖；含 `rwx`（JIT，仅 Linux x86-64） | ✅ 已拆出并发布 |
| net（http / websocket / tls，见 §6.2） | `xisp_net` | stdx.net.* / stdx.crypto | 规划 |
| data（compress / serialization / json） | `xisp_data` | stdx.compress.* / stdx.serialization | 规划 |
| async | 见 §6.3 | 可能零外部依赖 | 规划 |

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

## 7. 扩展接缝之一：外部值（`External` 变体 + 协议族）

> 这是整份设计中**唯一"只有 core 能给"**的东西：外部模块（NDArray、分布对象、句柄…）
> 要把自己的值交给 Lisp 层，就必须有一个 core 承认的值形态。

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

#### 7.2.1 协议设计：最小必需 + 能力接口（可组合）

```cangjie
// ystyle::xisp.types（core 侧，草案）

/** 必需：core 的通用逻辑（print / type-of / 错误信息）只依赖这两项 */
public interface LispExternal {
    prop typeName: String        // "ndarray"，供 type-of / 谓词 / 错误信息
    func display(): String       // repr；**实现方负责截断**（见 §7.2.2 第 6 条）
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

#### 7.2.2 语义决策（现在就定，别留到实现时随手定）

| # | 议题 | 建议 | 理由 |
|---|---|---|---|
| 1 | `=` 对 External 的语义 | **同一性 + 结构浅比较**（shape / dtype / 共享缓冲），**不做逐元素**；逐元素比较用显式 `array-equal?` | `=` 在 Lisp 里返回 Bool，藏在 `(if (= a b) …)` 里的 O(n) 比较是昂贵意外 |
| 2 | truthy | **External 恒为真**（对象存在即真） | 避免"空数组是假"这类跨域分歧；判空用显式谓词 |
| 3 | 可变 External 作 `HashMap` 键 | **禁止**（未实现 `LispExternalEquatable` 就不能作键） | 可变对象哈希不稳定，会静默破坏 `HashMap` |
| 4 | FFI 分配的资源 | 实现方自行实现 `Resource`；**core 不引入 `close` 语义** | core 不该知道谁持有外部内存 |
| 5 | 序列化 | External **不参与** JSON / 模块序列化；需要时实现 `LispConvertible.toLisp()` 显式转换 | 保持"序列化只处理内建变体"的封闭性 |
| 6 | `display()` 长度 | 由实现方截断 | core 若统一截断，想让实现方打全量就没法表达 |

### 7.3 破坏面实测：不是"全生态大改"，而是 **~24 处补分支**

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

## 8. 扩展接缝之二：域装配与中断注入

### 8.1 中断接缝：外部模块怎么拿到检查器

core 已有 `setInterruptChecker`，但 `Evaluator.isInterrupted()` 是**实例成员**，外部模块手里
只有自己的对象，拿不到求值器。若不解决，就是"`xisp_linalg` 跑 30 秒、宿主杀不掉"。

方案：**在域装配时显式注入**（不用全局状态）：

```cangjie
// core 侧（草案）
public class CapabilityContext {
    public let env: Environment
    public let isInterrupted: () -> Bool                                  // 长循环里周期性调用
    public let register: (String, (ArrayList<LispValue>) -> LispValue) -> Unit
    public let registerType: (String) -> Unit                             // 声明本域产生的外部类型名，供沙箱审计
    public let invoke: (LispValue, ArrayList<LispValue>) -> LispValue    // 调用任意 Lisp 函数值（§3.8.3 接缝 A）
    public let evalSource: (String) -> Unit                               // 以 Lisp 源码安装（宏包形态，§3.8.5）
}

public interface LispCapability {
    prop name: String
    prop symbols: Array<String>
    func install(ctx: CapabilityContext): Unit
}
```

契约（写进模块 README 与域声明）：

> 外部模块中任何 O(元素数) 以上的循环，必须周期性调用 `ctx.isInterrupted()`，
> 为真时以 `Error(RuntimeError, INTERRUPTED_MESSAGE)` 中止。

### 8.2 命名与符号冲突

- **模块名不能含连字符**（cjpm 要求合法标识符）：仓库可以叫 `xisp-net`，但模块必须是
  `xisp_net`，包名 `ystyle::xisp_net`；
- 扩展域会引入大量顶层符号（`matmul` / `mean` / `fft` / `shape`），与 core 内建**重名风险高**。
  建议：域注册时声明命名空间或统一前缀，并**把符号清单作为域声明的一部分**
  （§5.1 的 `LispCapability.symbols`）。好处有三：沙箱可审计、文档可自动生成、
  重名能在装载时被检出，而不是运行时静默覆盖。

---

## 9. 与 core 的接缝共三件 + 建议实施顺序

| # | 接缝 | 设计位置 | 状态 |
|---|---|---|---|
| 0 | **符号归类收敛**（①原语 / ②控制流原语 / ③纯函数 / ④Lisp 层宏；②收敛为一等值） | §3.6、§3.8 | **待做，且应最先**（§3.5 的实测表明不做它，能力域模型只覆盖 1/3 的符号） |
| 0a | **Lisp 函数值调用入口**（`applyProcedure` 公开化 / `ctx.invoke`） | §3.8.3 | **待做**；修 `for-each` 只认 `NativeFunc` 的静默失败，是"用户自行实现 map"的前提 |
| 0b | **②类符号"让位"机制**（已由用户定义则不走硬编码快速路径） | §3.8.3 | **待做**；这是"可替换"的核心 |
| 1 | 域抽象 `LispCapability` + 预置组合（`PRESET_FULL` / `PRESET_PURE`）+ `withDomains` | §3.7、§5.1 | **待做** |
| 2 | `External` 变体 + `LispExternal` 协议族 | §7 | **待做**，破坏性变更 |
| 3 | `CapabilityContext`（中断注入、类型声明） | §8.1、§5.1 | **待做** |
| 4 | 外部模块的**中断义务**（长循环必须查中断） | §8.1 | 机制已有，契约待写进模块 README |

> 第 3 条最容易被忽略：core 已有 `setInterruptChecker`（`evaluator.cj`），
> `INTERRUPTED_MESSAGE` 也已是 public。但沙箱 `timeout` 与宿主取消**只能打断求值热路径**
> —— 域模块里一个跑 30 秒的循环若不主动查中断，宿主就**杀不掉它**。

**建议实施顺序**（若决定做）：

1. **符号归类收敛**（接缝 0 / 0a / 0b）：把 §3.6 的四层落成代码事实——②类收敛为一等值
   （`(procedure? map)` 为真、可作值传递）**且可被用户实现替换**（接缝 0b）；
   公开 Lisp 函数值调用入口（接缝 0a，同时修 `for-each` 的 `NativeFunc`-only 静默失败）；
   并让两个后端语义一致（§3.5 问题 3.3）；顺带修 `funcChecker` 对①②类失效与 `and`/`or` 不短路
   （见 `docs/bugs/symbol-layering-findings.md`）。
   **这一步不做，后面的域装配只能管住纯函数，且用户无法用宏顶掉 core 的同能力实现**；
2. 域抽象落地：把现有 `registerXxx` 包装成 `LispCapability`，`registerAll` / `registerPure`
   改表述为 `PRESET_FULL` / `PRESET_PURE`（后者按 `escapable`/`hostPolicy` 推导），
   补 `withDomains` / `withPreset`；
3. `types` 包加协议族 + 变体 → 编译驱动修 core 内那 12 处，补单元测试；
4. core 通用语义按能力接口分派：`print` / `type-of` / `=` / `length` / `nth` / 谓词；
5. `CapabilityContext`（中断注入、类型声明）；
6. **先用一个测试用假扩展**（如 `ExternalDemo`：一个极简的外部计数器值）端到端验证
   —— 注册域 → 产生 External → `print` / `=` / 中断全部走通；
7. 之后才开科学计算族的地基模块（见下游文档）。

---

## 10. 风险与未决问题

| # | 项 | 说明 |
|---|---|---|
| C1 | 能力域粒度 | core 域的按域装配是否需要（如"只要算术+表，不要高阶函数"），需要真实宿主诉求来定；§3.3 的"能力 + 可逃逸性"主轴是待评审的倾向 |
| C2 | 域装配与执行后端接缝是否合并 | §5.1 的 `LispCapability` 与工作区化文档 §3.2 的 `ExecutionBackend` 形态相近，成熟后宜统一为同一套宿主接缝 |
| C3 | `External` 变体的归属 | 只加 `External`，还是同时加专用变体（见下游文档的比选）；**待拍板** |
| C4 | 变体与协议命名 | 建议变体 `External` + 协议 `LispExternal`（二者不能同名，§7.2） |
| C5 | 符号命名策略 | 命名空间（`(import sci.linalg)`）还是前缀（`linalg-`）？待定，与 `LispCapability.symbols` 声明配套 |
| C6 | 是否把"`LispValue` 的 match 必须有外部值分支"写成 lint | 加变体时编译器能强制一次，但**将来再加变体还会重演**；建议留一条 CI 检查 |
| C7 | stdx 环境变量 | `CANGJIE_STDX_PATH` 路径三段随安装方式而变，CI/容器/新机器默认不存在；对策见 §6.1，"core 与其它成员不得 import stdx"是硬约束 |
| C8 | 多成员样板重复 | `bin-dependencies` / `link-option` 必须在每个要发布的成员里各写全份（cordis-cj 实测 42 处重复），且 cjpm-ws 目前不同步这类字段。缓解：约定"只有能力域成员才允许出现 stdx 配置" + CI 校验各成员该段一致 |
| C9 | `registerPure` 的归宿 | 收敛为 `withPureEnv()` 还是保持现状（§3.3）；改动会影响 atelier 的手工装配路径（§4.2） |
| C10 | 网络域走哪条路 | 应用层走 stdx 还是第三方纯仓颉库，见 §6.2，需按"是否引入 stdx"这一判据调研后再定 |
| C11 | ②类符号收敛的具体形态 | 目标是"真一等值"，但有两条路：① 求值器保留原语语义、额外注册一个一等值（值即能力）；② 改为"取函数值再调用"（把控制流语义移进函数）。后者更干净但需改求值器核心，前者改动小但两套语义仍在。**待定** |
| C13 | **可替换性的边界**：哪些符号"允许"被用户顶掉 | ①明确不可替换；②③④应可替换。但需定：用户顶掉 core 实现时，**是静默生效还是需显式声明**（如 `withOverride("map", ...)`）？静默容易被误覆盖，显式则更啰嗦。**待定** |
| C14 | ②类"让位"的性能代价 | 现状 `+ - * / < > =` 走快速路径（`arithmetic_fast.cj`）是性能关键。若改为"已定义则让位"，需确认检查开销不进热路径（可用 `env.arithVersion` 那套版本号缓存，已有先例） |
| C15 | `evalSource` 安装形态的沙箱影响 | 宏包用 `eval` 安装源码；若宿主禁用了 `eval`，宏包能否安装？（安装是宿主行为、非脚本行为，应允许——需在实现时明确区分"宿主安装期"与"脚本运行期"） |
| C12 | `PRESET_PURE` 用减法还是显式列表 | core 内部可按 `escapable`/`hostPolicy` **推导**（推荐，默认安全）；宿主要做减法则只能自己列域（§3.7.1）。需确认"新增域默认 `escapable=true`"是否会给现有域带来意外排除 |
