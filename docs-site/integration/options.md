# 选项系统

Xisp 的选项系统用「选项数组」配置解释器。所有选项都是 `InterpreterOption` 类型（即 `(LispInterpreter) -> Unit`），可以自由组合、顺序应用。

```cangjie
public type InterpreterOption = (LispInterpreter) -> Unit
```

```cangjie
let interpreter = LispInterpreter([
    withStdLib(),
    withChineseKeywords(),
    withQuietMode()
])
```

本文档覆盖核心（`core/src/options.cj`）与可选后端模块（`xisp_bc/src/backend.cj`）提供的全部选项函数。
核心**不含**字节码 / JIT 选项——它们随 [模块与依赖](modules) 所述的后端模块提供。

---

## 装配档位（宿主首选）

不想逐个挑能力域时，用**档位**一次性配好 —— `withLevel(level)` 会**独占装配**：
先清空环境，只装该档需要的域，结果与"解释器默认装了什么"无关，可复现。

| 档位 | 装什么 | 典型用途 |
|------|--------|----------|
| `XispLevel.Expr` | 纯计算：算术 / 比较 / 列表 / 逻辑 / 谓词 / 字符串 / 哈希表 / 错误 / 别名。**无宏、无 `gensym`、无输出** | 公式、规则、表达式求值 —— 只要算出一个值 |
| `XispLevel.Pure` | `Expr` + ④层宏包（`when` / `unless` / `incf` / `defun` …）+ `gensym`。仍然无输出 | 需要语法糖的嵌入场景 |
| `XispLevel.Script` | 全量域 + `std.io` / `std.fs` / `std.collection` | 脚本 / CLI：读写文件、模块加载、打印 |

三档是**严格包含**关系：`Expr ⊂ Pure ⊂ Script`。

```cangjie
// 规则求值：没有 println，也不可能读写文件
let calc = LispInterpreter([withLevel(XispLevel.Expr)])
calc.eval("(+ 1 2)")      // => 3
calc.eval("(when #t 1)")  // => [UndefinedFunction] 'when'（Expr 档无宏包）
calc.eval("(println 1)")  // => [UndefinedFunction] 'println'（Expr 档无输出域）

// 脚本：全量 + 标准库，再叠一层沙箱
let script = LispInterpreter([withLevel(XispLevel.Script), withSandbox()])
```

::: tip 档位只管「装哪些能力」，不管「允许做什么」
`withSandbox` / `withTimeout` / `withMaxStackDepth` / `withAllowedPaths` 等策略选项
与档位**正交**，可任意叠加。
:::

::: warning 独占装配会清空环境
- 档位选项应**只出现一个**（多个时以最后应用的为准）；
- 在档位之后再 `withDomains(...)` 只做**追加**，不会把默认全量装回来；
- `clearEnvironmentBindings()` 换环境时会一并重建 ②类符号的一等值（`map` / `filter` / …）
  与桥接目标，`withNoFirstClassValues()` 的关闭状态仍然有效。
:::

需要更细的控制时，用 `withDomains` / `withPreset` 自己组合域集合（见[能力域](#能力域装配)）。

---

## 能力域装配

「能力域」是**一个域 = 一组顶层符号 + 一份安装函数**的抽象。core 内置 13 个域，
宿主也可以自己造域（这是宿主扩展解释器的唯一入口）。

### 内置域

| 域 | 符号 | 属性 |
|----|------|------|
| `base` | `nil` | — |
| `arithmetic` | `+ - * / mod round sum product max min` | — |
| `comparison` | `= < > <= >=` | — |
| `list` | `cons car cdr list length nth append reverse range` | — |
| `logic` | `not eq?` | — |
| `predicates` | `number? string? symbol? list? procedure? integer? float? zero? positive? negative? even? odd? null? external? external-type type-of` | — |
| — | `hashmap?` 在 `hashmap` 域（由 `registerHashMap` 注册） | — |
| `hashmap` | `hashmap hashmap-get hashmap-set! hashmap-remove! hashmap-contains? hashmap-keys hashmap-values hashmap-size hashmap? hget hset hdel hexists contains? hkeys hvals hlen hgetall` | — |
| `string` | `str string-length string=? string< string> string-contains?` | — |
| `print` | `print println princ newline display` | `hostPolicy`（触及宿主输出通道） |
| `symbols` | `gensym` | — |
| `aliases` | `first rest second third fourth prepend more cadr caddr cadddr cddr cdar caar setq progn string-append` | `after: base, list, string` |
| `error` | `error raise assert` | — |
| `macros.core` | 16 个内建宏（`when` / `unless` / `condb` / `if-let` / `let*` / `when-let*` / `and` / `or` / `incf` / `decf` / `negate` / `swap` / `push` / `pop` / `do` / `defun`） | `evalSource` 形态 |

**域的 `symbols` 声明就是契约**：单独装这个域（连同它 `after` 依赖的传递闭包）后，
声明里的符号全部可用。

### 两个预置组合

| 组合 | 内容 | 用途 |
|------|------|------|
| `presetFull()` | 全部 13 个域 | 「全量」，与 `LispInterpreter()` 默认装配等价 |
| `presetPure()` | 按属性推导：`!escapable && !hostPolicy` | 纯函数子集；**不含** `print` 域，输出由宿主用 `output` 接管 |

`presetPure()` 是**推导**出来的，不是手写清单 —— 新增域默认 `escapable = true`，
因此不会意外进入纯函数子集（默认安全）。

```cangjie
// 独占装配：清空默认全量，只留纯函数子集
let pure = LispInterpreter([withPreset(presetPure(), replace: true)])

// 追加：在默认全量之上再加自己的域
let extended = LispInterpreter([withDomains([myDomain])])
```

### 自己造域

```cangjie
public class MyCap <: LispCapability {
    public prop name: String { get() { "my.cap" } }
    public prop symbols: Array<String> { get() { ["my-fn"] } }
    public prop escapable: Bool { get() { false } }     // 能否绕过权限检查
    public prop hostPolicy: Bool { get() { false } }    // 是否触及宿主资源
    public prop after: Array<String> { get() { [] } }   // 安装顺序约束

    public func install(ctx: CapabilityContext): Unit {
        ctx.register("my-fn", { args => LispValue.Int(42) })
        // 声明外部类型名（供沙箱审计）
        // ctx.registerType("my-object")
    }
}
```

也可以直接用 `SimpleCapability`：

```cangjie
let domain = SimpleCapability("demo.counterbox", ["make-box"],
    { ctx =>
        ctx.register("make-box", { args => LispValue.External(CounterBox(0)) })
        ctx.registerType("counter-box")
    },
    escapable: false, hostPolicy: false)
```

`CapabilityContext` 提供：

| 成员 | 用途 |
|------|------|
| `env` | 目标环境（直接 `define` 也可以） |
| `register(name, handler)` | 注册一个仓颉侧函数 |
| `registerType(name)` | 声明一个外部类型名（沙箱审计用） |
| `invoke(fn, args)` | 以**求值器语义**调用一个 Lisp 函数值（能调闭包与宏以外的一切可调用值） |
| `evalSource(source)` | 用 Lisp 源码安装一域（**宿主安装期**行为，不受脚本侧 `eval` 权限约束） |
| `isInterrupted` | 中断检查器 |

::: warning 长循环必须查中断
外部域中任何与元素数 / 迭代步数相关的循环，都要周期性调用 `ctx.isInterrupted()`，
为真时返回错误值（`Error` + `INTERRUPTED_MESSAGE`）。不查中断的话，宿主**杀不掉**
一个跑几十秒的循环 —— 详见 `core/README.md`「扩展模块的中断义务」。
:::

### 域装配动不了的东西

- **①语言原语**（`if` / `let` / `lambda` / `quote` / `set!` / `define` / `match` / `import` …）
  不在环境里，域装配无法移除；
- **②求值器拦截的形式**（`map` / `filter` / `reduce` / `apply` / `eval` / `->` / `for-each`）
  不属任何域，同样装不掉。它们**既可被调用，也可作为值传递**（`(procedure? map)` → `true`）。

所以「只要算术+列表、不要高阶函数」是**做不到**的：高阶形式本就不在域里。
这是设计边界（能力域设计 §5.3），不是遗漏。

---

## 关键字别名

| 选项 | 说明 |
|------|------|
| `withKeywordAlias(alias, original)` | 注册单个别名，如 `定义` → `define` |
| `withKeywordAliases(aliases)` | 批量注册，参数为 `(别名, 原关键字)` 数组 |
| `withChineseKeywords()` | 预设中文：定义/过程/如果/让/打印/显示 等 |
| `withJapaneseKeywords()` | 预设日语：定義/もし/表示 等 |
| `withKoreanKeywords()` | 预设韩语：정의/만약/출력 等 |

```cangjie
// 企业场景：注册业务惯用别名
let interpreter = LispInterpreter([
    withKeywordAliases([
        ("def", "define"),
        ("fn", "lambda")
    ])
])

// 中文开发
let cn = LispInterpreter([withChineseKeywords()])
cn.eval("(定义 年龄 25)")
cn.eval("(打印 (过程 (x) (* x x)))")

// 多语言可叠加
let multi = LispInterpreter([
    withChineseKeywords(),
    withJapaneseKeywords(),
    withKeywordAlias("정의", "define")   // 额外补充韩语
])
```

::: tip 底层方法
选项最终调用 `interpreter.registerKeywordAlias(alias, original)`，在运行时也可直接调用该方法。
:::

---

## 标准库模块

| 选项 | 注册内容 |
|------|----------|
| `withStdIO()` | `cangjie::read-file` / `write-file` / `append-file` |
| `withStdFS()` | `cangjie::exists?` / `file?` / `directory?` / `list-dir` |
| `withStdCollection()` | `cangjie::vector` / `hashmap` / `hashset` / `interpolate`（现代语法字面量底层函数） |
| `withStdLib()` | 上述全部（io + fs + collection） |

```cangjie
// 仅启用文件读写
let interpreter = LispInterpreter([withStdIO()])

// 全部启用
let full = LispInterpreter([withStdLib()])
full.eval("(cangjie::write-file \"/tmp/a.txt\" \"hello\")")
full.eval("(cangjie::read-file \"/tmp/a.txt\")")
```

::: tip 默认行为
`LispInterpreter()` 默认构造已经注册了内置函数以及 std.io / std.fs / std.collection 桥接，`withStdLib()` 是显式声明这些能力，二者可并存（重复注册无害）。
:::

---

## 模式与输出

| 选项 | 说明 |
|------|------|
| `withQuietMode()` | 静默模式，不显示 REPL 横幅（调用 `setShowBanner(false)`） |
| `withVerboseMode()` | 详细模式，打印配置动作日志（调用 `setVerbose(true)`） |
| `withDebugMode()` | 调试模式（调用 `setDebug(true)`） |
| `withOutputFn(fn)` | 自定义输出函数，`fn: (String) -> Unit` |

```cangjie
// 把解释器输出重定向到日志
let interpreter = LispInterpreter([
    withQuietMode(),
    withOutputFn({ s => eprintln(s) })
])
```

---

## 加载模式

| 选项 | 说明 |
|------|------|
| `withScriptMode()` | 脚本模式，允许 `(import "./xxx")` 相对文件导入 |
| `withModuleMode()` | 模块模式，禁止文件导入（嵌入场景默认） |

```cangjie
// REPL / CLI 脚本：允许相对导入
let interpreter = LispInterpreter([
    withStdLib(),
    withScriptMode()
])
interpreter.eval("(import \"./utils.lisp\")")

// 嵌入场景：默认 ModuleMode，禁止文件导入
let embedded = LispInterpreter([withModuleMode()])
```

---

## 字节码与 JIT（可选后端）

这两项选项由可选模块 `ystyle::xisp_bc` 提供（核心不含），使用前需先依赖它：

```toml
[dependencies]
  "ystyle::xisp_bc" = "0.5.0"
```

| 选项 | 说明 |
|------|------|
| `withBytecodeCompiler()` | 启用字节码编译器：支持的表达式编译为字节码执行，不支持的自动回退 AST |
| `withJit()` | 启用 JIT（**叠在字节码之上**，需同时开字节码）；未支持的指令 / 类型按函数级回退解释器 |

```cangjie
import ystyle::xisp.*
import ystyle::xisp_bc.*

// 只启用字节码
let bc = LispInterpreter([withBytecodeCompiler()])

// 字节码 + JIT（仅 Linux x86-64，其它平台自动降级为字节码）
let jit = LispInterpreter([withBytecodeCompiler(), withJit()])
```

::: tip 不依赖后端就没有这些代码
不依赖 `ystyle::xisp_bc` 的宿主，产物中不含字节码与 JIT 代码；
其 `ExecutionBackend` 接缝保持未注入状态，全程 AST 解释。
:::

---

## 模块数据源与搜索路径

| 选项 | 说明 |
|------|------|
| `withModuleSource(source)` | 设置自定义模块数据源（`ModuleSource` 实现） |
| `withModulePath(path)` | 添加单个模块搜索路径 |
| `withModulePaths(paths)` | 批量添加模块搜索路径 |

`ModuleSource` 提供三种实现：`MemorySource`（内存）、`FileSystemSource`（文件系统，默认）以及任意自定义实现。

```cangjie
// 从内存注册并加载模块
let source = MemorySource()
source.registerModule(
    "myapp::utils",
    "(module utils (version \"1.0\"))",
    HashMap<String, String>([
        ("core.lisp", "(export greet) (define (greet name) (str \"Hello, \" name \"!\"))")
    ])
)

let interpreter = LispInterpreter([
    withStdLib(),
    withModuleSource(source)
])
interpreter.eval("(import myapp::utils)")
let result = interpreter.eval("(utils.greet \"World\")")
// result: Str("Hello, World!")
```

```cangjie
// 添加模块搜索路径
let interpreter = LispInterpreter([
    withModulePath("./lib/modules"),
    withModulePaths(["/usr/share/xisp/modules"])
])
```

---

## 沙箱与安全选项

| 选项 | 说明 |
|------|------|
| `withSandbox()` | 严格沙箱：栈深度 500、超时 30 秒、禁止文件写入、禁止模块加载、禁止 `eval`/`apply` |
| `withSandboxLenient()` | 宽松沙箱：栈深度 5000、无超时、不限制文件与函数 |
| `withTimeout(Option<Duration>)` | 执行超时，`None` 表示无限制 |
| `withMaxStackDepth(Int64)` | 最大调用栈深度（默认 1000） |
| `withMaxMemory(Option<Int64>)` | 内存限制（字节），`None` 表示无限制 |
| `withBlockedFunctions(Array<String>)` | 函数黑名单 |
| `withAllowedFunctions(Array<String>)` | 函数白名单 |
| `withNoFileWrite()` | 禁止所有文件写入 |
| `withAllowedPaths(Array<String>)` | 允许访问的路径前缀白名单 |
| `withNoModuleLoad()` | 禁止模块加载 |
| `withAllowedModulePaths(Array<String>)` | 允许加载模块的路径前缀白名单 |

```cangjie
import std.time.*

// 严格沙箱
let strict = LispInterpreter([
    withSandbox(),
    withStdIO(),       // 文件 I/O 可用，但写入被沙箱禁止
    withQuietMode()
])

// 只允许算术运算的白名单
let calc = LispInterpreter([
    withAllowedFunctions(["+", "-", "*", "/", "mod", "<", ">", "="]),
    withQuietMode()
])
calc.eval("(+ 1 2)")   // 3
calc.eval("(println \"x\")")  // Error: function not allowed

// 只允许写入 /tmp/
let paths = LispInterpreter([
    withAllowedPaths(["/tmp/"]),
    withStdIO()
])
paths.eval("(cangjie::write-file \"/tmp/safe.txt\" \"data\")")    // 允许
paths.eval("(cangjie::write-file \"/etc/passwd\" \"hack\")")      // 拒绝
```

::: warning 白名单行为
设置白名单后，只有名单内的函数可被调用。例如 `withAllowedFunctions(["+", "-"])` 会阻止 `*`、`/` 等未列入白名单的算术函数（这是已修复的预期安全行为）。白名单需包含所用到的全部函数与特殊形式关键字。
:::

::: tip 命名差异
选项层使用 `withNoFileWrite()` 与 `withNoModuleLoad()`；对应的解释器运行时方法名为 `blockFileWrite()` 与 `blockModuleLoad()`。
:::

沙箱机制的完整说明见 [沙箱](sandbox)。

---

## 完整选项速查表

| 选项 | 参数 | 默认行为 | 对应解释器方法 |
|------|------|----------|----------------|
| `withLevel` | `XispLevel`（`Expr` / `Pure` / `Script`） | — | 独占装配（清空 + 装域 [+ std 桥接]） |
| `withDomains` | `Array<LispCapability>`，可选 `replace` | 追加（`replace = false`） | `installDomains` |
| `withPreset` | `Array<LispCapability>`，可选 `replace` | 追加 | `installDomains` |
| `presetFull` | 无 | 全部 13 个域 | `BuiltinFunctions.presetFull` |
| `presetPure` | 无 | 按属性推导（无 `print`） | `BuiltinFunctions.presetPure` |
| `withNoFirstClassValues` | 无 | ②类符号不注册为一等值 | `setFirstClassValues(false)` |
| `withKeywordAlias` | `(alias, original): (String, String)` | — | `registerKeywordAlias` |
| `withKeywordAliases` | `Array<(String, String)>` | — | `registerKeywordAlias` |
| `withChineseKeywords` | 无 | 中文预设别名 | `registerKeywordAlias` |
| `withJapaneseKeywords` | 无 | 日语预设别名 | `registerKeywordAlias` |
| `withKoreanKeywords` | 无 | 韩语预设别名 | `registerKeywordAlias` |
| `withStdIO` | 无 | — | `registerStdIO` |
| `withStdFS` | 无 | — | `registerStdFS` |
| `withStdCollection` | 无 | — | `registerStdCollection` |
| `withStdLib` | 无 | io + fs + collection | `registerStdIO/FS/Collection` |
| `withQuietMode` | 无 | 隐藏横幅 | `setShowBanner(false)` |
| `withVerboseMode` | 无 | 打开详细日志 | `setVerbose(true)` |
| `withDebugMode` | 无 | 打开调试 | `setDebug(true)` |
| `withOutputFn` | `(String) -> Unit` | 默认 `println` | `setOutputFn` |
| `withScriptMode` | 无 | — | `setScriptMode` |
| `withModuleMode` | 无 | 默认嵌入模式 | `setModuleMode` |
| `withModuleSource` | `ModuleSource` | 默认文件系统 | `setModuleSource` |
| `withModulePath` | `String` | — | `addModuleSearchPath` |
| `withModulePaths` | `Array<String>` | — | `addModuleSearchPath` |
| `withSandbox` | 无 | 严格模式 | `enableSandbox` 等组合 |
| `withSandboxLenient` | 无 | 宽松模式 | `enableSandbox` 等组合 |
| `withTimeout` | `Option<Duration>` | 无超时 | `setTimeout` |
| `withMaxStackDepth` | `Int64` | 1000 | `setMaxStackDepth` |
| `withMaxMemory` | `Option<Int64>` | 无限制 | `setMaxMemory` |
| `withBlockedFunctions` | `Array<String>` | 空 | `blockFunction` |
| `withAllowedFunctions` | `Array<String>` | 无白名单 | `setAllowedFunctions` |
| `withNoFileWrite` | 无 | 允许写入 | `blockFileWrite` |
| `withAllowedPaths` | `Array<String>` | 无限制 | `setAllowedPaths` |
| `withNoModuleLoad` | 无 | 允许加载 | `blockModuleLoad` |
| `withAllowedModulePaths` | `Array<String>` | 无限制 | `setAllowedPaths` |
| `withBytecodeCompiler` ★ | 无 | 未启用（AST 解释） | `setExecutionBackend` |
| `withJit` ★ | 无 | 未启用 | `JitRuntime()` |

★ 由可选模块 `ystyle::xisp_bc` 提供，核心不含。

## 参见

- [嵌入解释器](embedding) - `LispInterpreter` 基本用法
- [沙箱](sandbox) - 沙箱选项的机制与最佳实践
- [桥接](bridge) - 标准库桥接函数与自定义函数注册
- [模块与依赖](modules) - 7 个成员模块、依赖方向与按需依赖
