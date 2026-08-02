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

本文档覆盖 `src/options.cj` 中全部选项函数。

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

## 参见

- [嵌入解释器](embedding) - `LispInterpreter` 基本用法
- [沙箱](sandbox) - 沙箱选项的机制与最佳实践
- [桥接](bridge) - 标准库桥接函数与自定义函数注册
