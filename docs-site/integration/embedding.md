# 嵌入解释器

将 Xisp 嵌入到仓颉程序中，核心入口是 `LispInterpreter` 类（`src/interpreter.cj`）。通过它可以在仓颉中执行 Lisp 代码、加载脚本、启动 REPL，以及反向调用 Lisp 函数。

---

## 创建解释器

```cangjie
import ystyle::xisp.*

// 默认配置
let interpreter = LispInterpreter()

// 带选项配置
let configured = LispInterpreter([
    withStdLib(),
    withQuietMode(),
    withMaxStackDepth(2000)
])
```

默认构造会完成以下初始化（`interpreter.cj`）：

- 创建 `Environment` 与 `Evaluator`
- 注册全部内置函数（`BuiltinFunctions.registerAll`）
- 注册标准桥接函数：std.io（`cangjie::read-file` 等）、std.fs（`cangjie::exists?` 等）、std.collection（`cangjie::vector` 等）
- 挂接函数权限、路径权限、模块加载权限检查回调

带选项的构造（`init(opts: Array<InterpreterOption>)`）会先初始化模块系统，再依次应用每个选项。选项系统见 [选项系统](options)。

::: tip 选项数组类型
`InterpreterOption` 即 `(LispInterpreter) -> Unit`，所有 `withXxx()` 函数都返回该类型，因此可以放进数组组合使用。
:::

---

## 求值 Lisp 代码

### eval：求值单个/多个表达式

`eval(code: String): LispValue` 解析并求值代码，返回最后一个表达式的值：

```cangjie
let interpreter = LispInterpreter([withStdLib()])

let result = interpreter.eval("(+ 1 2 3)")
// result: LispValue.Int(6)  即 6

// 多个表达式顺序执行，返回最后一个结果
let multi = interpreter.eval("(define x 10) (define y 20) (+ x y)")
// multi: 30
```

行为细节：

- 代码可包含多个顶层表达式，依次求值后返回最后一个结果。
- 解析或求值失败时打印 `Error: ...` 并返回 `Nil`。
- 若已启用沙箱（`enableSandbox` / `withSandbox`），会改走 `evalInSandboxInternal`，在独立线程中执行并受超时限制。

### evalMultiple：顺序求值（忽略错误）

`evalMultiple(code: String): LispValue` 与 `eval` 类似，但遇到 `Error` 结果时会打印错误信息并**继续**执行后续表达式：

```cangjie
let result = interpreter.evalMultiple("(error \"oops\") (define ok 1) ok")
// 打印错误信息，但继续执行，result 为 1
```

### evalFile：执行脚本文件

`evalFile(filePath: String): LispValue` 读取并执行 Lisp 脚本，返回最后一个表达式的结果：

```cangjie
// 脚本模式下支持 (import "./utils.lisp") 相对导入
let interpreter = LispInterpreter([withStdLib(), withScriptMode()])

let result = interpreter.evalFile("script.lisp")
```

执行期间会将 `currentFilePath` 设为该文件的规范化路径，以支持相对导入，结束后恢复原值。文件读取失败或执行异常时返回 `LispValue.Error` 并打印 `eprintln` 错误信息。

::: tip 命令行等价物
CLI 的 `./xisp script.lisp` 与 `./xisp -c "(+ 1 2)"` 内部即基于 `evalFile` 与 `eval` 实现。
:::

---

## 启动 REPL

```cangjie
let interpreter = LispInterpreter([withStdLib()])

// 增强版 REPL（推荐），返回 Int64
let code = interpreter.runREPL()

// 简易版 REPL
interpreter.runSimpleREPL()
```

- `runREPL(): Int64` 启动 `EnhancedRepl`，支持横幅、`import` 等增强交互。
- `runSimpleREPL(): Int64` 启动基础 `Repl`。
- 两者都可通过 `setShowBanner(false)` 或 `withQuietMode()` 关闭横幅。

---

## 反向调用 Lisp 函数（仓颉 → Lisp）

`call<T>(funcName: String, args: Array<T>): LispValue` 从仓颉调用已定义的 Lisp 函数，参数自动经 `LispConvertible` 转换为 LispValue：

```cangjie
let interpreter = LispInterpreter([withStdLib()])

// 先定义 Lisp 函数
interpreter.eval("(define (square x) (* x x))")

// 从仓颉调用，参数 [5] 中 Int64 自动转 LispValue
let result = interpreter.call("square", [5])

// 提取结果
if (let Some(i) <- result.asInt()) {
    println(i)  // 输出: 25
}

// 多参数、字符串、布尔、无参均支持
interpreter.call("add-three", [1, 2, 3])     // => 6
interpreter.call("greet", ["World"])          // => "Hello, World"
interpreter.call("is-positive", [5])          // => true
interpreter.call("constant", [])              // => 42
```

类型转换与 `asInt()` 等提取方法详见 [桥接](bridge)。

---

## 获取与重置环境

```cangjie
// 获取顶层环境，用于底层操作（注册函数、查询绑定等）
let env = interpreter.getEnvironment()

// 重置解释器：重建 Environment/Evaluator，重新注册内置函数与标准桥接
// 注意：不重置配置字段（showBanner/verbose/debug/outputFn）
interpreter.reset()
```

---

## 运行时配置

除构造时使用选项外，也可以在运行时调用配置方法（均对应一个 `withXxx` 选项）：

| 方法 | 说明 | 对应选项 |
|------|------|----------|
| `setMaxStackDepth(depth: Int64)` | 设置最大调用栈深度，并同步到 Evaluator | `withMaxStackDepth` |
| `setTimeout(timeout: Option<Duration>)` | 设置执行超时（沙箱下生效） | `withTimeout` |
| `setMaxMemory(bytes: Option<Int64>)` | 设置内存限制（字节） | `withMaxMemory` |
| `setShowBanner(show: Bool)` | 控制 REPL 横幅 | `withQuietMode` |
| `setVerbose(verbose: Bool)` | 详细模式，打印配置动作日志 | `withVerboseMode` |
| `setDebug(debug: Bool)` | 调试模式 | `withDebugMode` |
| `setOutputFn(fn: (String) -> Unit)` | 自定义输出函数（默认 `println`） | `withOutputFn` |
| `registerKeywordAlias(alias, original)` | 注册关键字别名（如 `定义` → `define`） | `withKeywordAlias` |

```cangjie
import std.time.*

let interpreter = LispInterpreter()

// 运行时修改配置
interpreter.setMaxStackDepth(500)
interpreter.setTimeout(Some(Duration.second * 30))
interpreter.setOutputFn({ s => eprintln(s) })   // 输出重定向到 stderr
interpreter.registerKeywordAlias("定义", "define")
```

沙箱相关的运行时配置（`enableSandbox`、`blockFunction`、`blockFileWrite`、`blockModuleLoad`、`setAllowedFunctions` 等）见 [沙箱](sandbox)。

---

## 完整示例

一个把 Xisp 作为配置/计算引擎嵌入的完整示例：

```cangjie
import std.time.*
import ystyle::xisp.*

main() {
    // 1. 创建带选项的解释器
    let interpreter = LispInterpreter([
        withStdLib(),                 // 标准库桥接
        withQuietMode(),              // 不显示横幅
        withScriptMode(),             // 允许相对导入
        withMaxStackDepth(2000),      // 栈深度限制
        withTimeout(Some(Duration.second * 10))  // 10 秒超时
    ])

    // 2. 定义 Lisp 工具函数
    interpreter.eval("""
        (define (discount price rate)
            (* price (- 1.0 rate)))
        (define (format-total total)
            (str "total: " total))
    """)

    // 3. 从仓颉调用
    let total = interpreter.call("discount", [200.0, 0.2])   // 160.0
    let text = interpreter.call("format-total", [160.0])
    if (let Some(s) <- text.asString()) {
        println(s)   // 输出: total: 160.0
    }

    // 4. 执行脚本文件
    interpreter.evalFile("calc.lisp")
}
```

---

## 关键 API 汇总

| API | 签名 | 说明 |
|-----|------|------|
| `eval` | `func eval(code: String): LispValue` | 求值代码，返回最后一个结果；沙箱下走独立线程并受超时控制 |
| `evalMultiple` | `func evalMultiple(code: String): LispValue` | 顺序求值，打印错误后继续执行 |
| `evalFile` | `func evalFile(filePath: String): LispValue` | 执行脚本文件，支持相对导入 |
| `runREPL` | `func runREPL(): Int64` | 启动增强版 REPL |
| `runSimpleREPL` | `func runSimpleREPL(): Int64` | 启动简易版 REPL |
| `call` | `func call<T>(funcName, args: Array<T>): LispValue where T <: LispConvertible` | 反向调用 Lisp 函数 |
| `getEnvironment` | `func getEnvironment(): Environment` | 获取顶层环境 |
| `reset` | `func reset(): Unit` | 重置环境与求值器（保留配置字段） |
| `setMaxStackDepth` | `func setMaxStackDepth(depth: Int64): Unit` | 设置栈深度 |
| `setTimeout` | `func setTimeout(timeout: Option<Duration>): Unit` | 设置超时 |
| `setMaxMemory` | `func setMaxMemory(bytes: Option<Int64>): Unit` | 设置内存限制 |
| `setOutputFn` | `func setOutputFn(fn: (String) -> Unit): Unit` | 自定义输出 |
| `registerKeywordAlias` | `func registerKeywordAlias(alias, original: String): Unit` | 注册关键字别名 |
| `registerBridgeFunction` | `func registerBridgeFunction(name: String, handler: (ArrayList<LispValue>) -> LispValue): Unit` | 注册无命名空间桥接函数 |
| `registerBridgeFunctionWithNS` | `func registerBridgeFunctionWithNS(ns, name: String, handler: (ArrayList<LispValue>) -> LispValue): Unit` | 注册带命名空间桥接函数 |

## 参见

- [选项系统](options) - `withXxx` 选项详解
- [沙箱](sandbox) - 安全执行不受信任的代码
- [桥接](bridge) - 双向类型转换与函数互操作
