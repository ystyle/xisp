# 沙箱

沙箱用于**安全地执行不受信任的 Lisp 代码**，通过组合「栈深度限制」「执行超时」「函数白/黑名单」「文件访问控制」「模块加载控制」多层机制提供防护。

::: warning 安全定位
沙箱是**软件层**的防御措施（栈深度、函数白名单、路径白名单等），不能替代操作系统级隔离（进程、容器、seccomp）。对于真正的恶意代码，请同时在进程/容器层面加固。
:::

---

## 机制总览

| 机制 | 作用 | 选项 | 运行时方法 |
|------|------|------|-----------|
| 栈深度限制 | 阻止无限递归导致栈溢出 | `withMaxStackDepth(depth)` | `setMaxStackDepth(depth)` |
| 执行超时 | 阻止死循环长期占用资源 | `withTimeout(Option<Duration>)` | `setTimeout(timeout)` |
| 函数黑名单 | 禁止特定函数 | `withBlockedFunctions([...])` | `blockFunction` / `blockFunctions` |
| 函数白名单 | 只允许特定函数 | `withAllowedFunctions([...])` | `setAllowedFunctions([...])` |
| 文件写入控制 | 禁止/允许写入 | `withNoFileWrite()` | `blockFileWrite` / `setFileWriteAllowed` |
| 路径白名单 | 只允许访问特定路径 | `withAllowedPaths([...])` | `setAllowedPaths([...])` |
| 模块加载控制 | 禁止 `import` | `withNoModuleLoad()` | `blockModuleLoad` / `setModuleLoadAllowed` |
| 内存限制 | 限制内存增量 | `withMaxMemory(Option<Int64>)` | `setMaxMemory(bytes)` |

---

## 预设沙箱模式

### withSandbox()：严格模式

`withSandbox()` 依次执行（`options.cj`）：

- `enableSandbox()` —— 启用沙箱，`eval` 改走独立线程执行
- `setMaxStackDepth(500)` —— 栈深度 500
- `setTimeout(Some(Duration.second * 30))` —— 30 秒超时
- `blockFileWrite()` —— 禁止文件写入
- `blockModuleLoad()` —— 禁止模块加载
- `blockFunctions(["eval", "apply"])` —— 禁止危险函数

```cangjie
let interpreter = LispInterpreter([
    withSandbox(),
    withStdIO(),      // 文件 I/O 可用，但写入会被拒绝
    withQuietMode()
])
```

### withSandboxLenient()：宽松模式（开发调试）

- `enableSandbox()`
- `setMaxStackDepth(5000)`
- `setTimeout(None)` —— 无超时

不限制文件写入、模块加载与函数调用。

```cangjie
let lenient = LispInterpreter([
    withSandboxLenient(),
    withStdIO()
])
```

---

## 运行时沙箱 API

### 启用/禁用

```cangjie
interpreter.enableSandbox()   // 启用后 eval 走独立线程 + 超时控制
```

沙箱启用后，`eval(code)` 改为调用 `evalInSandboxInternal`：在 `spawn { }` 线程中执行，通过 `Future.get(timeout)` 等待结果；超时则 `cancel()` 并返回 `Nil`。

### 栈深度

```cangjie
interpreter.setMaxStackDepth(100)   // 同步到 Evaluator
interpreter.getMaxStackDepth()      // 读取当前限制
```

超过栈深度时返回错误 `"Error: Maximum stack depth exceeded (limit: ...)"`。

### 超时

```cangjie
import std.time.*

interpreter.setTimeout(Some(Duration.second * 10))  // 10 秒
interpreter.setTimeout(None)                        // 取消限制
```

超时仅对沙箱模式下的 `eval` 生效（独立线程执行）。超时/取消时输出函数会收到「沙箱执行超时或被取消」提示，并返回 `Nil`。

### 内存限制

```cangjie
// 限制本次执行内存增量（字节），None 表示无限制
interpreter.setMaxMemory(Some(100 * 1024 * 1024))   // 100MB
interpreter.checkMemoryLimit()                      // 检查是否超限
```

实现基于 `std.runtime.getAllocatedHeapSize()` 计算执行前后的内存增量。

### 函数黑名单

```cangjie
interpreter.blockFunction("eval")               // 单个
interpreter.blockFunctions(["apply", "load"])   // 批量
```

### 函数白名单

```cangjie
// 只允许 + 和 -，其余函数一律禁止
interpreter.setAllowedFunctions(["+", "-"])
```

::: warning 白名单会阻止名单外的算术函数
设置 `setAllowedFunctions(["+", "-"])` 后，`*`、`/`、`println` 等**未列入白名单的函数都会被阻止**（这是已修复的预期安全行为，见 `sandbox_test.cj`）。白名单需完整包含业务所需的全部函数与特殊形式。

```cangjie
interpreter.setAllowedFunctions(["+", "-"])
interpreter.eval("(+ 1 2)")     // 3，允许
interpreter.eval("(* 2 3)")     // Error: function not allowed，被阻止
```
:::

### 权限检查顺序

`isFunctionAllowed(funcName)` 按如下顺序判定（`interpreter.cj`）：

1. 先查黑名单 —— 命中即禁止
2. 有白名单 —— 只允许白名单内函数
3. 无白名单 —— 放行（白名单优先级低于黑名单）

```cangjie
interpreter.setAllowedFunctions(["+", "-", "*", "/"])
interpreter.blockFunction("/")          // 黑名单优先：/ 被禁止
interpreter.eval("(* 2 3)")             // 6，允许
interpreter.eval("(/ 6 2)")             // Error: / not allowed
```

### 文件写入控制

```cangjie
interpreter.blockFileWrite()            // 禁止所有写入
interpreter.setFileWriteAllowed(true)  // 恢复允许
```

### 路径白名单

```cangjie
// 前缀匹配：只允许 /tmp/ 开头的路径
interpreter.setAllowedPaths(["/tmp/"])
interpreter.isPathAllowed("/tmp/a.txt", false)   // true，读允许
interpreter.isPathAllowed("/etc/passwd", false)  // false
interpreter.isPathAllowed("/tmp/a.txt", true)    // false，写入被禁止
```

写入还需通过 `allowFileWrite` 标志；读取只校验路径白名单。

### 模块加载控制

```cangjie
interpreter.blockModuleLoad()           // 禁止 import
interpreter.setModuleLoadAllowed(true)  // 恢复允许
interpreter.isModuleLoadAllowed()       // 读取状态
```

::: tip 回调挂接
`LispInterpreter` 构造时会把权限判定以回调方式挂到 `Evaluator` 与 `Bridge` 上（`evaluator.setFuncChecker`、`evaluator.setPathChecker`、`evaluator.setModuleLoadChecker`、`bridge.setPathChecker`）。因此沙箱限制对**内置函数、特殊形式、标准库桥接函数**统一生效。
:::

---

## 完整示例

### 示例 1：执行不受信任的用户代码

```cangjie
import std.time.*
import ystyle::xisp.*

main() {
    let interpreter = LispInterpreter([
        withSandbox(),                 // 严格沙箱
        withQuietMode()
    ])

    // 危险操作被阻止
    interpreter.eval("(cangjie::write-file \"/etc/passwd\" \"hack\")")
    // => Error: File write denied: /etc/passwd

    interpreter.eval("(eval (+ 1 2))")
    // => Error: function not allowed（eval 被禁止）

    // 正常计算不受影响
    let result = interpreter.eval("(define (square x) (* x x)) (square 5)")
    // => 25
}
```

### 示例 2：多层防护组合

```cangjie
import std.time.*
import ystyle::xisp.*

main() {
    let interpreter = LispInterpreter([
        // 资源限制
        withMaxStackDepth(100),
        withTimeout(Some(Duration.second * 10)),
        withMaxMemory(Some(50 * 1024 * 1024)),

        // 函数白名单：只允许纯计算
        withAllowedFunctions([
            "+", "-", "*", "/", "mod",
            ">", "<", ">=", "<=", "=",
            "list", "car", "cdr", "cons",
            "define", "lambda", "if", "let"
        ]),

        // 文件与模块：完全隔离
        withNoFileWrite(),
        withNoModuleLoad(),

        withQuietMode()
    ])

    let userCode = """
        (define (factorial n)
            (if (<= n 1)
                1
                (* n (factorial (- n 1)))))
        (factorial 10)
    """
    let result = interpreter.eval(userCode)
    println(result)   // 3628800
}
```

### 示例 3：只读沙箱（允许读取固定目录）

```cangjie
let interpreter = LispInterpreter([
    withAllowedPaths(["./data/"]),   // 只允许读 ./data/
    withNoFileWrite(),               // 任何写入都拒绝
    withStdIO(),
    withQuietMode()
])

interpreter.eval("(cangjie::read-file \"./data/config.json\")")   // 允许
interpreter.eval("(cangjie::write-file \"./data/out.txt\" \"x\")") // 拒绝
```

---

## 错误信息格式

沙箱拒绝操作时返回 `LispValue.Error`，错误信息可通过模式匹配提取：

```cangjie
let result = interpreter.eval("(println \"x\")")
match (result) {
    case Error(err) => println(err.message)  // 如 "... is not allowed"
    case _ => ()
}
```

常见错误信息：

```
Error: Maximum stack depth exceeded (limit: 500)
Error: File write denied: /etc/passwd
Error: Function 'eval' is not allowed
```

---

## 最佳实践

- **默认拒绝**：优先使用 `withSandbox()` 或白名单，而不是事后逐个封堵。
- **最小权限**：白名单只包含必需函数，路径白名单只开放必要目录。
- **多层防护**：栈深度 + 超时 + 白名单 + 文件/模块控制组合使用。
- **不受信任代码**：`withSandbox() + withQuietMode()`，并配合操作系统级隔离。
- **受信任代码**：`withMaxStackDepth(5000) + withTimeout(None)` 提高上限。
- **性能**：超时控制有独立线程开销；不需要时用 `withTimeout(None)`。

## 参见

- [选项系统](options) - 全部 `withXxx` 选项
- [嵌入解释器](embedding) - 运行时沙箱配置方法
- [桥接](bridge) - 标准库桥接函数与权限回调
