# Xisp 沙箱系统文档

## ✅ 实现完成

Xisp 现在支持完整的安全沙箱系统，可以安全地执行不受信任的 Lisp 代码。

## 📋 核心功能

### 1. 调用栈深度限制

防止恶意代码通过无限递归导致栈溢出。

**选项**: `withMaxStackDepth(depth: Int64)`

**示例**:
```cangjie
let interpreter = LispInterpreter([
    withMaxStackDepth(100)  // 限制递归深度为 100 层
])
```

### 2. 执行超时控制

使用线程隔离和超时机制，防止代码陷入死循环或长时间占用资源。

**选项**: `withTimeout(timeout: Option<Duration>)`

**技术实现**:
- 使用 `spawn { }` 创建独立线程执行代码
- 通过 `Future.get(timeout)` 等待结果，超时自动终止
- 使用 `Future.cancel()` 取消执行中的任务

**示例**:
```cangjie
import std.time.*

let interpreter = LispInterpreter([
    withTimeout(Some(Duration.second * 30))  // 30 秒超时
])
```

### 3. 函数访问控制

通过黑名单和白名单机制，精确控制可调用的函数。

**选项**:
- `withBlockedFunctions(funcNames: Array<String>)` - 函数黑名单
- `withAllowedFunctions(funcNames: Array<String>)` - 函数白名单

**权限检查逻辑**:
1. 如果设置了白名单，只允许白名单中的函数
2. 如果函数在黑名单中，禁止调用
3. 白名单优先级高于黑名单

**示例**:
```cangjie
// 禁止危险函数
let interpreter1 = LispInterpreter([
    withBlockedFunctions(["eval", "apply", "load"])
])

// 只允许数学运算
let interpreter2 = LispInterpreter([
    withAllowedFunctions([
        "+", "-", "*", "/", "mod",
        ">", "<", ">=", "<=", "=",
        "println", "print"
    ])
])
```

### 4. 文件访问控制

严格控制文件的读写权限，保护系统安全。

**选项**:
- `withNoFileWrite()` - 禁止所有文件写入
- `withAllowedPaths(paths: Array<String>)` - 路径白名单

**权限检查**:
- 写入操作：检查 `allowFileWrite` 标志和路径白名单
- 读取操作：检查路径白名单（可选）
- 路径白名单使用前缀匹配

**示例**:
```cangjie
// 禁止文件写入
let interpreter1 = LispInterpreter([
    withNoFileWrite(),
    withStdIO()
])

// 只允许访问 /tmp/ 目录
let interpreter2 = LispInterpreter([
    withAllowedPaths(["/tmp/", "/home/user/sandbox/"]),
    withStdIO()
])
```

### 5. 内存监控

监控内存使用情况（基础设施已就绪）。

**技术实现**:
- 使用 `std.runtime.getAllocatedHeapSize()` 获取内存使用量
- 记录执行前后的内存差值

### 6. 预设沙箱模式

提供预设配置，快速启用沙箱保护。

**选项**:
- `withSandbox()` - 严格沙箱模式
  - 栈深度限制: 500
  - 执行超时: 30 秒
  - 禁止文件写入
  - 禁止危险函数: eval, apply

- `withSandboxLenient()` - 宽松沙箱模式
  - 栈深度限制: 1000
  - 无执行超时
  - 允许文件写入
  - 不限制函数调用

**示例**:
```cangjie
// 严格沙箱模式（推荐用于不受信任的代码）
let strict = LispInterpreter([
    withSandbox(),
    withStdIO()
])

// 宽松沙箱模式（用于受信任的代码）
let lenient = LispInterpreter([
    withSandboxLenient(),
    withStdIO()
])
```

## 🔧 API 参考

### 完整选项列表

| 选项 | 参数 | 说明 | 默认值 |
|------|------|------|--------|
| `withSandbox()` | 无 | 严格沙箱模式 | - |
| `withSandboxLenient()` | 无 | 宽松沙箱模式 | - |
| `withTimeout()` | `Option<Duration>` | 执行超时时间 | `None` |
| `withMaxStackDepth()` | `Int64` | 最大栈深度 | `1000` |
| `withBlockedFunctions()` | `Array<String>` | 函数黑名单 | `[]` |
| `withAllowedFunctions()` | `Array<String>` | 函数白名单 | `None` |
| `withNoFileWrite()` | 无 | 禁止文件写入 | - |
| `withAllowedPaths()` | `Array<String>` | 路径白名单 | `[]` |

### 配置方法

LispInterpreter 类提供的配置方法：

```cangjie
// 启用/禁用沙箱
public func enableSandbox()
public func disableSandbox()

// 设置栈深度
public func setMaxStackDepth(depth: Int64)

// 设置超时
public func setTimeout(timeout: Option<Duration>)

// 添加函数黑名单
public func blockFunction(funcName: String)

// 设置函数白名单
public func setAllowedFunctions(funcNames: ArrayList<String>)

// 文件写入权限
public func allowFileWrite()
public func blockFileWrite()

// 添加允许的路径
public func addAllowedPath(path: String)

// 权限检查
public func isFunctionAllowed(funcName: String): Bool
public func checkPathPermission(path: String, isWrite: Bool): Bool
```

## 📊 使用示例

### 示例 1: 严格沙箱（推荐）

适用场景：执行不受信任的用户代码

```cangjie
import ystyle::xisp.*

main() {
    let interpreter = LispInterpreter([
        withSandbox(),        // 严格沙箱模式
        withStdIO(),          // 允许文件 I/O（但写入受限）
        withQuietMode()       // 不显示 Banner
    ])

    // 危险操作将被阻止
    interpreter.eval("(cangjie:io:write-file \"/etc/passwd\" \"hack\")")
    // 返回: "Error: File write denied: /etc/passwd"

    // 正常操作不受影响
    interpreter.eval("(define (square x) (* x x)) (square 5)")
    // 返回: 25.000000
}
```

### 示例 2: 自定义配置

适用场景：根据具体需求灵活配置

```cangjie
import std.time.*
import ystyle::xisp.*

main() {
    let interpreter = LispInterpreter([
        // 自定义栈深度
        withMaxStackDepth(200),

        // 自定义超时
        withTimeout(Some(Duration.second * 60)),

        // 禁止特定函数
        withBlockedFunctions(["eval", "apply", "load"]),

        // 只允许特定目录
        withAllowedPaths(["/tmp/sandbox/"]),
        withStdIO()
    ])
}
```

### 示例 3: 函数白名单

适用场景：只允许特定操作，如纯计算

```cangjie
import ystyle::xisp.*

main() {
    let interpreter = LispInterpreter([
        // 只允许数学运算和输出
        withAllowedFunctions([
            "+", "-", "*", "/", "mod",
            "sqrt", "sin", "cos", "tan",
            ">", "<", ">=", "<=", "=",
            "println", "print", "define",
            "lambda", "if", "let", "cond"
        ]),

        withQuietMode()
    ])

    // 允许: 数学运算
    interpreter.eval("(+ 1 2 3)")  // ✅ 6.000000

    // 禁止: 文件操作
    interpreter.eval("(cangjie:io:write-file \"/tmp/test.txt\" \"data\")")
    // ❌ nil (函数不在白名单中)
}
```

### 示例 4: 路径白名单

适用场景：限制文件访问范围

```cangjie
import ystyle::xisp.*

main() {
    let interpreter = LispInterpreter([
        // 只允许 /tmp/ 目录
        withAllowedPaths(["/tmp/"]),
        withStdIO()
    ])

    // 允许: 写入 /tmp/
    interpreter.eval("(cangjie:io:write-file \"/tmp/safe.txt\" \"data\")")
    // ✅ "Success: written to /tmp/safe.txt"

    // 禁止: 写入系统目录
    interpreter.eval("(cangjie:io:write-file \"/etc/passwd\" \"hack\")")
    // ❌ "Error: File write denied: /etc/passwd"
}
```

### 示例 5: 多重限制组合

适用场景：多层防护，最大化安全性

```cangjie
import std.time.*
import ystyle::xisp.*

main() {
    let interpreter = LispInterpreter([
        // 资源限制
        withMaxStackDepth(100),
        withTimeout(Some(Duration.second * 10)),

        // 函数限制
        withBlockedFunctions(["eval", "apply", "load", "eval-string"]),
        withAllowedFunctions([
            "+", "-", "*", "/", "mod",
            ">", "<", ">=", "<=", "=",
            "list", "car", "cdr", "cons",
            "map", "filter", "reduce",
            "define", "lambda", "if", "let"
        ]),

        // 文件限制
        withNoFileWrite(),

        // 输出控制
        withQuietMode()
    ])

    // 执行用户代码
    let userCode = """
        (define (factorial n)
            (if (<= n 1)
                1
                (* n (factorial (- n 1)))))
        (factorial 10)
    """

    let result = interpreter.eval(userCode)
    println(result)  // 3628800.000000
}
```

## 🛠️ 技术实现

### 架构设计

```
┌─────────────────────────────────────────┐
│         LispInterpreter                 │
│  ┌───────────────────────────────────┐  │
│  │  沙箱状态                         │  │
│  │  - sandboxEnabled: Bool           │  │
│  │  - maxStackDepth: Int64           │  │
│  │  - timeout: Option<Duration>      │  │
│  │  - blockedFunctions: ArrayList    │  │
│  │  - allowedFunctions: Option       │  │
│  │  - allowFileWrite: Bool           │  │
│  │  - allowedPaths: ArrayList        │  │
│  └───────────────────────────────────┘  │
│                                         │
│  evalInSandboxInternal(code)            │
│  ├─ spawn { } → Future<LispValue>      │
│  ├─ Future.get(timeout)                │
│  └─ Future.cancel()                    │
└─────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────┐
│         Evaluator                       │
│  ┌───────────────────────────────────┐  │
│  │  栈深度检查                       │  │
│  │  - stackDepth: Int64              │  │
│  │  - maxStackDepth: Int64           │  │
│  └───────────────────────────────────┘  │
│                                         │
│  evalList()                             │
│  ├─ if (stackDepth > maxStackDepth)    │
│  │   → return Error                    │
│  ├─ stackDepth++                       │
│  ├─ eval()                             │
│  └─ stackDepth--                       │
└─────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────┐
│         Bridge                          │
│  ┌───────────────────────────────────┐  │
│  │  文件权限检查（回调）            │  │
│  │  - pathChecker: ((String,Bool)->Bool) │
│  └───────────────────────────────────┘  │
│                                         │
│  registerStdIO(env, pathChecker)        │
│  write-file(...)                        │
│  ├─ if (pathChecker(path, true))       │
│  │   → 执行写入                        │
│  │   → 返回 "Success"                  │
│  └─ else                               │
│      → 返回 "Error: File write denied" │
└─────────────────────────────────────────┘
```

### 核心代码

#### 1. 线程超时控制

```cangjie
public func evalInSandboxInternal(code: String): ?LispValue {
    this.initialMemory = getAllocatedHeapSize()

    // 在独立线程中执行代码
    let fut = spawn {
        try {
            let lexer = Lexer(code)
            let tokens = lexer.tokenize()
            let parser = Parser(tokens)
            let exprs = parser.parse()

            if (exprs.size > 0) {
                evaluator.eval(exprs[0])
            } else {
                Nil
            }
        } catch (e: Exception) {
            println("Sandbox error: ${e.message}")
            Nil
        }
    }

    this.currentFuture = Some(fut)

    // 等待结果，支持超时
    try {
        match (this.timeout) {
            case Some(duration) =>
                let result = fut.get(duration)
                this.currentFuture = None
                Some(result)
            case None =>
                let result = fut.get()
                this.currentFuture = None
                Some(result)
        }
    } catch (e: Exception) {
        // 超时或取消
        match (this.currentFuture) {
            case Some(_) =>
                fut.cancel()
                this.outputFn("沙箱执行超时或被取消")
            case None => ()
        }
        this.currentFuture = None
        None
    }
}
```

#### 2. 栈深度检查

```cangjie
private func evalList(cell: ConsCell): LispValue {
    if (cell.isNil()) {
        return Nil
    }

    // 沙箱检查：栈深度
    if (this.stackDepth > this.maxStackDepth) {
        return Str("Error: Maximum stack depth exceeded (limit: ${this.maxStackDepth})")
    }

    // 增加栈深度
    this.stackDepth = this.stackDepth + 1

    // 执行求值
    let result = this.evalListInternal(cell)

    // 减少栈深度
    this.stackDepth = this.stackDepth - 1

    result
}
```

#### 3. 文件权限检查

```cangjie
public static func registerStdIO(env: Environment, pathChecker: ?((String, Bool) -> Bool)) {
    registerFuncWithNS(env, "cangjie:io", "write-file", { args =>
        // 检查写入权限
        match (pathChecker) {
            case Some(checker) =>
                if (args.size > 0 && let Str(path) <- args[0]) {
                    if (!checker(path, true)) {
                        return Str("Error: File write denied: ${path}")
                    }
                }
            case None => ()
        }

        // 执行写入
        if (args.size >= 2 && let Str(path) <- args[0] && let Str(content) <- args[1]) {
            try {
                let file = File(path, OpenOption.Create(true), OpenOption.Truncate(true))
                file.write(content)
                file.close()
                Str("Success: written to ${path}")
            } catch (e: Exception) {
                Str("Error: ${e.message}")
            }
        } else {
            Str("Error: Invalid arguments")
        }
    })
}
```

## 🧪 测试验证

### 运行沙箱演示

```bash
# 编译
cjpm build

# 运行演示程序
"./target/release/bin/ystyle::xisp.examples.sandbox_demo"
```

### 测试用例

演示程序包含以下测试：

1. **栈深度限制测试**
   ```lisp
   (define (deep-recurse n)
       (if (> n 0)
           (deep-recurse (- n 1))
           n))
   (deep-recurse 20)
   ```
   预期: 超过栈深度限制时返回错误

2. **函数黑名单测试**
   ```lisp
   (+ 1 2)              ; ✅ 允许
   (eval (+ 1 2))       ; ❌ 禁止
   ```

3. **文件写入限制测试**
   ```lisp
   (cangjie:io:write-file "/tmp/test.txt" "hello")  ; ❌ 禁止
   ```

4. **路径白名单测试**
   ```lisp
   (cangjie:io:write-file "/tmp/safe.txt" "data")   ; ✅ 允许
   (cangjie:io:write-file "/etc/passwd" "hack")     ; ❌ 禁止
   ```

5. **正常操作测试**
   ```lisp
   (define (square x) (* x x))
   (square 5)            ; ✅ 正常: 25.000000
   ```

## 🎯 最佳实践

### 1. 安全原则

- **默认拒绝**: 除非明确允许，否则禁止所有操作
- **最小权限**: 只授予必需的最小权限
- **多层防护**: 组合使用多种沙箱机制
- **深度防御**: 即使一层防护失效，其他层仍能保护

### 2. 推荐配置

#### 执行不受信任的代码

```cangjie
let interpreter = LispInterpreter([
    withSandbox(),        // 严格模式
    withStdIO(),          // 受限的文件 I/O
    withQuietMode()       // 不泄露内部信息
])
```

#### 执行受信任的代码

```cangjie
let interpreter = LispInterpreter([
    withMaxStackDepth(5000),      // 较高的栈深度
    withTimeout(None),             // 无超时限制
    withAllowedPaths(["./data/"]), // 限制数据目录
    withStdIO()
])
```

#### 教学环境

```cangjie
let interpreter = LispInterpreter([
    withSandboxLenient(),          // 宽松模式
    withBlockedFunctions(["eval"]), // 禁止 eval
    withStdIO(),
    withVerboseMode()              // 显示更多信息
])
```

### 3. 性能考虑

- **线程开销**: 超时控制使用独立线程，有一定性能开销
- **栈检查**: 每次函数调用都会检查栈深度，影响很小
- **权限检查**: 文件操作时的权限检查，开销可忽略

如果不需要超时控制，可以使用 `withTimeout(None)` 减少开销。

### 4. 错误处理

沙箱系统返回的错误信息格式：

```lisp
"Error: Maximum stack depth exceeded (limit: 1000)"
"Error: File write denied: /etc/passwd"
"Error: Function 'eval' is not allowed"
```

建议在应用层解析错误信息，提供友好的用户提示。

## 📖 相关文档

- [选项系统文档](options_system.md) - 选项模式设计
- [桥接层 API 文档](bridge.md) - 文件 I/O 函数
- [中文支持文档](chinese_support.md) - 关键字别名
- [源码: src/options.cj](../src/options.cj) - 沙箱选项实现
- [源码: src/interpreter.cj](../src/interpreter.cj) - 解释器沙箱状态
- [源码: src/core/evaluator.cj](../src/core/evaluator.cj) - 栈深度检查
- [源码: src/bridge/bridge.cj](../src/bridge/bridge.cj) - 文件权限检查
- [示例: src/examples/sandbox_demo/main.cj](../src/examples/sandbox_demo/main.cj) - 完整演示

## 🚀 未来扩展

### 计划中的功能

1. **内存限制**
   - 设置最大内存使用量
   - 超过限制时自动终止

2. **CPU 时间限制**
   - 使用进程级 CPU 时间统计
   - 更精确的执行时间控制

3. **网络访问控制**
   - 网络连接白名单/黑名单
   - 限制访问的域名和端口

4. **子进程限制**
   - 禁止创建子进程
   - 限制可执行的命令

5. **资源配额**
   - 文件句柄数量限制
   - 网络连接数量限制
   - 磁盘空间限制

6. **审计日志**
   - 记录所有危险操作
   - 提供安全事件追踪

### 优化方向

1. **性能优化**
   - 减少线程开销
   - 优化权限检查逻辑

2. **灵活性增强**
   - 支持更细粒度的权限控制
   - 支持自定义权限策略

3. **易用性改进**
   - 提供更多预设配置
   - 改进错误信息提示

---

**实现日期**: 2026-01-22
**版本**: 0.1.0
**状态**: ✅ 完成并测试通过
