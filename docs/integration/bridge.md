# Xisp 桥接层 API 文档

## 概述

Xisp 桥接层提供了 Lisp 与仓颉（Cangjie）之间的双向互操作能力，包括：

- **LispConvertible 接口**：统一类型转换系统
- **类型转换器**：便捷的类型转换工具
- **LispInterpreter**：面向对象的解释器 API
- **桥接管理器**：灵活的函数注册机制
- **标准库桥接**：std.io、std.fs 等标准库的 Lisp 接口

---

## LispInterpreter 类

### 创建解释器

```cangjie
import ystyle::xisp.*

let interpreter = LispInterpreter()
```

创建解释器时会自动：
- 初始化 Lisp 环境
- 注册所有内置函数
- 注册标准桥接函数（std.io, std.fs）

### 基本方法

#### eval(code: String)

求值单个 Lisp 表达式。

```cangjie
public func eval(code: String): LispValue
```

**示例**：

```cangjie
let result = interpreter.eval("(+ 1 2 3)")
// result: Number(6.0)
```

#### evalMultiple(code: String)

求值多个 Lisp 表达式（顺序执行）。

```cangjie
public func evalMultiple(code: String): LispValue
```

**示例**：

```cangjie
let code = "
    (define x 10)
    (define y 20)
    (+ x y)
"
let result = interpreter.evalMultiple(code)
// result: Number(30.0)
```

#### runREPL()

启动交互式 REPL。

```cangjie
public func runREPL(): Int64
```

**示例**：

```cangjie
// 直接启动 REPL
interpreter.runREPL()

// 或在 main 函数中
main(): Int64 {
    let interpreter = LispInterpreter()
    interpreter.runREPL()
}
```

#### getEnvironment()

获取顶层环境，用于底层操作。

```cangjie
public func getEnvironment(): Environment
```

#### reset()

重置解释器状态（清空环境，重新注册函数）。

```cangjie
public func reset(): Unit
```

### 桥接函数注册

#### registerBridgeFunction()

注册自定义桥接函数（不带命名空间）。

```cangjie
public func registerBridgeFunction(
    name: String,
    handler: (ArrayList<LispValue>) -> LispValue
)
```

**示例**：

```cangjie
// 注册一个平方函数
interpreter.registerBridgeFunction("square", { args =>
    if (args.size > 0 && let Number(n) <- args[0]) {
        Number(n * n)
    } else {
        Str("Error: argument must be a number")
    }
})

// 在 Lisp 中使用
// interpreter.eval("(square 5)")  // => 25.0
```

#### registerBridgeFunctionWithNS()

注册带命名空间的桥接函数。

```cangjie
public func registerBridgeFunctionWithNS(
    ns: String,
    name: String,
    handler: (ArrayList<LispValue>) -> LispValue
)
```

**示例**：

```cangjie
// 注册到 "mycalc" 命名空间
interpreter.registerBridgeFunctionWithNS("mycalc", "add", { args =>
    if (args.size >= 2 && let Number(a) <- args[0] && let Number(b) <- args[1]) {
        Number(a + b)
    } else {
        Str("Error: requires 2 numbers")
    }
})

// 在 Lisp 中使用
// interpreter.eval("(mycalc:add 3 4)")  // => 7.0
```

### BridgeManager

解释器的 `bridge` 成员提供更底层的桥接管理。

```cangjie
let bridge = interpreter.bridge
```

#### 注册函数

```cangjie
// 不带命名空间
bridge.register("func-name", { args => ... })

// 带命名空间
bridge.registerWithNS("namespace", "func-name", { args => ... })
```

#### 注册标准库

```cangjie
// 注册 std.io 模块（read-file, write-file, append-file）
bridge.registerStdIO()

// 注册 std.fs 模块（exists?, file?, directory?, list-dir）
bridge.registerStdFS()
```

---

## LispConvertible 接口

### 接口定义

```cangjie
public interface LispConvertible {
    func toLisp(): LispValue
}
```

### 内置类型支持

所有仓颉基本类型都实现了 `LispConvertible` 接口：

#### 数值类型

- `Int8`, `Int16`, `Int32`, `Int64` → `Number`
- `UInt8`, `UInt16`, `UInt32`, `UInt64` → `Number`
- `Float16`, `Float32`, `Float64` → `Number`

```cangjie
let num: Int64 = 42
let lispValue = num.toLisp()  // Number(42.0)
```

#### 字符和字符串

- `Rune` → `Str`
- `String` → `Str`

```cangjie
let str: String = "Hello"
let lispValue = str.toLisp()  // Str("Hello")
```

#### 布尔值

- `Bool` → `Boolean`

```cangjie
let flag: Bool = true
let lispValue = flag.toLisp()  // Boolean(true)
```

#### 集合类型

##### ArrayList / Array

```cangjie
let numbers = ArrayList<Int64>()
numbers.add(1)
numbers.add(2)
numbers.add(3)

let lispList = numbers.toLisp()  // (1.0 2.0 3.0)
```

##### HashMap

```cangjie
let map = HashMap<String, Int64>()
map["a"] = 1
map["b"] = 2

let lispMap = map.toLisp()  // (("a" 1.0) ("b" 2.0))
```

**注意**：HashMap 转换为 Lisp 的关联列表（alist），每个键值对表示为 `(key . value)`。

##### HashSet

```cangjie
let set = HashSet<Int64>()
set.add(10)
set.add(20)

let lispSet = set.toLisp()  // (10.0 20.0)
```

##### Option

```cangjie
let some: Option<Int64> = Some(42)
let none: Option<Int64> = None

some.toLisp()  // Number(42.0)
none.toLisp()  // Nil
```

---

## TypeConverter 工具类

### 静态方法

#### from<T>()

将任意 `LispConvertible` 类型转换为 `LispValue`。

```cangjie
public static func from<T>(value: T): LispValue where T <: LispConvertible
```

**示例**：

```cangjie
let num: Int64 = 42
let lispNum = TypeConverter.from(num)  // 等同于 num.toLisp()
```

#### fromList<T>()

将 `ArrayList` 转换为 Lisp 列表。

```cangjie
public static func fromList<T>(values: ArrayList<T>): LispValue
    where T <: LispConvertible
```

**示例**：

```cangjie
let numbers = ArrayList<Int64>()
numbers.add(1)
numbers.add(2)
numbers.add(3)

let lispList = TypeConverter.fromList(numbers)  // (1.0 2.0 3.0)
```

#### 类型提取方法

从 `LispValue` 中提取仓颉类型：

```cangjie
// 提取字符串
public static func asString(value: LispValue): ?String

// 提取浮点数
public static func asFloat(value: LispValue): ?Float64

// 提取整数
public static func asInt64(value: LispValue): ?Int64

// 提取布尔值
public static func asBool(value: LispValue): ?Bool
```

**示例**：

```cangjie
let lispValue = Str("Hello")

match (TypeConverter.asString(lispValue)) {
    case Some(s) => println(s)  // "Hello"
    case None => println("Not a string")
}
```

---

## 便捷函数

### toLisp<T>()

转换单个值。

```cangjie
public func toLisp<T>(value: T): LispValue
    where T <: LispConvertible
```

### toLispList<T>()

转换列表。

```cangjie
public func toLispList<T>(values: ArrayList<T>): LispValue
    where T <: LispConvertible
```

**示例**：

```cangjie
let numbers = ArrayList<Int64>()
numbers.add(1)
numbers.add(2)

let result = toLispList(numbers)  // (1.0 2.0)
```

---

## 桥接函数注册

### Bridge 类

#### 注册函数

```cangjie
// 注册不带命名空间的函数
public static func registerFunc(
    env: Environment,
    name: String,
    handler: (ArrayList<LispValue>) -> LispValue
)

// 注册带命名空间的函数
public static func registerFuncWithNS(
    env: Environment,
    ns: String,
    name: String,
    handler: (ArrayList<LispValue>) -> LispValue
)
```

**示例**：

```cangjie
import ystyle::xisp.bridge.*

// 不带命名空间
Bridge.registerFunc(env, "my-func", { args =>
    // 处理参数
    Number(42.0)
})

// 带命名空间
Bridge.registerFuncWithNS(env, "mymodule", "func", { args =>
    // 函数名将是 mymodule:func
    Number(42.0)
})
```

### 错误处理

桥接函数应该使用 `try-catch` 捕获异常，并返回友好的错误信息：

```cangjie
Bridge.registerFuncWithNS(env, "myapi", "operation", { args =>
    try {
        // 尝试执行操作
        Str("Success")
    } catch (e: FSException) {
        Str("Error: ${e.message}")
    } catch (e: Exception) {
        Str("Error: ${e.message}")
    }
})
```

---

## 已实现的桥接函数

### std.io 模块

文件操作函数，命名空间前缀：`cangjie:io:`

#### cangjie:io:read-file

读取文件内容。

```lisp
(cangjie:io:read-file "path/to/file.txt")
```

**返回值**：
- 成功：文件内容的字符串
- 失败：`"Error: ..."` 错误信息字符串

#### cangjie:io:write-file

写入文件（覆盖模式）。

```lisp
(cangjie:io:write-file "path/to/file.txt" "Hello, World!")
```

**返回值**：
- 成功：`"Success: written to path/to/file.txt"`
- 失败：`"Error: ..."` 错误信息字符串

#### cangjie:io:append-file

追加内容到文件。

```lisp
(cangjie:io:append-file "path/to/file.txt" "\nNew line")
```

**返回值**：
- 成功：`"Success: appended to path/to/file.txt"`
- 失败：`"Error: ..."` 错误信息字符串

### std.fs 模块

文件系统操作函数，命名空间前缀：`cangjie:fs:`

#### cangjie:fs:exists?

检查文件或目录是否存在。

```lisp
(cangjie:fs:exists? "path/to/file.txt")
```

**返回值**：
- 存在：`true`
- 不存在：`false`

#### cangjie:fs:file?

判断是否为文件。

```lisp
(cangjie:fs:file? "path/to/file.txt")
```

**返回值**：
- 是文件：`true`
- 不是文件：`false`

#### cangjie:fs:directory?

判断是否为目录。

```lisp
(cangjie:fs:directory? "path/to/dir")
```

**返回值**：
- 是目录：`true`
- 不是目录：`false`

#### cangjie:fs:list-dir

列出目录内容。

```lisp
(cangjie:fs:list-dir "path/to/dir")
```

**返回值**：
- 成功：文件名列表 `("file1.txt" "file2.cj" ...)`
- 失败：`"Error: ..."` 错误信息字符串

---

## 完整使用示例

### 示例 1：在 Lisp 中使用文件 I/O

```lisp
; 写入配置文件
(cangjie:io:write-file "config.txt" "name=Xisp\nversion=0.1.0")

; 读取配置
(define config (cangjie:io:read-file "config.txt"))
(println config)

; 检查文件是否存在
(if (cangjie:fs:exists? "config.txt")
    (println "配置文件存在")
    (println "配置文件不存在"))

; 列出当前目录
(define files (cangjie:fs:list-dir "."))
(println "文件列表:")
(define (print-files lst)
  (if (not (null? lst))
      (begin
        (println "  -" (first lst))
        (print-files (rest lst)))))
(print-files files)
```

### 示例 2：在仓颉中注册自定义函数

```cangjie
package ystyle::xisp.bridge

import ystyle::xisp.core.*
import std.collection.ArrayList

public class MyCustomBridge {
    public static func registerAll(env: Environment) {
        // 注册一个简单的加法函数
        Bridge.registerFuncWithNS(env, "mycalc", "add", { args =>
            if (args.size >= 2) {
                if (let Number(a) <- args[0] && let Number(b) <- args[1]) {
                    Number(a + b)
                } else {
                    Str("Error: arguments must be numbers")
                }
            } else {
                Str("Error: add requires 2 arguments")
            }
        })

        // 注册一个字符串连接函数
        Bridge.registerFuncWithNS(env, "mycalc", "concat", { args =>
            if (args.size >= 2) {
                if (let Str(a) <- args[0] && let Str(b) <- args[1]) {
                    Str(a + b)
                } else {
                    Str("Error: arguments must be strings")
                }
            } else {
                Str("Error: concat requires 2 arguments")
            }
        })
    }
}
```

然后在 REPL 中使用：

```lisp
xisp> (mycalc:add 1 2)
3.000000

xisp> (mycalc:concat "Hello" " World")
"Hello World"
```

### 示例 3：使用 LispConvertible 在桥接函数中

```cangjie
Bridge.registerFuncWithNS(env, "myapi", "process-list", { args =>
    if (args.size > 0 && let LispValue(lst) <- args[0]) {
        // 假设我们要处理一个仓颉 ArrayList
        let numbers = ArrayList<Int64>()
        numbers.add(1)
        numbers.add(2)
        numbers.add(3)

        // 使用 toLispList 转换
        toLispList(numbers)
    } else {
        Str("Error: invalid argument")
    }
})
```

---

## 注意事项

### 类型安全

- 桥接函数应该验证参数类型
- 使用模式匹配（`let` 模式）提取 LispValue 的内容
- 处理类型不匹配的情况，返回友好的错误信息

### 错误处理

- 所有可能抛出异常的代码都应该用 `try-catch` 包裹
- 捕获特定异常类型（如 `FSException`）和通用异常
- 返回字符串形式的错误信息，而不是让异常传播到 Lisp 环境

### 性能考虑

- 尽量减少不必要的类型转换
- 对于频繁调用的函数，考虑缓存结果
- 大文件操作应该分批处理，避免一次性加载到内存

---

## 扩展阅读

- [Lisp 核心功能设计](./core.md)
- [仓颉嵌入式 Lisp 设计](./design.md)
- [std.fs API 文档](https://cangjie-lang.cn/libs/std/fs/)
- [std.io API 文档](https://cangjie-lang.cn/libs/std/io/)

---

**最后更新**: 2026-01-21
**版本**: 0.1.0
