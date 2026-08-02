# 仓颉桥接与互操作

Xisp 桥接层（`src/bridge/`）提供 Lisp 与仓颉之间的**双向互操作**：

- **仓颉 → Lisp**：注册桥接函数、标准库桥接（文件 I/O、文件系统）
- **Lisp → 仓颉**：`call<T>()` 反向调用 Lisp 函数，`asInt()` 等提取结果
- **类型转换**：`LispConvertible`（仓颉 → LispValue）、`LispDeserializable`（LispValue → 仓颉）

---

## 注册自定义桥接函数

### LispInterpreter 便捷方法

在解释器上直接注册（`interpreter.cj`）：

```cangjie
import ystyle::xisp.*

let interpreter = LispInterpreter([withStdLib()])

// 无命名空间：函数名为 square
interpreter.registerBridgeFunction("square", { args =>
    if (args.size > 0 && let LispValue.Int(n) <- args[0]) {
        LispValue.Int(n * n)
    } else {
        LispValue.Error(XispError(ErrorType.TypeMismatch, "square: expected a number"))
    }
})
interpreter.eval("(square 5)")   // => 25
```

带命名空间的版本：

```cangjie
interpreter.registerBridgeFunctionWithNS("mycalc", "add", { args =>
    if (args.size >= 2 && let LispValue.Int(a) <- args[0] && let LispValue.Int(b) <- args[1]) {
        LispValue.Int(a + b)
    } else {
        LispValue.Error(XispError(ErrorType.TypeMismatch, "add: requires 2 numbers"))
    }
})
```

::: warning 命名空间分隔符差异
源码中两处 `registerWithNS` 行为**不一致**，使用时务必区分：

- `interpreter.registerBridgeFunctionWithNS(ns, name, handler)` 走 `BridgeManager.registerWithNS`，生成**单冒号**符号：`ns:name`，Lisp 中调用 `(mycalc:add 3 4)`。
- `Bridge.registerFuncWithNS(env, ns, name, handler)` 走 `Bridge.registerFuncWithNS`，生成**双冒号**符号：`ns::name`，Lisp 中调用 `(cangjie::read-file "x")`。

标准库桥接函数（`cangjie::read-file` 等）均为双冒号形式。
:::

### 底层：Bridge / BridgeManager

在任意 `Environment` 上注册（适合模块级批量注册，`bridge.cj`）：

```cangjie
import ystyle::xisp.bridge.*

// 静态方法，可直接传入 Environment
Bridge.registerFunc(env, "my-func", { args => LispValue.Int(42) })
Bridge.registerFuncWithNS(env, "mymodule", "func", { args => LispValue.Int(42) })

// 一次性注册标准库
Bridge.registerStdIO(env)                       // 无路径检查
Bridge.registerStdIO(env, Some({ path, isWrite => true }))  // 带路径检查回调
Bridge.registerStdFS(env)
Bridge.registerStdCollection(env)
Bridge.registerAll(env)                         // 上述全部
```

解释器的 `bridge` 成员是 `BridgeManager`，提供实例方法：

```cangjie
let bridge = interpreter.bridge
bridge.register("func-name", { args => ... })          // 无命名空间
bridge.registerWithNS("ns", "func-name", { args => ... })  // 单冒号 ns:func-name
bridge.registerStdIO()                                  // 绑定解释器的路径检查回调
bridge.registerStdFS()
```

::: tip 桥接函数签名
所有 handler 类型都是 `(ArrayList<LispValue>) -> LispValue`，接收 LispValue 参数列表，返回 LispValue。建议用 `match`/`let` 模式校验参数类型，用 `LispValue.Error(XispError(...))` 返回错误，而不是抛出异常。
:::

---

## 标准库桥接函数

默认构造已注册以下函数（命名空间 `cangjie`，双冒号）：

| Lisp 调用 | 说明 |
|-----------|------|
| `(cangjie::read-file "path")` | 读取文件，返回内容字符串；文件不存在返回 `nil`，异常返回 `Error` |
| `(cangjie::write-file "path" "content")` | 覆盖写入，成功返回 `"Success: written to path"` |
| `(cangjie::append-file "path" "content")` | 追加写入，成功返回 `"Success: appended to path"` |
| `(cangjie::exists? "path")` | 文件/目录是否存在 |
| `(cangjie::file? "path")` | 是否为普通文件 |
| `(cangjie::directory? "path")` | 是否为目录 |
| `(cangjie::list-dir "path")` | 列出目录内容，返回文件名列表 |

```lisp
(cangjie::write-file "config.txt" "name=Xisp")
(cangjie::read-file "config.txt")          ; => "name=Xisp"
(cangjie::exists? "config.txt")            ; => true
(cangjie::list-dir ".")                    ; => ("config.txt" ...)
```

`cangjie::read-file` / `write-file` / `append-file` 会经过解释器的路径检查回调（`pathChecker`），因此受 [沙箱](sandbox) 的路径白名单与写入限制约束。

---

## 反向调用：call 泛型方法

`LispInterpreter.call<T>(funcName, args: Array<T>)` 从仓颉调用已定义的 Lisp 函数，参数自动经 `toLisp()` 转换，返回 `LispValue`：

```cangjie
let interpreter = LispInterpreter([withStdLib()])

// 定义 Lisp 函数
interpreter.eval("(define (square x) (* x x))")
interpreter.eval("(define (greet name) (str \"Hello, \" name))")
interpreter.eval("(define (is-positive x) (> x 0))")

// 反向调用，参数数组里的仓颉类型自动转换
let r1 = interpreter.call("square", [5])
let r2 = interpreter.call("greet", ["World"])
let r3 = interpreter.call("is-positive", [5])

// 用便捷方法提取结果
if (let Some(i) <- r1.asInt()) { println(i) }        // 25
if (let Some(s) <- r2.asString()) { println(s) }     // Hello, World
if (let Some(b) <- r3.asBool()) { println(b) }       // true
```

支持变长/无参数调用、浮点参数，以及将结果转为任意实现了 `LispDeserializable` 的类型：

```cangjie
interpreter.eval("(define (add-three a b c) (+ a b c))")
let r = interpreter.call("add-three", [1, 2, 3])
if (let Some(i) <- r.asCjValue<Int64>()) { println(i) }   // 6

interpreter.call("constant", [])   // 无参调用
```

---

## 类型转换

### LispValue 枚举

`LispValue`（`src/types/types.cj`）是桥接的核心数据类型，主要变体：

| 变体 | 对应仓颉类型 |
|------|-------------|
| `LispValue.Int(Int64)` | 整数 |
| `LispValue.Float(Float64)` | 浮点数 |
| `LispValue.Str(String)` | 字符串 |
| `LispValue.Boolean(Bool)` | 布尔 |
| `LispValue.Symbol(String)` | 符号 |
| `Nil` | 空值/空列表 |
| `LispValue.Cons(ConsCell)` | 列表 |
| `LispValue.HashMap(HashMap<String, LispValue>)` | 哈希映射 |
| `LispValue.Error(XispError)` | 错误 |

### LispConvertible：仓颉 → Lisp

接口定义（`lisp_value_extension.cj`）：

```cangjie
public interface LispConvertible {
    func toLisp(): LispValue
}
```

所有基本类型均已实现：`Int8/16/32/64`、`UInt8/16/32/64`、`Float16/32/64` → 数值；`Rune`、`String` → `Str`；`Bool` → `Boolean`；`Array<T>` / `ArrayList<T>` → 列表；`HashMap<K, V>` → `LispValue.HashMap`；`HashSet<T>` → 列表；`Option<T>` → 值或 `Nil`。

```cangjie
let num: Int64 = 42
num.toLisp()          // LispValue.Int(42)

let str: String = "Hello"
str.toLisp()          // LispValue.Str("Hello")

let arr = ArrayList<Int64>([1, 2, 3])
arr.toLisp()          // (1 2 3)

let map = HashMap<String, Int64>([("a", 1)])
map.toLisp()          // LispValue.HashMap(...)

let some: Option<Int64> = Some(42)
some.toLisp()         // LispValue.Int(42)
let none: Option<Int64> = None
none.toLisp()         // Nil
```

### LispDeserializable：Lisp → 仓颉

接口定义（`lisp_deserializable.cj`）：

```cangjie
public interface LispDeserializable<T> {
    static func fromLisp(value: LispValue): ?T
}
```

内置实现：`Int64`、`Float64`、`String`、`Bool`、`ArrayList<T>`、`HashMap<String, V>`。

```cangjie
// 静态方法调用
match (Int64.fromLisp(LispValue.Int(42))) {
    case Some(i) => println(i)          // 42
    case None => ()
}

// Float 转 Int 自动截断
Int64.fromLisp(LispValue.Float(3.14))   // Some(3)

// Str/Symbol 都能转 String
String.fromLisp(LispValue.Symbol("sym"))   // Some("sym")

// ArrayList<T> 从 Lisp 列表转换
let lst: LispValue = list(ArrayList<LispValue>([
    LispValue.Int(1), LispValue.Int(2)
]))
ArrayList<Int64>.fromLisp(lst)           // Some([1, 2])
```

### ExtendLispValue：LispValue 便捷方法

`LispValue` 实现了 `ExtendLispValue` 接口（`asInt`/`asFloat`/`asString`/`asBool`/`asCjValue<T>`），是结果提取的最常用入口：

```cangjie
let result = interpreter.eval("(+ 1 2)")

if (let Some(i) <- result.asInt()) {          // 提取 Int64
    println(i)                                // 3
}
if (let Some(s) <- result.asString()) { ... } // 提取 String
if (let Some(b) <- result.asBool()) { ... }   // 提取 Bool
if (let Some(f) <- result.asFloat()) { ... }  // 提取 Float64

// 泛型转换：任意实现了 LispDeserializable 的类型
if (let Some(i) <- result.asCjValue<Int64>()) { ... }
```

类型不匹配时这些方法统一返回 `None`：

```cangjie
let result = interpreter.eval("(str \"hello\")")
if (let Some(i) <- result.asInt()) {
    // 不会进入
} else {
    println("Not an integer")
}
```

### 自定义类型的双向转换

```cangjie
class MyType {
    let data: String
    public init(data: String) { this.data = data }
}

// Lisp → 仓颉
extend MyType <: LispDeserializable<MyType> {
    public static func fromLisp(value: LispValue): ?MyType {
        match (value) {
            case LispValue.Str(s) => Some(MyType(s))
            case _ => None
        }
    }
}

// 仓颉 → Lisp
extend MyType <: LispConvertible {
    public func toLisp(): LispValue {
        LispValue.Str(this.data)
    }
}

let lispValue = LispValue.Str("test data")
if (let Some(myType) <- MyType.fromLisp(lispValue)) {
    println(myType.data)   // test data
}
```

---

## 完整示例：桥接函数 + 反向调用

```cangjie
import ystyle::xisp.*

main() {
    let interpreter = LispInterpreter([withStdLib(), withQuietMode()])

    // 1. 注册仓颉函数到 Lisp
    interpreter.registerBridgeFunctionWithNS("math", "double", { args =>
        if (let LispValue.Int(n) <- args[0]) {
            LispValue.Int(n * 2)
        } else {
            LispValue.Error(XispError(ErrorType.TypeMismatch, "double: expected number"))
        }
    })

    // 2. Lisp 中使用桥接函数
    interpreter.eval("(define x (math:double 21))")
    let result = interpreter.eval("(* x 2)")
    if (let Some(i) <- result.asInt()) {
        println(i)   // 84
    }

    // 3. 反向调用 Lisp 函数
    interpreter.eval("(define (sum-list lst) (reduce + 0 lst))")
    let total = interpreter.call("sum-list", [ArrayList<Int64>([1, 2, 3, 4])])
    if (let Some(i) <- total.asInt()) {
        println(i)   // 10
    }
}
```

---

## 注意事项

- **参数校验**：桥接函数务必用模式匹配校验参数类型，避免把异常抛回 Lisp 求值环境。
- **错误返回**：用 `LispValue.Error(XispError(kind, message))` 返回错误，配合 `ErrorType`（如 `TypeMismatch`、`IOError`、`ArityMismatch`）。
- **命名空间**：注意单冒号（`BridgeManager.registerWithNS`）与双冒号（`Bridge.registerFuncWithNS`）的区别。
- **类型转换**：`asXxx()` 类型不匹配时返回 `None`，用 `if let`/`match` 处理。

## 参见

- [嵌入解释器](embedding) - `eval` / `call` / 运行时配置
- [选项系统](options) - `withStdLib` / `withStdIO` / `withStdFS`
- [沙箱](sandbox) - 路径权限回调与函数白名单
