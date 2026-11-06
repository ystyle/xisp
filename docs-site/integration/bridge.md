# 仓颉桥接与互操作

Xisp 桥接层（`core/src/bridge/`）提供 Lisp 与仓颉之间的**双向互操作**：

- **仓颉 → Lisp**：注册桥接函数、标准库桥接（文件 I/O、文件系统）
- **Lisp → 仓颉**：`call<T>()` 反向调用 Lisp 函数，`asInt()` 等提取结果
- **类型转换**：`LispConvertible`（仓颉 → LispValue）、`LispDeserializable`（LispValue → 仓颉）

---

## 注册自定义桥接函数

### LispInterpreter 便捷方法

在解释器上直接注册（`interpreter.cj`）：

```cangjie
import ystyle::xisp.*          // LispInterpreter / 选项
import ystyle::xisp.types.*    // LispValue / XispError / ErrorType
import ystyle::xisp.bridge.*   // asInt() 等（ExtendLispValue）

let interpreter = LispInterpreter([withStdLib()])

// 无命名空间：函数名为 square
interpreter.registerBridgeFunction("square", { args =>
    if (args.size > 0 && let LispValue.Int(n) <- args[0]) {
        LispValue.Int(n * n)
    } else {
        LispValue.Error(XispError(ErrorType.TypeError, "square: expected a number"))
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
        LispValue.Error(XispError(ErrorType.TypeError, "add: requires 2 numbers"))
    }
})
```

::: tip 命名空间分隔符
命名空间与函数名统一用**双冒号** `::` 连接，生成符号 `ns::name`（如 `(mycalc::add 3 4)`）。

`interpreter.registerBridgeFunctionWithNS` 与 `Bridge.registerFuncWithNS` 是同一个约定：
前者只是后者的实例方法入口（`BridgeManager.registerWithNS` 直接委托），不再各自拼一种分隔符。
`BridgeManager.registerWithNS` 与 `Bridge.registerFuncWithNS` 现在产出完全相同的符号。

标准库桥接函数（`cangjie::read-file` 等）同为双冒号形式。

命名空间本身**不允许包含 `::`**；注册时会校验并拒绝。
:::

### 底层：Bridge / BridgeManager

在任意 `Environment` 上注册（适合模块级批量注册，`bridge.cj`）：

```cangjie
import ystyle::xisp.bridge.*
import ystyle::xisp.types.*    // LispValue

// 静态方法，可直接传入 Environment
Bridge.registerFunc(env, "my-func", { args => LispValue.Int(42) })
Bridge.registerFuncWithNS(env, "mymodule", "func", { args => LispValue.Int(42) })  // 双冒号 mymodule::func

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
bridge.registerWithNS("ns", "func-name", { args => ... })  // 双冒号 ns::func-name
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

`LispValue`（`core/src/types/types.cj`）是桥接的核心数据类型，主要变体：

| 变体 | 对应仓颉类型 |
|------|-------------|
| `LispValue.Int(Int64)` | 整数 |
| `LispValue.Float(Float64)` | 浮点数 |
| `LispValue.Str(String)` | 字符串 |
| `LispValue.Boolean(Bool)` | 布尔 |
| `LispValue.Symbol(String)` | 符号 |
| `Nil` | 空值/空列表 |
| `LispValue.Cons(ConsCell)` | 列表 |
| `LispValue.NativeFunc((ArrayList<LispValue>) -> LispValue)` | 仓颉实现的函数 |
| `LispValue.Procedure` / `ProcedureFromParams` / `Closure` | Lisp 过程 / 编译闭包 |
| `LispValue.Macro` / `MacroFromParams` | 宏 |
| `LispValue.External(LispExternal)` | **外部值**（宿主 / 扩展模块提供的值，见下节） |
| `LispValue.HashMap(HashMap<String, LispValue>)` | 哈希映射 |
| `LispValue.Error(XispError)` | 错误 |

#### 外部值：`External` 变体与 `LispExternal` 协议族

外部模块（NDArray、分布对象、句柄、FFI 指针等）要把自己的值交给 Lisp 层，
就必须有一个 core 承认的值形态 ——这就是 `LispValue.External(LispExternal)`。
core **只认识协议**，不认识任何具体外部类型。

```cangjie
// ystyle::xisp.types（core/src/types/lisp_external.cj）

/** 必需：core 的通用逻辑（print / type-of / 错误信息）只依赖这两项 */
public interface LispExternal {
    prop typeName: String        // "ndarray"，供 type-of / 谓词 / 错误信息
    func display(): String       // repr；实现方负责截断
}
```

以下能力接口**按需实现**，core 内建按"是否实现"分派，互不牵连：

| 接口 | 必需？ | 作用 |
|------|--------|------|
| `LispExternal` | **必需** | `typeName` + `display()` |
| `LispExternalEquatable` | 按需 | `equals(other: LispExternal): Bool`，用于 `=` 的语义；未实现则不能作 `HashMap` 键 |
| `LispExternalIndexable` | 按需 | `length(): Int64` + `at(index: Int64): LispValue`，供 `length` 等内建分派 |
| `LispExternalComparable` | 按需 | `compare(other: LispExternal): Int64`，供 `<` / `>` 等比较内建分派 |
| `LispExternalCallable` | 按需 | `invoke(args: ArrayList<LispValue>): LispValue`，让外部值能被"调用" |

Lisp 侧看到的效果（`make-counter` 由下面的宿主示例注册）：

```lisp
(external? (make-counter 5))       ; => true
(external-type (make-counter 5))   ; => "counter-box"   实现方声明的 typeName
(type-of (make-counter 5))         ; => "counter-box"
(str (make-counter 7))             ; 走实现方的 display()
(length (make-counter 5))          ; => 5   走 LispExternalIndexable
(external? 42)                     ; => false   普通值
(null? (external-type 1))          ; => true    非外部值返回 nil
```

完整示例（宿主侧实现一个外部类型）：

```cangjie
import ystyle::xisp.*          // LispInterpreter / withStdLib
import ystyle::xisp.types.*    // LispValue / LispExternal / LispExternalIndexable
import ystyle::xisp.bridge.*   // Bridge

/** 定长计数器盒子：对外声明 typeName = "counter-box" */
class CounterBox <: LispExternal & LispExternalIndexable {
    let cap: Int64
    init(cap: Int64) { this.cap = cap }

    public prop typeName: String { get() { "counter-box" } }
    public func display(): String { "#<counter-box cap=${this.cap}>" }
    public func length(): Int64 { this.cap }
    public func at(index: Int64): LispValue {
        if (index >= 0 && index < this.cap) { LispValue.Int(index) } else { Nil }
    }
}

let interpreter = LispInterpreter([withStdLib()])
Bridge.registerFunc(interpreter.getEnvironment(), "make-counter", { args =>
    let cap = match (args[0]) {
        case LispValue.Int(n) => n
        case _ => 0
    }
    LispValue.External(CounterBox(cap))
})
```

> 上述 Lisp 侧输出（`external?` / `type-of` / `external-type` / `str` / `length`）
> 由 `xisp_tests/src/external_value_test.cj` 用同一个 `CounterBox` 端到端验证，
> AST 与字节码两个后端结果一致。

::: warning 实现方须遵守的语义约定
1. `=` 是**同一性 + 结构浅比较**，不做逐元素（逐元素比较请另立 `array-equal?` 之类的显式函数）；
2. 外部值**恒为真**（对象存在即真），判空请用显式谓词；
3. **可变外部值禁止作 `HashMap` 键**；
4. FFI 资源由实现方自行实现 `Resource`，core 不引入 `close` 语义；
5. 外部值**不参与**序列化；需要时实现 `LispConvertible.toLisp()` 显式转换；
6. `display()` 的截断由**实现方**负责（core 统一截断会让"想打全量"无法表达）；
7. `cloneValue()` 对外部值按**引用**复制（深拷会破坏零拷贝这一设计前提），可变性由实现方负责。
:::

### 能力接口与 Lisp 内建的对应关系

五个能力接口都已在 core 里接上分派，对应关系如下（未实现的接口 = 该能力**不支持**，
不是"静默成功"）：

| 能力接口 | 被哪个 Lisp 内建消费 | 未实现时的行为 |
|---|---|---|
| `LispExternal`（`typeName` / `display`） | `type-of` / `external?` / `external-type` / `str` | 必需接口，不实现就不是外部值 |
| `LispExternalIndexable`（`length` / `at`） | `length` / `nth` | `length` → `0`（与"非列表返回 0"一致）；`nth` → `nil` |
| `LispExternalEquatable`（`equals`） | `=` / `!=` | 两个外部值**不相等**（外部值与其它类型恒不相等，不做隐式转换） |
| `LispExternalComparable`（`compare`） | `<` `>` `<=` `>=` | 该比较恒为 `false`（注意不能当成"相等"，否则 `<=` 会误判） |
| `LispExternalCallable`（`invoke`） | `((ext) args...)` 调用形式 | `TypeError`：non-callable external value |

```lisp
(= (make-counter 3) (make-counter 3))    ; => true    走 equals
(< (make-counter 3) (make-counter 5))    ; => true    走 compare
(nth (make-counter 5) 3)                 ; => 3       走 at(index)
(nth '(10 20 30) 1)                      ; => 20      列表同样可用
((make-doubler) 21)                      ; => 42      走 invoke
```

::: tip 为什么分派放在"能力接口"上
core 不认识任何具体外部类型，只探测"它实现了哪个接口"。这样扩展族新增类型时
core 无需改动，也符合"可裁剪 / 可替换"的设计前提（见设计文档 §7.2.1）。
:::

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
import ystyle::xisp.types.*    // LispValue / LispConvertible / LispDeserializable

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
import ystyle::xisp.*          // LispInterpreter / 选项
import ystyle::xisp.types.*    // LispValue / XispError / ErrorType
import ystyle::xisp.bridge.*   // asInt() 等（ExtendLispValue）
import std.collection.ArrayList

main() {
    let interpreter = LispInterpreter([withStdLib(), withQuietMode()])

    // 1. 注册仓颉函数到 Lisp
    interpreter.registerBridgeFunctionWithNS("math", "double", { args =>
        if (args.size > 0 && let LispValue.Int(n) <- args[0]) {
            LispValue.Int(n * 2)
        } else {
            LispValue.Error(XispError(ErrorType.TypeError, "double: expected number"))
        }
    })

    // 2. Lisp 中使用桥接函数
    interpreter.eval("(define x (math::double 21))")
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

    // 4. 列表 + 标量的异构实参：`call` 的实参是 `Array<T>`（同构），
    //    异构实参用 `LispValue` 装箱成 `Array<LispValue>`
    interpreter.eval("(define (len-plus lst n) (+ (length lst) n))")
    let lst: LispValue = ArrayList<Int64>([1, 2, 3]).toLisp()
    let mixed = interpreter.call("len-plus", [lst, LispValue.Int(7)])
    if (let Some(i) <- mixed.asInt()) {
        println(i)   // 10
    }
}
```

> 本示例有对应的**可编译版本**：[`xisp_examples/src/bridge_demo/main.cj`](https://gitcode.com/ystyle/xisp/blob/master/xisp_examples/src/bridge_demo/main.cj)。
> 文档里的 API 名字受它的编译校验保护；改这里请同步改那份。

::: tip 实参按值注入
`call` 会把实参转换成 Lisp 值后**按值**传给目标函数：列表值（`Array` / `ArrayList` /
`HashSet`）会包成 `(quote value)` 再进调用式，不会被求值器当作代码再次求值。

正因为实参走的是 `Array<T>`（同构），要传「一个列表 + 一个标量」这类异构组合，
需要先把各项装箱成 `LispValue`（`LispValue` 实现了 `LispConvertible`，`toLisp()` 为恒等转换）。
:::

---

## 注意事项

- **参数校验**：桥接函数务必用模式匹配校验参数类型，避免把异常抛回 Lisp 求值环境。
- **错误返回**：用 `LispValue.Error(XispError(kind, message))` 返回错误。`ErrorType` 是**封闭枚举**，全部成员为
  `UnboundVariable` / `UndefinedFunction` / `TypeError` / `ArityMismatch` / `SyntaxError` / `RuntimeError` / `IOError` / `ModuleError`
  —— 类型不符用 `TypeError`（没有 `TypeMismatch` 这个成员）。
- **命名空间**：命名空间与函数名统一用双冒号 `::` 连接（`ns::name`），`interpreter.registerBridgeFunctionWithNS`
  与 `Bridge.registerFuncWithNS` 是同一约定、同一符号。
- **类型转换**：`asXxx()` 类型不匹配时返回 `None`，用 `if let`/`match` 处理。
- **`LispValue` 的 `match` 可能被 `External` 打断**：`LispValue` 是**封闭枚举**，新增 `External(LispExternal)`
  变体后，宿主侧把所有变体逐一列出、且**没有 `case _` 兜底**的 `match` 会变成**非穷尽**（仓颉里这是
  **编译错误**，不是警告）。升级后编译报 `non-exhaustive patterns` 的地方，补一个分支即可：

  ```cangjie
  match (value) {
      case LispValue.Int(n) => …
      case LispValue.Str(s) => …
      case LispValue.External(ext) => ext.display()   // 新增的这个分支
      case _ => …
  }
  ```

  大多数宿主代码本来就有 `case _` 兜底，实际需要改动的位置很少（core 自身只有 `toString` 与
  `cloneValue` 两处），但**升级前请先编译一遍**，让编译器把所有位置指出来。

## 参见

- [嵌入解释器](embedding) - `eval` / `call` / 运行时配置
- [选项系统](options) - `withStdLib` / `withStdIO` / `withStdFS`
- [沙箱](sandbox) - 路径权限回调与函数白名单
