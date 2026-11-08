# 谓词

本页覆盖类型检查与数值谓词，共 17 个函数。所有谓词都返回布尔值（`external-type` 例外，它返回类型名或 `nil`）。

---

## 类型检查

### `integer?` - 是否为整数

签名：`(integer? x)`

```lisp
(integer? 42)       ; => true
(integer? 3.14)     ; => false
(integer? "42")     ; => false
```

### `float?` - 是否为浮点数

签名：`(float? x)`

```lisp
(float? 3.14)       ; => true
(float? 42)         ; => false
```

### `number?` - 是否为数字

签名：`(number? x)`

整数或浮点数都返回 `true`。

```lisp
(number? 42)        ; => true
(number? 3.14)      ; => true
(number? "x")       ; => false
```

### `string?` - 是否为字符串

签名：`(string? x)`

```lisp
(string? "hello")   ; => true
(string? 42)        ; => false
```

### `symbol?` - 是否为符号

签名：`(symbol? x)`

```lisp
(symbol? 'foo)      ; => true
(symbol? :kw)       ; => true   关键字也是符号
(symbol? "foo")     ; => false
```

### `list?` - 是否为列表

签名：`(list? x)`

`nil` 也是列表（空列表）。

```lisp
(list? '(1 2 3))    ; => true
(list? '())         ; => true
(list? [1 2 3])     ; => true   向量求值为列表
(list? "abc")       ; => false
```

### `null?` - 是否为空 / nil

签名：`(null? x)`

```lisp
(null? nil)         ; => true
(null? '())         ; => true   nil 与空列表等价
(null? '(1 2))      ; => false
```

### `procedure?` - 是否为过程

签名：`(procedure? x)`

判断是否为可调用值：内置函数、`lambda`、用户定义函数、编译闭包，以及
`map` / `filter` / `reduce` / `apply` / `for-each` / `eval` / `->` 这 7 个**②类符号的一等值**。

```lisp
(procedure? +)                  ; => true   内置函数
(procedure? car)                ; => true
(procedure? (lambda (x) x))     ; => true   lambda
(procedure? 42)                 ; => false

; ②类符号已一等值化，可取作值
(procedure? map)                ; => true
(procedure? for-each)           ; => true
(procedure? ->)                 ; => true
```

::: tip `procedure?` 对宏也返回 `true`
`when`、`negate`、`and`、`or`、`condb`、`let*` 等**宏**同样是 `true`
（它们与过程在 `isProcedure` 判断上归为一类），但 `type-of` 会把它们报成 `"macro"`。
要区分宏与过程，请用 `type-of`：

```lisp
(procedure? negate)             ; => true
(type-of negate)                ; => "macro"
(type-of map)                   ; => "procedure"
```

真·特殊形式（`if` `quote` `lambda` `let` `define` `set!` `begin` `match` `import` `export`
`macroexpand` `macroexpand-all`）在环境里**没有绑定**，取作值会报 `[UnboundVariable]`：

```lisp
(procedure? let)                ; => [UnboundVariable] Undefined variable: 'let'
(procedure? if)                 ; => [UnboundVariable] Undefined variable: 'if'
```
:::

### `external?` - 是否为外部值

签名：`(external? x)`

判断是否为**宿主 / 扩展模块注入的外部值**（`LispValue.External`）。普通的内建值一律 `false`。

```lisp
(external? 1)                   ; => false
(external? "s")                 ; => false
(external? (lambda (x) x))      ; => false
```

外部值由宿主用 `LispValue.External(...)` 产出，详见[桥接 - 外部值](../integration/bridge)。

### `external-type` - 外部值的类型名

签名：`(external-type x)`

返回外部值实现方声明的 `typeName`（如 `"ndarray"`）；非外部值返回 `nil`。

```lisp
(null? (external-type 1))       ; => true    非外部值为 nil
(null? (external-type "s"))     ; => true
```

### `hashmap?` - 是否为哈希映射

签名：`(hashmap? x)`

```lisp
(hashmap? {:a 1})   ; => true
(hashmap? '(1 2))   ; => false
```

### `type-of` - 获取类型名称

签名：`(type-of x)`

返回类型的字符串名称，可用于类型分发。

```lisp
(type-of 42)          ; => "integer"
(type-of 3.14)        ; => "float"
(type-of "s")         ; => "string"
(type-of 'sym)        ; => "symbol"
(type-of '(1 2))      ; => "list"
(type-of nil)         ; => "nil"
(type-of #t)          ; => "boolean"
(type-of {:a 1})      ; => "hashmap"
(type-of +)           ; => "procedure"
(type-of map)         ; => "procedure"   ②类符号的一等值
(type-of (lambda (x) x))  ; => "procedure"
(type-of negate)      ; => "macro"       宏
```

编译闭包（字节码后端下的 `lambda`）与解释器过程**都报 `"procedure"`**，两个后端一致。

外部值报其实现方声明的 `typeName`，而非笼统的 `"external"`：

```lisp
; 假设宿主注册了 (make-counter 5) 返回 typeName 为 "counter-box" 的外部值
(type-of (make-counter 5))     ; => "counter-box"
(external-type (make-counter 5))  ; => "counter-box"
```

> 外部值只能由宿主（仓颉侧）注入，命令行 `-c` 里造不出来；
> 上面两行的实测由 `xisp_tests/src/external_value_test.cj` 覆盖（AST 与字节码两个后端一致）。

返回名称汇总：`nil`、`boolean`、`integer`、`float`、`string`、`symbol`、`list`、`hashmap`、
`procedure`、`macro`、`error`，以及外部值实现方的自定义 `typeName`。

错误值（`Error` 变体）报 `"error"`：脚本侧通常看不到它（实参里出现错误值时，
调用会先短路传播该错误），但宿主与桥接在检查返回值类型时会直接拿到这个变体。

::: tip 编译闭包也报 `"procedure"`，没有单独的 `"closure"`
字节码后端下的 `lambda` 在内部是 `Closure` 变体，但 `type-of` 对它与解释器过程**统一返回
`"procedure"`**（两个后端必须一致）；`toString` 同样统一为 `#<procedure>`。
所以返回名称列表里**没有** `"closure"` 这一项。
:::

::: tip 关于 `"unknown"`
实现里保留了一个 `"unknown"` 兜底分支，但当前所有内建变体都已显式处理，
正常求值路径下取不到它。参数个数不为 1 时返回 `"error"`：

```lisp
(type-of 1 2)         ; => "error"
```
:::

---

## 数值谓词

### `zero?` - 是否为零

签名：`(zero? x)`

```lisp
(zero? 0)           ; => true
(zero? 0.0)         ; => true
(zero? 5)           ; => false
(zero? "0")         ; => false   非数字
```

### `positive?` - 是否为正数

签名：`(positive? x)`

```lisp
(positive? 5)       ; => true
(positive? 3.5)     ; => true
(positive? 0)       ; => false
(positive? -5)      ; => false
```

### `negative?` - 是否为负数

签名：`(negative? x)`

```lisp
(negative? -5)      ; => true
(negative? -2.5)    ; => true
(negative? 0)       ; => false
```

### `even?` - 是否为偶数

签名：`(even? x)`

```lisp
(even? 4)           ; => true
(even? 0)           ; => true
(even? 3)           ; => false
(even? 4.0)         ; => false   只接受整数
```

### `odd?` - 是否为奇数

签名：`(odd? x)`

```lisp
(odd? 3)            ; => true
(odd? 4)            ; => false
(odd? 3.0)          ; => false   只接受整数
```

---

## 结合使用

```lisp
; 用 type-of 做类型分发
(define (describe x)
  (condb
    (number? x) "a number"
    (string? x) "a string"
    (list? x) "a list"
    else "unknown"))

(describe 42)        ; => "a number"
(describe "hi")      ; => "a string"
(describe '(1 2))    ; => "a list"
(describe {:a 1})    ; => "unknown"
```
