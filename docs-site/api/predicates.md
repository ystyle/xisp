# 谓词

本页覆盖类型检查与数值谓词，共 15 个函数。所有谓词都返回布尔值。

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

判断是否为可调用过程（内置函数或 `lambda`）。

```lisp
(procedure? +)                  ; => true   内置函数
(procedure? (lambda (x) x))     ; => true   lambda
(procedure? 42)                 ; => false
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
(type-of (lambda (x) x))  ; => "procedure"
```

返回名称汇总：`nil`、`boolean`、`integer`、`float`、`string`、`symbol`、`list`、`hashmap`、`procedure`、`macro`、`unknown`。

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
