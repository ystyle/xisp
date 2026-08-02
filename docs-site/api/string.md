# 字符串

本页覆盖字符串相关函数，共 7 个。

::: warning 字符串比较
Xisp 的 `=` 只用于数字比较，**字符串相等请使用 `string=?`**。`(= "a" "a")` 返回 `false`。
:::

---

### `str` - 拼接为字符串

签名：`(str x1 x2 ...)`

把所有参数转为字符串并拼接。字符串参数直接使用内容（不加引号），其他值用其字符串表示。

```lisp
(str 42)                 ; => "42"
(str "a" 1 "b" 2.5)      ; => "a1b2.500000"
(str "Hello" " " "World")  ; => "Hello World"
(str (list 1 2))         ; => "(1 2)"
(str)                    ; => ""
```

::: tip 浮点显示
浮点转字符串保留 6 位小数：`(str 3.14)` → `"3.140000"`。
:::

### `string-append` - 字符串连接

签名：`(string-append x1 x2 ...)`

与 `str` 完全等价（Common Lisp 风格别名）。

```lisp
(string-append "Hello" " " "World")   ; => "Hello World"
(string-append "x" 42)                ; => "x42"
```

### `string=?` - 字符串相等

签名：`(string=? str1 str2)`

两个参数都必须是字符串，否则返回 `false`。

```lisp
(string=? "a" "a")       ; => true
(string=? "a" "b")       ; => false
(string=? "a" 42)        ; => false   非字符串
```

### `string<` / `string>` - 字典序比较

签名：`(string< str1 str2)` / `(string> str1 str2)`

按字典序（UTF-8 编码）比较。参数不是字符串时返回 `nil`。

```lisp
(string< "a" "b")        ; => true
(string< "abc" "abd")    ; => true
(string< "b" "a")        ; => false
(string> "b" "a")        ; => true
(string> "a" "b")        ; => false
(string< "b" 1)          ; => nil    非字符串
```

### `string?` - 是否为字符串

签名：`(string? x)`

```lisp
(string? "hello")        ; => true
(string? 42)             ; => false
(string? 'sym)           ; => false
```

::: tip 详见谓词页
`string?` 与 `type-of` 的完整说明见[谓词](predicates)。
:::

### `type-of` - 类型名称

签名：`(type-of x)`

```lisp
(type-of "hello")        ; => "string"
(type-of 42)             ; => "integer"
```

---

## 结合使用

```lisp
; 拼接并比较
(define greeting (str "Hello, " "world" "!"))
greeting                 ; => "Hello, world!"
(string=? greeting "Hello, world!")   ; => true

; 按名字排序（字典序）
(define names ["bob" "alice" "charlie"])
(sort-names names)       ; 需要配合自定义排序，此处仅示意
(string< "alice" "bob")  ; => true
```

::: warning 字符串插值
Xisp 支持字符串插值 `"Value: ${x}"`，在[现代语法](../guide/03-modern)指南中有介绍。
:::
