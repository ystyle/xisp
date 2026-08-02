# 逻辑

本页覆盖逻辑运算，共 3 个函数。

::: tip 真值规则
求值时只有 `nil`、`#f`/`false`、`0`、`0.0` 为假，其余值（包括空字符串、非零数字、符号、列表）均为真。
:::

---

### `and` - 逻辑与

签名：`(and x1 x2 ...)`

短路求值：从左到右，遇到假值立即返回 `false`；全为真时返回**最后一个值**；无参数返回 `true`。

```lisp
(and #t #t)          ; => true
(and #t #f)          ; => false
(and #f #t)          ; => false   短路，不再求值后面的表达式
(and)                ; => true    无参
(and 1 2 3)          ; => 3       返回最后一个真值
(and "a" nil)        ; => false
```

### `or` - 逻辑或

签名：`(or x1 x2 ...)`

短路求值：从左到右，遇到真值立即返回该值；全为假返回 `false`；无参数返回 `false`。

```lisp
(or #t #f)           ; => true
(or #f #f)           ; => false
(or)                 ; => false   无参
(or nil "x")         ; => "x"     返回第一个真值
(or #f 0)            ; => false   0 是假值
```

::: tip 返回值技巧
`and` / `or` 返回的并不总是布尔值：
- `(or nil "default")` 返回 `"default"`，可用作缺省值。
- `(and config config.value)` 在 `config` 为真时返回 `config.value`。
:::

### `not` - 逻辑非

签名：`(not x)`

取反，返回布尔值。

```lisp
(not #t)             ; => false
(not #f)             ; => true
(not nil)            ; => true
(not 0)              ; => true     数字 0 是假值
(not 42)             ; => false    非零数字是真值
(not "")             ; => false    空字符串是真值
```

---

## 与流程控制结合

```lisp
; 条件与缺省值
(define name nil)
(println (or name "anonymous"))       ; => "anonymous"

; 多条件守卫
(define (classify n)
  (and (number? n)
       (> n 0)
       "positive"))
(classify 5)                          ; => "positive"
(classify -1)                         ; => false
```
