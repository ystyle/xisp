# 列表操作

本页覆盖列表的构造、访问、组合与高阶操作。**推荐使用现代语法**（`first`/`rest`/`second`/`third`/`fourth`/`prepend`），传统语法（`car`/`cdr`/`cadr`/`cons`）仅作兼容说明。

::: tip 列表字面量
`'(1 2 3)` 与 `[1 2 3]` 都会求值为列表；`[1 2 3]`（向量）求值后即列表。`nil` 与 `'()` 都表示空列表。
:::

---

## 构造与拼接

### `list` - 构造列表

签名：`(list x1 x2 ...)`

```lisp
(list 1 2 3)     ; => (1 2 3)
(list)           ; => nil
(list 1 "a" :k)  ; => (1 "a" :k)
```

### `prepend` - 在头部添加元素

签名：`(prepend x lst)`

把 `x` 作为头元素构造新列表。

```lisp
(prepend 0 '(1 2))    ; => (0 1 2)
(prepend 'a '(b c))   ; => (a b c)
(prepend 1 '())       ; => (1)
```

::: tip 传统兼容
`cons` 是 `prepend` 的传统别名：`(cons 0 '(1 2))` 与 `(prepend 0 '(1 2))` 等价。
:::

### `append` - 连接列表

签名：`(append lst1 lst2 ...)`

将多个列表的元素依次连接成新列表。

```lisp
(append '(1 2) '(3 4))   ; => (1 2 3 4)
(append '(1) '() '(3))   ; => (1 3)
(append '() '())         ; => nil
```

::: warning `append` 只接受列表
`(append '(1) 2)` 返回 `nil`。若要在尾部加元素，用 `(append lst (list 2))`。
:::

### `range` - 生成数字序列

签名：`(range start end [step])`

生成 `[start, end)` 的数列，步长默认 1。**区间左闭右开，不含 end**。

```lisp
(range 1 5)         ; => (1 2 3 4)        不含 end
(range 0 10 3)      ; => (0 3 6 9)        指定步长
(range 1.0 4.0)     ; => (1.000000 2.000000 3.000000)  支持浮点
```

边界：只传 1 个参数（`(range 5)`）或 `start >= end` 时返回 `nil`。

### `reverse` - 反转列表

签名：`(reverse lst)`

```lisp
(reverse '(1 2 3))   ; => (3 2 1)
(reverse '())        ; => nil
```

---

## 访问与提取

### `first` - 取第一个元素

签名：`(first lst)`

```lisp
(first '(1 2 3))      ; => 1
(first '(1))          ; => 1
(first '())           ; => nil   空列表
```

::: tip 传统兼容
`car` 是 `first` 的传统别名：`(car '(1 2 3))` → `1`。
:::

### `rest` - 去掉第一个元素

签名：`(rest lst)`

```lisp
(rest '(1 2 3))      ; => (2 3)
(rest '(1))          ; => nil
(rest '())           ; => nil
```

::: tip 传统兼容
`cdr` 是 `rest` 的传统别名，`more` 也是：`(cdr '(1 2 3))` → `(2 3)`。
:::

### `second` / `third` / `fourth` - 取第 N 个元素

签名：`(second lst)` / `(third lst)` / `(fourth lst)`

```lisp
(second '(1 2 3))      ; => 2
(third '(1 2 3))       ; => 3
(fourth '(1 2 3 4))    ; => 4
(second '(1))          ; => nil   越界返回 nil
```

### `length` - 列表长度

签名：`(length lst)`

```lisp
(length '(1 2 3))    ; => 3
(length '())         ; => 0
(length "abc")       ; => 0   非列表返回 0
```

### 传统 C 系列组合函数（兼容）

以下为传统 Lisp 风格组合，仅用于阅读旧代码或与其他 Lisp 互通，**新代码推荐使用 `first`/`rest`/`second` 等**：

| 传统函数 | 等价 | 示例 | 结果 |
|------|------|------|------|
| `cadr` | `(first (rest x))` | `(cadr '(1 2 3))` | `2` |
| `caddr` | `(first (rest (rest x)))` | `(caddr '(1 2 3 4))` | `3` |
| `cadddr` | `(first (rest (rest (rest x))))` | `(cadddr '(1 2 3 4 5))` | `4` |
| `cddr` | `(rest (rest x))` | `(cddr '(1 2 3 4))` | `(3 4)` |
| `cdar` | `(rest (first x))` | `(cdar '((1 2) 3))` | `(2)` |
| `caar` | `(first (first x))` | `(caar '((1 2) 3))` | `1` |

---

## 高阶操作

::: warning 特殊形式
`map`、`filter`、`reduce`、`apply` 是**特殊形式**而非普通函数，参数不按普通规则求值。它们不能作为值直接传递，例如 `(map map lst)` 无效。
:::

### `map` - 映射

签名：`(map fn lst)`

对列表中每个元素应用 `fn`，返回结果列表。`fn` 可以是 `lambda` 或内置函数名。

```lisp
(map (lambda (x) (* x x)) [1 2 3 4])   ; => (1 4 9 16)
(map (lambda (x) (+ x 1)) '(1 2 3))    ; => (2 3 4)
(map (lambda (x) (- 0 x)) '(1 2 3))    ; => (-1 -2 -3)
```

### `filter` - 过滤

签名：`(filter pred lst)`

保留 `pred` 返回真值的元素。

```lisp
(filter (lambda (x) (even? x)) [1 2 3 4 5 6])   ; => (2 4 6)
(filter (lambda (x) (> x 2)) '(1 2 3 4))        ; => (3 4)
```

### `reduce` - 折叠

签名：`(reduce fn init lst)`

从左到右依次把累加器与每个元素交给 `fn`。

```lisp
(reduce + 0 [1 2 3 4 5])    ; => 15
(reduce * 1 '(1 2 3 4))     ; => 24
(reduce (lambda (acc x) (cons x acc)) '() '(1 2 3))
; => (3 2 1)                按元素前插实现反转
```

### `for-each` - 逐元素执行

签名：`(for-each proc lst1 lst2 ...)`

对每个元素调用过程，返回 `nil`。支持多个等长列表，每次取各列表同下标元素。

```lisp
(for-each println '(10 20 30))
; 打印三行 10 / 20 / 30，返回 nil
```

::: warning 限制
`for-each` 目前**只能接受原生内置函数**（如 `println`）作为过程；传入用户定义的 `lambda` 时不会执行任何调用，直接返回 `nil`。
:::

### `apply` - 展开调用

签名：`(apply fn x1 x2 ... lst)`

把最后一个列表参数展开后与其他参数一起传给 `fn`。

```lisp
(apply + '(1 2 3))          ; => 6
(apply + 1 2 '(3 4))        ; => 10   前面参数 + 列表展开
(apply * '(2 3 4))          ; => 24
(apply list '(1 2 3))       ; => (1 2 3)
(apply max '(5 3 8))        ; => 8
```

### `->` - 管道（thread-first）

签名：`(-> value form1 form2 ...)`

将上一步的结果作为第一个参数传入下一个形式，支持 `(-> v sym)` 或 `(-> v (fn args...))` 两种形态。

```lisp
(-> 5 (* 2) (+ 1))       ; => 11   等价于 (+ (* 2 5) 1)
(-> '(1 2 3) reverse)    ; => (3 2 1)
(-> [1 2 3 4] (map (lambda (x) (* x x))) (reduce + 0))
; => 30   1+4+9+16
```
