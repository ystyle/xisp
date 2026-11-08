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
(length 42)          ; => 0   非列表返回 0
```

::: warning `length` 只处理列表，字符串也是「非列表」
`length` 对**非列表一律返回 `0`**（这是有意设计，不是缺陷）——字符串同样如此。
取字符串长度请用 [`string-length`](string)：

```lisp
(length "hello")        ; => 0
(string-length "hello") ; => 5
(string-length "中文")  ; => 6    按 UTF-8 字节数，不是字符数
```

宿主注入的外部值若实现了 `LispExternalIndexable`，`length` 会按该接口取长度；
未实现则同样按「非列表返回 `0`」处理。见[桥接 - 外部值](../integration/bridge)。
:::

### `nth` - 按下标取元素

签名：`(nth x index)`

列表按链表走，下标从 `0` 开始；**越界返回 `nil`**（与 `first`/`second` 一致）。
外部值若实现 `LispExternalIndexable`，则走其 `at(index)`。

```lisp
(nth '(10 20 30) 0)     ; => 10
(nth '(10 20 30) 2)     ; => 30
(nth '(10 20 30) 9)     ; => nil   越界
(nth '(10 20 30) -1)    ; => nil   负下标
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

::: tip 特殊形式，同时是一等值
`map`、`filter`、`reduce`、`apply`、`for-each`、`eval`、`->` 这 7 个符号走的是**双轨**：

- **调用位置**（`(map f lst)`）：仍由求值器直接分派（快速路径，参数按求值器语义处理，不走普通的"先求值全部参数"规则）；
- **值位置**（`(procedure? map)`、把 `map` 当参数传出去）：环境里有与之一致的可调用值。

```lisp
(procedure? map)          ; => true
(type-of map)             ; => "procedure"

(define (go f) (f (lambda (x) (* x 2)) '(1 2 3)))
(go map)                  ; => (2 4 6)
```

需要注意：取作值之后**调用时仍按求值器语义执行**（尤其是列表实参），不是纯函数语义；
把 `map` / `apply` 这类"自己接收过程"的符号当作被映射的元素时也不会得到期望结果
（`(map map '((1 2) (3 4)))` → `(nil nil)`）。

其它**真·特殊形式**（`if` `quote` `define` `lambda` `let` `set!` `begin` `match` `import` `export`
`macroexpand` `macroexpand-all`）不在此列，取作值仍是 `[UnboundVariable]`。
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

支持任意可调用值（含 `lambda` 与其它用户定义函数）。非可调用值会返回 `TypeError` 而不是静默跳过：

```lisp
(for-each 42 '(1 2))          ; => [TypeError] for-each: expected a procedure, got 42
(for-each negate '(1 2 3))    ; => [TypeError] for-each: expected a procedure, got #<macro>
```

> `negate` / `swap` / `when` 这类是**宏**，不能当过程传（`procedure?` 虽为 `true`，`type-of` 是 `"macro"`）。
> 需要时包一层：`(for-each (lambda (x) (negate x)) '(1 2 3))`。

②类符号的一等值则可以正常传入或取出使用：

```lisp
; 从列表里取出 map 当函数用
((first (list map for-each)) (lambda (x) (* x 2)) '(1 2 3))   ; => (2 4 6)
```

多列表时以**最短列表**为准；过程返回 `Error` 时立即中止并返回该错误。

```lisp
(for-each (lambda (a b) (println a b)) '(1 2 3) '(4 5))
; 只打印两行：1 4 / 2 5（以最短列表为准）

(for-each (lambda (x) (if (= x 2) (error "boom") x)) '(1 2 3))
; => [RuntimeError] "boom"   立即中止
```

`for-each` 与其它高阶函数一样**可由用户重新定义顶替**：`(define for-each 我的实现)` 之后调用即走用户实现。

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
