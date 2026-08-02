# 流程控制

本页覆盖条件分支、变量修改宏、元编程与输出/错误函数，共 23 个符号。

::: tip 宏与特殊形式
`when`、`unless`、`incf`、`decf`、`swap`、`negate`、`push`、`pop`、`do`、`defun` 是**宏**；`if-let`、`when-let*`、`condb`、`eval`、`macroexpand`、`macroexpand-all` 是**特殊形式**。它们都不按普通函数求值规则处理参数。
:::

---

## 条件分支

### `when` - 条件成立时执行

签名：`(when test body...)`

测试为真时按顺序执行 body（支持多表达式，返回最后一个），为假时返回 `nil`。

```lisp
(when (> 5 3) (println "big") 100)   ; 打印 "big"，返回 100
(when #f (println "not run"))        ; => nil  条件假不执行 body
(when #t 1 2 3)                      ; => 3    多表达式取最后一个
```

::: tip
`when` 支持多表达式，条件假返回 `nil`。
:::

### `unless` - 条件不成立时执行

签名：`(unless test body...)`

`when` 的取反：测试为假时执行 body，为真时返回 `nil`。

```lisp
(unless (= 1 2) "ok")      ; => "ok"
(unless #t "not run")      ; => nil   条件真不执行
```

### `if-let` - 绑定后判断

签名：`(if-let (var value) then else)`

先把 `value` 绑定到 `var`，再判断其真假：为真执行 `then`，为假执行 `else`。

```lisp
(if-let (v 10) "yes" "no")      ; => "yes"   10 是真值
(if-let (v nil) "yes" "no")     ; => "no"    nil 是假值
(if-let (v "hello") (str "got " v) "empty")
; => "got hello"
```

### `when-let*` - 顺序绑定并判断

签名：`(when-let* ((var1 expr1) (var2 expr2) ...) body...)`

按顺序绑定变量（后面的绑定可引用前面的），最后一个绑定值为真时执行 body；否则返回 `nil`。

```lisp
(when-let* ((x 5) (y (+ x 1)))
  (str "sum=" (+ x y)))         ; => "sum=11"

(when-let* ((x nil))
  (println "不会执行"))          ; => nil   最后一个绑定为假
```

### `condb` - 增强条件表达式

签名：`(condb cond1 result1 cond2 result2 ... [else default])`
签名：`(condb (:let var value) ... cond1 result1 ... [else default])`

子句**平铺**排列：条件与结果两两成对；命中第一个真条件并返回其结果；`else` 匹配所有未命中的情况。

```lisp
(condb (= 1 2) "a" (= 1 1) "b" else "c")   ; => "b"
(condb (= 1 2) "a" else "c")               ; => "c"
(condb else "default")                     ; => "default"
(condb (= 1 1) "only-true")                ; => "only-true"
(condb (= 1 2) "x")                        ; => nil   无匹配无 else
```

支持 `:let` 绑定，条件可使用绑定的变量：

```lisp
(condb (:let n 5)
       (> n 3) "big"
       else "small")          ; => "big"

(condb (:let n 2) (:let m 3)
       (= n m) "equal"
       (> n m) "n>m"
       else "n<m")            ; => "n<m"
```

---

## 变量修改宏

### `incf` / `decf` - 自增 / 自减

签名：`(incf var)` / `(decf var)`

等价于 `(set! var (+ var 1))` / `(set! var (+ var -1))`。

```lisp
(define count 5)
(incf count)       ; count => 6
(incf count)       ; count => 7
(decf count)       ; count => 6
```

### `swap` - 交换两个变量

签名：`(swap a b)`

```lisp
(define x 1)
(define y 2)
(swap x y)
x                  ; => 2
y                  ; => 1
```

### `negate` - 取反

签名：`(negate x)`

等价于 `(* x -1)`，支持整数与浮点数。

```lisp
(negate 5)         ; => -5
(negate -3)        ; => 3
(negate 2.5)       ; => -2.500000
```

::: warning 宏不可作为值传递
`negate` 是宏，不能像函数一样传给 `map` 等高阶函数（`(map negate lst)` 会得到 `(nil nil ...)`）。请用 `(lambda (x) (negate x))`。
:::

### `push` / `pop` - 栈操作

签名：`(push elem lst)` / `(pop lst)`

- `push` 实现为 `(prepend elem lst)`（`cons` 的现代别名），返回新列表。
- `pop` 实现为 `(rest lst)`（`cdr` 的现代别名），返回去掉首元素的新列表。

**两者都返回新列表，不修改原列表。**

```lisp
(define stack '(2 3))
(push 1 stack)     ; => (1 2 3)   原 stack 不变
stack              ; => (2 3)
(pop stack)        ; => (3)       原 stack 不变
```

### `do` - 顺序执行别名

签名：`(do body...)`

`begin` 的别名，按顺序执行并返回最后一个值。

```lisp
(do 1 2 3)         ; => 3
```

### `defun` - 定义函数（兼容）

签名：`(defun name (params...) body...)`

Common Lisp 风格的 `define`。

```lisp
(defun square (x) (* x x))
(square 5)         ; => 25
```

---

## 元编程

### `eval` - 动态求值

签名：`(eval expr)`

先求值参数得到表达式，再对表达式求值。

```lisp
(eval '(+ 1 2))          ; => 3
(eval (list '+ 10 20))   ; => 30
```

### `macroexpand` / `macroexpand-all` - 宏展开

签名：`(macroexpand expr)` / `(macroexpand-all expr)`

对引用（不执行）的表达式做宏展开，返回展开后的语法树。`macroexpand` 只展开最外层，`macroexpand-all` 完全展开。

```lisp
(macroexpand '(when #t 1))       ; => (if true (begin 1) nil)
(macroexpand-all '(when #t 1))   ; => (if true (begin 1) nil)

(macroexpand '(incf x))          ; => (setq x (+ x 1))
```

::: tip
需要给宏展开传入**未求值**的表达式，因此使用 `quote`（`'`）引用。
:::

---

## 输出与错误

所有输出函数返回 `nil`。

::: tip 字符串显示
输出字符串参数时带双引号（repr 形式）：`(print "A")` 输出 `"A"`。
:::

### `print` - 打印不换行

签名：`(print x1 x2 ...)`

参数之间用空格分隔，不换行。

```lisp
(print "A" "B")     ; 输出: "A" "B"
```

### `println` - 打印并换行

签名：`(println x1 x2 ...)`

参数之间用空格分隔，末尾换行。

```lisp
(println "Hello" "World")   ; 输出: "Hello" "World"
```

### `princ` / `display` - 打印单个值

签名：`(princ x)` / `(display x)`

只打印第一个参数，不加空格、不换行。两者行为一致。

```lisp
(princ "X")     ; 输出: "X"
(display "Y")   ; 输出: "Y"
```

### `newline` - 输出换行

签名：`(newline)`

```lisp
(print "A") (newline) (print "B")
; 输出两行: A 换行 B
```

### `error` / `raise` - 抛出运行时错误

签名：`(error [message])` / `(raise [message])`

创建并返回一个运行时错误（求值器会中断执行）。两者等价。

```lisp
(error "boom")      ; => [RuntimeError] "boom"
(raise "boom2")     ; => [RuntimeError] "boom2"
(error)             ; => [RuntimeError] error
```

### `assert` - 断言

签名：`(assert condition [message])`

条件为真返回 `true`；为假则抛出运行时错误（可用第二个参数自定义消息）；非布尔条件按真处理。

```lisp
(assert #t)               ; => true
(assert #t "msg")         ; => true
(assert (= 1 1) "ok")     ; => true
(assert #f "custom msg")  ; => [RuntimeError] "custom msg"
```

---

## 综合示例

```lisp
; 用 when-let* + condb 组合业务逻辑
(define (grade score)
  (condb
    (>= score 90) "A"
    (>= score 80) "B"
    (>= score 60) "C"
    else "F"))

(grade 95)      ; => "A"
(grade 70)      ; => "C"

; 防御式取值：错误后给出默认值
(define (safe-div a b)
  (if (zero? b)
      (error "divide by zero")
      (/ a b)))
```
