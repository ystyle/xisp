# 流程控制

本页覆盖条件分支、变量修改宏、元编程与输出/错误函数，共 24 个符号。

::: tip 宏、特殊形式与普通函数
- **宏**（编译期展开）：`when` `unless` `incf` `decf` `swap` `negate` `push` `pop` `do` `defun`
  `condb` `if-let` `when-let*` `let*` `and` `or`
- **特殊形式**（求值器直接处理）：`macroexpand` `macroexpand-all`
- **②类符号**（求值器分派，但另有一等值）：`eval`
- **普通函数**：`print` `println` `princ` `display` `newline` `error` `raise` `assert` `gensym`

用 `type-of` 可以区分宏与过程：宏返回 `"macro"`，过程返回 `"procedure"`。

```lisp
(type-of negate)      ; => "macro"
(type-of condb)       ; => "macro"
(type-of eval)        ; => "procedure"   ②类符号另有一等值
(type-of println)     ; => "procedure"
```

`macroexpand` / `macroexpand-all` 在环境里没有绑定，取作值会报 `[UnboundVariable]`。
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

::: tip 不会捕获同名临时变量
`swap` 用 `gensym` 生成临时符号，因此即使调用点有同名变量也不会被破坏：

```lisp
(macroexpand '(swap a b))
; => (let ((#:g1 a)) (begin (set! a b) (set! b #:g1)))

(define tmp 10)
(define a 1)
(define b 2)
(swap a b)
(list a b tmp)     ; => (2 1 10)   tmp 不受影响
```
:::

### `negate` - 取反

签名：`(negate x)`

等价于 `(* x -1)`，支持整数与浮点数。

```lisp
(negate 5)         ; => -5
(negate -3)        ; => 3
(negate 2.5)       ; => -2.500000
```

::: warning 宏不可作为值传递
`negate` 是宏，不能像函数一样传给 `map` 等高阶函数——`(map negate lst)` 会得到 `(nil nil ...)`
（`for-each` 更严格，直接报 `TypeError: expected a procedure, got #<macro>`）。
请包一层 lambda：

```lisp
(map negate '(1 2 3))                  ; => (nil nil nil)   不要把宏当函数传
(map (lambda (x) (negate x)) '(1 2 3)) ; => (-1 -2 -3)      正确写法
```

注意与 **②类符号的一等值** 区分：`map` / `filter` / `reduce` / `apply` / `for-each` / `eval` / `->`
这 7 个**不是宏**，它们有真正的一等值，可以自由传递（见[列表操作](list)）。
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

对引用（不执行）的表达式做宏展开，返回展开后的语法树。`macroexpand` 只展开**最外层**，
`macroexpand-all` 反复展开直到不再变化（含展开式中新出现的宏调用）。

```lisp
(macroexpand '(when #t 1))       ; => (if true (begin 1) nil)
(macroexpand-all '(when #t 1))   ; => (if true (begin 1) nil)

(macroexpand '(incf x))          ; => (set! x (+ x 1))
(macroexpand '(decf x))          ; => (set! x (+ x -1))
(macroexpand '(push 1 lst))      ; => (cons 1 lst)
```

两者的差别在**展开结果里还有宏**时才显现——`when-let*` 展开出 `let*`，
只有 `macroexpand-all` 会继续把 `let*` 展开掉：

```lisp
(macroexpand '(when-let* ((x 1)) x))
; => (let* ((x 1)) (if x (begin x) nil))

(macroexpand-all '(when-let* ((x 1)) x))
; => (let ((x 1)) (if x (begin x) nil))
```

::: tip
需要给宏展开传入**未求值**的表达式，因此使用 `quote`（`'`）引用。
:::

### `gensym` - 生成唯一符号

签名：`(gensym)` / `(gensym prefix)`

返回一个**保证唯一**的新符号，形如 `#:g1`；带前缀时形如 `#:g1-tmp`（前缀拼在末尾，
只用于可读性，唯一性由全局计数器保证）。返回值的 `type-of` 是 `"symbol"`。

```lisp
(gensym)              ; => 形如 #:gN（单独运行时是 #:g1，之后依次 +1）
(gensym "tmp")        ; => 形如 #:gN-<前缀>（如 #:g2-tmp，编号取决于此前调用次数）
(type-of (gensym))    ; => "symbol"
(symbol? (gensym))    ; => true
(string? (gensym))    ; => false
```

::: warning 编号不稳定
计数器**全局递增且进程内共享**，每次运行、每次插入其它 `gensym` 调用都会改变编号。
**不要在示例或断言里写死编号**，只依赖「两次 `(gensym)` 结果一定不同」：

```lisp
(define a (gensym))
(define b (gensym))
(= a b)               ; => false
```
:::

用途：Xisp 的 `defmacro` 是**非卫生宏**（宏体里的符号与调用点共享符号空间），
宏内部引入的临时变量会捕获调用点的同名变量。用 `gensym` 在展开期索要一个唯一符号即可避免：

```lisp
(defmacro swap (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a)) (begin (set! ,a ,b) (set! ,b ,tmp)))))
```

内置宏 `swap` 与 `or` 已经这样实现，因此不会再捕获用户的同名变量：

```lisp
(macroexpand '(swap a b))
; => (let ((#:g1 a)) (begin (set! a b) (set! b #:g1)))

(define tmp 10)
(define a 1)
(define b 2)
(swap a b)
(list a b tmp)        ; => (2 1 10)   tmp 没有被 swap 破坏
```

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
