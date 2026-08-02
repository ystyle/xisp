# 基础语法

**阅读时间**: 30 分钟

本指南介绍 Xisp 的核心语法和基础功能。

---

## 数据类型

### 原子类型

```lisp
42           ; 整数
3.14         ; 浮点数
-10          ; 负数
"hello"      ; 字符串
#t           ; 真（也支持 #true / true）
#f           ; 假（也支持 #false / false）
nil          ; 空值，也表示空列表
foo          ; 符号
:keyword     ; 关键字（冒号开头，自求值）
```

::: tip 布尔值输出
代码中可写 `#t`、`#true` 或 `true`，求值结果输出为 `true` / `false`。
:::

**关键字自求值**：`:keyword` 是自求值符号，不需要 quote，直接使用即可。

```lisp
:hget    ; => :hget
```

### 组合类型

```lisp
'(1 2 3)          ; 列表字面量
(list 1 2 3)      ; 构造列表
[1 2 3]           ; 向量（现代语法，见下章）
{:name "张三"}    ; 哈希映射（现代语法，见下章）
'()               ; 空列表
```

---

## 特殊形式

特殊形式是 Xisp 的核心构建块，它们不遵循普通的求值规则。

### define - 定义变量和函数

```lisp
; 定义变量
(define x 10)
(define name "Alice")

; 定义函数
(define (square x)
  (* x x))

(square 5)    ; => 25

; 多参数函数
(define (add x y)
  (+ x y))

(add 3 4)     ; => 7

; 多表达式函数体（返回最后一个值）
(define (process x)
  (println "Processing:" x)
  (* x 2))

(process 5)   ; 打印 "Processing:" 5，返回 10
```

### lambda - 匿名函数

```lisp
(lambda (参数...) 函数体)

(define square
  (lambda (x)
    (* x x)))

(square 5)    ; => 25

; 直接使用
(map (lambda (x) (* x x)) [1 2 3])
; => (1 4 9)
```

### if - 条件判断

```lisp
; (if 条件 then-表达式 else-表达式)
(if (> x 10)
    "large"
    "small")

; 嵌套
(if (= x 0)
    "zero"
    (if (> x 0)
        "positive"
        "negative"))
```

::: tip 真值规则
假值：`nil`、`#f`、数字 `0`、`0.0`。其他都是真值。
:::

### quote - 引用

```lisp
(quote x)           ; => x (符号)
'(1 2 3)            ; => (1 2 3)
'(+ 1 2)            ; => (+ 1 2) (未求值)
```

### begin - 顺序执行

```lisp
(begin
  (println "Step 1")
  (println "Step 2")
  (+ 1 2))
; 打印两行，返回 3
```

### set! - 修改变量

```lisp
(define x 10)
(set! x 20)
x                   ; => 20
```

---

## 内置函数

### 算术运算

```lisp
(+ 1 2 3 4)         ; => 10
(+ 10)              ; => 10
(- 10 3)            ; => 7
(- 10 3 2)          ; => 5
(* 2 3 4)           ; => 24
(/ 10 2)            ; => 5.000000
(/ 20 2 2)          ; => 5.000000
(/ 10 4)            ; => 2.500000
(mod 10 3)          ; => 1
```

::: tip 除法返回浮点
除法总是返回浮点数，固定保留 6 位小数显示：`(/ 10 2)` → `5.000000`，`(/ 1 3)` → `0.333333`。
:::

### 比较运算

```lisp
(= 1 1)             ; => true
(= 1 2)             ; => false
(!= 1 2)            ; => true
(< 1 2)             ; => true
(<= 1 2)            ; => true
(> 5 3)             ; => true
(>= 5 5)            ; => true
```

::: warning `=` 只比较数字
`=` 仅支持数字比较（Int/Float 可混合）。字符串比较用 `string=?`。
:::

### 逻辑运算

```lisp
(and #t #t)         ; => true
(and #t #f)         ; => false
(and)               ; => true
(or #t #f)          ; => true
(or #f #f)          ; => false
(or)                ; => false
(not #t)            ; => false
(not nil)           ; => true
```

### 列表操作

```lisp
(define lst '(1 2 3 4 5))

(first lst)         ; => 1
(rest lst)          ; => (2 3 4 5)
(second lst)        ; => 2
(third lst)         ; => 3
(prepend 0 lst)     ; => (0 1 2 3 4 5)
(list 1 2 3)        ; => (1 2 3)
(length lst)        ; => 5
(null? '())         ; => true
(null? lst)         ; => false
```

### 列表构造

```lisp
(prepend 1 '(2 3))  ; => (1 2 3)
(append '(1 2) '(3 4))  ; => (1 2 3 4)
(reverse '(1 2 3))  ; => (3 2 1)
(range 1 5)         ; => (1 2 3 4)
```

::: tip 传统兼容
`cons` 是 `prepend` 的传统别名：`(cons 1 2)` → `(1 . 2)` 点对。
:::

### 谓词函数

```lisp
(number? 42)        ; => true
(string? "hello")   ; => true
(symbol? 'foo)      ; => true
(list? '(1 2 3))    ; => true
(procedure? +)      ; => true
(zero? 0)           ; => true
(positive? 5)       ; => true
(negative? -5)      ; => true
(even? 4)           ; => true
(odd? 3)            ; => true
```

### 字符串操作

```lisp
(string-append "Hello" " " "World")  ; => "Hello World"
(str "Hello" " " "World")            ; => "Hello World"（同 string-append）
(str 42)                             ; => "42"
(string=? "a" "a")                   ; => true 字符串相等
```

---

## 函数定义与应用

### 高阶函数

```lisp
(map (lambda (x) (* x x)) [1 2 3 4])
; => (1 4 9 16)

(filter (lambda (x) (even? x)) [1 2 3 4 5])
; => (2 4)

(reduce + 0 [1 2 3 4 5])
; => 15
```

### apply - 应用函数

```lisp
(define nums '(1 2 3 4 5))
(apply + nums)         ; => 15
(apply * nums)         ; => 120
```

### 可变参数

```lisp
(define (list-all . args)
  args)

(list-all 1 2 3 4)     ; => (1 2 3 4)
```

### 命名参数与默认值

```lisp
(define (create-person &key name age)
  (list name age))

(create-person :name "张三" :age 25)
; => ("张三" 25)

(define (connect &key (host "localhost") (port 8080))
  (list host port))

(connect)
; => ("localhost" 8080)

(connect :port 9000)
; => ("localhost" 9000)
```

---

## 作用域与绑定

### let - 局部绑定

```lisp
(let ((x 10)
      (y 20))
  (+ x y))
; => 30
```

### let* - 顺序绑定

```lisp
(let* ((x 10)
       (y (* x 2))
       (z (+ y 5)))
  z)
; => 25 (x=10, y=20, z=25)
```

---

## 传统语法兼容

Xisp 提供部分传统 Lisp 语法兼容：

| 现代语法 | 传统语法 | 说明 |
|---------|---------|------|
| `first` | `car` | 首元素 |
| `rest` | `cdr` | 剩余部分 |
| `second` | `cadr` | 第 2 个元素 |
| `third` | `caddr` | 第 3 个元素 |
| `fourth` | `cadddr` | 第 4 个元素 |
| `prepend` | `cons` | 前缀添加 |
| `begin` | `progn` | 顺序执行 |
| `set!` | `setq` | 赋值 |

```lisp
; Common Lisp 风格
(defun square (x)
  (* x x))

(square 5)    ; => 25
```

::: warning 已知不支持
- 字符类型（如 `\a`），请用字符串（如 `"a"`）
- `cond`（用嵌套 `if` 或 `match` 替代）
- `member` 等部分 Common Lisp 函数
:::

---

## 下一步

- [现代语法](03-modern) - 字面量、解构、管道、模式匹配
- [宏系统](04-macros) - 元编程和代码生成
- [API 参考](../api/) - 全部内置函数
