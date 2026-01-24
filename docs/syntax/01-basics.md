# Xisp 基础语法

**版本**: 1.0.0
**目标读者**: Lisp 初学者
**阅读时间**: 30 分钟

本文档介绍 Xisp 的核心语法和基础功能，是学习 Xisp 的起点。

---

## 目录

1. [数据类型](#数据类型)
2. [特殊形式](#特殊形式)
3. [基础函数](#基础函数)
4. [函数定义与应用](#函数定义与应用)
5. [条件判断](#条件判断)
6. [作用域与绑定](#作用域与绑定)
7. [传统语法兼容](#传统语法兼容)

---

## 数据类型

### 原子类型

#### 数字

```lisp
42        ; 整数
3.14      ; 浮点数
-10       ; 负数
```

#### 字符串

```lisp
"hello"           ; 普通字符串
"Hello\nWorld"    ; 支持转义字符
#"Hello #{name}"  ; 字符串插值（详见现代语法文档）
```

#### 布尔值

```lisp
#t        ; 真 (也支持 #true)
#f        ; 假 (也支持 #false)
```

#### 空值

```lisp
nil       ; 空值，也表示空列表
```

#### 符号

```lisp
foo           ; 普通符号
:keyword      ; 关键字（以冒号开头，自求值）
```

**关键字自求值**：`:keyword` 是自求值符号，不需要 quote，直接使用即可。

```lisp
; ✅ 正确：关键字直接使用
:hget         ; => :hget (关键字符号本身)

; ❌ 不需要：使用 quote 反而会创建带引号的符号
':hget        ; => :hget (但这样做是不必要的)
```

### 组合类型

#### 列表

```lisp
'(1 2 3)             ; 列表字面量
(list 1 2 3)         ; 构造列表
'()                  ; 空列表
```

#### 向量（现代语法）

```lisp
[1 2 3]              ; 向量字面量
```

#### 哈希映射（现代语法）

```lisp
{:name "张三" :age 25}     ; 哈希映射
```

---

## 特殊形式

特殊形式是 Xisp 的核心构建块，它们不遵循普通的求值规则。

### define - 定义变量和函数

#### 定义变量

```lisp
(define x 10)
(define name "Alice")
(define pi 3.14159)
```

#### 定义函数

```lisp
; 语法：(define (函数名 参数...) 函数体)
(define (square x)
  (* x x))

; 使用
(square 5)    ; => 25

; 多参数函数
(define (add x y)
  (+ x y))

(add 3 4)     ; => 7

; 多表达式函数体
(define (process x)
  (println "Processing:" x)
  (* x 2))

(process 5)   ; 打印 "Processing: 5"，返回 10
```

### lambda - 匿名函数

```lisp
; 基础语法
(lambda (参数...) 函数体)

; 示例
(define square
  (lambda (x)
    (* x x)))

(square 5)    ; => 25

; 直接使用匿名函数
(map (lambda (x) (* x x)) [1 2 3])
; => (1 4 9)

; 多参数
(define add
  (lambda (x y)
    (+ x y)))

(add 10 20)   ; => 30
```

### if - 条件判断

```lisp
; 语法：(if 条件 then-表达式 else-表达式)

; 基础用法
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

### quote - 引用

```lisp
; 阻止求值，返回字面量
(quote x)           ; => x (符号)
'(1 2 3)            ; => (1 2 3)
'(+ 1 2)            ; => (+ 1 2) (未求值)

; 比较
(list 1 2 3)        ; => (1 2 3) (求值后)
'(1 2 3)            ; => (1 2 3) (字面量)
```

### begin - 顺序执行

```lisp
; 顺序执行多个表达式，返回最后一个的值

(begin
  (println "Step 1")
  (println "Step 2")
  (+ 1 2))
; 打印两行，返回 3

; 在函数体中的使用
(define (multi-step x)
  (begin
    (println "Input:" x)
    (set! x (* x 2))
    (println "Doubled:" x)
    x))
```

### set! - 修改变量

```lisp
(define x 10)
x                       ; => 10

(set! x 20)
x                       ; => 20

; 修改多次
(define counter 0)
(set! counter (+ counter 1))
counter                 ; => 1
```

---

## 基础函数

### 算术运算

```lisp
; 加法
(+ 1 2 3 4)            ; => 10
(+ 10)                 ; => 10

; 减法
(- 10 3)               ; => 7
(- 10 3 2)             ; => 5

; 乘法
(* 2 3 4)              ; => 24
(* 5)                  ; => 5

; 除法
(/ 10 2)               ; => 5
(/ 20 2 2)             ; => 5

; 取模
(mod 10 3)             ; => 1
```

### 比较运算

```lisp
; 等于
(= 1 1)                ; => #t
(= 1 2)                ; => #f
(= 1 1 1)              ; => #t

; 不等于
(!= 1 2)               ; => #t

; 小于
(< 1 2)                ; => #t
(< 1 2 3)              ; => #t

; 小于等于
(<= 1 2)               ; => #t
(<= 2 2)               ; => #t

; 大于
(> 5 3)                ; => #t

; 大于等于
(>= 5 5)               ; => #t
```

### 逻辑运算

```lisp
; 逻辑与
(and #t #t)            ; => #t
(and #t #f)            ; => #f
(and)                  ; => #t

; 逻辑或
(or #t #f)             ; => #t
(or #f #f)             ; => #f
(or)                   ; => #f

; 逻辑非
(not #t)              ; => #f
(not #f)              ; => #t
(not nil)             ; => #t
```

### 列表操作（现代语法）

```lisp
(define lst '(1 2 3 4 5))

; 获取首元素
(first lst)            ; => 1

; 获取剩余部分
(rest lst)             ; => (2 3 4 5)

; 获取第二个元素
(second lst)           ; => 2

; 获取第三个元素
(third lst)            ; => 3

; 前缀添加
(prepend 0 lst)        ; => (0 1 2 3 4 5)

; 构造列表
(list 1 2 3)           ; => (1 2 3)

; 列表长度
(length lst)           ; => 5

; 判断空列表
(null? '())            ; => #t
(null? lst)            ; => #f
```

### 列表构造

```lisp
; cons - 构造点对
(cons 1 '(2 3))        ; => (1 2 3)
(cons 1 2)             ; => (1 . 2)

; append - 拼接列表
(append '(1 2) '(3 4)) ; => (1 2 3 4)
(append '(1) '(2) '(3)) ; => (1 2 3)

; reverse - 反转列表
(reverse '(1 2 3))     ; => (3 2 1)

; range - 生成范围
(range 1 5)            ; => (1 2 3 4)
```

### 谓词函数

```lisp
; 类型检查
(number? 42)           ; => #t
(number? "hello")      ; => #f

(string? "hello")      ; => #t
(string? 42)           ; => #f

(symbol? 'foo)         ; => #t
(symbol? 42)           ; => #f

(list? '(1 2 3))       ; => #t
(list? 42)             ; => #f

(procedure? +)         ; => #t
(procedure? 42)        ; => #f

; 值的检查
(zero? 0)              ; => #t
(zero? 1)              ; => #f

(positive? 5)          ; => #t
(positive? -5)         ; => #f

(negative? -5)         ; => #t
(negative? 5)          ; => #f

(even? 4)              ; => #t
(odd? 3)               ; => #t
```

### 字符串操作

```lisp
; 拼接（string-append 和 str 功能相同，是别名关系）
(string-append "Hello" " " "World")  ; => "Hello World"
(str "Hello" " " "World")            ; => "Hello World"

; 转换为字符串并拼接
(str 42)               ; => "42"
(str "age:" 30)        ; => "age:30"
(str "value:" x " + " y)  ; 支持多个参数

; 打印
(println "Hello")      ; 打印并换行
(print "Hello")        ; 打印不换行
```

---

## 函数定义与应用

### 函数定义

```lisp
; 无参函数
(define (greet)
  (println "Hello, World!"))

(greet)               ; => "Hello, World!"

; 单参数函数
(define (square x)
  (* x x))

; 多参数函数
(define (add-three a b c)
  (+ a b c))

(add-three 1 2 3)      ; => 6

; 计算平均值
(define (average a b c)
  (/ (+ a b c) 3))

(average 10 20 30)     ; => 20.000000
; (define (sum . nums)
;   (apply + nums))
```

### 高阶函数

```lisp
; map - 对每个元素应用函数
(map (lambda (x) (* x x)) [1 2 3 4])
; => (1 4 9 16)

; filter - 过滤元素
(filter (lambda (x) (even? x)) [1 2 3 4 5])
; => (2 4)

; reduce - 归约
(reduce + 0 [1 2 3 4 5])
; => 15

(reduce * 1 [1 2 3 4])
; => 24
```

### apply - 应用函数

```lisp
; 将列表的元素作为参数传递给函数
(define nums '(1 2 3 4 5))
(apply + nums)         ; => 15
(apply * nums)         ; => 120

; 与部分参数结合
(define (add-three a b c)
  (+ a b c))

(apply add-three '(10 20 30))
; => 60
```

### 函数组合

```lisp
; 定义复合函数
(define (compose f g)
  (lambda (x)
    (f (g x))))

; 使用
(define square
  (lambda (x)
    (* x x)))

(define increment
  (lambda (x)
    (+ x 1)))

(define square-then-increment
  (compose increment square))

(square-then-increment 5)
; => 26 (square(5) = 25, increment(25) = 26)
```

---

## 条件判断

### if 表达式

```lisp
; 基础用法
(if (> x 10)
    (println "x is large")
    (println "x is small"))

; 返回值
(define (abs x)
  (if (< x 0)
      (- x)
      x))

(abs -5)              ; => 5
(abs 5)               ; => 5

; 嵌套
(define (sign x)
  (if (> x 0)
      1
      (if (< x 0)
          -1
          0)))

(sign 10)             ; => 1
(sign -5)             ; => -1
(sign 0)              ; => 0
```

### 逻辑组合

```lisp
; and - 所有条件为真
(define (valid-age age)
  (and (number? age)
       (>= age 0)
       (<= age 150)))

(valid-age 25)        ; => #t
(valid-age -5)        ; => #f
(valid-age 200)       ; => #f

; or - 任一条件为真
(define (is-positive-or-negative x)
  (or (positive? x)
      (negative? x)))

(is-positive-or-negative 5)    ; => #t
(is-positive-or-negative 0)    ; => #f

; not - 取反
(define (not-zero x)
  (not (zero? x)))

(not-zero 5)          ; => #t
(not-zero 0)          ; => #f
```

---

## 作用域与绑定

### let - 局部绑定

```lisp
; 基础语法
(let ((x 10)
      (y 20))
  (+ x y))
; => 30

; 多个绑定
(let ((name "Alice")
      (age 25)
      (city "Beijing"))
  (println name)
  (println age)
  (println city))

; 注意：let 中的绑定不能相互引用
; (let ((a 1) (b a) b)  ; => Error: a 未定义

; 局部变量不影响外部
(define x 100)
(let ((x 5))
  (println "Inside:" x))    ; => Inside: 5
(println "Outside:" x))    ; => Outside: 100
```

### let* - 顺序绑定

```lisp
; 后面的绑定可以使用前面的值
(let* ((x 10)
       (y (* x 2))
       (z (+ y 5)))
  z)
; => 25 (x=10, y=20, z=25)

; 没有绑定时直接执行 body
(let* ()
  (+ 1 2))
; => 3

; 与 let 的区别
; let - 所有绑定同时创建，后面的不能引用前面的
(let ((a 1) (b a))
  b)
; => Error: a 未定义

; let* - 按顺序创建绑定，后面的可以引用前面的
(let* ((a 1) (b a))
  b)
; => 1
```

### 全局与局部

```lisp
; 全局变量
(define global-var 100)

(define (test-scope x)
  (define local-var 50)
  (println global-var)    ; => 100 (可访问全局)
  (println local-var)     ; => 50
  (println x)             ; => 参数值
)

(test-scope 5)
; (println local-var)     ; => Error: 局部变量不可访问
```

---

## 传统语法兼容

Xisp 提供部分传统 Lisp 语法的支持，但**不保证完全兼容**所有 Common Lisp/Scheme 代码。

```lisp
; 现代语法（推荐）
(first lst)            ; => 首元素
(rest lst)             ; => 剩余部分
(second lst)           ; => 第二个元素
(prepend 1 lst)        ; => 前缀添加

; 传统语法（等价）
(car lst)              ; => 首元素
(cdr lst)              ; => 剩余部分
(cadr lst)             ; => 第二个元素
(cons 1 lst)           ; => 前缀添加
```

### 两者对比

| 现代语法 | 传统语法 | 说明 |
|---------|---------|------|
| `first` | `car` | 获取首元素 |
| `rest` | `cdr` | 获取剩余部分 |
| `second` | `cadr` | 第2个元素 |
| `third` | `caddr` | 第3个元素 |
| `prepend` | `cons` | 前缀添加 |
| `begin` | `progn` | 顺序执行 |

**注意**：Xisp 使用 `match` 进行模式匹配，不支持传统 `cond`。多分支条件请使用嵌套 `if` 或 `match`（详见现代语法文档）。

### 什么时候使用传统语法？

- **阅读旧代码**：Common Lisp、Scheme 代码使用 car/cdr
- **与其他 Lisp 互通**：复制粘贴其他 Lisp 的代码
- **个人偏好**：如果你更熟悉传统语法

### 推荐做法

```lisp
; 新代码：使用现代语法
(define (sum-list lst)
  (if (null? lst)
      0
      (+ (first lst) (sum-list (rest lst)))))
```

**注意事项**：
- Xisp 不支持字符类型（如 `\a`），请使用字符串（如 `"a"`）
- Xisp 不支持 `cond`，请使用嵌套 `if` 或 `match`
- Common Lisp 代码可能需要修改才能在 Xisp 中运行
- 详见现代语法文档了解 Xisp 的扩展特性

---

## 练习

### 基础练习

1. **定义函数**：编写一个函数 `cube`，计算立方的值
2. **列表操作**：使用 `first` 和 `rest` 遍历列表
3. **条件判断**：编写函数判断是否为闰年
4. **高阶函数**：使用 `map` 和 `filter` 处理列表

### 进阶练习

1. **递归**：使用递归实现 `length` 函数
2. **高阶函数**：实现 `find` 函数，找到满足条件的第一个元素
3. **函数组合**：使用 `compose` 创建复合函数

---

## 下一步

- [现代语法特性](02-modern.md) - 学习向量、解构、管道等现代语法
- [宏系统](03-macros.md) - 掌握元编程和代码生成
- [示例代码](../../examples/) - 查看完整示例

---

**文档版本**: 1.0.0
**最后更新**: 2026-01-24
**维护者**: Xisp Team
