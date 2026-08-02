# 宏系统

**阅读时间**: 50 分钟

宏是 Lisp 最强大的特性之一，允许你扩展语言语法、创建 DSL、消除代码重复。

---

## 宏基础

### 什么是宏？

宏是一种编译时代码转换机制。与函数不同，宏在代码执行之前进行转换，可以操作和生成代码结构。

| 特性 | 函数 | 宏 |
|------|------|-----|
| 参数求值 | 立即求值 | 不求值（保留为符号） |
| 执行时机 | 运行时 | 编译时（展开时） |
| 返回值 | 计算结果 | 代码结构 |
| 用途 | 处理数据 | 生成代码 |

### defmacro - 定义宏

```lisp
(defmacro 名称 (参数...)
  宏体)
```

### 简单示例

```lisp
; 定义 when 宏
(defmacro when (test then)
  `(if ,test ,then nil))

; 使用
(define x 15)
(when (> x 10)
  (println "x is large"))
; 输出: "x is large"

; 展开为：
; (if (> x 10) (println "x is large") nil)
```

### 带多个参数

```lisp
; 交换两个变量的值
(defmacro swap (x y)
  `(let ((temp ,x))
     (set! ,x ,y)
     (set! ,y temp)))

(define a 1)
(define b 2)
(swap a b)
; a => 2, b => 1
```

::: tip 逗号不能少
`set!` 的参数前必须加逗号 `,`，否则宏展开时无法正确替换变量名。
:::

---

## 反引号语法

反引号（backquote）是宏系统的核心，提供了简洁的模板语法。

### 反引号 `` ` ``

```lisp
'(a b c)        ; => (a b c)
`(a b c)        ; => (a b c)  效果相同
```

### 逗号 `,`

逗号取消引用，在反引号内对表达式求值。

```lisp
(define x 2)
(define y 3)

`(a x c)        ; => (a x c)   未求值
`(a ,x c)       ; => (a 2 c)   求值 x
`(a ,x ,y)      ; => (a 2 3)
```

### 逗号-at `,@`

逗号-at 将列表拼接（splice）到外层列表中。

```lisp
(define lst '(b c))

`(a ,lst d)     ; => (a (b c) d)    嵌套
`(a ,@lst d)    ; => (a b c d)      拼接
```

### 综合示例

```lisp
; 构造函数定义
(defmacro create-function (name params . body)
  `(define (,name ,@params)
     ,@body))

(create-function square (x) (* x x))
; 展开为：(define (square x) (* x x))
(square 5)    ; => 25
```

---

## 宏展开

### macroexpand - 单层展开

```lisp
(defmacro when (test then)
  `(if ,test ,then nil))

(macroexpand '(when (> x 10) (println "large")))
; => (if (> x 10) (println "large") nil)
```

### macroexpand-all - 完全展开

```lisp
; 递归展开所有嵌套宏
(macroexpand-all expr)
```

`macroexpand` 展开最外层宏一次，`macroexpand-all` 递归展开到没有宏为止。

---

## 内置宏

Xisp 在启动时自动注册以下内置宏。

### when - 条件执行

```lisp
; 条件为真时执行表达式并返回最后一个值；否则返回 nil
(when (> x 10)
  (println "x is large")
  (set! x (+ x 1)))
```

### unless - 反向条件

```lisp
; 条件为假时执行表达式
(unless (< x 5)
  (println "x is not small"))
```

### incf / decf - 自增自减

```lisp
(define counter 0)
(incf counter)        ; counter => 1
(decf counter)        ; counter => 0
```

### swap - 交换变量

```lisp
(define x 1)
(define y 2)
(swap x y)
; x => 2, y => 1
```

### negate - 数值取反

```lisp
(negate 5)     ; => -5
(negate -3)    ; => 3
```

### push / pop - 栈操作

```lisp
(define lst '(2 3))
(push 1 lst)
; 返回 (1 2 3)，lst 本身不变（不可变数据结构）

(define new-lst (push 1 lst))
; new-lst => (1 2 3)

(pop lst)
; => (3)   返回去除首元素后的列表
```

### if-let - 条件绑定

```lisp
(if-let (x 5) x nil)       ; => 5
(if-let (x 0) x 100)       ; => 100  绑定值 0 为假
(if-let (x (+ 2 3)) (* x x) nil)  ; => 25
```

### when-let* - 条件+顺序绑定

```lisp
(when-let* ((x 5) (y (* x 2))) (+ x y))
; => 15 (x=5, y=10, y 为真)

(when-let* ((x 5) (y 0)) (+ x y))
; => nil (y 为假)
```

### condb - 增强的条件表达式

```lisp
(condb (:let x 5)
       (> x 3) "large"
       else "small")
; => "large"

; 多个绑定
(condb (:let x 5)
       (:let y (* x 2))
       (> y 8) "large"
       else "small")
; => "large" (y = 10)
```

---

## 自定义控制结构

### while 循环

```lisp
(defmacro while (condition . body)
  `(if ,condition
       (begin ,@body (while ,condition ,@body))
       nil))

(define i 0)
(while (< i 5)
  (println i)
  (set! i (+ i 1)))
; 输出: 0 1 2 3 4
```

### dotimes

```lisp
(defmacro dotimes (var n . body)
  `(let ((counter 0)
         (limit ,n))
     (define (loop)
       (if (< counter limit)
           (begin
             (let ((,var counter))
               ,@body)
             (set! counter (+ counter 1))
             (loop))
           nil))
     (loop)))

(dotimes i 5
  (println i))
; 输出: 0 1 2 3 4
```

---

## 最佳实践

### 1. 使用反引号语法

```lisp
; ❌ 不推荐 - 手工构造
(defmacro bad (x)
  (list 'quote (list '+ x 1)))

; ✅ 推荐 - 使用反引号（逗号作用在参数上）
(defmacro good (x)
  `'(+ ,x 1))

(bad 5)    ; => (+ 5 1)
(good 5)   ; => (+ 5 1)
```

### 2. 避免变量捕获

```lisp
; ⚠️ 危险 - 可能捕获外部 temp
(defmacro dangerous (var)
  `(let ((temp ,var))
     (set! var (+ temp 1))))

; ✅ 更安全 - 使用唯一的符号名
(defmacro safer (var)
  `(let ((__temp__ ,var))
     (set! ,var (+ __temp__ 1))))
```

### 3. 优先使用函数

```lisp
; ✅ 如果函数能做到，使用函数
(define (square x) (* x x))

; ⚠️ 只在需要时使用宏
(defmacro unless (condition . body)
  `(if (not ,condition)
      (begin ,@body)
      nil))
```

---

## 实现原理

### 语法转换

Reader（词法分析器）将特殊语法转换为 S-表达式：

```lisp
`expr         → (backquote expr)
,expr         → (comma expr)
,@expr        → (comma-at expr)
```

### 求值过程

1. **识别宏**：求值器检查列表首元素是否为宏
2. **展开宏**：以未求值的形式调用宏
3. **递归求值**：对展开后的表达式重新求值

---

## 下一步

- [模块系统](05-modules) - 代码组织与命名空间
- [API 参考](../api/) - 全部内置函数
