# Xisp 宏系统

**版本**: 1.0.0
**目标读者**: 需要元编程的开发者
**前置知识**: [基础语法](01-basics.md)、[现代语法特性](02-modern.md)
**阅读时间**: 50 分钟

本文档介绍 Xisp 的宏系统，宏是 Lisp 最强大的特性之一，允许你扩展语言语法、创建 DSL、消除代码重复。

---

## 目录

1. [宏基础](#宏基础)
2. [反引号语法](#反引号语法)
3. [宏展开](#宏展开)
4. [内置宏](#内置宏)
5. [高级宏示例](#高级宏示例)
6. [最佳实践](#最佳实践)
7. [实现原理](#实现原理)

---

## 宏基础

### 什么是宏？

宏是一种编译时代码转换机制。与函数不同，宏在代码执行之前进行转换，可以操作和生成代码结构。

### 宏 vs 函数

| 特性 | 函数 | 宏 |
|------|------|-----|
| 参数求值 | 立即求值 | 不求值（保留为符号） |
| 执行时机 | 运行时 | 编译时（展开时） |
| 返回值 | 计算结果 | 代码结构 |
| 用途 | 处理数据 | 生成代码 |

### 示例对比

```lisp
; ===== 函数 =====

(define (square x)
  (* x x))

(square 5)
; 调用时：
; 1. 求值参数：5
; 2. 执行函数体：(* 5 5)
; 3. 返回结果：25

; ===== 宏 =====

(defmacro when (test then)
  `(if ,test ,then nil))

(define x 15)
(when (> x 10) x)
; 调用时：
; 1. 不求值参数，保留为符号
; 2. 展开宏：(if (> x 10) x nil)
; 3. 对展开后的代码求值 => 15
```

### defmacro - 定义宏

#### 基础语法

```lisp
(defmacro 名称 (参数...)
  宏体)
```

#### 简单示例

```lisp
; 定义一个 when 宏
(defmacro when (test then)
  `(if ,test ,then nil))

; 使用示例 1：条件为真
(define x 15)
(when (> x 10)
  (println "x is large"))
; 输出: "x is large"
; 返回: nil（println 返回 nil）

; 使用示例 2：返回值
(define result (when (> x 10) x))
; result => 15

; 展开为：
; (if (> x 10) (println "x is large") nil)
```

**说明**：`println` 返回 `nil`，所以 `when` 也返回 `nil`。如果需要返回值，可以使用其他表达式。

#### 带多个参数

```lisp
; 定义一个宏，交换两个变量的值
(defmacro swap (x y)
  `(let ((temp ,x))
     (set! ,x ,y)
     (set! ,y temp)))    ; 注意：,y 逗号不能少！

; 使用
(define a 1)
(define b 2)
(swap a b)
; a => 2, b => 1
```

**重要提示**：`set!` 的参数前必须加逗号 `,`，否则宏展开时无法正确替换变量名。

---

## 反引号语法

反引号（backquote）是宏系统的核心，提供了简洁的模板语法。

### 反引号 `` ` ``

反引号创建一个引用的模板，类似 `quote`，但允许内部使用逗号取消引用。

```lisp
; 普通引用
'(a b c)        ; => (a b c)

; 反引号（效果相同）
`(a b c)        ; => (a b c)
```

### 逗号 `,`

逗号取消引用，在反引号内对表达式求值。

```lisp
(define x 2)
(define y 3)

; 不使用逗号 - 返回符号 x
`(a x c)        ; => (a x c)

; 使用逗号 - 求值 x
`(a ,x c)       ; => (a 2 c)

; 多个逗号
`(a ,x ,y)      ; => (a 2 3)

; 在宏中使用
(defmacro inc (var)
  `(set! ,var (+ ,var 1)))

(define counter 0)
(inc counter)
; counter => 1
```

### 逗号-at `,@`

逗号-at 将列表拼接（splice）到外层列表中。

```lisp
(define lst '(b c))

; 不使用 ,@ - 嵌套列表
`(a ,lst d)     ; => (a (b c) d)

; 使用 ,@ - 拼接列表
`(a ,@lst d)    ; => (a b c d)

; 实际应用：在宏中使用预定义的列表
(defmacro print-first-two (x y)
  `(begin
     (println ,x)
     (println ,y)))

(print-first-two "Hello" "World")
; 输出:
; Hello
; World
```

**注意**：在宏展开时，只有基本的求值功能可用。复杂的高阶函数如 `map` 可能在宏展开时不可用。如果需要处理可变数量的表达式，建议使用递归或模板模式。

### 综合示例

```lisp
; 构造函数定义
(defmacro create-function (name params . body)
  `(define (,name ,@params)
     ,@body))

(create-function square (x) (* x x))
; 展开为：
; (define (square x) (* x x))

; 测试
(square 5)
; => 25

; 条件构建：多表达式 unless
(defmacro my-unless (condition . body)
  `(if (not ,condition)
      (begin ,@body)
      nil))

(my-unless (< x 5)
  (println "x is not small")
  (println "x is >= 5"))
; 当 x >= 5 时输出两行

; 注意：Xisp 也提供了内置的 unless 宏
(unless (< x 5) (println "x is not small"))
```

---

## 宏展开

### macroexpand - 单层展开

```lisp
; 定义宏
(defmacro when (test then)
  `(if ,test ,then nil))

; 方法1: 先定义表达式，再展开
(define expr '(when (> x 10) (println "large")))
(macroexpand expr)
; => (if (> x 10) (println "large") nil)

; 方法2: 直接展开（在脚本模式中）
(macroexpand '(when (> x 10) x))
; => (if (> x 10) x nil)

; 另一个示例
(defmacro inc (var)
  `(set! ,var (+ ,var 1)))

(define expr2 '(inc counter))
(macroexpand expr2)
; => (set! counter (+ counter 1))
```

**注意**：
- `macroexpand` 需要接收一个未求值的表达式
- 如果直接使用 quote 语法，在某些情况下可能需要先定义变量
- 展开结果依赖于宏的定义和当前环境

### macroexpand-all - 完全展开

```lisp
; 递归展开所有嵌套宏
(defmacro outer (x)
  `(when (> ,x 0) (println "positive")))

(define expr3 '(outer 5))
(macroexpand expr3)
; => (when (> 5 0) (println "positive"))

(macroexpand-all expr3)
; => (if (> 5 0) (begin (println "positive")) nil)
```

**说明**：
- `macroexpand` 展开最外层的宏一次
- `macroexpand-all` 递归展开所有嵌套的宏，直到没有宏为止

### 展开过程

```lisp
; 先定义变量
(define x 15)

; 代码
(when (> x 10) (println "large"))
; => large

; 步骤1：识别为宏
; 步骤2：展开宏
; (if (> x 10) (println "large") nil)

; 步骤3：对展开后的代码求值
; [执行 if 表达式，打印 "large"]
```

---

## 内置宏

Xisp 提供了多个常用内置宏，它们在启动时自动注册到环境中。

### when - 条件执行

```lisp
; 语法
(when 条件 表达式)

; 示例：先定义变量
(define x 15)

; 条件为真时执行表达式并返回其值；否则返回 nil
(when (> x 10)
  (println "x is large"))
; => x is large

; 执行多个表达式
(when (> x 10)
  (begin
    (println "x is large")
    (set! x (+ x 1))))
```

### unless - 反向条件

```lisp
; 语法
(unless 条件 表达式)

; 示例：假设 x = 15
(define x 15)

; 条件为假时执行表达式
(unless (< x 5)
  (println "x is not small"))
; => x is not small

; 执行多个表达式
(unless (< x 5)
  (begin
    (println "x is not small")
    (println "x is >= 5")))
```

### incf / decf - 自增自减

```lisp
; 语法
(incf 变量)
(decf 变量)

; 使用
(define counter 0)
(incf counter)        ; counter => 1

(decf counter)        ; counter => 0
```

### swap - 交换变量

```lisp
; 语法
(swap 变量1 变量2)

; 使用
(define x 1)
(define y 2)
(swap x y)
; x => 2, y => 1
```

### push / pop - 栈操作

```lisp
; 语法
(push 元素 列表)
(pop 列表)

; 使用
(define lst '(2 3))
(push 1 lst)
; lst => (1 2 3)

(pop lst)
; => (2 3)
```

### negate - 数值取反

```lisp
; 语法
(negate 数值)

; 使用
(negate 5)     ; => -5
(negate -3)    ; => 3
```

### let* - 顺序绑定

```lisp
; 语法
(let* ((变量1 值1) (变量2 值2) ...) 表达式)

; 后面的绑定可以使用前面的变量
(let* ((a 1)
       (b (+ a 10))
       (c (* b 2)))
  c)
; => 22 (a=1, b=11, c=22)

; 没有绑定时直接执行 body
(let* ()
  (+ 1 2))
; => 3

; 与 let 的区别
(let ((a 1) (b a)) b)
; => Error: a 未定义

(let* ((a 1) (b a)) b)
; => 1
```

### if-let - 条件绑定

```lisp
; 语法
(if-let (变量 值) then表达式 else表达式)

; 如果绑定成功且值为真，执行 then 分支；否则执行 else 分支
(if-let (x 5) x nil)
; => 5

(if-let (x 0) x 100)
; => 100

; 可以使用表达式
(if-let (x (+ 2 3)) (* x x) nil)
; => 25
```

### when-let* - 条件+顺序绑定

```lisp
; 语法
(when-let* ((变量1 值1) (变量2 值2) ...) 表达式)

; 按顺序绑定变量，如果最后一个绑定值为真，执行表达式；否则返回 nil
(when-let* ((x 5) (y (* x 2))) (+ x y))
; => 15 (x=5, y=10, y 为真)

(when-let* ((x 5) (y 0)) (+ x y))
; => nil

; 链式处理
(when-let* ((data (get-data))
            (parsed (parse data))
            (result (process parsed)))
  (handle result))
```

### condb - 增强的条件表达式

```lisp
; 语法
(condb (:let 变量 值) ... 条件 结果 ... else 默认值)

; 增强版的 cond，支持在条件分支前绑定变量
(condb
  (:let x 5)
  (> x 3) "large"
  else "small")
; => "large"

; 多个绑定
(condb
  (:let x 5)
  (:let y (* x 2))
  (> y 8) "large"
  else "small")
; => "large" (y = 10, 10 > 8)

; 多个条件分支
(condb
  (:let x 10)
  (= x 5) "five"
  (= x 10) "ten"
  else "other")
; => "ten"

; 链式数据处理
(condb
  (:let data (get-user-input))
  (:let validated (validate data))
  (validated.success) validated.result
  else "validation failed")
```

---

## 高级宏示例

### 自定义控制结构

Xisp 现在完全支持 rest parameters（`. rest` 和 `&rest rest` 语法），可以编写更灵活的宏。

```lisp
; 多表达式 while 循环
(defmacro while (condition . body)
  `(if ,condition
       (begin ,@body (while ,condition ,@body))
       nil))

; 使用
(define i 0)
(while (< i 5)
  (println i)
  (set! i (+ i 1)))
; 输出: 0 1 2 3 4

; 另一个版本：使用 loop
(defmacro while2 (condition . body)
  `(let ((loop (lambda ()
                 (if ,condition
                     (begin ,@body (loop))
                     nil))))
     (loop)))

(while2 (< i 10)
  (println i)
  (set! i (+ i 1)))
```

### 实现 dotimes

使用可变参数支持多个表达式：

```lisp
; 重复执行 n 次（支持多表达式）
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

; 使用
(dotimes i 5
  (println i)
  (println "Current:" i))
; 打印:
; 0 Current: 0
; 1 Current: 1
; 2 Current: 2
; 3 Current: 3
; 4 Current: 4

; 单个表达式
(dotimes i 3
  (println i))
; 打印: 0 1 2
```

### 实现 cond

**注意**：这是一个教学示例，展示了如何实现简单的条件宏。实际使用时，建议使用 Xisp 内置的 `match` 或 `if-let` 宏。

```lisp
; 简化版 cond（仅用于演示，实际使用 match 更好）
; 这个版本接收一个包含所有条件的列表
(defmacro my-cond (clauses)
  `(if (null? ,clauses)
       nil
       (let ((first (first ,clauses))
             (rest (rest ,clauses)))
         (if (eq? (first first) 'else)
             (begin (second first))
           `(if ,(first first)
                (begin ,(second first))
                (my-cond ,rest))))))

; 使用（注意：需要将所有条件放在一个列表中）
(define x 15)
; 由于这个宏的局限性，实际使用建议使用 Xisp 的 match
; 例如：(match x ((> 10) "large") ((< 5) "small") (else "medium"))
```

### 断言宏

```lisp
; 定义断言
(defmacro assert (condition message)
  `(if (not ,condition)
      (error #"Assertion failed: #{,message}")
      #t))

; 使用
(define x 5)
(assert (> x 0) "x must be positive")
; => #t (断言通过)
```

### 属性访问宏

**注意**：在宏展开时使用 `map` 等高阶函数不可用。如果需要实现类似功能，可以使用递归或限制参数数量：

```lisp
; 简化版：只支持两个属性
(defmacro ->> (obj slot1 slot2)
  `(let ((o ,obj))
     (get-slot o ',slot1)
     (get-slot o ',slot2)))

; 使用（需要先实现 get-slot 函数）
; (->> user name age)
```

或者使用链式调用：
```lisp
; 使用内置的 -> (thread-first) 宏
(-> user
    (get-slot 'name)
    (get-slot 'age))
```

---

## 最佳实践

### 1. 使用反引号语法

```lisp
; ❌ 不推荐 - 手工构造
(defmacro bad (x)
  (list 'quote (list '+ x 1)))

; ✅ 推荐 - 使用反引号
(defmacro good (x)
  `'(,+ x 1))
```

### 2. 避免变量捕获

```lisp
; ⚠️ 危险 - 可能捕获变量
(defmacro dangerous (var)
  `(let ((temp ,var))
     (set! var (+ temp 1))))

; ✅ 更安全 - 使用唯一的符号名
(defmacro safer (var)
  `(let ((__temp__ ,var))
     (set! ,var (+ __temp__ 1))))
```

### 3. 提供展开测试

```lisp
; 定义宏后，测试展开
(defmacro when (test then)
  `(if ,test ,then nil))

; 测试展开
(macroexpand '(when (> x 10) (println "large")))
; => (if (> x 10) (println "large") nil)

; 确保展开结果正确
```

### 4. 文档化你的宏

```lisp
; 在注释中说明宏的用途和示例

;; incf - 自增变量
;; 用法：(incf counter)
;; 效果：counter 的值增加 1
(defmacro incf (var)
  `(set! ,var (+ ,var 1)))

;; incf-by - 自增变量（指定增量）
;; 用法：(incf-by counter 5)
;; 效果：counter 的值增加 5
(defmacro incf-by (var delta)
  `(set! ,var (+ ,var ,delta)))
```

### 5. 保持宏简单

```lisp
; ✅ 好的宏 - 单一职责
(defmacro when (test then)
  `(if ,test ,then nil))

; ❌ 复杂的宏 - 做太多事情
(defmacro complex-macro (x y z)
  ; ... 复杂逻辑 ...
)
```

### 6. 优先使用函数

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

```lisp
; 完整流程
(define x 15)
(when (> x 10) (println "large"))
  ↓ 识别为宏
  ↓ 展开宏
(if (> x 10) (println "large") nil)
  ↓ 求值
large
```

### 宏展开时机

```
源代码
   ↓
Reader（词法分析）
   ↓
Parser（语法分析）
   ↓
宏展开（编译时）
   ↓
求值（运行时）
```

---

## 限制和注意事项

### 1. 宏展开时的环境限制

在宏展开时，只有基本的求值功能可用。以下限制需要注意：

**不可用的功能**：
- ❌ 高阶函数（`map`, `filter`, `reduce`）- 这些是特殊形式，在宏展开环境中不可用
- ❌ 用户定义的函数 - 宏展开时还未定义
- ❌ 复杂的嵌套求值

**可用的功能**：
- ✅ 基本运算符（`+`, `-`, `*`, `/`）
- ✅ 比较运算符（`=`, `<`, `>`, `eq?`）
- ✅ `lambda` 创建匿名函数
- ✅ `list`, `cons`, `append` 等列表操作
- ✅ `if`, `begin`, `let` 等特殊形式

**示例对比**：

```lisp
; ❌ 错误：在宏中使用 map
(defmacro bad (. args)
  `(begin
     ,@(map (lambda (arg) `(println ,arg)) args)))
; map 在宏展开时不可用，导致展开失败

; ✅ 正确：使用递归或直接展开
(defmacro good (x y)
  `(begin
     (println ,x)
     (println ,y)))
```

**解决方案**：
- 使用递归宏而不是高阶函数
- 限制参数数量，直接展开
- 在运行时使用 `map`，而不是在宏展开时

### 2. 单独使用逗号

在反引号外使用逗号会返回错误：

```lisp
(define x 1)
,x                    ; => Error: comma used outside of backquote
```

### 2. 宏是编译时

宏不能访问运行时值：

```lisp
; ⚠️ 错误 - limit 必须是编译时常量
(defmacro bad (limit)
  `(when (> counter ,limit)
     (println "over limit")))

; ✅ 正确 - 创建函数
(defmacro good (limit)
  `(lambda (counter)
     (when (> counter ,limit)
       (println "over limit"))))
```

### 3. 宏展开次数

每次宏调用都会展开，复杂的嵌套宏可能影响性能：

```lisp
; 简单宏，展开快
(defmacro inc (var)
  `(set! ,var (+ ,var 1)))

; 复杂宏，考虑性能
(defmacro complex-macro (x)
  ; ... 复杂展开逻辑 ...
)
```

### 4. 调试难度

宏的错误可能难以调试：

```lisp
; 技巧：先查看展开结果
(macroexpand '(complex-macro data))

; 确认展开正确后再使用
```

---

## 练习

### 基础练习

1. **简单宏**：实现 `double` 宏，将值乘以 2
2. **反引号**：使用反引号语法构造代码
3. **内置宏**：使用 `when`、`unless` 等内置宏

### 进阶练习

1. **控制结构**：实现 `while` 或 `dotimes` 宏
2. **条件宏**：实现自定义的条件宏
3. **DSL**：为特定领域创建一个简单的 DSL

### 挑战练习

1. **优化宏**：优化现有宏的性能
2. **调试宏**：创建宏的调试工具
3. **宏库**：创建一个有用的宏库

---

## 下一步

- [示例代码](../../examples/04-macros/) - 查看完整示例
- [实现代码](../../src/core/builtin_macros.cj) - 了解宏的实现
- [设计文档](../design.md) - 阅读设计文档

---

## 相关资源

### 示例程序
- `examples/04-macros/01_macro_simple.lisp` - 简单宏演示
- `examples/04-macros/02_macro_basics.lisp` - 基础宏（反引号语法）
- `examples/04-macros/03_macro_advanced.lisp` - 高级宏特性

### 实现代码
- `src/core/eval_macro.cj` - 宏展开特殊形式
- `src/core/builtin_macros.cj` - 内置宏定义
- `src/parser/lexer.cj` - 反引号词法分析
- `src/parser/parser.cj` - 反引号语法解析

---

**文档版本**: 1.1.0
**最后更新**: 2026-01-27
**维护者**: Xisp Team
