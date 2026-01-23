# 宏系统

Xisp 提供了强大的宏系统，支持元编程和代码生成。宏允许你扩展语言的语法，创建新的语言特性。

## 什么是宏？

宏是一种编译时代码转换机制。与函数不同，宏在代码执行之前进行转换，可以操作和生成代码结构。

### 宏 vs 函数

| 特性 | 函数 | 宏 |
|------|------|-----|
| 参数求值 | 立即求值 | 不求值（保留为符号） |
| 执行时机 | 运行时 | 编译时（展开时） |
| 返回值 | 计算结果 | 代码结构 |
| 用途 | 处理数据 | 生成代码 |

---

## defmacro - 定义宏

### 语法

```lisp
(defmacro 名称 (参数...)
  宏体)
```

### 示例

```lisp
; 定义一个简单的 when 宏
(defmacro when (test then)
  `(if ,test ,then nil))

; 使用
(when (> x 10)
  (println "x is large"))
```

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
```

### 逗号-at `,@`

逗号-at 将列表拼接（splice）到外层列表中。

```lisp
(define lst '(b c))

; 不使用 ,@ - 嵌套列表
`(a ,lst d)     ; => (a (b c) d)

; 使用 ,@ - 拼接列表
`(a ,@lst d)    ; => (a b c d)
```

---

## 宏展开过程

### 展开流程

```
代码 → 宏调用 → 宏展开 → 展开后的代码 → 求值
```

### macroexpand - 单层展开

```lisp
; 定义宏
(defmacro when (test then)
  `(if ,test ,then nil))

; 查看单层展开
(macroexpand '(when (> x 10) (println "large")))
; => (if (> x 10) (println "large") nil)
```

### macroexpand-all - 完全展开

```lisp
; 递归展开所有嵌套宏
(macroexpand-all '(when (> x 10) (println "large")))
; 如果有嵌套宏，会全部展开
```

---

## 内置宏

Xisp 提供了多个常用内置宏，它们在启动时自动注册到环境中。

### when - 条件执行

```lisp
(when 条件 表达式)
```

条件为真时执行表达式并返回其值；否则返回 `nil`（或 0）。

```lisp
(when (> x 10)
  (println "x is large"))
```

**提示**：如需执行多个表达式，请使用 `begin` 包裹：
```lisp
(when (> x 10)
  (begin
    (println "x is large")
    (setq x (+ x 1))))
```

### unless - 反向条件

```lisp
(unless 条件 表达式)
```

条件为假时执行表达式。

```lisp
(unless (< x 5)
  (println "x is not small"))
```

**提示**：如需执行多个表达式，请使用 `begin` 包裹：
```lisp
(unless (< x 5)
  (begin
    (println "x is not small")
    (println "x is >= 5")))
```

### incf - 自增

```lisp
(incf 变量)
```

变量的值增加 1。

```lisp
(define counter 0)
(incf counter)        ; counter => 1
```

**提示**：如需指定增量，可定义 `incf-by` 宏：
```lisp
(defmacro incf-by (var delta)
  `(setq ,var (+ ,var ,delta)))
(incf-by counter 5)   ; counter => 6
```

### decf - 自减

```lisp
(decf 变量)
```

变量的值减少 1。

```lisp
(define counter 10)
(decf counter)        ; counter => 9
```

**提示**：如需指定减量，可定义 `decf-by` 宏：
```lisp
(defmacro decf-by (var delta)
  `(setq ,var (+ ,var ,delta)))
(decf-by counter 3)   ; counter => 6
```

### swap - 交换变量

```lisp
(swap 变量1 变量2)
```

交换两个变量的值。

```lisp
(define x 1)
(define y 2)
(swap x y)
; x => 2, y => 1
```

### push - 头部插入

```lisp
(push 元素 列表)
```

在列表头部插入元素。

```lisp
(define lst '(2 3))
(push 1 lst)
; lst => (1 2 3)
```

### pop - 头部移除

```lisp
(pop 列表)
```

移除并返回列表头部元素。

```lisp
(define lst '(1 2 3))
(pop lst)
; => (2 3)
```

### negate - 数值取反

```lisp
(negate 数值)
```

返回数值的相反数。

```lisp
(negate 5)     ; => -5
(negate -3)    ; => 3
```

### let* - 顺序绑定

```lisp
(let* ((变量1 值1) (变量2 值2) ...) 表达式)
```

顺序绑定变量，后面的绑定可以使用前面的变量。与 `let` 不同，`let*` 按顺序创建作用域。

```lisp
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

; 单个绑定（退化为普通 let）
(let* ((x 5))
  (* x x))
; => 25
```

**提示**：如需执行多个表达式，请使用 `begin` 包裹：
```lisp
(let* ((a 1) (b 2))
  (begin
    (println a)
    (println b)
    (+ a b)))
```

### if-let - 条件绑定

```lisp
(if-let (变量 值) then表达式 else表达式)
```

如果绑定成功且值为真，执行 then 分支；否则执行 else 分支。

```lisp
; 条件为真时执行 then
(if-let (x 5) x nil)
; => 5

; 条件为假时执行 else
(if-let (x 0) x 100)
; => 100

; 可以使用表达式
(if-let (x (+ 2 3)) (* x x) nil)
; => 25
```

### when-let* - 条件+顺序绑定

```lisp
(when-let* ((变量1 值1) (变量2 值2) ...) 表达式)
```

按顺序绑定变量，如果最后一个绑定值为真，执行表达式；否则返回 `nil`。

```lisp
; 最后一个值为真时执行
(when-let* ((x 5) (y (* x 2))) (+ x y))
; => 15 (x=5, y=10, y 为真)

; 最后一个值为假时返回 nil
(when-let* ((x 5) (y 0)) (+ x y))
; => nil

; 多个绑定
(when-let* ((a 10)
            (b (* a 2))
            (c (+ b 5)))
  c)
; => 25

; 没有绑定时返回 nil
(when-let* () 42)
; => nil
```

**提示**：`when-let*` 结合了 `let*` 的顺序绑定和条件判断，适合需要链式处理并判断的场景：
```lisp
; 链式处理，每一步都依赖上一步
(when-let* ((data (get-data))
            (parsed (parse data))
            (result (process parsed)))
  (handle result))
```

### condb - 增强的条件表达式

```lisp
(condb (:let 变量 值) ... 条件 结果 ... else 默认值)
```

增强版的 `cond`，支持在条件分支前绑定变量。使用 `:let` 关键字声明绑定，后面的条件可以使用这些变量。

**语法**：
```lisp
(condb
  (:let 变量1 值1)
  (:let 变量2 值2)
  条件1 结果1
  条件2 结果2
  ...
  else 默认值)
```

**基础用法**：
```lisp
; 单个绑定
(condb
  (:let x 5)
  (> x 3) "large"
  else "small")
; => "large"

; 多个绑定（后面的绑定可以使用前面的）
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
```

**高级用法**：
```lisp
; 没有绑定时的行为（退化为普通 cond）
(condb
  (= 1 1) "true"
  else "false")
; => "true"

; 复杂条件计算
(condb
  (:let base 100)
  (:let rate 0.05)
  (:let years 3)
  (> years 5) "long term"
  (= years 3) "medium term"
  else "short term")
; => "medium term"

; 链式数据处理
(condb
  (:let data (get-user-input))
  (:let validated (validate data))
  (validated.success) validated.result
  else "validation failed")
```

**特点**：
- ✅ **变量绑定**：使用 `:let` 在条件判断前绑定变量
- ✅ **顺序绑定**：后面的绑定可以使用前面的变量（类似 `let*`）
- ✅ **多分支**：支持多个条件分支和 `else` 默认分支
- ✅ **作用域隔离**：所有绑定在独立作用域中，不影响外部环境
- ✅ **向后兼容**：没有绑定时退化为普通 `cond` 行为

**提示**：`condb` 适合需要临时计算值并在多个条件中使用的场景：
```lisp
; 场景：根据用户角色和权限判断操作
(condb
  (:let user (current-user))
  (:let role (user.role))
  (:let permissions (get-permissions role))
  (and (has-permission? permissions "admin") (action.needs-admin))
    "允许管理员操作"
  (has-permission? permissions "user")
    "允许普通用户操作"
  else
    "权限不足")
```

---

## 高级宏示例

### cond - 多分支条件（演示用）

```lisp
(defmacro cond (clauses)
  `(if (null? ,clauses)
       nil
       (let ((first (car ,clauses))
             (rest (cdr ,clauses)))
         (if (eq? (car first) 'else)
             (begin (cdr first))
           `(if ,(car first)
                (begin ,(cdr first))
                (cond ,rest))))))

; 注意：这是一个简化演示，完整实现需要更复杂的展开逻辑
```

**注意**：`cond` 宏的完整实现涉及复杂的递归展开，在当前版本中建议使用嵌套的 `if` 代替。

### 管道操作符（简化版）

```lisp
; 两参数管道
(defmacro pipe2 (x f)
  `(,f ,x))

; 使用
(pipe2 3 square)
; => (square 3)
```

**注意**：多参数管道操作符需要 rest 参数支持（`&`），当前版本尚未实现。建议使用嵌套函数调用或 `->` 线程优先宏。

---

## 宏的最佳实践

### 1. 使用反引号语法

```lisp
; 不推荐 - 手工构造
(defmacro bad (x)
  (list 'quote (list '+ x 1)))

; 推荐 - 使用反引号
(defmacro good (x)
  `'(,+ x 1))
```

### 2. 避免变量捕获

```lisp
; 危险 - 可能捕获变量
(defmacro dangerous (var)
  `(let ((temp ,var))
     (setq var (+ temp 1))))

; 更安全 - 使用 gensym（未实现）
; 或使用唯一的符号名
```

### 3. 提供展开测试

```lisp
; 定义宏后，测试展开
(macroexpand '(when (> x 10) (println "large")))
; 确保展开结果正确
```

### 4. 文档化你的宏

```lisp
; 在注释中说明宏的用途和示例
;; incf - 自增变量（固定增量 1）
;; (incf counter)     ; 增加 1
(defmacro incf (place)
  `(setq ,place (+ ,place 1)))

;; incf-by - 自增变量（指定增量）
;; (incf-by counter 5)   ; 增加 5
(defmacro incf-by (place delta)
  `(setq ,place (+ ,place ,delta)))
```

**注意**：当前版本不支持可选参数（`&optional`），需要定义多个宏来处理不同参数数量。

---

## 宏实现原理

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
; 代码
(when (> x 10) (println "large"))

; 展开过程
(when (> x 10) (println "large"))
  ↓ 识别为宏
(if (> x 10) (println "large") nil)
  ↓ 求值
[执行 if 表达式]
```

---

## 限制和注意事项

### 1. 单独使用逗号

在反引号外使用逗号会返回错误：

```lisp
(define x 1)
,x                    ; => Error: comma used outside of backquote
```

### 2. 宏是编译时

宏不能访问运行时值：

```lisp
(defmacro bad (limit)
  `(when (> counter ,limit)     ; limit 必须是编译时常量
     (println "over limit")))

; 正确做法
(defmacro good (limit)
  `(lambda (counter)
     (when (> counter ,limit)
       (println "over limit"))))
```

### 3. 宏展开次数

每次宏调用都会展开，复杂的嵌套宏可能影响性能。

---

## 相关资源

- **示例程序**：
  - `examples/macro_simple.lisp` - 简单宏演示
  - `examples/macros.lisp` - 完整宏系统演示
  - `examples/macro_test.lisp` - 宏功能测试
  - `examples/advanced_macros.lisp` - 高级宏特性演示（let*, if-let, when-let*, condb）
- **实现代码**：
  - `src/core/eval_macro.cj` - 宏展开特殊形式
  - `src/core/eval_special_forms.cj` - 特殊形式求值（包含 let*, if-let, when-let*, condb）
  - `src/core/builtin_macros.cj` - 内置宏定义
  - `src/parser/lexer.cj` - 反引号词法分析
  - `src/parser/parser.cj` - 反引号语法解析
- **设计文档**：`docs/design.md`
- **核心功能**：`docs/core.md`
- **测试代码**：
  - `src/evaluator_test.cj` - 包含 AdvancedMacroTest 和 MacroTest
  - `src/letstar_test.cj` - let* 和 when-let* 的详细测试
