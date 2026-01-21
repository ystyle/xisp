# 管道操作符文档

## 概述

Xisp 支持管道操作符 `->`（thread-first macro），这是一种让代码更易读的现代函数式编程特性。管道操作符将表达式的结果作为第一个参数传递给下一个函数。

## 语法

### 基础语法

```lisp
(-> value form1 form2 ...)
```

将 `value` 作为第一个参数传递给每个 `form`，结果继续传递给下一个。

### 使用示例

#### 1. 基础管道

```lisp
(-> 5 (+ 3))
; 等价于 (+ 5 3)
; => 8
```

#### 2. 多步管道

```lisp
(-> 5 (+ 3) (* 2))
; 等价于 (* (+ 5 3) 2)
; => 16
```

#### 3. 符号形式

```lisp
(-> -5 -)
; 等价于 (- -5)
; => 5
```

#### 4. 带额外参数

```lisp
(-> 10 (- 3))
; 等价于 (- 10 3)
; => 7
```

#### 5. 复杂管道

```lisp
(-> 100 (/ 10) (+ 5) (* 2))
; 等价于 (* (+ (/ 100 10) 5) 2)
; => 30

; 执行步骤：
; 100 / 10 = 10
; 10 + 5 = 15
; 15 * 2 = 30
```

#### 6. 与列表函数结合

```lisp
(-> '(1 2 3) length (+ 1))
; 等价于 (+ (length '(1 2 3)) 1)
; => 4
```

#### 7. 在 lambda 中使用

```lisp
((lambda (x) (-> x (+ 10) (* 2))) 5)
; => 30
```

## 实现细节

### 核心逻辑

管道操作符在 `evaluator.cj` 的以下函数中实现：

1. **`evalThreadFirst`** (src/core/evaluator.cj:603-632)
   - 求值初始值
   - 遍历每个形式
   - 将当前值传递给下一个形式

2. **`threadFirstStep`** (src/core/evaluator.cj:637-643)
   - 分发到不同的处理函数
   - 根据形式类型（符号/列表/字面量）选择处理方式

3. **`threadFirstWithSymbol`** (src/core/evaluator.cj:648-658)
   - 处理符号形式
   - 直接调用符号代表的函数，将值作为第一个参数

4. **`threadFirstWithList`** (src/core/evaluator.cj:663-689)
   - 处理列表形式
   - 将值插入到参数列表的第一个位置
   - 求值其他参数

### 处理流程

```
(-> 5 (+ 3) (* 2))
  ↓
evalThreadFirst
  ↓
求值初始值：5
  ↓
第一步：(+ 3)
  threadFirstStep(5, (+ 3))
    → threadFirstWithList
    → 插入 5：(+ 5 3)
    → 求值：8
  ↓
第二步：(* 2)
  threadFirstStep(8, (* 2))
    → threadFirstWithList
    → 插入 8：(* 8 2)
    → 求值：16
  ↓
返回：16
```

### 关键设计

1. **惰性求值**：只对需要插入位置之后的参数进行求值
2. **类型识别**：自动识别符号、列表和字面量形式
3. **错误处理**：无效函数调用返回 Nil

## 与其他语言的对比

### Clojure

```clojure
;; Clojure 的 -> 宏
(-> 5 (+ 3) (* 2))
; => 16
```

Xisp 的 `->` 与 Clojure 的 `->` 完全相同。

### Elixir

```elixir
# Elixir 的管道操作符
5 |> &(IO.puts &1)
```

Elixir 使用 `|>` 操作符，而 Xisp 使用 `->` 特殊形式。

### Racket

```racket
# Racket 需要 racket/function 库
(require racket/function)
(-> 5 (+ 3) (* 2))
; => 16
```

Racket 的 `->` 与 Xisp 的实现类似。

## 优势

### 1. 可读性

传统写法：
```lisp
(* (+ (/ 100 10) 5) 2)  ; 难以阅读
```

管道写法：
```lisp
(-> 100 (/ 10) (+ 5) (* 2))  ; 清晰的数据流
```

### 2. 嵌套减少

传统写法（深层嵌套）：
```lisp
(+ (* (- x 1) 2) 3)
```

管道写法（扁平）：
```lisp
(-> x (- 1) (* 2) (+ 3))
```

### 3. 函数组合

```lisp
; 定义复合函数
(define process-data
  (lambda (data)
    (-> data
      validate
      transform
      save)))

; 使用
(process-data my-data)
```

## 限制

1. **只支持 thread-first**
   - 只将值插入到第一个位置
   - 不支持 thread-last（`->>`），需要单独实现

2. **性能考虑**
   - 每个步骤都需要函数调用
   - 对于简单的数学运算，嵌套调用可能更高效

3. **调试难度**
   - 管道中的错误可能难以定位
   - 需要在每步添加日志来调试

## 未来改进

1. **Thread-last 操作符 `->>`**
   ```lisp
   (->> (list 1 2 3) (map inc) (filter even?))
   ; 等价于 (filter even? (map inc (list 1 2 3)))
   ```

2. **管道组合**
   ```lisp
   (defn pipeline [f g h]
     (fn [x] (-> x f g h)))
   ```

3. **错误处理增强**
   - 在管道失败时提供更好的错误信息
   - 支持中断管道的 `->when` 等变体

## 测试

完整的测试用例位于：`src/examples/test_pipeline/main.cj`

运行测试：
```bash
cjpm build
target/release/bin/ystyle::xisp.examples.test_pipeline
```

测试覆盖：
- ✅ 基础管道：(-> 5 (+ 3)) => 8
- ✅ 多步管道：(-> 5 (+ 3) (* 2)) => 16
- ✅ 符号形式：(-> -5 -) => 5
- ✅ 带额外参数：(-> 10 (- 3)) => 7
- ✅ 复杂管道：(-> 100 (/ 10) (+ 5) (* 2)) => 30
- ✅ 与 list 结合：(-> '(1 2 3) length (+ 1)) => 4
- ✅ 在 lambda 中使用：((lambda (x) (-> x (+ 10) (* 2))) 5) => 30

## 参考

- 实现：src/core/evaluator.cj (603-689行)
- 测试：src/examples/test_pipeline/main.cj
- 任务：task.md 7.2
- Clojure 文档：https://clojure.org/guides/threading_macros
