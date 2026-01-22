# 现代化语法支持

Xisp 支持现代化的语法特性，让代码更简洁、更易读。

## 向量字面量 `[]`

### 语法
```lisp
[元素1 元素2 元素3 ...]
```

### 示例
```lisp
; 数字向量
[1 2 3 4 5]

; 字符串向量
["hello" "world" "xisp"]

; 混合向量
[1 "two" :symbol]

; 嵌套向量
[[1 2] [3 4] [5 6]]
```

### 转换规则
```
[1 2 3] → (cangjie:vector 1 2 3)
```

### 当前实现
- 返回列表形式：`(1 2 3)`
- 未来可扩展为真正的向量（支持 O(1) 随机访问）

---

## 哈希映射字面量 `{}`

### 语法
```lisp
{:键1 值1 :键2 值2 ...}
```

### 示例
```lisp
; 用户信息
{:name "张三" :age 25 :city "北京"}

; 配置映射
{:host "localhost" :port 8080 :debug true}

; 嵌套映射
{:user {:name "Alice" :age 30} :roles [:admin :editor]}
```

### 转换规则
```
{:a 1 :b 2} → (cangjie:hashmap (quote :a) 1 (quote :b) 2)
```

**注意**：键（特别是符号）会自动被 quote，防止被求值为 nil。

### 当前实现
- 返回关联列表（alist）：`((:name . "张三") (:age . 25))`
- 键被正确保留（包括 `:name` 这样的关键字符号）
- 未来可扩展为真正的 HashMap

---

## 哈希集合字面量 `#{}`

### 语法
```lisp
#{元素1 元素2 元素3 ...}
```

### 示例
```lisp
; 数字集合
#{1 2 3 4 5}

; 符号集合
#{:red :green :blue}

; 字符串集合
#{"apple" "banana" "orange"}
```

### 转换规则
```
#{1 2 3} → (cangjie:hashset 1 2 3)
#{:red :green} → (cangjie:hashset (quote :red) (quote :green))
```

**注意**：符号元素会自动被 quote，防止被求值为 nil。

### 当前实现
- 返回列表形式：`(1 2 3)` 或 `(:red :green :blue)`
- 支持混合类型：数字、字符串、符号
- 符号元素被正确保留（包括 `:red` 这样的关键字符号）
- 未来可扩展为真正的 HashSet（自动去重）

---

## 字符串插值 `#""`

### 语法
```lisp
#"文本 #{表达式} 更多文本"
```

### 示例
```lisp
; 简单变量插值
(define name "张三")
(define age 25)
#"Hello #{name}!"
; => "Hello 张三!"

; 多个插值
#"你好 #{name}，你今年 #{age} 岁"
; => "你好 张三，你今年 25.000000 岁"

; 表达式插值
#"年龄加5是 #{+ age 5}"
; => "年龄加5是 30.000000"

; 混合使用
#"#{name} 的年龄是 #{age}，居住在 #{city}"
; => "张三 的年龄是 25.000000，居住在 北京"
```

### 转换规则
```
#"Hello #{name}" → (cangjie:interpolate "Hello " name)
#"Sum is #{+ a b}" → (cangjie:interpolate "Sum is " (+ a b))
```

**注意**：
- 插值表达式使用 `#{...}` 语法
- 多 token 的表达式（如 `#{+ age 5}`）会自动转换为函数调用 `(+ age 5)`
- 字符串片段直接拼接，表达式先求值后再转换为字符串

---

## 解构绑定

Xisp 支持现代化的向量解构语法，使用 `[]` 和 `&` 符号让代码更简洁。

### 语法

```lisp
(let [pattern value] body...)
```

### 基础解构

```lisp
; 解构列表元素
(let [[x y] '(1 2 3 4)]
  (list x y))
; => (1 2)

; 使用 & 收集剩余元素
(let [[x y & rest] '(1 2 3 4 5)]
  (list x y rest))
; => (1 2 (3 4 5))
```

### 嵌套解构

```lisp
; 嵌套列表解构
(let [[[a b] c] '((1 2) 3)]
  (list a b c))
; => (1 2 3)

; 复杂嵌套
(let [[[a b] [c d & rest]] '((1 2) (3 4 5 6))]
  (list a b c d rest))
; => (1 2 3 4 (5 6))
```

### 多个绑定

```lisp
; 混合使用解构和普通绑定
(let [[x y] data
      z 10]
  (list x y z))
```

### 实际应用

```lisp
; 解析配置
(define config '(("localhost" 8080) ("example.com" 80)))

(let [[[host port] & rest] config]
  (println "Host: " host)
  (println "Port: " port))

; 分离头部和尾部
(let [[first & rest] numbers]
  (list "First is" first
        "Rest is" rest))
```

### 传统语法（向后兼容）

传统点对语法继续有效：

```lisp
; 传统点对解构
(let ((x . y) '(1 2 3))
  x)
; => 1

; 传统嵌套点对解构
(let (((x . y) . z) '((1 2) 3 4))
  (list x y z))
; => (1 2 (3 4))
```

**建议**：新代码推荐使用现代向量语法，更清晰直观。

---

## 与传统语法的对比

### 列表操作

**传统语法：**
```lisp
(define numbers (list 1 2 3 4 5))
(define first (car numbers))
(define rest (cdr numbers))
```

**现代语法：**
```lisp
(define numbers [1 2 3 4 5])
(define first (car numbers))
(define rest (cdr numbers))
```

### 哈希映射

**传统语法（需要手动 quote 键）：**
```lisp
(define user
  (list (list ':name "张三")    ; 注意 ':name 中的 ' 表示 quote
        (list ':age 25)
        (list ':city "北京")))
; 或使用 quote 函数
(define user
  (list (list (quote :name) "张三")
        (list (quote :age) 25)
        (list (quote :city) "北京")))
```

**现代语法（自动 quote，推荐）：**
```lisp
(define user {:name "张三" :age 25 :city "北京"})
```

### 函数组合

**传统语法：**
```lisp
(define result
  (list
    (list :sum (+ 1 2 3))
    (list :avg (/ (+ 1 2 3) 3))))
```

**现代语法：**
```lisp
(define result
  {:sum (+ 1 2 3)
   :avg (/ (+ 1 2 3) 3)})
```

---

## 实际应用示例

### 1. 配置管理
```lisp
(define app-config
  {:server "localhost"
   :port 8080
   :debug false
   :log-level :info
   :database {:host "db.example.com"
              :port 5432
              :name "mydb"}})
```

### 2. 数据处理
```lisp
(define process-data
  (lambda (data)
    {:result (map square data)
     :count (length data)
     :sum (apply + data)}))

(process-data [1 2 3 4 5])
; => {:result (1 4 9 16 25) :count 5 :sum 15}
```

### 3. 状态管理
```lisp
(define initial-state
  {:todos []
   :filter :all
   :next-id 1})

(define add-todo
  (lambda (state text)
    {:todos (cons {:id (state :next-id)
                   :text text
                   :done false}
                  (state :todos))
     :filter (state :filter)
     :next-id (+ (state :next-id) 1)}))
```

---

## 设计考虑

### 为什么使用字面量语法？

1. **可读性**：`{:name "张三"}` 比 `(list (list :name "张三"))` 更清晰
2. **简洁性**：`[1 2 3]` 比 `(list 1 2 3)` 更简短
3. **现代性**：与 Racket、Clojure 等现代 Lisp 方言保持一致
4. **灵活性**：可以逐步实现，当前返回列表，未来可优化为真正的数据结构

### 转换策略

所有字面量语法在 Reader（解析器）层被转换为函数调用：
- `[...]` → `(cangjie:vector ...)`
- `{...}` → `(cangjie:hashmap ...)`
- `#{...}` → `(cangjie:hashset ...)`

这种设计：
- **向后兼容**：不改变核心求值器
- **易于扩展**：可以单独优化每个函数的实现
- **保持一致**：符合 Lisp 的"代码即数据"哲学

---

## 性能说明

### 当前实现
- **向量**：列表形式，访问 O(n)
- **哈希映射**：关联列表，查找 O(n)
- **哈希集合**：列表形式，查找 O(n)

### 未来优化
- **向量**：ArrayList，访问 O(1)
- **哈希映射**：HashMap，查找 O(1) 平均
- **哈希集合**：HashSet，查找 O(1) 平均

对于大多数脚本和配置场景，当前的实现已经足够高效。

---

## 相关资源

- **解构绑定详细文档**：`docs/syntax/destructuring.md`
- **示例程序**：`src/examples/modern_syntax/main.cj`
- **解构测试**：运行 `./target/release/bin/ystyle::xisp.examples.test_destruct`
- **设计文档**：`docs/syntax/design.md`
