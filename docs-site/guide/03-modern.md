# 现代语法特性

**阅读时间**: 40 分钟

本指南介绍 Xisp 的现代语法特性：字面量语法、字符串插值、解构绑定、管道操作符和模式匹配。

---

## 字面量语法

### 向量

```lisp
[1 2 3 4 5]          ; 数字向量
["hello" "world"]    ; 字符串向量
[1 "two" :symbol]    ; 混合向量
[[1 2] [3 4]]        ; 嵌套向量
[]                   ; 空向量
```

向量求值为列表：

```lisp
(map (lambda (x) (* x x)) [1 2 3])
; => (1 4 9)
```

### 哈希映射

```lisp
{:name "张三" :age 25}           ; 哈希映射
{:host "localhost" :port 8080}   ; 配置映射
{:user {:name "Alice"}}          ; 嵌套映射
{}                               ; 空映射
```

::: tip 关键字自求值
`:keyword` 是自求值符号，不需要 quote。`(hget config :name)` 直接使用。
:::

#### 哈希映射操作（Redis 风格）

```lisp
(define config {:host "localhost" :port 8080})

(hget config :host)        ; => "localhost"
(hset config :port 9090)   ; 修改端口
(hexists config :host)     ; => true
(hlen config)              ; => 2
(hkeys config)             ; => ("host" "port")
(hvals config)             ; => ("localhost" 9090)
(hgetall config)           ; => {host "localhost" port 9090}
(hdel config :debug)       ; 删除字段
```

#### 完整函数名（向后兼容）

```lisp
(hashmap-get config :host)
(hashmap-set! config :port 9090)
(hashmap-remove! config :debug)
(hashmap-contains? config :host)
(hashmap-size config)
(hashmap-keys config)
(hashmap-values config)
```

::: warning 关于 `#{...}` 集合
`#{1 2 3}` 会被解析为普通列表，不是独立集合类型。判断元素是否在集合中，
请使用 `(contains? map key)` 判断哈希映射，或用 `(member? ...)` 风格的列表遍历。
:::

---

## 字符串插值

Xisp 支持字符串插值，让字符串拼接更简洁。

### 基础语法

```lisp
#"文本 #{表达式} 更多文本"
```

### 简单插值

```lisp
(define name "张三")
(define age 25)

#"Hello #{name}!"
; => "Hello 张三!"

#"你好 #{name}，你今年 #{age} 岁"
; => "你好 张三，你今年 25 岁"
```

### 表达式插值

```lisp
(define age 25)
#"年龄加5是 #{+ age 5}"
; => "年龄加5是 30"

(define x 10)
(define y 20)
#"结果: #{+ (* x 2) y}"
; => "结果: 40"
```

### 与传统方式对比

```lisp
; 传统方式
(string-append "姓名: " name ", 年龄: " (str age))
; => "姓名: 张三, 年龄: 25"

; 现代方式（推荐）
#"姓名: #{name}, 年龄: #{age}"
; => "姓名: 张三, 年龄: 25"
```

---

## 解构绑定

Xisp 支持向量解构，使用 `[]` 和 `&` 收集剩余元素。

### 基础解构

```lisp
(let [[x y] '(1 2 3 4)]
  (list x y))
; => (1 2)

(let [[x y & rest] '(1 2 3 4 5)]
  (list x y rest))
; => (1 2 (3 4 5))
```

### 嵌套解构

```lisp
(let [[[a b] c] '((1 2) 3)]
  (list a b c))
; => (1 2 3)

(let [[[a b] [c d & rest]] '((1 2) (3 4 5 6))]
  (list a b c d rest))
; => (1 2 3 4 (5 6))
```

### 传统点对解构

```lisp
(let ((x . y) '(1 2 3))
  (list x y))
; => (1 (2 3))
```

---

## 管道操作符

`->`（thread-first）将值作为第一个参数依次传递给每个形式。

### 基础用法

```lisp
(-> 5 (+ 3))
; 等价于 (+ 5 3)
; => 8

(-> 5 (+ 3) (* 2))
; 等价于 (* (+ 5 3) 2)
; => 16

(-> -5 -)
; 等价于 (- -5)
; => 5
```

### 与列表函数结合

```lisp
(-> '(1 2 3 4 5)
    (map (lambda (x) (* x x)))
    (filter even?)
    length)
; => 2 (4 和 16 是偶数)
```

### 可读性对比

```lisp
; 传统嵌套，难以阅读
(* (+ (/ 100 10) 5) 2)

; 管道方式，清晰数据流
(-> 100 (/ 10) (+ 5) (* 2))
; => 30.000000  （/ 返回浮点，固定 6 位显示）
```

---

## 模式匹配

Xisp 提供 `match` 表达式，实现强大的数据解构和条件判断。

### 值匹配

```lisp
(match 5
  1 "one"
  2 "two"
  5 "five"
  _ "other")
; => "five"
```

### 符号匹配

```lisp
(define value :admin)

(match value
  :admin "Administrator"
  :user "Normal user"
  _ "Unknown")
; => "Administrator"
```

### 列表解构匹配

```lisp
(match '(1 2 3)
  (a b c) (list "three" a b c)
  (a b) (list "two" a b)
  _ "other")
; => ("three" 1 2 3)

; 收集剩余元素
(match '(1 2 3 4 5)
  (x & rest) (list x rest))
; => (1 (2 3 4 5))
```

### 嵌套匹配

```lisp
(match '((1 2) 3)
  ((a b) c) (list "nested" a b c)
  _ "other")
; => ("nested" 1 2 3)
```

### 哈希映射匹配

```lisp
(match {:name "Alice" :age 30}
  {:name n :age a} (list "User" n a)
  _ "other")
; => ("User" "Alice" 30)
```

### 守卫条件

使用 `when` 添加额外条件：

```lisp
; 单元素模式
(match 15
  (x when (> x 10)) (str "large: " x)
  (x when (< x 5)) (str "small: " x)
  _ "medium")
; => "large: 15"

; 多元素模式需要用括号包裹
(match '(5 10)
  ((x y) when (< x y) "x < y")
  ((x y) when (= x y) "x = y")
  _ "other")
; => "x < y"
```

### 递归处理

```lisp
(define (sum-list lst)
  (match lst
    () 0
    (first & rest) (+ first (sum-list rest))))

(sum-list '(1 2 3 4 5))
; => 15
```

---

## 下一步

- [宏系统](04-macros) - 元编程和代码生成
- [API 参考](../api/) - 全部内置函数
