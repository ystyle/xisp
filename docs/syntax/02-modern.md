# Xisp 现代语法特性

**版本**: 1.0.0
**目标读者**: 需要编写现代化代码的开发者
**前置知识**: [基础语法](01-basics.md)
**阅读时间**: 40 分钟

本文档介绍 Xisp 的现代语法特性，包括别名系统、字面量语法、解构绑定、管道操作符和模式匹配。

---

## 目录

1. [别名系统](#别名系统)
2. [字面量语法](#字面量语法)
3. [字符串插值](#字符串插值)
4. [解构绑定](#解构绑定)
5. [管道操作符](#管道操作符)
6. [模式匹配](#模式匹配)
7. [综合示例](#综合示例)

---

## 别名系统

Xisp 提供了现代化的函数别名，让代码更易读、更符合现代编程习惯。

### 列表操作别名

```lisp
(define lst '(1 2 3 4 5))

; 现代语法（推荐）
(first lst)            ; => 1
(second lst)           ; => 2
(third lst)            ; => 3
(rest lst)             ; => (2 3 4 5)

; 对比传统语法
(car lst)              ; => 1 (等价于 first)
(cadr lst)             ; => 2 (等价于 second)
(caddr lst)            ; => 3 (等价于 third)
(cdr lst)              ; => (2 3 4 5) (等价于 rest)
```

### 构造操作别名

```lisp
; 现代语法
(prepend 0 lst)        ; => (0 1 2 3 4 5)

; 传统语法
(cons 0 lst)           ; => (0 1 2 3 4 5)
```

### 顺序执行别名

```lisp
; 现代语法（推荐）
(begin
  (println "Step 1")
  (println "Step 2")
  (+ 1 2))

; 传统语法
(progn
  (println "Step 1")
  (println "Step 2")
  (+ 1 2))
```

### 别名对照表

| 现代语法 | 传统语法 | 说明 |
|---------|---------|------|
| `first` | `car` | 获取首元素 |
| `rest` / `more` | `cdr` | 获取剩余部分 |
| `second` | `cadr` | 第2个元素 |
| `third` | `caddr` | 第3个元素 |
| `fourth` | `cadddr` | 第4个元素 |
| `prepend` | `cons` | 前缀添加 |
| `begin` | `progn` | 顺序执行 |

### 推荐实践

```lisp
; ✅ 推荐：新代码使用现代语法
; 对列表每个元素求平方
(define (square-list lst)
  (if (null? lst)
      '()
      (prepend (* (first lst) (first lst))
               (square-list (rest lst)))))

; 使用
(square-list '(1 2 3 4))
; => (1 4 9 16)

; ⚠️ 兼容：旧代码或 Common Lisp 互通时可使用传统语法
```

---

## 字面量语法

Xisp 支持现代化的字面量语法，让数据结构更简洁、更易读。

### 向量字面量

#### 基础语法

```lisp
[元素1 元素2 元素3 ...]
```

#### 示例

```lisp
; 数字向量
[1 2 3 4 5]

; 字符串向量
["hello" "world" "xisp"]

; 混合向量
[1 "two" :symbol]

; 嵌套向量
[[1 2] [3 4] [5 6]]

; 空向量
[]
```

#### 转换规则

```
[1 2 3] → (cangjie:vector 1 2 3)
```

#### 使用场景

```lisp
; 定义配置
(define config [8080 "localhost" #t])

; 数据处理
(define numbers [1 2 3 4 5])

(map square numbers)
; => (1 4 9 16 25)
```

### 哈希映射字面量

#### 基础语法

```lisp
 {:键1 值1 :键2 值2 ...}
```

#### 示例

```lisp
; 用户信息
{:name "张三" :age 25 :city "北京"}

; 配置映射
{:host "localhost" :port 8080 :debug #t}

; 嵌套映射
{:user {:name "Alice" :age 30}
 :roles [:admin :editor]}

; 空映射
{}
```

**关键字自求值**：`:keyword` 是自求值符号，不需要 quote

```lisp
; ✅ 正确：关键字直接使用
(hget config :name)

; ❌ 错误：不需要 quote
(hget config ':name)  ; 这样会查找名为 "':name" 的键
```

#### 转换规则

```
{:a 1 :b 2} → (hashmap (quote :a) 1 (quote :b) 2)
```

注意：`:keyword` 在词法层面就被识别为关键字，不会被求值为变量。

#### 哈希映射操作（Redis 风格）

Xisp 提供了 Redis 风格的哈希操作 API，简短易用：

```lisp
; 创建
(define config {:host "localhost" :port 8080})

; 获取值 - HGET
(hget config :host)        ;; => "localhost"

; 设置值 - HSET
(hset config :port 9090)    ;; 修改端口

; 删除字段 - HDEL
(hdel config :debug)       ;; 删除 debug 字段

; 检查字段 - HEXISTS
(hexists config :host)     ;; => #t

; 获取大小 - HLEN
(hlen config)              ;; => 2

; 获取所有键 - HKEYS
(hkeys config)             ;; => ("host" "port")

; 获取所有值 - HVALS
(hvals config)             ;; => ("localhost" 9090)

; 获取全部 - HGETALL
(hgetall config)           ;; => {:host "localhost" :port 9090}
```

#### 完整函数名（向后兼容）

```lisp
; 也可以使用完整函数名（功能相同）
(hashmap-get config :host)
(hashmap-set! config :port 9090)
(hashmap-remove! config :debug)
(hashmap-contains? config :host)
(hashmap-size config)
(hashmap-keys config)
(hashmap-values config)
```

#### 使用场景

```lisp
; 定义配置
(define app-config
  {:server "localhost"
   :port 8080
   :debug #f
   :log-level :info
   :database {:host "db.example.com"
              :port 5432
              :name "mydb"}})

; 访问配置（Redis 风格）
(println "Server: " (hget app-config :server))
(println "Port: " (hget app-config :port))

; 修改配置
(hset app-config :debug #t)
(println "Debug mode: " (hget app-config :debug))

; 嵌套访问
(define db (hget app-config :database))
(println "DB host: " (hget db :host))
```

### 哈希集合字面量

#### 基础语法

```lisp
#{元素1 元素2 元素3 ...}
```

#### 示例

```lisp
; 数字集合
#{1 2 3 4 5}

; 符号集合
#{:red :green :blue}

; 字符串集合
#{"apple" "banana" "orange"}

; 混合集合
#{1 "two" :three}
```

#### 转换规则

```
#{1 2 3} → (cangjie:hashset 1 2 3)
#{:red :green} → (cangjie:hashset (quote :red) (quote :green))
```

#### 使用场景

```lisp
; 定义权限集合
(define admin-permissions #{:read :write :delete :admin})

; 检查权限
(if (contains? admin-permissions :delete)
    (println "Can delete")
    (println "Cannot delete"))

; 定义标签集合
(define tags #{:lisp :functional :modern})
```

### 字面量语法对比

#### 传统方式 vs 现代方式

```lisp
; ===== 列表 =====

; 传统方式
(define user-data
  (list (list 'name "张三")
        (list 'age 25)
        (list 'city "北京")))

; 现代方式（推荐）
(define user-data
  {:name "张三"
   :age 25
   :city "北京"})

; ===== 嵌套结构 =====

; 传统方式
(define config
  (list 'server
        (list 'host "localhost")
        (list 'port 8080)))

; 现代方式（推荐）
(define config
  {:server {:host "localhost"
            :port 8080}})
```

### 实际应用示例

#### 1. 配置管理

```lisp
(define app-config
  {:server {:host "localhost" :port 8080}
   :database {:host "db.example.com" :port 5432}
   :features #{:auth :logging :cache}
   :limits {:max-users 100 :max-connections 50}})
```

#### 2. 数据处理

```lisp
; 定义辅助函数
(define (square x) (* x x))

(define process-data
  (lambda (data)
    {:result (map square data)
     :count (length data)
     :sum (apply + data)}))

(process-data [1 2 3 4 5])
; => {:result (1 4 9 16 25) :count 5 :sum 15}
```

#### 3. 状态管理

```lisp
(define initial-state
  {:todos []
   :filter :all
   :next-id 1})

(define add-todo
  (lambda (state text)
    {:todos (prepend {:id (hget state :next-id)
                     :text text
                     :done #f}
                     (hget state :todos))
     :filter (hget state :filter)
     :next-id (+ (hget state :next-id) 1)}))

; 添加待办事项
(define state1 (add-todo initial-state "Learn Xisp"))
; => {:todos ({:id 1 :text "Learn Xisp" :done #f})
;     :filter :all
;     :next-id 2}
```

---

## 字符串插值

Xisp 支持字符串插值语法，让字符串拼接更简洁、更直观。

### 基础语法

```lisp
#"文本 #{表达式} 更多文本"
```

### 简单插值

```lisp
; 变量插值
(define name "张三")
(define age 25)

#"Hello #{name}!"
; => "Hello 张三!"

; 多个插值
#"你好 #{name}，你今年 #{age} 岁"
; => "你好 张三，你今年 25 岁"
```

### 表达式插值

```lisp
; 计算表达式
(define age 25)
#"年龄加5是 #{+ age 5}"
; => "年龄加5是 30"

; 函数调用（浮点数）
(define radius 5)
#"圆的面积是 #{* 3.14159 (* radius radius)}"
; => "圆的面积是 78.539750"

; 复杂表达式
(define x 10)
(define y 20)
#"结果: #{+ (* x 2) y}"
; => "结果: 40"
```

### 多行插值

```lisp
(define name "Alice")
(define age 30)
(define city "Beijing")

#"姓名: #{name}
年龄: #{age}
城市: #{city}"
; => "姓名: Alice
;     年龄: 30
;     城市: Beijing"
```

### 嵌套调用

```lisp
(define (greet name)
  #"Hello, #{name}!")

(define (introduce name age)
  #"I'm #{name}, #{age} years old.")

(println (greet "Alice"))
(println (introduce "Bob" 25))
```

### 转换规则

```
#"Hello #{name}" → (cangjie:interpolate "Hello " name)
#"Sum is #{+ a b}" → (cangjie:interpolate "Sum is " (+ a b))
```

**注意**：
- 插值表达式使用 `#{...}` 语法
- 多 token 的表达式会自动转换为函数调用
- 字符串片段直接拼接，表达式先求值后再转换为字符串

### 与传统方式对比

```lisp
; ===== 传统方式 =====

(define name "张三")
(define age 25)

; 使用 string-append
(string-append "Hello " name "!")
; => "Hello 张三!"

; 复杂拼接
(string-append "姓名: " name
               ", 年龄: " (str age))
; => "姓名: 张三, 年龄: 25"

; ===== 现代方式（推荐）=====

; 使用字符串插值
#"Hello #{name}!"
; => "Hello 张三!"

#"姓名: #{name}, 年龄: #{age}"
; => "姓名: 张三, 年龄: 25"
```

---

## 解构绑定

Xisp 支持现代化的解构绑定语法，使用向量 `[]` 和 `&` 符号，让代码更简洁。

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

; 向量解构
(let [[[x y] z] [[1 2] 3]]
  (list x y z))
; => (1 2 3)
```

### 多个绑定

```lisp
; 混合使用解构和普通绑定
(let [[x y] data
      z 10]
  (list x y z))
; 如果 data = '(1 2 3 4)
; => (1 2 10)
```

### 函数参数解构

```lisp
; 在函数定义中解构参数
(define (first-and-second lst)
  (let [[x y] lst]
    (list x y)))

(first-and-second '(1 2 3 4))
; => (1 2)

; 嵌套解构
(define (process-pair data)
  (let [[[a b] c] data]
    (+ a b c)))

(process-pair '((1 2) 3))
; => 6
```

### 实际应用

#### 1. 解析配置

```lisp
(define config '(("localhost" 8080) ("example.com" 80)))

(let [[[host port] & rest] config]
  (println "Host: " host)
  (println "Port: " port)
  (println "Rest: " rest))
; => Host: localhost
; => Port: 8080
; => Rest: (("example.com" 80))
```

#### 2. 分离头部和尾部

```lisp
(define numbers [1 2 3 4 5])

(let [[first & rest] numbers]
  (println "First: " first)
  (println "Rest: " rest))
; => First: 1
; => Rest: (2 3 4 5)
```

#### 3. 数据处理

```lisp
(define (process-result result)
  (let [[status msg] result]
    (if (= status 1)
        (println "Success:" msg)
        (println "Error:" msg))))

(process-result '(1 "Data loaded"))
; => Success: Data loaded

(process-result '(0 "Connection failed"))
; => Error: Connection failed
```

### 传统语法兼容

```lisp
; 传统点对解构（向后兼容）
(let ((x . y) '(1 2 3))
  x)
; => 1

(let ((x . y) '(1 2 3))
  y)
; => (2 3)

; 传统嵌套点对解构
(let (((x . y) . z) '((1 2) 3 4))
  (list x y z))
; => (1 2 (3 4))
```

**建议**：新代码推荐使用现代向量语法，更清晰直观。

---

## 管道操作符

Xisp 支持管道操作符 `->`（thread-first macro），让代码更易读、更符合数据流思维。

### 基础语法

```lisp
(-> value form1 form2 ...)
```

将 `value` 作为第一个参数传递给每个 `form`，结果继续传递给下一个。

### 基础用法

```lisp
; 两参数管道
(-> 5 (+ 3))
; 等价于 (+ 5 3)
; => 8

; 多步管道
(-> 5 (+ 3) (* 2))
; 等价于 (* (+ 5 3) 2)
; => 16

; 符号形式
(-> -5 -)
; 等价于 (- -5)
; => 5

; 带额外参数
(-> 10 (- 3))
; 等价于 (- 10 3)
; => 7
```

### 复杂管道

```lisp
; 多步处理
(-> 100 (/ 10) (+ 5) (* 2))
; 等价于 (* (+ (/ 100 10) 5) 2)
; => 30

; 执行步骤：
; 100 / 10 = 10
; 10 + 5 = 15
; 15 * 2 = 30
```

### 与列表函数结合

```lisp
(define numbers '(1 2 3 4 5))

; 管道式处理
(-> numbers
    (map (lambda (x) (* x x)))
    (filter even?)
    length)
; 等价于
; (length (filter even? (map (lambda (x) (* x x)) numbers)))
; => 2 (4 和 16)

; 另一个例子
(-> '(1 2 3 4 5)
    length
    (+ 1))
; => 6
```

### 在函数中使用

```lisp
; 定义管道式函数
(define (process-data data)
  (-> data
      validate
      transform
      save))

; 在 lambda 中使用
((lambda (x) (-> x (+ 10) (* 2))) 5)
; => 30

; 组合函数
(define process
  (lambda (x)
    (-> x
        (map square)
        (filter even?)
        length)))
```

### 可读性对比

```lisp
; ===== 传统嵌套方式 =====

; 深层嵌套，难以阅读
(+ (* (- x 1) 2) 3)

; 需要从内向外读
(* (+ (/ 100 10) 5) 2)

; ===== 管道方式（推荐）=====

; 扁平结构，从左到右读
(-> x (- 1) (* 2) (+ 3))

; 清晰的数据流
(-> 100 (/ 10) (+ 5) (* 2))
```

### 实际应用

#### 1. 数据转换

```lisp
(define (format-user-data user)
  (-> user
      (get-name)
      (string-append "User: ")
      (string-upper)))

; 等价于
; (string-upper (string-append "User: " (get-name user)))
```

#### 2. 验证链

```lisp
(define (validate-input input)
  (-> input
      trim
      validate-length
      check-format
      sanitize))

; 每一步都接收上一步的结果
```

#### 3. 计算管道

```lisp
(define (calculate-discount price)
  (-> price
      (* 0.9)              ; 9折
      (- 10)               ; 减10元
      (* 0.95)             ; 再95折
      (round 2)))          ; 保留2位小数
```

---

## 模式匹配

Xisp 提供强大的模式匹配功能，通过 `match` 表达式实现复杂的数据解构和条件判断。

### 基础匹配

```lisp
; 值匹配
(match 5
  [1 "one"]
  [2 "two"]
  [5 "five"]
  [_ "other"])
; => "five"

; 符号匹配
(define value :admin)

(match value
  [:admin "Administrator"]
  [:user "Normal user"]
  [:guest "Guest"]
  [_ "Unknown"])
; => "Administrator"
```

### 列表解构匹配

```lisp
; 匹配列表结构
(match '(1 2 3)
  [(a b c) (list "three elements" a b c)]
  [(a b) (list "two elements" a b)]
  [_ "other"])
; => ("three elements" 1 2 3)

; 使用 & 收集剩余
(match '(1 2 3 4 5)
  [(first & rest) (list "First:" first "Rest:" rest)])
; => ("First:" 1 "Rest:" (2 3 4 5))

; 嵌套匹配
(match '((1 2) 3)
  [((a b) c) (list "nested" a b c)]
  [_ "other"])
; => ("nested" 1 2 3)
```

### 向量匹配

```lisp
; 匹配向量
(match [1 2 3]
  [[a b c] (list "matched" a b c)]
  [_ "other"])
; => ("matched" 1 2 3)

; 带剩余的向量匹配
(match [1 2 3 4 5]
  [[x y & rest] (list x y rest)])
; => (1 2 (3 4 5))
```

### 哈希映射匹配

```lisp
; 匹配映射（未来功能）
(match {:name "Alice" :age 30}
  [{:name n :age a} (list "User" n a)]
  [_ "other"])
; => ("User" "Alice" 30)
```

### 守卫条件

```lisp
; 使用守卫添加额外条件
(match 15
  [x (when (> x 10)) "large"]
  [x (when (< x 5)) "small"]
  [_ "medium"])
; => "large"

; 复杂守卫
(match '(5 10)
  [(x y) (when (> x y)) "x > y"]
  [(x y) (when (< x y)) "x < y"]
  [(x y) (when (= x y)) "x = y"])
; => "x < y"
```

### 实际应用

#### 1. 处理不同类型

```lisp
(define (describe-value x)
  (match x
    [(n) (when (number? n)) "a number"]
    [(s) (when (string? s)) "a string"]
    [(lst) (when (list? lst)) "a list"]
    [_ "something else"]))

(describe-value 42)
; => "a number"

(describe-value "hello")
; => "a string"
```

#### 2. 解析数据结构

```lisp
(define (process-command cmd)
  (match cmd
    [('load file) (println "Loading:" file)]
    [('save file) (println "Saving:" file)]
    [('quit) (println "Quitting...")]
    [_ (println "Unknown command")]))

(process-command '(load "data.txt"))
; => Loading: data.txt
```

#### 3. 递归处理

```lisp
(define (sum-list lst)
  (match lst
    [('acc)] acc]
    [(first & rest) (sum-list rest (+ acc first))]))

(sum-list '(1 2 3 4 5))
; => 15
```

---

## 综合示例

### 示例1：数据处理管道

```lisp
; 定义数据
(define users
  [{:name "Alice" :age 30 :city "Beijing"}
   {:name "Bob" :age 25 :city "Shanghai"}
   {:name "Charlie" :age 35 :city "Beijing"}])

; 处理函数 - 过滤年龄大于30的用户
(define (filter-by-age users min-age)
  (filter (lambda (u) (> (hget u :age) min-age)) users))

(define (get-names users)
  (map (lambda (u) (hget u :name)) users))

(define (format-names names)
  (string-append "Users: " (str names)))

; 管道式处理
(-> users
    (filter-by-age 30)
    get-names
    format-names
    println)
; => Users: (Charlie)
```

### 示例2：配置解析

```lisp
; 定义配置
(define config
  {:server {:host "localhost" :port 8080}
   :database {:host "db.example.com" :port 5432}
   :debug #t})

; 解构配置
(let ((server-config (hget config :server))
      (db-config (hget config :database))
      (debug (hget config :debug)))
  (println "Server:" (hget server-config :host) ":" (hget server-config :port))
  (println "Database:" (hget db-config :host) ":" (hget db-config :port))
  (println "Debug:" debug))
; => Server: localhost:8080
; => Database: db.example.com:5432
; => Debug: #t
```

### 示例3：状态管理

```lisp
; 初始状态
(define state
  {:todos []
   :filter :all
   :next-id 1})

; 添加 todo
(define (add-todo state text)
  (let ((todos (hget state :todos))
        (next-id (hget state :next-id)))
    {:todos (prepend {:id next-id :text text :done #f} todos)
     :filter (hget state :filter)
     :next-id (+ next-id 1)}))

; 切换完成状态
(define (toggle-todo state id)
  {:todos (map (lambda (todo)
                 (if (= (hget todo :id) id)
                     {:id (hget todo :id)
                      :text (hget todo :text)
                      :done (not (hget todo :done))}
                     todo))
               (hget state :todos))
   :filter (hget state :filter)
   :next-id (hget state :next-id)})

; 使用
(define state1 (add-todo state "Learn Xisp"))
; => {:todos ({:id 1 :text "Learn Xisp" :done #f})
;     :filter :all
;     :next-id 2}

(define state2 (toggle-todo state1 1))
; => {:todos ({:id 1 :text "Learn Xisp" :done #t})
;     :filter :all
;     :next-id 2}
```

---

## 练习

### 基础练习

1. **字面量语法**：使用向量、哈希映射定义一个用户配置
2. **字符串插值**：使用 `#{}` 格式化输出用户信息
3. **解构绑定**：从嵌套列表中提取数据
4. **管道操作**：使用 `->` 重构嵌套函数调用

### 进阶练习

1. **数据处理**：结合 `->`、`map`、`filter` 处理数据列表
2. **模式匹配**：使用 `match` 处理不同格式的输入
3. **状态管理**：使用哈希映射和解构实现简单的状态管理

---

## 下一步

- [宏系统](03-macros.md) - 掌握元编程和代码生成
- [示例代码](../../examples/) - 查看完整示例
- [设计文档](../design.md) - 了解实现细节

---

**文档版本**: 1.0.0
**最后更新**: 2026-01-24
**维护者**: Xisp Team
