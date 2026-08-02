# 哈希映射

本页覆盖哈希映射的创建、访问、修改与查询，共 18 个函数。

::: tip 创建方式
- 现代字面量：`{:name "张三" :age 30}`
- 内置函数：`(hashmap :name "张三" :age 30)`
- 键可以是字符串 `"name"`、关键字 `:name` 或符号，统一转换为字符串键，因此 `:name` 与 `"name"` 访问同一个条目。
:::

::: warning 没有 HashSet
Xisp **没有** HashSet 类型，`#{...}` 会被解析为普通列表。集合成员判断请用 `(contains? map key)`。
:::

---

## 创建与类型判断

### `hashmap` - 创建哈希映射

签名：`(hashmap key1 val1 key2 val2 ...)`

参数必须是偶数个，成对提供键和值。

```lisp
(hashmap :a 1 :b 2)              ; => {a 1 b 2}
(hashmap "name" "张三" "age" 30) ; 字符串键
{:x 10 :y 20}                    ; 字面量等价写法
```

边界：参数为奇数个时报错并返回 `nil`。

### `hashmap?` - 判断是否为哈希映射

签名：`(hashmap? value)`

```lisp
(hashmap? (hashmap :a 1))    ; => true
(hashmap? 42)                ; => false
(hashmap? '(1 2))            ; => false
```

---

## 查询操作

### `hashmap-get` - 获取值

签名：`(hashmap-get map key)`

键不存在时返回 `nil`（不报错）。

```lisp
(define cfg {:name "张三" :age 30})
(hashmap-get cfg :name)      ; => "张三"
(hashmap-get cfg "name")     ; => "张三"   :name 与 "name" 等价
(hashmap-get cfg :missing)   ; => nil      键不存在
```

### `hashmap-contains?` - 检查键是否存在

签名：`(hashmap-contains? map key)`

```lisp
(define cfg {:a 1})
(hashmap-contains? cfg :a)   ; => true
(hashmap-contains? cfg :zz)  ; => false
```

::: tip `contains?` 是别名
`(contains? map key)` 与 `hashmap-contains?` 完全等价，是后者的别名。它**不接受集合**，只能用于哈希映射。
:::

### `hashmap-size` - 大小

签名：`(hashmap-size map)`

```lisp
(hashmap-size {:a 1 :b 2})   ; => 2
```

### `hashmap-keys` - 获取键列表

签名：`(hashmap-keys map)`

返回所有键组成的字符串列表。顺序不保证。

```lisp
(hashmap-keys {:a 1 :b 2})   ; => ("a" "b")
```

### `hashmap-values` - 获取值列表

签名：`(hashmap-values map)`

```lisp
(hashmap-values {:a 1 :b 2}) ; => (1 2)
```

### `hgetall` - 获取整个映射

签名：`(hgetall map)`

返回映射本身（用于遍历/打印）。

```lisp
(hgetall {:a 1 :b 2})    ; => {a 1 b 2}
```

---

## 修改操作

::: warning 副作用
`hashmap-set!` 与 `hashmap-remove!` 是**原地修改**，并返回修改后的映射本身。
:::

### `hashmap-set!` - 设置键值

签名：`(hashmap-set! map key value)`

键已存在则覆盖。

```lisp
(define m {:a 1})
(hashmap-set! m :b 2)
(hashmap-get m :b)       ; => 2
(hashmap-size m)         ; => 2
```

### `hashmap-remove!` - 删除键

签名：`(hashmap-remove! map key)`

```lisp
(define m {:a 1 :b 2})
(hashmap-remove! m :a)
(hashmap-contains? m :a) ; => false
(hashmap-size m)         ; => 1
```

---

## Redis 风格别名

以下 `h` 前缀函数是完整函数名的缩写，行为完全一致：

| 缩写 | 完整函数 | 说明 |
|------|----------|------|
| `hget` | `hashmap-get` | 获取值 |
| `hset` | `hashmap-set!` | 设置键值 |
| `hdel` | `hashmap-remove!` | 删除键 |
| `hexists` | `hashmap-contains?` | 检查键存在 |
| `hlen` | `hashmap-size` | 大小 |
| `hkeys` | `hashmap-keys` | 键列表 |
| `hvals` | `hashmap-values` | 值列表 |
| `hgetall` | - | 获取整个映射 |

```lisp
(define cfg {:host "localhost" :port 8080})

(hget cfg :host)          ; => "localhost"
(hset cfg :port 9090)     ; 修改端口
(hget cfg :port)          ; => 9090
(hexists cfg :host)       ; => true
(hlen cfg)                ; => 2
(hkeys cfg)               ; => ("host" "port")
(hvals cfg)               ; => ("localhost" 9090)
(hgetall cfg)             ; => {host "localhost" port 9090}
(hdel cfg :port)
(hexists cfg :port)       ; => false
```

---

## 综合示例

```lisp
(define user {:name "张三" :skills ["Lisp" "Cangjie"]})

(if (hexists user :name)
    (println "Hello," (hget user :name))
    (println "no name"))

(hset user :age 30)
(println (hlen user))     ; => 3

(for-each println (hkeys user))
```
