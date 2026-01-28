# 附录 A：标准库符号参考

**版本**: 0.1.0
**最后更新**: 2026-01-28

本文档列出了 Xisp REPL 启动后已注册的所有标准符号，按功能分类。

---

## 目录

- [1. 算术运算](#1-算术运算)
- [2. 比较运算](#2-比较运算)
- [3. 逻辑运算](#3-逻辑运算)
- [4. 列表操作](#4-列表操作)
- [5. 谓词函数](#5-谓词函数)
- [6. 字符串操作](#6-字符串操作)
- [7. 输出函数](#7-输出函数)
- [8. HashMap 操作](#8-hashmap-操作)
- [9. HashSet 操作](#9-hashset-操作)
- [10. 数据结构创建](#10-数据结构创建)
- [11. 文件 I/O](#11-文件-io)
- [12. 条件控制](#12-条件控制)
- [13. 宏](#13-宏)
- [14. 符号别名](#14-符号别名)

---

## 1. 算术运算

| 符号 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `+` | 函数 | 加法 | `(+ 1 2)` → `3` |
| `-` | 函数 | 减法 | `(- 5 2)` → `3` |
| `*` | 函数 | 乘法 | `(* 3 4)` → `12` |
| `/` | 函数 | 除法 | `(/ 10 2)` → `5` |
| `mod` | 函数 | 取模 | `(mod 10 3)` → `1` |
| `round` | 函数 | 四舍五入 | `(round 3.7)` → `4` |
| `sum` | 函数 | 求和 | `(sum [1 2 3])` → `6` |
| `product` | 函数 | 求积 | `(product [2 3 4])` → `24` |
| `max` | 函数 | 最大值 | `(max 5 3)` → `5` |
| `min` | 函数 | 最小值 | `(min 5 3)` → `3` |

---

## 2. 比较运算

| 符号 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `=` | 函数 | 相等比较 | `(= 3 3)` → `true` |
| `<` | 函数 | 小于 | `(< 3 5)` → `true` |
| `>` | 函数 | 大于 | `(> 5 3)` → `true` |
| `<=` | 函数 | 小于等于 | `(<= 3 5)` → `true` |
| `>=` | 函数 | 大于等于 | `(>= 5 3)` → `true` |

---

## 3. 逻辑运算

| 符号 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `and` | 函数 | 逻辑与 | `(and true true)` → `true` |
| `or` | 函数 | 逻辑或 | `(or false true)` → `true` |
| `not` | 函数 | 逻辑非 | `(not true)` → `false` |

---

## 4. 列表操作

### 4.1 基础操作

| 符号 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `cons` | 函数 | 构造点对 | `(cons 1 '(2 3))` → `(1 2 3)` |
| `car` | 函数 | 获取列表第一个元素 | `(car '(1 2 3))` → `1` |
| `cdr` | 函数 | 获取列表除第一个外的元素 | `(cdr '(1 2 3))` → `(2 3)` |
| `list` | 函数 | 创建列表 | `(list 1 2 3)` → `(1 2 3)` |
| `length` | 函数 | 列表长度 | `(length '(1 2 3))` → `3` |
| `append` | 函数 | 追加元素 | `(append '(1 2) 3)` → `(1 2 3)` |

### 4.2 高级操作

| 符号 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `first` | 函数 | 别名 car | `(first '(1 2 3))` → `1` |
| `rest` | 函数 | 别名 cdr | `(rest '(1 2 3))` → `(2 3)` |
| `second` | 函数 | 获取第二个元素 | `(second '(1 2 3))` → `2` |
| `third` | 函数 | 获取第三个元素 | `(third '(1 2 3))` → `3` |
| `reverse` | 函数 | 反转列表 | `(reverse '(1 2 3))` → `(3 2 1)` |
| `prepend` | 函数 | 在列表头添加元素 | `(prepend '(2 3) 1)` → `(1 2 3)` |
| `more` | 函数 | 别名 rest | `(more '(1 2 3))` → `(2 3)` |

### 4.3 CDR 组合操作

| 符号 | 类型 | 说明 | 等价于 |
|------|------|------|--------|
| `cadr` | 函数 | 获取第二个元素 | `(car (cdr ...))` |
| `caddr` | 函数 | 获取第三个元素 | `(car (cdr (cdr ...)))` |
| `cddr` | 函数 | 获取除前两个外的元素 | `(cdr (cdr ...))` |
| `cdar` | 函数 | 获取第一个元素的 cdr | `(cdr (car ...))` |
| `caar` | 函数 | 获取第一个元素的 car | `(car (car ...))` |

---

## 5. 谓词函数

### 5.1 类型检查

| 符号 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `eq?` | 函数 | 相等判断 | `(eq? '(1 2) '(1 2))` → `true` |
| `integer?` | 函数 | 是否为整数 | `(integer? 42)` → `true` |
| `float?` | 函数 | 是否为浮点数 | `(float? 3.14)` → `true` |
| `number?` | 函数 | 是否为数字（整数或浮点） | `(number? 42)` → `true` |
| `string?` | 函数 | 是否为字符串 | `(string? "hello")` → `true` |
| `symbol?` | 函数 | 是否为符号 | `(symbol? 'foo)` → `true` |
| `list?` | 函数 | 是否为列表 | `(list? '(1 2 3))` → `true` |
| `null?` | 函数 | 是否为空值 | `(null? nil)` → `true` |
| `procedure?` | 函数 | 是否为过程/函数 | `(procedure? +)` → `true` |
| `hashmap?` | 函数 | 是否为哈希映射 | `(hashmap? {:a 1})` → `true` |

### 5.2 数值谓词

| 符号 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `zero?` | 函数 | 是否为零 | `(zero? 0)` → `true` |
| `positive?` | 函数 | 是否为正数 | `(positive? 5)` → `true` |
| `negative?` | 函数 | 是否为负数 | `(negative? -5)` → `true` |
| `even?` | 函数 | 是否为偶数 | `(even? 4)` → `true` |
| `odd?` | 函数 | 是否为奇数 | `(odd? 3)` → `true` |

---

## 6. 字符串操作

| 符号 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `str` | 函数 | 转换为字符串 | `(str 42)` → `"42"` |
| `string-append` | 函数 | 字符串连接 | `(string-append "Hello" " World")` → `"Hello World"` |
| `string=?` | 函数 | 字符串相等比较 | `(string=? "a" "a")` → `true` |
| `string<` | 函数 | 字符串小于比较 | `(string< "a" "b")` → `true` |
| `string>` | 函数 | 字符串大于比较 | `(string> "b" "a")` → `true` |
| `cangjie::interpolate` | 函数 | 字符串插值 | `(cangjie::interpolate "Value: ${x}")` |

---

## 7. 输出函数

| 符号 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `print` | 函数 | 打印值（后加空格） | `(print "Hello")` |
| `println` | 函数 | 打印值（后换行） | `(println "Hello")` |
| `princ` | 函数 | 打印值（不添加空格） | `(princ "A" "B")` |
| `newline` | 函数 | 输出换行符 | `(newline)` |
| `display` | 函数 | 显示值（不添加额外字符） | `(display "test")` |

---

## 8. HashMap 操作

### 8.1 创建和查询

| 符号 | 类型 | 命名空间 | 说明 | 示例 |
|------|------|----------|------|------|
| `hashmap` | 函数 | - | 创建 HashMap | `(hashmap {:a 1 :b 2})` |
| `hashmap-get` | 函数 | - | 获取值 | `(hashmap-get h "key")` |
| `hashmap-contains?` | 函数 | - | 检查键是否存在 | `(hashmap-contains? h "key")` |
| `hashmap-keys` | 函数 | - | 获取所有键 | `(hashmap-keys h)` |
| `hashmap-values` | 函数 | - | 获取所有值 | `(hashmap-values h)` |
| `hashmap-size` | 函数 | - | 获取大小 | `(hashmap-size h)` |

### 8.2 修改操作

| 符号 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `hashmap-set!` | 函数 | 设置键值（覆盖） | `(hashmap-set! h "key" "value")` |
| `hashmap-remove!` | 函数 | 删除键 | `(hashmap-remove! h "key")` |

### 8.3 缩写和别名

| 符号 | 类型 | 说明 | 等价于 |
|------|------|------|--------|
| `hget` | 函数 | 获取值（缩写） | `hashmap-get` |
| `hset` | 函数 | 设置值（缩写） | `hashmap-set!` |
| `hdel` | 函数 | 删除键（缩写） | `hashmap-remove!` |
| `hexists` | 函数 | 检查键存在（缩写） | `hashmap-contains?` |
| `hkeys` | 函数 | 获取键列表（缩写） | `hashmap-keys` |
| `hvals` | 函数 | 获取值列表（缩写） | `hashmap-values` |
| `hlen` | 函数 | 获取大小（缩写） | `hashmap-size` |
| `hgetall` | 函数 | 获取所有键值对 | 遍历 HashMap |

---

## 9. HashSet 操作

| 符号 | 类型 | 命名空间 | 说明 | 示例 |
|------|------|----------|------|------|
| `cangjie::hashset` | 函数 | cangjie | 创建 HashSet | `(cangjie::hashset 1 2 3)` |

---

## 10. 数据结构创建

| 符号 | 类型 | 命名空间 | 说明 | 示例 |
|------|------|----------|------|------|
| `cangjie::vector` | 函数 | cangjie | 创建向量/数组 | `(cangjie::vector 1 2 3)` |
| `cangjie::hashmap` | 函数 | cangjie | 创建 HashMap | `(cangjie::hashmap {:a 1 :b 2})` |
| `cangjie::hashset` | 函数 | cangjie | 创建 HashSet | `(cangjie::hashset 1 2 3)` |

---

## 11. 文件 I/O

| 符号 | 类型 | 命名空间 | 说明 | 示例 |
|------|------|----------|------|------|
| `cangjie::read-file` | 函数 | cangjie | 读取文件内容 | `(cangjie::read-file "path.txt")` |
| `cangjie::write-file` | 函数 | cangjie | 写入文件（覆盖） | `(cangjie::write-file "path.txt" "content")` |
| `cangjie::append-file` | 函数 | cangjie | 追加写入文件 | `(cangjie::append-file "log.txt" "line\n")` |
| `cangjie::exists?` | 函数 | cangjie | 检查文件/目录是否存在 | `(cangjie::exists? "path.txt")` |
| `cangjie::list-dir` | 函数 | cangjie | 列出目录内容 | `(cangjie::list-dir ".")` |
| `cangjie::directory?` | 函数 | cangjie | 是否为目录 | `(cangjie::directory? ".")` |
| `cangjie::file?` | 函数 | cangjie | 是否为文件 | `(cangjie::file? "test.lisp")` |

---

## 12. 条件控制

| 符号 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `when` | 宏 | 条件执行（单分支） | `(when test-expr body...)` |
| `if` | 宏 | 条件执行（单分支） | `(when test-expr body...)` |
| `unless` | 宏 | 条件执行（否定分支） | `(unless test-expr body...)` |

---

## 13. 宏

| 符号 | 类型 | 说明 | 示例 |
|------|------|------|------|
| `incf` | 宏 | 递增变量 | `(incf x)` → `x = x + 1` |
| `decf` | 宏 | 递减变量 | `(decf x)` → `x = x - 1` |
| `swap` | 宏 | 交换两个变量的值 | `(swap a b)` |
| `push` | 宏 | 将值压入栈 | `(push x)` |
| `pop` | 宏 | 从栈弹出值 | `(pop x)` |
| `negate` | 宏 | 取反数值 | `(negate x)` → `-x` |

---

## 14. 符号别名

| 符号 | 类型 | 说明 | 实现方式 |
|------|------|------|----------|
| `first` | 函数 | car 的别名 | 直接映射到 `car` |
| `rest` | 函数 | cdr 的别名 | 直接映射到 `cdr` |
| `second` | 函数 | cadr 的别名 | `(define (second x) (car (cdr x)))` |
| `third` | 函数 | caddr 的别名 | `(define (third x) (car (cdr (cdr x))))` |
| `more` | 函数 | rest 的别名 | 直接映射到 `cdr` |
| `hget` | 函数 | hashmap-get 的缩写 | 直接映射到 `hashmap-get` |
| `hset` | 函数 | hashmap-set! 的缩写 | 直接映射到 `hashmap-set!` |
| `hdel` | 函数 | hashmap-remove! 的缩写 | 直接映射到 `hashmap-remove!` |
| `hexists` | 函数 | hashmap-contains? 的缩写 | 直接映射到 `hashmap-contains?` |
| `hkeys` | 函数 | hashmap-keys 的缩写 | 直接映射到 `hashmap-keys` |
| `hvals` | 函数 | hashmap-values 的缩写 | 直接映射到 `hashmap-values` |
| `hlen` | 函数 | hashmap-size 的缩写 | 直接映射到 `hashmap-size` |

---

## 15. 符号统计

### 15.1 按命名空间分类

| 命名空间 | 数量 | 符号列表 |
|---------|------|---------|
| 全局（无命名空间前缀） | 49 | +, -, *, /, mod, round, sum, product, max, min, =, <, >, <=, >=, and, or, not, cons, car, cdr, list, length, append, reverse, first, rest, second, third, prepend, more, cadr, caddr, cddr, cdar, caar, str, string-append, print, println, princ, newline, display, when, unless, incf, decf, swap, push, pop, negate |
| `cangjie::` | 10 | read-file, write-file, append-file, exists?, list-dir, directory?, file?, vector, hashmap, hashset, interpolate |
| 缩写（h开头） | 8 | hget, hset, hdel, hexists, hkeys, hvals, hlen, hgetall |

### 15.2 按类型分类

| 类型 | 数量 | 说明 |
|------|------|------|
| 函数 | 77 | 大部分符号是函数 |
| 宏 | 7 | when, unless, incf, decf, swap, push, pop, negate |

### 15.3 按功能分类

| 功能分类 | 数量 | 说明 |
|---------|------|------|
| 算术运算 | 11 | +, -, *, /, mod, round, sum, product, max, min |
| 比较运算 | 5 | =, <, >, <=, >= |
| 逻辑运算 | 3 | and, or, not |
| 列表操作 | 18 | cons, car, cdr, list, length, append, reverse, first, rest, second, third, prepend, more, cadr, caddr, cddr, cdar, caar |
| 谓词函数 | 15 | eq?, integer?, float?, number?, string?, symbol?, list?, null?, procedure?, zero?, positive?, negative?, even?, odd?, hashmap?, string=?, string<, string> |
| 字符串操作 | 6 | str, string-append, string=?, string<, string>, cangjie::interpolate |
| 输出函数 | 5 | print, println, princ, newline, display |
| HashMap | 19 | hashmap, hashmap-get, hashmap-set!, hashmap-remove!, hashmap-contains?, hashmap-keys, hashmap-values, hashmap-size, hget, hset, hdel, hexists, hkeys, hvals, hlen, hgetall |
| HashSet | 1 | cangjie::hashset |
| 数据结构创建 | 3 | cangjie::vector, cangjie::hashmap, cangjie::hashset |
| 文件 I/O | 7 | read-file, write-file, append-file, exists?, list-dir, directory?, file? |
| 条件控制 | 2 | when, unless |
| 宏 | 7 | incf, decf, swap, push, pop, negate |
| 类型检查 | 1 | type-of |

---

## 16. 使用示例

### 16.1 算术运算

```lisp
xisp> (+ 1 2 3)
6

xisp> (sum [1 2 3 4])
10

xisp> (max 10 20)
20
xisp> (mod 10 3)
1
```

### 16.2 列表操作

```lisp
xisp> (cons 1 '(2 3))
(1 2 3)

xisp> (car '(1 2 3))
1

xisp> (cdr '(1 2 3))
(2 3)

xisp> (append '(1 2) 3)
(1 2 3)

xisp> (reverse '(1 2 3))
(3 2 1)
```

### 16.3 HashMap 操作

```lisp
xisp> (define h (hashmap {:a 1 :b 2}))
xisp> (hashmap-get h "a")
1

xisp> (hashmap-set! h "c" 3)
xisp> (hashmap-keys h)
("a" "b" "c")

xisp> (hashmap-size h)
3

;; 使用缩写
xisp> (hget h "a")
1

xisp> (hset h "d" 4)
4
xisp> (hkeys h)
("a" "b" "c" "d")
```

### 16.4 文件 I/O

```lisp
xisp> (cangjie::write-file "test.txt" "Hello")
xisp> (cangjie::read-file "test.txt")
"Hello"

xisp> (cangjie::exists? "test.txt")
true

xisp> (cangjie::list-dir ".")
("test.txt" "main.cj" "src/")
```

---

## 17. 参见

- [HashMap 完整指南](../examples/02-intermediate/05_hashmap.lisp)
- [文件 I/O 文档](../examples/01-basics/03_file_io.lisp)
- [宏系统文档](./syntax/macros.md)

---

**文档版本**: 0.1.0
**最后更新**: 2026-01-28
**符号总数**: 77 个函数 + 7 个宏 = 84 个符号
