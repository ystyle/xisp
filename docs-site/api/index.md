# API 参考

本页是 Xisp 内置函数的完整参考，所有函数由仓颉核心求值器直接提供（内置函数与特殊形式）。

---

## 约定

### 真值与假值

求值时只有以下值被视为 **假**，其余皆为真值：

| 值 | 说明 |
|----|------|
| `nil` | 空值 / 空列表 |
| `#f` / `#false` / `false` | 布尔假 |
| `0` / `0.0` | 数字零 |

::: tip 布尔值输出
源码中可写 `#t`、`#true` 或 `true`，求值结果统一输出为 `true` / `false`。
:::

### 数字显示

- 整数以十进制显示，如 `42`。
- 浮点数固定保留 6 位小数显示，如 `(/ 10 2)` 显示为 `5.000000`、`(/ 1 3)` 显示为 `0.333333`（内部是 `Float64`）。

### 本页示例

示例中的 `; =>` 注释表示 REPL 或脚本中该表达式的求值结果，均已实测验证。

---

## 按文件分类

| 文档 | 覆盖函数 | 数量 |
|------|----------|------|
| [算术与比较](arithmetic) | `+` `-` `*` `/` `mod` `round` `sum` `product` `max` `min` `=` `!=` `<` `<=` `>` `>=` `eq?` | 17 |
| [列表操作](list) | `first` `rest` `second` `third` `fourth` `prepend` `more` `append` `reverse` `range` `length` `list` `map` `filter` `reduce` `for-each` `apply` `cons` `car` `cdr` `cadr` `caddr` `cadddr` `cdar` `cddr` `caar` | 26 |
| [哈希映射](hashmap) | `hashmap` `hashmap?` `hashmap-get` `hashmap-set!` `hashmap-remove!` `hashmap-contains?` `hashmap-size` `hashmap-keys` `hashmap-values` `hget` `hset` `hdel` `hexists` `hlen` `hkeys` `hvals` `hgetall` `contains?` | 18 |
| [谓词](predicates) | `number?` `string?` `symbol?` `list?` `procedure?` `integer?` `float?` `zero?` `positive?` `negative?` `even?` `odd?` `null?` `hashmap?` `type-of` | 15 |
| [逻辑](logic) | `and` `or` `not` | 3 |
| [字符串](string) | `str` `string-append` `string=?` `string<` `string>` `string?` `type-of` | 7 |
| [流程控制](control) | `when` `unless` `incf` `decf` `swap` `negate` `push` `pop` `if-let` `when-let*` `condb` `do` `eval` `macroexpand` `macroexpand-all` `print` `println` `princ` `display` `newline` `error` `raise` `assert` | 23 |
| **合计** | | **109** |

---

## 特殊形式与宏

部分符号不是普通函数，而是**特殊形式**（由求值器直接处理）或**宏**（编译期展开），区别在于参数不求值或部分求值：

| 类型 | 符号 |
|------|------|
| 特殊形式 | `if` `quote` `define` `defmacro` `set!` `lambda` `begin` `let` `let*` `match` `map` `filter` `reduce` `apply` `eval` `->` `import` `export` `macroexpand` `macroexpand-all` |
| 内置宏 | `when` `unless` `incf` `decf` `swap` `push` `pop` `negate` `do` `defun` |

::: warning 关于 map/filter/reduce/apply
`map`、`filter`、`reduce`、`apply` 是**特殊形式**而非普通函数，因此不能作为值传递（例如传给 `procedure?` 时需引用）。用法见[列表操作](list)。
:::

::: warning 没有 HashSet
Xisp **没有** HashSet 类型。`#{...}` 会被解析为普通列表，`(type-of '#{1 2 3})` 返回 `"list"`。集合操作请使用 `hashmap` 与 `contains?`。
:::

---

## 快速示例

```lisp
; 算术
(+ 1 2 3)                 ; => 6
(/ 1 3)                   ; => 0.333333

; 列表
(define lst '(1 2 3 4))
(map (lambda (x) (* x x)) lst)    ; => (1 4 9 16)

; 哈希映射
(define cfg {:host "localhost" :port 8080})
(hget cfg :host)          ; => "localhost"

; 流程控制
(when (> 5 3)
  (println "bigger"))
; 打印 bigger
```

## 参见

- [指南 - 基础语法](../guide/02-basics) - 特殊形式与数据类型
- [指南 - 现代语法](../guide/03-modern) - 字面量、解构、管道
- [指南 - 宏系统](../guide/04-macros) - defmacro 与代码生成
