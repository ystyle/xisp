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
| [列表操作](list) | `first` `rest` `second` `third` `fourth` `prepend` `more` `append` `reverse` `range` `length` `nth` `list` `map` `filter` `reduce` `for-each` `apply` `cons` `car` `cdr` `cadr` `caddr` `cadddr` `cdar` `cddr` `caar` | 27 |
| [哈希映射](hashmap) | `hashmap` `hashmap?` `hashmap-get` `hashmap-set!` `hashmap-remove!` `hashmap-contains?` `hashmap-size` `hashmap-keys` `hashmap-values` `hget` `hset` `hdel` `hexists` `hlen` `hkeys` `hvals` `hgetall` `contains?` | 18 |
| [谓词](predicates) | `number?` `string?` `symbol?` `list?` `procedure?` `integer?` `float?` `zero?` `positive?` `negative?` `even?` `odd?` `null?` `hashmap?` `external?` `external-type` `type-of` | 17 |
| [逻辑](logic) | `and` `or` `not` | 3 |
| [字符串](string) | `str` `string-append` `string=?` `string<` `string>` `string-length` `string-contains?` `string?` `type-of` | 9 |
| [流程控制](control) | `when` `unless` `incf` `decf` `swap` `negate` `push` `pop` `if-let` `when-let*` `condb` `do` `eval` `macroexpand` `macroexpand-all` `print` `println` `princ` `display` `newline` `error` `raise` `assert` `gensym` | 24 |
| [并发](async) | `spawn` `future-get` `future-try-get` `future-done?` `future-pending?` `future-ok?` `future-error` `future-cancel` | 8 |
| **合计** | | **123** |

> 表中「数量」是该篇文档覆盖的符号个数；个别符号（如 `type-of`）在多个页面都会讲到，
> 跨页重复计入。
>
> 上表只统计**核心内置**（123 = 内置函数 115 + 并发 8）。扩展模块的能力不计入，
> 见下一节。

## 本页之外：扩展模块

有些能力要额外依赖（JSON、HTTP），它们单独成模块，文档也在单独的章节：

| 模块 | 覆盖函数 | 数量 |
|------|----------|------|
| [JSON](/ext/json)（`ystyle::xisp_json`） | `cangjie::json-get` `cangjie::json-get-many` `cangjie::json-parse` `cangjie::json-valid?` | 4 |
| [HTTP 客户端](/ext/net)（`ystyle::xisp_net`） | `cangjie::http-request` `cangjie::http-get` `cangjie::http-post` `cangjie::http-put` `cangjie::http-delete` `cangjie::http-patch` `cangjie::http-head` `cangjie::url-encode` | 8 |
| [HTTP 服务端](/ext/httpd)（`ystyle::xisp_httpd`） | `cangjie::httpd-listen` `cangjie::httpd-shutdown` `cangjie::httpd-addr` `cangjie::httpd-running?` | 4 |

**为什么不放在这里**：API 参考讲的是「核心内置、开箱可用」的东西；扩展模块要额外依赖、
要显式装配、还有各自的构建前提（网络要 stdx 环境），混在一起会让人以为它们默认就能用。
见[扩展模块](/ext/)。

并发（`spawn` / `future-*`）**在核心里**，用 `std.core` 的线程，所以它在上面那张内置函数表里。

---

## 特殊形式与宏

部分符号不是普通函数，而是**特殊形式**（由求值器直接处理）或**宏**（编译期展开），区别在于参数不求值或部分求值：

| 类型 | 符号 | 能否取作值 |
|------|------|-----------|
| 特殊形式 | `if` `quote` `define` `defmacro` `set!` `lambda` `begin` `let` `match` `import` `export` `macroexpand` `macroexpand-all` | ❌ `[UnboundVariable]` |
| ②类符号（求值器分派 + 一等值） | `map` `filter` `reduce` `apply` `for-each` `eval` `->` | ✅ `(procedure? map)` → `true` |
| 内置宏 | `when` `unless` `incf` `decf` `swap` `push` `pop` `negate` `do` `defun` `and` `or` `condb` `if-let` `let*` `when-let*` | `procedure?` 为 `true`，但 `(type-of x)` → `"macro"`，**不能**当过程传给高阶函数 |

::: tip 关于 map/filter/reduce/apply
`map`、`filter`、`reduce`、`apply`、`for-each`、`eval`、`->` 这 7 个符号走**双轨**：

- **调用位置**（`(map f lst)`）仍由求值器直接分派，语义与性能不变；
- **值位置**可以自由传递，`(procedure? map)` → `true`、`(type-of map)` → `"procedure"`。

```lisp
(procedure? map)          ; => true
(type-of map)             ; => "procedure"

(define (go f) (f (lambda (x) (* x 2)) '(1 2 3)))
(go map)                  ; => (2 4 6)
```

注意：取作值后**调用时仍按求值器语义**而非纯函数语义，有几处需要留意：

- 把 `map` 自己当成被映射的元素拿不到期望结果：`(map map '((1 2) (3 4)))` → `(nil nil)`；
- `->` 的一等值只对"符号形式"的步骤有效（`(let ((f ->)) (f '(1 2 3) 'reverse))` → `(3 2 1)`），
  传 `(fn args...)` 形态的步骤会返回 `nil`。

普通的取用则与调用位置一致：

```lisp
(let ((f map)) (f (lambda (x) (* x 2)) '(1 2 3)))   ; => (2 4 6)
(let ((f apply)) (f + 1 2 '(3 4)))                  ; => 10
(let ((f eval)) (f '(+ 1 2)))                       ; => 3
```

用法见[列表操作](list)。
:::

::: warning 宏不等于过程
`negate`、`swap`、`when` 等是**宏**，`(procedure? negate)` 虽然是 `true`，但把它传给
`map` 得不到期望结果（`(map negate '(1 2 3))` → `(nil nil nil)`），`for-each` 会直接报
`TypeError`。请包一层 lambda：`(map (lambda (x) (negate x)) '(1 2 3))` → `(-1 -2 -3)`。

用 `type-of` 区分：宏报 `"macro"`，过程报 `"procedure"`。
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
; 输出 "bigger"（字符串按 repr 形式带引号显示）
```

## 参见

- [指南 - 基础语法](../guide/02-basics) - 特殊形式与数据类型
- [指南 - 现代语法](../guide/03-modern) - 字面量、解构、管道
- [指南 - 宏系统](../guide/04-macros) - defmacro 与代码生成
