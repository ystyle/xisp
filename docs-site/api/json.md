# JSON

::: warning 来自可选模块
本页的四个函数由**可选模块** `ystyle::xisp_json` 提供，**核心不含**（核心零外部依赖）。
使用前需要：

```toml
[dependencies]
  "ystyle::xisp" = "0.6.0"
  "ystyle::xisp_json" = "0.6.0"
```

```cangjie
import ystyle::xisp.*
import ystyle::xisp_json.*

let interp = LispInterpreter([withJson()])   // 在默认装配之上追加 JSON 域
```

不用 JSON 的宿主完全不接触这个模块，也就不会引入它的依赖（`gjson`）。
:::

JSON 能力分**两层**，不是冗余：

- **路径取值**（`json-get` / `json-get-many`）—— 走路径直达，**不把整棵树转成 Lisp 值**，大响应省内存；
- **解析成树**（`json-parse`）—— 转成 `HashMap` / 列表，便于遍历与模式匹配。

对同一个大 JSON 取多个字段时用 `json-get-many`（一次扫描），不要循环调 `json-get`（每次都重扫全文）。

---

## 取值

### `cangjie::json-get` - 按路径取值

签名：`(cangjie::json-get json path)`

```lisp
(cangjie::json-get "{\"a\":1}" "a")                        ; => 1
(cangjie::json-get "{\"a\":[1,2,3]}" "a.1")                ; => 2
(cangjie::json-get "{\"user\":{\"name\":\"ann\"}}" "user.name")  ; => "ann"
```

**路径不存在返回 `nil`，不是错误** —— 与 `hashmap-get` 一致，`(if x ...)` 直接可用：

```lisp
(cangjie::json-get "{\"a\":1}" "zzz")   ; => nil
```

路径语法直接沿用 gjson 的点号路径（数组用下标），不另造语法。

### `cangjie::json-get-many` - 批量取值

签名：`(cangjie::json-get-many json paths)`

返回值的列表，顺序与 `paths` 一致，缺的补 `nil`：

```lisp
(cangjie::json-get-many "{\"a\":1,\"b\":2}" '("a" "b" "zzz"))   ; => (1 2 nil)
```

---

## 解析

### `cangjie::json-parse` - 解析成树

签名：`(cangjie::json-parse json)`

```lisp
(cangjie::json-parse "{\"a\":1,\"b\":\"x\"}")   ; => {a 1 b "x"}
(cangjie::json-parse "[1,2,3]")                 ; => (1 2 3)
(cangjie::json-parse "42")                      ; => 42
(cangjie::json-parse "42.5")                    ; => 42.500000
(cangjie::json-parse "null")                    ; => nil
(cangjie::json-parse "true")                    ; => true
```

`object` 得到 `HashMap`，可用 `hashmap-get` 或 `let` 解构：

```lisp
(hashmap-get (cangjie::json-parse "{\"a\":1,\"b\":2}") "b")   ; => 2
(let [{:a x} (cangjie::json-parse "{\"a\":9}")] x)            ; => 9
```

`array` 得到列表，可用列表函数：

```lisp
(car (cangjie::json-parse "[7,8]"))          ; => 7
(length (cangjie::json-parse "[7,8,9]"))     ; => 3
```

遍历 + 聚合：

```lisp
(reduce + 0 (map (lambda (m) (hashmap-get m "n"))
                 (cangjie::json-parse "[{\"n\":1},{\"n\":2},{\"n\":3}]")))
; => 6
```

### `cangjie::json-valid?` - 语法检查

签名：`(cangjie::json-valid? json)`

```lisp
(cangjie::json-valid? "{\"a\":1}")   ; => true
(cangjie::json-valid? "{bad}")       ; => false
(cangjie::json-valid? 123)           ; => false
```

非字符串返回 `false` 而不报错 —— 谓词惯例，与 `exists?` 一致。

---

## 类型映射

| JSON | Xisp 值 |
|---|---|
| `null` | `nil` |
| `true` / `false` | `true` / `false` |
| 字符串 | 字符串 |
| **整数形态的数字** | **整数** |
| 带小数点 / 指数的数字 | 浮点数 |
| 数组 | 列表 |
| 对象 | `HashMap` |

::: tip 布尔值的字面量是 `#t` / `#f`
`json-parse` 解析出的布尔值**显示**为 `true` / `false`，但**输入**时要写 `#t` / `#f`：

```lisp
(cangjie::json-parse "true")   ; => true
#t                              ; => true      输入用这个
true                            ; => [UnboundVariable] —— 不是变量
```
:::

**整数不丢整数性**：判据是原始文本形态（不含 `.` / `e` / `E`）：

```lisp
(type-of (cangjie::json-get "{\"n\":7}" "n"))          ; => "integer"
(+ (cangjie::json-get "{\"n\":42}" "n") 1)             ; => 43  （不是 43.0）
```

::: warning 两条已知边界

**一、超过 `Int64` 范围的整数会变成浮点数。** JSON 没有大整数类型，判据是「能否装进 `Int64`」：

```lisp
(cangjie::json-get "{\"n\":9223372036854775807}" "n")   ; => 9223372036854775807
(cangjie::json-get "{\"n\":9223372036854775808}" "n")   ; => 9223372036854775808.000000
```

更大的值会丢精度。需要精确大整数时不要用 JSON 传。

**二、空数组与 `null` 都得到 `nil`。** Xisp 里空列表就是 `nil`（`'()` = `nil`），这是语言既有语义：

```lisp
(cangjie::json-parse "[]")     ; => nil
(cangjie::json-parse "null")   ; => nil
```

映射后两者不可区分。需要区分时用 `json-get` 取具体字段，或先看原始文本。
:::

---

## 错误语义

| 情况 | 结果 |
|---|---|
| JSON 文本非法 | `[SyntaxError]` |
| 路径不存在 | `nil`（不是错误） |
| 参数类型错 | `[TypeError]` |
| 参数个数错 | `[ArityMismatch]` |

```lisp
(cangjie::json-parse "{bad}")   ; => [SyntaxError] json-parse: invalid JSON text
```

错误是**值**，会中止当前求值链 —— 校验不会被静默忽略：

```lisp
(begin (cangjie::json-parse "{bad}") 42)   ; => [SyntaxError]，不是 42
```

---

## 尚未实现

- **序列化为 JSON 文本**（`LispValue` → JSON）：gjson 是只读库，没有编码能力。计划自己实现
  `(cangjie::json-dump value)`。
- 流式解析、JSON Schema 校验、JSON5 容错。

---

## 许可

本模块采用 MIT。JSON 解析基于 [gjson-cj](https://gitcode.com/ystyle/gjson-cj)（MulanPSL-2.0），
其许可声明见模块 README。
