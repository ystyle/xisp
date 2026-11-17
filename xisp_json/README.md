# ystyle::xisp_json

星枢（Xisp）JSON 能力：**按路径取值**与**解析成树**。

- **包**：`ystyle::xisp_json`
- **依赖**：`ystyle::xisp`（核心）+ [`gjson`](https://gitcode.com/ystyle/gjson-cj)（JSON 解析，零依赖）
- **不含**：JSON 序列化（`LispValue` → JSON 文本）—— 列为第二批，见下文
- **设计**：`docs/plans/2026-11-17-json-design.md`

## 为什么是独立模块

核心 `ystyle::xisp` 的 `[dependencies]` 为空，「核心零外部依赖」是它的红线。JSON 能力
放进独立成员，**不用 JSON 的宿主完全不接触 gjson**。

## 用法

```cangjie
import ystyle::xisp.*
import ystyle::xisp_json.*

let interp = LispInterpreter([withJson()])
let v = interp.eval(#"(cangjie::json-get "{\"user\":{\"name\":\"ann\"}}" "user.name")"#)
// => "ann"
```

`withJson()` 是**追加**：在默认装配之上加 JSON 域。只想给 JSON（不要 `println` 等）时：

```cangjie
LispInterpreter([withDomains([jsonDomain()], replace: true)])
```

## 四个符号

| 符号 | 语义 |
|---|---|
| `(cangjie::json-get json path)` | 按路径取值；路径不存在返回 `nil` |
| `(cangjie::json-get-many json paths)` | 批量取值，顺序与 `paths` 一致；缺的补 `nil` |
| `(cangjie::json-parse json)` | 解析成 `LispValue` 树 |
| `(cangjie::json-valid? json)` | 是否合法 JSON |

**为什么分两层**：`json-get` 走路径直达，**不把整棵树转成 `LispValue`**（大响应省内存）；
`json-parse` 才转树，便于遍历与模式匹配。对同一个大 JSON 取多个字段时用
`json-get-many`（一次扫描），不要循环调 `json-get`（每次都重扫全文）。

```lisp
;; 路径直达
(cangjie::json-get "{\"a\":[1,2,3]}" "a.1")        ; => 2

;; 批量
(cangjie::json-get-many "{\"a\":1,\"b\":2}" '("a" "b" "zzz"))   ; => (1 2 nil)

;; 解析成树后遍历
(let [{:data items} (cangjie::json-parse "{\"data\":[{\"n\":1},{\"n\":2}]}")]
  (reduce + 0 (map (lambda (m) (hashmap-get m "n")) items)))    ; => 3
```

## 映射规则

| JSON | `LispValue` |
|---|---|
| `null` | `nil` |
| `true` / `false` | `true` / `false` |
| `string` | `Str` |
| `number` | **整数形态 → `Int`**（`42`），否则 `Float`（`42.0` / `1e3`） |
| `array` | 列表（`Cons`） |
| `object` | `HashMap`（可用 `hashmap-get` / `let` 解构） |

**整数不丢整数性**：判据是原始文本形态（不含 `.` / `e` / `E`），所以 `(+ n 1)` 得到 `43`
而不是 `43.0`。

两条已知边界：

- **超过 `Int64` 范围的整数会回落成 `Float`**（JSON 没有大整数类型，gjson 给的是 `Float64`）。
  `9223372036854775808` 及更大值会丢精度。
- **空数组与 `null` 都得到 `nil`** —— xisp 里空列表就是 `nil`（`'()` = `nil`），
  这是语言既有语义，本模块不为此破例。需要区分时用 `json-get` 取具体字段。

## 错误语义

| 情况 | 结果 |
|---|---|
| JSON 文本非法 | `Error(SyntaxError)` |
| 路径不存在 | `nil`（**不是错误**，与 `hashmap-get` 一致） |
| 参数类型错 | `Error(TypeError)` |
| 参数个数错 | `Error(ArityMismatch)` |
| `json-valid?` 收到非字符串 | `false`（谓词惯例，与 `exists?` 一致） |

错误是**值**，会中止当前求值链（与核心 0.6.0 的语义一致）：`(begin (json-parse "{bad}") 42)`
不会算出 `42`。

## 沙箱

JSON 是**纯计算** —— 不碰文件、网络、进程。域属性 `escapable = false`、`hostPolicy = false`，
因此装到沙箱环境里无需额外门控。

唯一的资源风险是 `json-parse` 在大输入上的内存放大：它会把整棵树转成 `LispValue`。
大响应用 `json-get` / `json-get-many`，不要 `json-parse`。

## 尚未实现

- **JSON 序列化**（`LispValue` → JSON 文本）：gjson 是只读库，没有 `encode` / `set`。
  计划自己实现 `(cangjie::json-dump value)`，可序列化 `nil` / 布尔 / 数字 / 字符串 /
  列表 / `HashMap`，其余变体（过程、宏、外部值、错误）报 `TypeError`。
- 流式解析、JSON Schema 校验、JSON5 容错。

## 许可

本模块采用 **MIT**。

JSON 解析基于 [gjson-cj](https://gitcode.com/ystyle/gjson-cj)（作者 ystyle，版本 1.2.2），
采用 **MulanPSL-2.0**（木兰宽松许可证第 2 版）。该许可证允许衍生作品采用其它许可，
但要求保留原声明 —— 本文件即为该声明。gjson 的完整许可文本见其仓库根目录。
