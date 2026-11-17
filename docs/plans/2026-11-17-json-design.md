# Xisp JSON 能力（xisp_json）设计

> **状态**：**已实施**（2026-11-17）—— 模块 `xisp_json` 与四个符号已落地，
> 测试 20 条全绿；JSON 序列化（§8）仍为待办
> **前置**：无（可独立开工，不依赖异步/网络）
> **后续**：`xisp_net` 会用到它，但**不依赖它**（见 §7）
> **相关**：
> [`2026-10-11-capability-domains-design.md`](2026-10-11-capability-domains-design.md)（能力域与扩展接缝）
> · [`2026-11-17-async-design.md`](2026-11-17-async-design.md) · [`2026-11-17-net-design.md`](2026-11-17-net-design.md)

---

## 1. 现状与动机

**xisp 现在完全没有 JSON 能力。** `core/src` 里没有任何 json 相关实现，`docs-site/api/` 里也没有对应章节。

这不是网络的附带品 —— 任何脚本要读配置、处理接口响应、序列化数据都缺这一块。而它恰好是三个待办（JSON / 异步 / 网络）里**唯一零技术未知、零额外依赖、不需要异步配合**的一块：

| 属性 | JSON | 网络 | 异步 |
|---|---|---|---|
| 外部依赖 | gjson（零依赖、自仓） | stdx 或手写 HTTP | std.core |
| 需要 TLS | 否 | **是（唯一真风险）** | 否 |
| 需要并发 | 否 | 会撞共享 Client | 是本体 |
| 规模 | 桥接层数百行 | 数千行 | 改核心 |

所以它是三件事里应先做的一块。

---

## 2. 目标与非目标

**目标**

1. 能从 JSON 文本**按路径取值**（主路径，快、省内存）；
2. 能把 JSON 文本**解析成 LispValue 树**（便于遍历与模式匹配）；
3. 语义与既有桥接层（`registerStdIO` / `registerStdFS`）一致：错误是值、不抛异常；
4. 作为**独立能力域**可装配，核心与其它成员不因它增加依赖。

**非目标（本轮不做，避免范围蔓延）**

- **JSON 序列化（写出）**：gjson 是**只读库**，没有 `encode` / `set` / `marshal`。写 JSON 另立一条，见 §8；
- JSON Schema 校验、JSON Pointer/RFC6901 完整实现、JSON5 / 注释容错；
- 流式解析（大文件边读边解）。

---

## 3. 归属：独立模块 `xisp_json`，**不进 core**

| 方案 | 取舍 |
|---|---|
| **A（采用）**：独立成员 `xisp_json`，依赖 `gjson` | core 保持零依赖；不用 JSON 的宿主完全不接触 |
| B：进 core | **破红线** —— core 的 `[dependencies]` 现在为空，「核心零外部依赖」写在 README 第一条与工作区验收标准里 |
| C：塞进 `xisp_net` | JSON 与 HTTP 是两件事，绑定后想要 JSON 却被拖进整个网络栈（含 TLS 依赖） |

采用 A。这与 `xisp_bc`（可选后端独立成成员）是同一个模式。

**依赖**：

```toml
[dependencies]
  "gjson" = "1.2.2"          # 中心仓已发布，无组织名（default org），零依赖
```

**许可差异（发版时必须处理）**：gjson 是 **MulanPSL-2.0**，xisp 是 **MIT**。MulanPSL-2.0 是宽松许可、允许衍生作品采用其它许可，但**要求保留原声明**。故：

- `xisp_json/README.md` 必须含 gjson 的许可声明与出处；
- 制品（`.cjp`）里要能看到该声明 → README 会被打包，满足；
- 发版前核对时把这一条列入检查项。

---

## 4. 符号与命名

沿用既有桥接前缀 `cangjie::`（与 `cangjie::read-file` / `cangjie::exists?` / `cangjie::vector` 一致）：

| 符号 | 说明 |
|---|---|
| `cangjie::json-get` | `(json-get json path)` → 值 或 `nil` |
| `cangjie::json-get-many` | `(json-get-many json paths)` → 值的列表 |
| `cangjie::json-parse` | `(json-parse json)` → LispValue 树 |
| `cangjie::json-valid?` | `(json-valid? json)` → `Bool` |

**没叫 `json-read` / `json-decode` 的理由**：`json-parse` 与「解析成树」的直觉一致；`json-get` 与 gjson 的 `get` 一致，用户查 gjson 文档能直接对上。

---

## 5. 映射规则（**必须定死**）

`json-parse` 把 gjson 的 `Result` 递归转成 `LispValue`：

| JSON | `LispValue` | 说明 |
|---|---|---|
| `null` | `Nil` | |
| `true` / `false` | `Boolean` | |
| `string` | `Str` | |
| `number` | `Int` 或 `Float` | 见 §5.1 |
| `array` | `Cons` 列表（`list(...)`） | **不加 vector 变体**。`vector` 不是 `LispValue` 变体，只是 `cangjie::vector` 这个桥接函数 |
| `object` | `HashMap<String, LispValue>` | 引用类型，可用 `hashmap-get` / `hashmap-set!` / `let` 解构 |

### 5.1 数字：整数形态给 `Int`，否则 `Float`

JSON 本身不分整数与浮点，但 Lisp 用户分。规则：**`raw` 文本里不含 `.` / `e` / `E` 且能装进 `Int64` → `Int`，否则 `Float`**。

- `{"n": 42}` → `Int(42)`，`(+ (hashmap-get m "n") 1)` → `43`（不是 `43.0`）
- `{"n": 42.0}` → `Float(42.0)`
- `{"n": 1e10}` → `Float(1e10)`

### 5.2 大整数精度（已知限制，写进文档）

gjson 的 `Result.num` 是 `Float64`，超过 2^53 的整数会丢精度。故：

- **整数取值走 `raw` 文本重新解析成 `Int64`**，不走 `num`；
- `raw` 解析失败（超出 `Int64` 范围）→ 回落 `Float`，并在文档里标注「超大整数会被降级成浮点」。

### 5.3 路径不存在 → `nil`，不是错误

gjson 的语义是 `get` 拿不到就返回不存在的值。对齐：

```lisp
(cangjie::json-get "{\"a\":1}" "b")      ; → nil
(cangjie::json-get "{\"a\":1}" "a")      ; → 1
```

理由：与 `hashmap-get` 一致（取不到 `nil`），且 Lisp 里 `(if x ...)` 直接可用。**语法错**才是 `Error`。

---

## 6. 错误语义（对齐 0.6.0）

| 情况 | 结果 |
|---|---|
| JSON 文本语法非法（`json-parse` / `json-get` 的输入本身坏掉） | `Error(XispError(SyntaxError, "…"))` |
| 路径不存在 | `nil` |
| 参数类型不对（非字符串 / 非列表） | `Error(XispError(TypeError, "…"))` |
| 参数个数不对 | `Error(XispError(ArityMismatch, "…"))`（与 `registerStdFS` 的既有写法一致） |

**不新增 `ErrorType`**：语法错用现成的 `SyntaxError` 就够，不需要 `JsonError`。`ErrorType` 是 8 臂穷尽 match，能不加就不加。

---

## 7. 与网络的关系：**解耦**

`xisp_net` **不依赖 `xisp_json`**，也不提供 `(resp-json r)` 这类便利函数。理由：

- cjpm **没有可选依赖** —— 一旦 `xisp_net` 依赖 `xisp_json`，只想要 HTTP 的宿主也被迫拖进 gjson；
- 组合是脚本层的事，一行就够：

```lisp
(let [{:body text} (cangjie::http-get url {:timeout 5000})]
  (cangjie::json-get text "data.items.0.name"))
```

代价是写起来比 `(resp-json r "…")` 多一点。**接受**：能力之间不互相绑定，比少打几个字重要。

---

## 8. 序列化（写 JSON）：本轮不做，但要记下有缺口

gjson 只读。要写 JSON 有三条路：

| 方案 | 说明 | 评价 |
|---|---|---|
| 自己手写序列化（`LispValue` → JSON 文本） | 结构简单（16 个变体里可序列化的只有 8 个），几百行 | **推荐**，零依赖，且完全可控 |
| 引入 sjson 类库 | tidwall 的 sjson 是「原地编辑」，不是序列化 | 不匹配 |
| 用 `std.serialization` 之类 | 会带进外部依赖 | 与独立模块的初衷冲突 |

**决策**：写 JSON 作为 `xisp_json` 的第二批功能，自己实现：

- `(cangjie::json-dump value)` / `(json-dump value {:pretty true})`
- 可序列化变体：`Nil` `Boolean` `Int` `Float` `Str` `Cons`（列表）`HashMap`
- **不可序列化变体**（`Procedure` / `NativeFunc` / `Closure` / `Macro*` / `External` / `Error`）→ `Error(TypeError, "cannot serialize …")`
- 与 `json-parse` 的映射互逆（往返测试要钉住）

---

## 9. 沙箱

JSON 是**纯计算**：不碰文件、不碰网络、不碰进程。故：

- 域属性 `escapable: false, hostPolicy: false` → 符合进纯函数子集的条件。
  **但 `presetPure()` 只遍历 `coreDomains()`**（`core/src/core/builtin/domains_preset.cj`），
  外部模块的域**不会**自动进 —— 宿主必须显式装配（`withJson()` 就是这件事的入口）。
  实现时核对过这一点，原设计写"会自动进"是错的；
- 不需要额外的权限回调（对照网络要 `urlChecker`）；
- 唯一的资源风险是**大输入的内存放大**（`json-parse` 会把整棵树转成 `LispValue`）。对策见 §10。

---

## 10. 待定与风险

| # | 项 | 现状 | 处置 |
|---|---|---|---|
| J1 | `json-parse` 对超大 JSON 的内存放大 | 未测 | 先量一个基准（1MB / 10MB 响应分别占多少内存）；若放大严重，在文档里明确「大响应走 `json-get`，不要 `json-parse`」 |
| J2 | gjson 的路径语法对 Lisp 用户是否够用 | gjson 支持点号路径、数组下标、通配符、修饰符 | 直接透传 + 在文档里给出语法速查（不自己发明语法） |
| J3 | `json-valid?` 是否真的需要 | gjson 有 `valid.cj` | 先做，它是零成本的（一条 try 包住 parse） |
| J4 | 制品里 MulanPSL-2.0 声明的位置 | — | 首次发版前核对（`scripts/check-members.sh` 可扩一条检查） |

---

## 11. 验收标准

1. `cjpm build` / `cjpm test` 全绿；新模块有独立单元测试（不少于 §5 每条映射各一例 + 错误语义 4 例 + 往返 1 例）；
2. 三条不变量有测试钉住：
   - 整数不丢整数性：`(json-parse "{\"n\":42}")` 取出的 `n` 是 `Int`，`(+ n 1)` = `43`；
   - 路径不存在返回 `nil` 而非 `Error`；
   - `json-parse` / `json-dump` 往返一致；
3. `xisp_json` **不引入到 core 的依赖**：发布后 `core/cjpm.toml` 的 `[dependencies]` 仍为空；
4. 三后端（AST / BC / JIT）行为一致 —— 桥接函数是 `NativeFunc`，天然一致，但要在差分语料里加一个 JSON 脚本；
5. `docs-site/api/` 新增 JSON 章节，示例全部实测过。

### 实施记录（2026-11-17）

- 模块 `xisp_json`：`src/json.cj`（映射 + 四个符号）、`src/options.cj`（`withJson()`）、
  `src/json_test.cj`（白盒 7 条）、`README.md`（含 MulanPSL-2.0 声明）；
- 契约测试 `xisp_tests/src/json_test.cj` 13 条，**每条都跑 AST / BC / JIT 三个后端**；
- 制品核对：`xisp_json-0.6.0.cjp` 顶层只有 `README.md` / `cjpm.toml` / `src`，
  无 `docs` / `docs-site` / `examples` / `tests` / `AGENTS.md`；
- 全量 `cjpm test` 574/574，`check-examples.sh` 22/22，`cjpm ws check` 与
  `scripts/check-members.sh` 均通过。

实施中遇到两个与设计稿不符处，均以上述方式订正：

1. **`presetPure` 不会自动纳入外部域**（§9，见上）；
2. **空数组与 `null` 都映射成 `Nil`** —— 设计 §5 只说"array → Cons 列表"，
   但 xisp 里空列表就是 `nil`（`list()` 对空数组返回 `Nil`），所以 `[]` 与 `null`
   映射后不可区分。这是语言既有语义，本模块不破例；已在 `README.md` 与
   用户文档里写明。

---

## 12. 与能力域的关系

`xisp_json` 提供**一个域**：

```cangjie
SimpleCapability("json",
    ["cangjie::json-get", "cangjie::json-get-many", "cangjie::json-parse", "cangjie::json-valid?"],
    { ctx => registerJson(ctx.env) },
    escapable: false, hostPolicy: false)
```

装配方式：`withDomains([...])` 追加，或作为 `xisp_json` 模块自带的 `withJson()` 选项。默认不装（与 `withStdLib` 的既有约定一致：core 不默认带外部模块的能力）。
