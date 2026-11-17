# Xisp 网络（xisp_net）设计

> **状态**：**已实施**（2026-11-17）—— 模块 `xisp_net` 与八个符号已落地，
> 测试 11 条全绿（真实网络部分默认跳过）。§7 的 TLS 实测结论见 §7.1 —— **不冲突**。
> **前置**：①异步（子线程/共享 Client 的线程安全，见 [`2026-11-17-async-design.md`](2026-11-17-async-design.md)）；
> ②**§7 的 TLS 实测**（唯一可能推翻方案的点，必须最先做）
> **相关**：[`2026-10-11-capability-domains-design.md`](2026-10-11-capability-domains-design.md) §6.2 ·
> [`2026-11-17-json-design.md`](2026-11-17-json-design.md)（互补，不互相依赖）

---

## 1. 目标与非目标

**目标**：让脚本能发 HTTP 请求，并以 Lisp 习惯的方式处理响应。

```lisp
(let [{:status code :body text} (cangjie::http-get "https://api.example.com/users")]
  (if (= code 200)
    (cangjie::json-get text "0.name")
    (error (str "HTTP " code))))
```

**非目标**

- **axios 式的有状态客户端对象**（baseURL 继承、拦截器链、实例默认值、自动重试）—— 那是 Lisp 宏该干的，见 §3；
- WebSocket / SSE / HTTP/2 服务端（首版只做客户端 + HTTP/1.1，其余按需再加）；
- 手写 HTTP 协议解析（用现成库，见 §7）；
- JSON 编解码（`xisp_net` 不打包，见 §5.4）。

---

## 2. 归属：独立模块 `xisp_net`

| 理由 | 说明 |
|---|---|
| TLS 依赖与 core 冲突 | 走 stdx 要 **OpenSSL 3 的 `ssl` / `crypto` 动态库**，而 core 的 `cjpm.toml` 挂着 `--static` |
| 敏感度 | 网络比文件读写敏感得多，`withNetwork()` 应**默认不注册** |
| 规模 | HTTP 客户端 + 沙箱门控是数千行，core 已 14k 行 |
| 一致性 | `xisp_bc`（可选后端）已是"独立成员"模式 |

---

## 3. 分层：原语层是 **fetch 形态**，axios 那层留给 Lisp 宏

这是本设计的核心决策。

```
原语层（仓颉，xisp_net）   cangjie::http-request / http-get / http-post / url-encode
                          薄、正交、返回完整响应
        ↓
封装层（Lisp 宏）          with-base-url / with-retry / 拦截器 —— 用户或标准库自己写
```

**为什么原语层不做 axios**：axios 的价值全在「有状态配置对象」（baseURL 继承、拦截器链、实例默认值、自动重试）。把它编译进桥接层，等于**把宏系统干的事硬编码**，与 `docs/core.md` 的原则冲突：*所有 IO、系统交互都可以作为可选的桥接层实现，而不是核心的一部分*。

而 xisp 已经具备写 axios 的全部条件 —— `HashMap` 一等值 + `let` 解构 + `defmacro` 拿未求值 AST + 命名参数（`MacroFromParams`），所以**该由用户写**。

---

## 4. 符号清单

沿用既有桥接前缀 `cangjie::`（与 `cangjie::read-file` / `cangjie::exists?` 一致）：

| 符号 | 说明 |
|---|---|
| `cangjie::http-request` | `(http-request method url opts)` —— 通用入口 |
| `cangjie::http-get` | `(http-get url opts)` |
| `cangjie::http-post` | `(http-post url body opts)` |
| `cangjie::http-put` / `http-delete` / `http-patch` / `http-head` | 便利层 |
| `cangjie::url-encode` | `(url-encode s)` —— query 拼接必须给，自己手拼必错 |

**没叫 `fetch` 的理由**：这名字会让人以为有 Web/Promise 语义（09-23 讨论的结论，仍成立）。

---

## 5. 响应与请求形态

### 5.1 响应 = `HashMap`（不是 `External`）

```lisp
{:status  200
 :ok      true
 :headers {:content-type "application/json" :content-length "1234"}
 :body    "{\"name\":\"x\"}"
 :url     "https://api.example.com/users"}
```

| 为什么是 HashMap 而不是 `External` | 说明 |
|---|---|
| `let` 解构零成本 | `(let [{:status code :body text} resp] ...)` —— **实测可用**（含嵌套 `let [{:h {:x v}} m]`） |
| 可序列化 / 可打印 | 直接 `(println resp)` 能看，`External` 只能调 `display()` |
| 与 JSON 能力天然配合 | `body` 是字符串，直接喂给 `cangjie::json-get` |
| 不需要生命周期 | 首版整包读入内存，没有需要显式释放的资源 |

**注**：`Response` 做成 `External` 的好处是能装流式 body、能持连接池句柄。首版不做流式（见 §5.3），所以不需要。

### 5.2 请求选项 `opts` 也是 HashMap

```lisp
(cangjie::http-get url {:timeout 5000
                        :headers {:accept "application/json"
                                  :authorization "Bearer xxx"}})
```

**这里要纠正一个已过时的结论**：09-23 的讨论判断「`HashMap` 没有字面量语法，用户得写 `(cangjie::hashmap "timeout" 5000)`，很难看」，因此建议「在桥接层手工扫 `:key value` 对」。

**实测现在不需要了** —— xisp 已支持 map 字面量：

```
$ ./target/release/bin/ystyle::xisp_cli -c '(hashmap-get {:a 1 :b 2} :a)'      → 1
$ ./target/release/bin/ystyle::xisp_cli -c '(hashmap-get {:h {:x 9}} :h)'      → {x 9}
$ ./target/release/bin/ystyle::xisp_cli -c '(let [{:a x :b y} {:a 1 :b 2}] (+ x y))' → 3
```

所以 opts **就是一个普通的 `HashMap` 参数**，不需要任何特殊扫描逻辑。少一层约定，少一种出错方式。

可选项：

| key | 类型 | 默认 | 说明 |
|---|---|---|---|
| `:timeout` | `Int`（毫秒） | 30_000 | 连接 + 读取总超时 |
| `:headers` | `HashMap` | `{}` | 请求头，同名多值给列表 |
| `:body` | `Str` | `nil` | 请求体（`http-post` 也可用第二参数传） |
| `:max-body-bytes` | `Int` | 8 MiB | 响应体上限，超过报 `Error` |

### 5.3 响应体大小上限（防 OOM）

**必须有上限**。忆时塔里有一条实测教训：*stdx HTTP 客户端把响应体整包缓冲：仓颉程序下载大响应会 OOM（官方 cjpm 亦如此）*。所以不能指望底层库帮忙。

- 默认 `:max-body-bytes` = 8 MiB；
- 超限 → `Error(XispError(NetworkError, "response body exceeds N bytes"))`，**不返回部分数据**（部分数据会让人以为请求成功）；
- 首版不做流式。真需要下载大文件时，另开 `(http-download url path)`（边读边写盘，不进 LispValue）—— 列为后续。

### 5.4 不打包 JSON

`xisp_net` **不依赖 `xisp_json`**，也不提供 `(resp-json r)`：

- cjpm **没有可选依赖** —— 一旦依赖，只想要 HTTP 的宿主也被迫拖进 gjson；
- 组合在脚本层一行就够：`(cangjie::json-get (hashmap-get resp :body) "data.0.name")`。

---

## 6. 错误语义：分两层（**别照搬 fetch 的 reject，也别照搬 axios 的抛异常**）

| 情况 | 结果 | 理由 |
|---|---|---|
| DNS 失败 / 连接拒绝 / 超时 / TLS 握手失败 | `Error(XispError(NetworkError, "…"))` | 请求**没完成**，没有响应可谈 |
| HTTP 4xx / 5xx | **正常数据流**，`status` 字段体现 | 这是一次**成功完成的 HTTP 交互**，语义与 fetch 一致；且 404/500 是预期内的业务信息，不该打断求值链 |
| 响应体超限 | `Error(NetworkError, …)` | 拿不到完整响应 |
| url 非字符串 / opts 非 map | `Error(TypeError, …)` | |
| 域名被 `urlChecker` 拒绝 | `Error(NetworkError, "url not allowed: …")` | |

**需要新增 `ErrorType.NetworkError`**：现在 8 个（`UnboundVariable` / `UndefinedFunction` / `TypeError` / `ArityMismatch` / `SyntaxError` / `RuntimeError` / `IOError` / `ModuleError`），没有网络相关的。

好消息是 0.6.0 加了 `ErrorType.name()`，它是**8 臂穷尽 match、无 `case _`** —— 加 `NetworkError` 时编译器会强制你去补那个分支，不会漏。

---

## 7. 底层选型与 **TLS vs `--static`**（开工前第一件事）

| 档 | 能力 | 依赖 | 结论 |
|---|---|---|---|
| 应用层 | HTTP/1.1 + TLS | `stdx.net.http` + `stdx.crypto` | **首选**，但要 OpenSSL 3 |
| 传输层 | TCP/UDP/UDS | `std.net`（零依赖） | HTTP/1.1 得自己写（数千行），不做 |
| 备选 | 第三方纯仓颉 HTTP 库 | 待调研 | 判据：是否纯 `std.net` 实现 —— 若是则零环境变量 |

**真风险点**：`stdx.net.http` 的包概述明确写「使用本包需要外部依赖 `OpenSSL 3` 的 `ssl` 和 `crypto` 动态库文件」。而 core（以及现在的全部成员）都用 `--static`。

### 7.1 实测结论（2026-11-17）：**不冲突**

**`--static` 可执行 + stdx.net.http + 真实 HTTPS，三者可以共存。**

验证方式：建一个独立探针工程（库 `import stdx.net.http` / `stdx.net.tls`，可执行 `--static`），
实测结果：

| 项 | 结果 |
|---|---|
| 库构建（无 `--static`） | ✅ |
| 可执行构建（`--static`，依赖该库） | ✅ |
| `ldd` 产物 | 只有 `libstdc++` / `libm` / `libc` / `libgcc_s` / `ld-linux` —— **没有 `libssl.so`** |
| 真实 `https://httpbin.org/status/200` | ✅ 200 |
| 真实 `http://httpbin.org/status/200` | ✅ 200 |

原因：stdx 的 OpenSSL 是**运行时动态加载**的（随 `libcangjie-dynamicLoader-opensslFFI.a`
一起静态进产物），链接期没有 `libssl.so` 依赖，所以与 `--static` 不冲突。

原先列的"三条待实测方案"（静态+动态混用 / 单独不用 `--static` / 只做纯 HTTP）**都不需要**。

**另有一个必须知道的实现细节**：`ClientBuilder()` 默认不带 TLS 配置，发 HTTPS 会报
`Tls must be configured when HTTPS requests are sent`，必须显式 `.tlsConfig(TlsClientConfig())`。

---

原设计里列的待实测项（已由上表回答，保留备查）：

1. `xisp_net` 用 `--static` + 动态链接 OpenSSL，能否构建成功、运行期能否加载；
2. 若不行：`xisp_net` 单独不用 `--static`（其它成员保持 `--static`）是否可行；
3. 若都不行：只做纯 HTTP（`http://` 可用、`https://` 报 `Error`），把 TLS 推迟。

**这是唯一可能推翻整个方案的未知**，所以排第一。实测结果要写回本文档 §7。

**连带约束**（来自能力域文档 §6.1，一旦引入 stdx 就全部适用）：

- `bin-dependencies` / `link-option` 必须在 `xisp_net/cjpm.toml` **各自完整声明**（全平台各一条），**不要只写在 workspace 根** —— 根那份在成员目录内 `bundle` / `publish` 时不读；
- 用到 TLS 还要按官方文档补 `-lstdx.net.tlsFFI -lcangjie-dynamicLoader-opensslFFI` 之类；
- `xisp_net/README.md` 必须写明构建前要 `eval "$(cjvs stdx-env zsh)"`，并**明确警告 cjpm 的报错会指向 cjpm.toml、掩盖环境变量缺失**；
- `scripts/check-members.sh` 的 `NATIVE_ALLOWED` 白名单要加 `xisp_net`（否则闸门会拦）。

---

## 8. 沙箱：域名白名单

现有 `pathChecker: (String, Bool) -> Bool` 只管路径，网络要更严 —— **用 `(String) -> Bool`**：

```cangjie
public func withNetwork(allowedHosts: Array<String>)
```

| 规则 | 说明 |
|---|---|
| 默认**不注册**网络域 | 网络比文件读写敏感；`LispInterpreter()` 默认装配**不含** `xisp_net` 的域 |
| 白名单为空 = 全部拒绝 | **不是"全部允许"**（安全默认） |
| 匹配 `host` 而非整条 URL | 避免 `https://evil.com/?x=https://good.com` 这类前缀绕过 |
| 支持后缀匹配 `*.example.com` | 显式声明，不做隐式通配 |
| 重定向 | **每跳都校验**。首版建议默认**不跟随重定向**（`3xx` 作为结果返回），跟随则必须先校验新 host |

`withSandbox()` 与网络的关系：沙箱本身不注册网络；如果宿主既装了网络又开了沙箱，网络域必须**继承** urlChecker（不能被沙箱绕过）。

---

## 9. 与异步的关系（为什么网络排在异步之后）

网络模块内部持有一个**共享的仓颉 `Client` 实例**（拿连接池、TLS 会话缓存的收益）。而：

- 仓颉 `Client` 的**线程安全性未确认**；
- 一旦脚本能 `(spawn (cangjie::http-get url))`，多线程就会串用同一个 `Client` —— 这是潜在的崩溃点。

**所以顺序是：异步先落地 → 网络落地时就按"多线程会并发调 Client"来设计**（每线程一个 Client？还是一个 Client 加锁？连接池是否本身线程安全？）。先做网络再做异步，等于给自己留一个必须回头改的坑。

若异步尚未落地就要先做网络，则必须显式限制：**`xisp_net` 的域声明里标 `escapable: true`，并在无异步时保证单线程调用**（文档里写明）。

---

## 10. 待定与风险

| # | 项 | 现状 | 处置 |
|---|---|---|---|
| N1 | **TLS vs `--static`** | 未测 | **开工第一件事**，见 §7 |
| N2 | 仓颉 `Client` 的线程安全 | 未确认 | 查 stdx 文档 + 实测；决定"每线程一个 Client" vs "共享 + 加锁" |
| N3 | 是否要支持 HTTP/2 | stdx 支持 | 首版不显式配置，用库默认 |
| N4 | 重定向策略 | 未定 | 首版不跟随（3xx 原样返回），文档明说 |
| N5 | 代理支持 | stdx `Client` 有 proxy 配置 | 首版不做 `:proxy` 选项；宿主需要时通过域安装时的 Client 配置给 |
| N6 | 大文件下载 | 会撞 §5.3 上限 | 后续加 `http-download`（流式落盘，不进 LispValue） |
| N7 | 第三方纯仓颉 HTTP 库调研 | 未做 | 只有当 §7 的 TLS 实测失败时才启动这项调研 |

---

## 11. 验收标准

1. §7 的 TLS 实测结论已写回本文档（无论成败）；
2. 基础请求：GET / POST，`status` / `headers` / `body` / `url` 四个字段都对；
3. 错误语义：
   - 连不上的域名 → `Error(NetworkError)`；
   - `https://httpbin.org/status/404` → **正常返回** `{:status 404 …}`（不是 Error）；
   - 超 `:max-body-bytes` → `Error(NetworkError)`；
4. 沙箱：白名单外域名 → `Error`；空白名单 → 全部拒绝；`*.example.com` 后缀匹配生效；
5. 选项：`{:timeout …}` / `{:headers …}` 生效（用 httpbin 的回显端点验）；
6. `xisp_tests` 里的网络用例**默认跳过**（`@Ignore` 或环境变量开关），CI 不依赖外网 —— 这是硬要求，否则 CI 会因网络抖动变红；
7. `docs-site/api/` 新增网络章节；`xisp_net/README.md` 含 stdx 环境变量警告与许可声明。

---

## 12. 与既有文档的关系

- 能力域文档 §6.2（网络域分层选型）：结论一致，本设计把它落成具体符号与门控；
- 能力域文档 §6.1（stdx 只能走二进制依赖）：一旦引入 stdx，那一节的全部约束适用；
- `docs/design.md`：无网络相关章节，本设计是其首次成文；
- 09-23 的三轮讨论（fetch vs axios / 闭包当对象 / 对象模型是独立决策）：
  - 「原语层 fetch 形态、axios 留 Lisp 宏」→ **采纳并固化**；
  - 「响应用 HashMap、错误分两层」→ **采纳**；
  - 「命名参数要手工扫 `:key value`」→ **已过时**（map 字面量已支持，§5.2）；
  - 「`LispValue` 没有宿主对象」→ 0.6.0 已加 `External`；但仍**无成员访问语法**，所以「闭包 + 状态当对象」的路子依然成立。

---

## 13. 实施记录（2026-11-17）

### 13.1 落地内容

| 文件 | 内容 |
|---|---|
| `xisp_net/cjpm.toml` | 依赖 `ystyle::xisp` + stdx；`link-option` 与 7 个平台的 `bin-dependencies` 在成员内完整声明 |
| `xisp_net/src/net.cj` | `NetConfig`、`hostOf`、`hostAllowed`、`doRequest`、`registerNet`、`urlEncode`、`netDomain` |
| `xisp_net/src/options.cj` | `withNetwork(allowedHosts)`、`withNetworkUnrestricted()`（仅供测试） |
| `xisp_net/src/net_test.cj` | 11 条用例 |
| `xisp_net/README.md` | stdx 构建警告、白名单规则、错误分层、实测结论 |
| `core/src/types/types.cj` | `ErrorType` 加第 9 个变体 `NetworkError`（`name()` 与 `==` 两处穷尽 match 由编译器强制补） |
| `repl/src/highlighter.cj` | 顺带去掉手写的 ErrorType→String 映射，改用 `ErrorType.name()` |
| `docs-site/api/net.md` | 用户文档（含 `--static` 实测与 HTTPS 必须配 TLS 两条实现说明） |
| `scripts/check-members.sh` | `NATIVE_ALLOWED` 加 `xisp_net`（唯一允许平台配置的成员） |
| `.github/workflows/ci.yml` | 加 `Ensure stdx env` 步骤 —— 缺 `CANGJIE_STDX_PATH` 时 cjpm 会报"没加依赖"指向错误方向，前置检查让原因一眼可见 |

### 13.2 实现中定下的几件事

**每次请求新建 `Client`，用完关闭。** 不共享实例：`Client` 的线程安全性未确认，而
`spawn` 之后多线程会并发调网络函数，共享等于埋竞态。代价是每次多一次握手 ——
对"脚本发几个请求"可接受。要做连接池需先确认线程安全并按线程分配。

**响应头按 `HttpHeaders <: Iterable<(String, Collection<String>)>` 遍历。** 起初
按 `(String, String)` 写，编译报类型不匹配才发现：同名可以有多个值（典型是 `Set-Cookie`）。
单值给字符串、多值给列表。

**`url-encode` 手写查表而不是 `toString(radix: 16)`。** 后者的 API 形态不确定，
手写十六进制表更稳，也不依赖标准库细节。

**测试里"子域放行"不写真连断言。** 第一版真的去连 `http://a.example.org/x`，
DNS 解析失败要等几十秒，整个用例 248 秒。改成只验证"主域被拒"（快），
通配规则的放行侧由白盒 `hostAllowed` 覆盖。

**"传输层失败"用例在有代理时跳过。** 设了 `http_proxy` 时 stdx 把请求交给代理，
代理对连不上的目标回 `502` —— 那就成了"正常响应"，测不到直连失败。这是环境差异，
按环境跳过而不是放宽断言。

### 13.3 测试

11 条，**默认不依赖外网**：

| 分组 | 用例 |
|---|---|
| 白盒 | `hostOf`（剥协议/端口/userinfo，IPv6 保方括号）、`hostAllowed`（精确/后缀通配/主域不自动放行/空白名单全拒）、`urlEncode`（unreserved/结构字符/UTF-8 中文） |
| 装配 | 八个符号装上、域属性（escapable + hostPolicy）、`url-encode` 可用 |
| 沙箱 | 空白名单全部拒绝、白名单外域名被拒、后缀通配不放行主域 |
| 参数 | 类型错 TypeError、个数错 ArityMismatch、URL 缺协议 TypeError |
| 真实网络（`XISP_NET_TESTS=1`） | HTTP 200/404 形状、HTTPS 通、传输层失败 → NetworkError |

验证：`cjpm test` **600/600**；带 `XISP_NET_TESTS=1` 且无代理时 11/11 全绿
（真实 HTTP/HTTPS 都通过）；`check-examples.sh` 22/22；`cjpm ws check` 与成员闸门通过
（9 个成员）；`docs-site` 构建成功。

### 13.4 发版注意

`xisp_net` 依赖 core 里**未发布**的 `NetworkError`，所以在成员目录单独
`cjpm bundle` 会失败（依赖从 registry 解析）。发版顺序必须是：

1. 先发 `ystyle::xisp`（含 `NetworkError`）；
2. 再发 `xisp_net`（以及其它依赖它的成员）。

`cjpm ws publish` 按拓扑序处理，正好覆盖这个顺序。
