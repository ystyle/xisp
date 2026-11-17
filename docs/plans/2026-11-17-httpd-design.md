# Xisp HTTP 服务端（xisp_httpd）设计

> **状态**：设计已定，**未实施**
> **前置**：§3 —— tachyon 需要补一个**请求头遍历 API**（改动很小，但要发 0.2.2）
> **相关**：[`2026-11-17-net-design.md`](2026-11-17-net-design.md)（HTTP 客户端，互补不重叠）·
> [`2026-11-17-async-design.md`](2026-11-17-async-design.md)（每请求一协程靠它撑住）

---

## 1. 目标与非目标

**目标**：让脚本能起一个 HTTP/1.1 服务。

```lisp
(define srv
  (httpd-listen 8080
    (lambda (req)
      {:status 200
       :headers {:content-type "text/plain"}
       :body (str "hello " (hashmap-get (hashmap-get req :query) "name"))})))

;; ... 服务在后台跑
(httpd-shutdown srv)
```

**非目标**

- **TLS / HTTP/2 / WebSocket**：tachyon 明确不做（fasthttp 也不做）。需要的话在**前置 LB**（nginx / Caddy / Traefik）终结，后端走 HTTP/1.1；
- **路由框架**：只给一个 handler，路由由 Lisp 自己写（见 §5）。理由与「axios 留给 Lisp 宏」一致 —— baseURL 继承、前缀匹配、路径参数这些用 `HashMap` + `defmacro` 都能表达，固化进桥接层就把宏系统该干的活硬编码了；
- 静态文件服务、multipart 上传的便利封装（tachyon 有底层能力，首版不包）。

---

## 2. 为什么是 tachyon，不是 stdx.net.http

`xisp_net`（客户端）走 stdx，因为 stdx 的 HTTP 客户端是现成的。**服务端不一样** —— tachyon 在自己的仓里，且三个特性正好对上 xisp：

| | tachyon | stdx.net.http |
|---|---|---|
| 依赖 | **仅 `std.*`**（中心仓 0.2.1 实测 `dependencies: []`） | stdx + **OpenSSL 3 动态库** |
| 宿主门槛 | 加个依赖即可 | **要配 `CANGJIE_STDX_PATH`** |
| 并发模型 | **每连接一协程**（M:N，20k 连接约 14 个内核线程） | 通用 HTTP 栈 |
| 内存 | 31 MB 稳态 | 291 MB 稳态 |

「每连接一协程」这一点尤其关键：**它跟 xisp 为 `spawn` 做的「共享环境树 + 写锁」是同一类需求**。异步那一轮的改造，正好是这里需要的（§6.2）。

**性能预期要说清楚**：tachyon 的「每请求 0 分配」「2 倍 CPU 优势」是**仓颉 handler** 的数字。用 Lisp 写 handler 时，瓶颈在解释器不在 HTTP 栈 —— 那些数字不代表 Lisp handler 能跑几十万 QPS。tachyon 在这里的真实价值是三条：**零依赖**（部署简单）、**协程模型**（连接数上得去）、**内存低**。

---

## 3. 前置改造：tachyon 补请求头遍历 API

**现状**：`RequestCtx` 只有 `headerGet(name)` 单个取，**没有遍历全部请求头的 public API** —— 内部的 `HeaderTable.count()` / `nameAt(i)` / `valueAt(i)` 都是包内可见。

而本设计要把请求转成 `HashMap` 快照（§4.1），拿不到全部请求头就等于做不到（`Authorization`、自定义头全都读不到）。

**改动**（tachyon 仓，约 10 行）：

```cangjie
/** 遍历全部请求头；回调返回 false 时提前停止 */
public func visitRequestHeaders(f: (Bytes, Bytes) -> Bool): Unit {
    var i = 0
    let n = req.header.count()
    while (i < n) {
        if (!f(req.header.nameAt(i), req.header.valueAt(i))) {
            return
        }
        i += 1
    }
}
```

这是 fasthttp `VisitAll` 的等价物，属合理 API 补齐。**tachyon 侧要：加实现 + 单测 + 发 0.2.2**（按 tachyon 自己的发版流程）。

`xisp_httpd` 依赖 `ystyle::tachyon = "0.2.2"` 或更高。

---

## 4. 接口形态

### 4.1 请求 → `HashMap` 快照（**关键决策**）

**不直接暴露 `RequestCtx`。** 它是**池化复用**的（fasthttp 风格）：handler 返回后对象被回收并交给下一个请求。Lisp 用户只要写一句 `(define saved req)` 或让闭包捕获它，之后读到的就是**别的请求的数据** —— 这种 bug 极难查。

所以每次请求都把需要的东西**复制**成 `HashMap`：

```lisp
{:method      "POST"
 :path        "/api/users"
 :raw-query   "page=2&q=ann"
 :query       {:page "2" :q "ann"}
 :headers     {:user-agent "curl/8" :authorization "Bearer ..."}
 :body        "{\"name\":\"x\"}"
 :host        "127.0.0.1:8080"
 :remote      "127.0.0.1:54321"
 :form        {:name "x"}      ; 仅当 content-type 是表单时
 :multipart   {...}            ; 仅当是 multipart 时（文件字段给 {:filename :content-type :size :body}）
 :cookies     {:sid "abc"}}
```

- **值全是字符串**（`Bytes` → `String`）—— 不做零拷贝视图，因为 `Bytes` 的生命周期同样绑定在这次请求上；
- `:headers` 键**统一小写**（与 `xisp_net` 的响应头口径一致）；
- `:query` / `:form` 重复键取**最后一个**（简单、可预期；需要全部值用 `:raw-query` 自己解析）；
- 缺失的字段就不放这个键（`(hashmap-get req :form)` → `nil`）。

**代价是每请求构造一次 HashMap。** 这个取舍是刻意的：Lisp 层本来就在解释执行，省这次分配没有意义；而「ctx 被复用」是正确性问题，不是性能问题。

### 4.2 响应 ← 三种返回值

| handler 返回 | 响应 |
|---|---|
| `HashMap` | `{:status … :headers … :body …}`，缺省 `status = 200`、`content-type = text/plain; charset=utf-8` |
| 字符串 | `200` + `text/plain; charset=utf-8` |
| `nil` | `204 No Content` |
| `Error` | **`500`** + 错误页（错误不进连接，也不中止别的请求） |

**出错不让整条连接崩**，这是服务端的底线：一个 handler 写错不能让服务挂掉。

### 4.3 符号

| 符号 | 签名 | 说明 |
|---|---|---|
| `cangjie::httpd-listen` | `(port handler [opts])` | **非阻塞**，返回 server 句柄（`External`） |
| `cangjie::httpd-shutdown` | `(server [timeout-ms])` | 优雅关闭（`shutdownGracefully`），不传超时用 `shutdown()` |
| `cangjie::httpd-addr` | `(server)` | 实际监听地址，如 `"127.0.0.1:8080"` |
| `cangjie::httpd-running?` | `(server)` | 是否还在跑 |

**只有这四个**。`httpd-listen` 非阻塞（内部 `spawn` 跑 tachyon 的 `listenAndServe`），用户不需要自己 `spawn` —— 否则「起个服务」这种基本动作还要懂并发，门槛太高。

`opts`（可选 `HashMap`）只暴露**安全无关**的调优项：

| key | 默认 | 说明 |
|---|---|---|
| `:read-timeout` | 无 | 毫秒 |
| `:write-timeout` | 无 | 毫秒 |
| `:idle-timeout` | 无 | 毫秒 |
| `:max-body-bytes` | 4 MiB | 请求体上限（tachyon `maxRequestBodySize`） |
| `:max-conns` | 16384 | 并发连接上限 |

**绑定地址与端口范围不在 `opts` 里** —— 那是宿主策略，见 §7。

---

## 5. 路由留给 Lisp

只给一个 handler，路由自己写：

```lisp
(define (dispatch req)
  (let [path (hashmap-get req :path)
        m    (hashmap-get req :method)]
    (cond
      (and (eq? m "GET")  (eq? path "/"))       (index-page req)
      (and (eq? m "GET")  (eq? path "/health")) "ok"
      (and (eq? m "POST") (eq? path "/api/u"))  (create-user req)
      (else {:status 404 :body "not found"}))))
```

要更顺手就写个宏（`defmacro route`），那是用户的事 —— 与 `xisp_net` 的「原语层 fetch 形态、axios 留 Lisp 宏」是同一条原则。

---

## 6. 三个必须处理的坑

### 6.1 请求级 `Evaluator`

tachyon 每连接一协程，**多个连接会并发调 handler**。而 `Evaluator` 有十余个「当前正在求值什么」的可变字段（`evalDepth` / `compileSuspended` / `currentModule` / 各类守卫缓存），跨协程共享必错。

**每个请求新建一个 `Evaluator`**，共享同一棵 `Environment` 树 —— 与 `spawn` 里的做法完全一致（`Evaluator.copyConfigTo` 已具备，直接复用）。

### 6.2 环境必须标记为跨协程共享

服务启动时对根环境调一次 `Environment.shareAcrossThreads()`。否则并发 `define` / `set!` 会损坏 `HashMap`。

这是**异步那轮改造的直接复用** —— 没有那套懒分配读写锁，这个模块做不安全。

### 6.3 中断与取消要接

`httpd-shutdown` 时，正在跑的 handler 要能收尾。tachyon 的 `shutdownGracefully(timeout)` 负责等连接结束，但 handler 里的长循环要能被打断 —— 走既有的 `Evaluator.interruptChecker`（检查 server 是否在关停）。

---

## 7. 沙箱

起服务是**触碰宿主资源**（占端口、对外可达），默认不注册：

```cangjie
public func withHttpd(bind!: String = "127.0.0.1", portRange!: (UInt16, UInt16) = (1024, 65535))
```

| 规则 | 说明 |
|---|---|
| **默认只绑 `127.0.0.1`** | 对外监听必须宿主显式改 `bind`；绑 `0.0.0.0` 相当于把脚本暴露到网络 |
| **默认拒绝特权端口**（< 1024） | 由 `portRange` 限制，改它要宿主显式做 |
| 端口被占用 | `Error(NetworkError, "…")`，不静默换端口 |
| 域属性 | `escapable: true` + `hostPolicy: true`（`presetPure` 会排除） |

**脚本能选的只有端口**，绑定地址是宿主决策 —— 与 `xisp_net` 里「脚本选 URL、宿主给域名白名单」是同一条分界。

---

## 8. 待定与风险

| # | 项 | 现状 | 处置 |
|---|---|---|---|
| H1 | tachyon 0.2.2 的请求头遍历 API | 未做 | §3，第一步 |
| H2 | 每请求构造 `HashMap` 的分配量 | 未测 | 量一下（预期几 KB/请求）；若过重再考虑惰性视图，但**不牺牲安全性** |
| H3 | handler 里调 `spawn` | 可行（协程套协程）但未验证 | 实测；理论上没问题（仓颉 M:N 协程） |
| H4 | 同一进程起多个 server | 未验证 | 实测；不行就报错而不是静默 |
| H5 | `httpd-shutdown` 后句柄的状态 | 未定 | `httpd-running?` 返回 false，再 `shutdown` 幂等 |
| H6 | `Bytes → String` 的 UTF-8 校验开销 | tachyon 文档说 1 MiB ≈ 544 µs | 请求体通常不大，接受；大 body 场景另议 |

---

## 9. 验收标准

1. **前置**：tachyon 0.2.2 已发布，含 `visitRequestHeaders` 与其单测；
2. 基础：`(httpd-listen port handler)` 后能收到请求，`HashMap` 返回 → 对应状态码/头/body；`Str` → 200；`nil` → 204；
3. 请求快照完整：method / path / query / headers（含自定义头）/ body / cookies / form 都能读到，且**值是本次请求的**（用两个不同请求交替打，确认没有串数据）；
4. 错误不崩：handler 返回 `Error` → 500，**后续请求仍正常**；
5. 并发：多连接同时打，无崩溃、无数据串（`shareAcrossThreads` 生效）；
6. 沙箱：非 `127.0.0.1` 绑定被拒（除非宿主改配置）；特权端口被拒；端口占用报错；
7. 关闭：`httpd-shutdown` 后端口释放，`httpd-running?` → false；
8. 三后端一致（AST / BC / JIT）—— handler 是桥接闭包，理论上天然一致，测一下；
9. 测试**默认不依赖真实网络端口**（用随机高位端口 + `127.0.0.1`，且不测外网）。

---

## 10. 与既有文档的关系

- `xisp_net`（HTTP 客户端）与本文档**互补**：一个发请求、一个收请求。不合并 —— 依赖不同（stdx vs tachyon），合并会让只想要一边的宿主被迫拖进另一边；
- 能力域文档 §6.2「网络域分层选型」只讨论过**客户端**（`stdx.net.*`）。服务端是新的一档：**tachyon 零依赖**，所以它比客户端模块的宿主门槛更低；
- 异步设计文档 §5「共享环境树 + 写入加锁」是本模块 §6.2 的直接依据。
