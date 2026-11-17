# ystyle::xisp_httpd

星枢（Xisp）HTTP 服务端：起一个 HTTP/1.1 服务。

- **包**：`ystyle::xisp_httpd`
- **依赖**：`ystyle::xisp`（核心）+ `ystyle::tachyon` **0.2.2+**（HTTP 服务端，**零 stdx**）
- **设计**：`docs/plans/2026-11-17-httpd-design.md`

## 与 `xisp_net` 的分工

| | `xisp_net` | `xisp_httpd` |
|---|---|---|
| 方向 | HTTP **客户端**（发请求） | HTTP **服务端**（收请求） |
| 底层 | stdx.net.http | tachyon |
| 宿主门槛 | 要配 `CANGJIE_STDX_PATH` | **只要加个依赖** |

不合并成一个模块：依赖不同，合并会让只想要一边的宿主被迫拖进另一边。

## 快速开始

```toml
[dependencies]
  "ystyle::xisp" = "0.7.0"
  "ystyle::xisp_httpd" = "0.7.0"
```

```cangjie
import ystyle::xisp.*
import ystyle::xisp_httpd.*

let interp = LispInterpreter([withHttpd()])
```

```lisp
(define srv
  (cangjie::httpd-listen 8080
    (lambda (req)
      {:status 200
       :headers {:content-type "text/plain; charset=utf-8"}
       :body (str "hello " (hashmap-get (hashmap-get req :query) "name"))})))

;; 服务在后台跑，这行之后可以继续做别的事
(httpd-addr srv)        ; => "127.0.0.1:8080"
(httpd-shutdown srv)    ; 优雅关闭
```

`httpd-listen` **不阻塞** —— 起个服务不该要求脚本懂并发。

## 符号

| 符号 | 签名 | 说明 |
|---|---|---|
| `cangjie::httpd-listen` | `(port handler [opts])` | 起服务，返回服务句柄 |
| `cangjie::httpd-shutdown` | `(server [timeout-ms])` | 优雅关闭；幂等 |
| `cangjie::httpd-addr` | `(server)` | 实际监听地址 `"host:port"` |
| `cangjie::httpd-running?` | `(server)` | 是否仍在监听 |

`opts`（可选 `HashMap`）只暴露调优项：

| key | 默认 | 说明 |
|---|---|---|
| `:max-body-bytes` | 4 MiB | 请求体上限 |
| `:max-conns` | 16384 | 并发连接上限 |
| `:read-timeout` / `:write-timeout` / `:idle-timeout` | 无 | 毫秒 |

**绑定地址与端口范围不在 `opts` 里** —— 那是宿主策略，见「沙箱」。

## 请求：`HashMap` 快照

```lisp
{:method    "POST"
 :path      "/api/users"
 :uri       "/api/users?page=2"
 :raw-query "page=2&q=ann"
 :query     {:page "2" :q "ann"}
 :headers   {:user-agent "curl/8" :authorization "Bearer ..." :cookie "sid=abc"}
 :body      "{\"name\":\"x\"}"
 :host      "127.0.0.1:8080"
 :remote    "127.0.0.1:54321"
 :cookies   {:sid "abc"}
 :form      {:name "x"}}          ; 仅 x-www-form-urlencoded 时
```

- **值是纯字符串**，不是你拿到就失效的视图；
- `:headers` 键统一小写；
- 重复的 query / form 键取**最后一个**（要全部值用 `:raw-query` 自己拆）；
- 缺失的键不出现（`(hashmap-get req :form)` → `nil`）。

::: warning 为什么是快照
tachyon 的请求上下文是**池化复用**的 —— handler 返回后对象被下一个请求改写。
如果直接把上下文交给 Lisp，一句 `(define saved req)` 之后读到的就是**别的请求的数据**。
所以这里一律复制。代价是每请求几次分配，但这是正确性问题，不是性能问题。
:::

## 响应：三种返回值

| handler 返回 | 响应 |
|---|---|
| `HashMap` | 按 `:status` / `:headers` / `:body`，缺省 `200` + `text/plain; charset=utf-8` |
| 字符串 | `200` + `text/plain; charset=utf-8` |
| `nil` | `204 No Content` |
| `Error` | **`500`** + 错误页，**不影响后续请求** |

```lisp
(lambda (req)
  (if (string=? (hashmap-get req :path) "/health")
    "ok"                                          ; 200 text/plain
    {:status 404 :body "not found"}))              ; 按 map 写
```

**出错不让服务崩**是底线：一个 handler 写错只影响那一次请求。

## 路由：自己写

只给一个 handler，路由用 Lisp 表达：

```lisp
(define (dispatch req)
  (let [path (hashmap-get req :path)
        m    (hashmap-get req :method)]
    (cond
      (and (string=? m "GET") (string=? path "/"))       (index-page req)
      (and (string=? m "GET") (string=? path "/health")) "ok"
      (else {:status 404 :body "not found"}))))
```

要更顺手就写个 `defmacro route` —— 那是宏系统该干的活，不该固化进桥接层。

## 沙箱

| 规则 | 说明 |
|---|---|
| **默认只绑 `127.0.0.1`** | 对外监听必须宿主显式改 |
| **默认拒绝特权端口**（< 1024） | 由 `portRange` 限制 |
| **默认不注册** | 必须 `withHttpd()` 显式启用 |

```cangjie
// 对外提供服务
let interp = LispInterpreter([withHttpd(bind: "0.0.0.0", portRange: (8080, 8090))])
```

**脚本能选的只有端口，绑定地址是宿主决策** —— 与 `xisp_net` 里「脚本选 URL、宿主给域名白名单」是同一条分界。

## 不做的事

- **TLS / HTTP/2 / WebSocket**：tachyon 不做。在前置 LB（nginx / Caddy / Traefik）终结，后端走 HTTP/1.1；
- **静态文件服务、multipart 上传的便利封装**：tachyon 有底层能力，本模块首版不包；
- **路由框架**：见上。

## 实现要点

- **每请求一个求值器**：tachyon 每连接一协程，多个连接会并发调 handler，而 `Evaluator` 有大量"当前正在求值什么"的可变字段。每个请求用新的求值器（继承宿主配置），共享同一棵环境树；
- **环境标记为跨协程共享**：服务启动时对根环境调一次 `shareAcrossThreads()`，否则并发写入会损坏绑定表；
- **编译闭包会降级**：字节码后端下 handler 可能是个编译闭包（内含 VM 内部函数 id，跨求值器调不通），调用时会自动降级成等价的 AST 闭包 —— 所以 AST / BC / JIT 三种后端下行为一致；
- **起服务会等到端口真的可连**才返回，否则调用方紧接着发请求会连不上，而绑定失败也会藏进后台协程里。

## 已知限制

- 同一端口可重复监听（tachyon 开了 `SO_REUSEPORT`，这是负载均衡语义，不是 bug）；
- handler 里再 `spawn` 可行，但嵌套太深意义不大；
- 响应体整包在内存里（受 `:max-body-bytes` 限制），大文件下载需流式接口，尚未提供。

## 测试

13 条用例，全部用 `127.0.0.1` + 高位端口，**不依赖外网**。客户端是手写的最小 HTTP 请求器
（`TcpSocket` 直接发字节）—— 刻意不引 `xisp_net`，那会把 stdx 拖进本模块的测试依赖。

```shell
cjpm test --filter '*HttpdTest*'
```

## 许可

MIT。依赖的 tachyon 为 MulanPSL-2.0。
