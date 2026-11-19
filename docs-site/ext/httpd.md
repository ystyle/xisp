# HTTP 服务端

::: warning 来自扩展模块
本页的函数由**扩展模块** `ystyle::xisp_httpd` 提供，**核心不含**。它依赖
[tachyon](https://atomgit.com/ystyle/tachyon)（fasthttp 风格的 HTTP/1.1 服务端，**零 stdx**）。

```toml
[dependencies]
  "ystyle::xisp" = "0.9.0"
  "ystyle::xisp_httpd" = "0.9.0"
```

与 [HTTP 客户端](net) 分工：一个发请求、一个收请求。客户端要 stdx，服务端不要。
:::

## 起一个服务

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

(httpd-addr srv)        ; => "127.0.0.1:8080"
(httpd-shutdown srv)    ; 优雅关闭
```

`httpd-listen` **不阻塞**，返回服务句柄 —— 起个服务不该要求脚本懂并发。

---

## 符号

| 符号 | 签名 |
|---|---|
| `cangjie::httpd-listen` | `(port handler [opts])` |
| `cangjie::httpd-shutdown` | `(server [timeout-ms])` —— 幂等 |
| `cangjie::httpd-addr` | `(server)` |
| `cangjie::httpd-running?` | `(server)` |

`opts` 只暴露调优项（`:max-body-bytes` / `:max-conns` / `:read-timeout` /
`:write-timeout` / `:idle-timeout`）；**绑定地址与端口范围不在这里**，见「沙箱」。

---

## 请求是一个 `HashMap`

```lisp
{:method    "POST"
 :path      "/api/users"
 :uri       "/api/users?page=2"
 :raw-query "page=2&q=ann"
 :query     {:page "2" :q "ann"}
 :headers   {:user-agent "curl/8" :authorization "Bearer ..."}
 :body      "{\"name\":\"x\"}"
 :host      "127.0.0.1:8080"
 :remote    "127.0.0.1:54321"
 :cookies   {:sid "abc"}
 :form      {:name "x"}}          ; 仅 x-www-form-urlencoded 时
```

值是普通字符串；`:headers` 键统一小写；重复键取最后一个；缺失的键不出现。

::: tip 为什么是快照
底层框架的请求上下文是**池化复用**的 —— handler 返回后对象会被下一个请求改写。
直接把上下文交给 Lisp，一句 `(define saved req)` 之后读到的就是**别的请求的数据**。
所以这里一律复制。每请求多几次分配，但这是正确性问题。
:::

---

## 响应：三种返回值

| handler 返回 | 响应 |
|---|---|
| `HashMap` | 按 `:status` / `:headers` / `:body`，缺省 `200` + `text/plain; charset=utf-8` |
| 字符串 | `200` + `text/plain; charset=utf-8` |
| `nil` | `204 No Content` |
| `Error` | **`500`**，**不影响后续请求** |

```lisp
(lambda (req)
  (if (string=? (hashmap-get req :path) "/health")
    "ok"
    {:status 404 :body "not found"}))
```

一个 handler 写错只影响那一次请求，服务不崩 —— 这是服务端的底线。

---

## 路由自己写

只给一个 handler：

```lisp
(define (dispatch req)
  (let [path (hashmap-get req :path)
        m    (hashmap-get req :method)]
    (cond
      (and (string=? m "GET") (string=? path "/"))       (index-page req)
      (and (string=? m "GET") (string=? path "/health")) "ok"
      (else {:status 404 :body "not found"}))))
```

要更顺手就写个 `defmacro route` —— 与「axios 那层留给 Lisp 宏」是同一条原则。

搭配 [JSON](/ext/json) 处理请求体：

```lisp
(cangjie::json-get (hashmap-get req :body) "user.name")
```

---

## 沙箱

| 规则 | 说明 |
|---|---|
| **默认只绑 `127.0.0.1`** | 对外监听必须宿主显式改 |
| **默认拒绝特权端口**（< 1024） | 由 `portRange` 限制 |
| **默认不注册** | 必须 `withHttpd()` |

```cangjie
// 对外提供服务
let interp = LispInterpreter([withHttpd(bind: "0.0.0.0", portRange: (8080, 8090))])
```

**脚本能选的只有端口，绑定地址是宿主决策。**

---

## 不做的事

- **TLS / HTTP/2 / WebSocket**：底层框架不做。在**前置 LB**（nginx / Caddy / Traefik）
  终结 TLS + h2，后端走 HTTP/1.1 —— 这是 fasthttp 生态的标准做法；
- **静态文件、multipart 上传封装**：底层有能力，本模块首版不包。

---

## 实现说明

::: details 每请求一个求值器
底层框架是「每连接一协程」，多个连接会并发调 handler。而求值器有大量「当前正在求值什么」
的可变字段，跨协程共享必错 —— 所以每个请求用新的求值器（继承宿主配置），共享同一棵环境树。
服务启动时对根环境标记一次「跨协程共享」，否则并发写入会损坏绑定表。
:::

::: details 三种后端下都一致
字节码后端下，handler 可能是个编译闭包 —— 它内含 VM 内部函数 id，跨求值器调不通。
调用时会自动降级成等价的 AST 闭包（参数名 + AST 体 + 捕获环境是纯数据），
所以 AST / BC / JIT 三种后端行为一致。
:::

::: details 起服务会等到端口真的可连
否则调用方紧接着发请求会连不上，而绑定失败会藏进后台协程里、脚本却以为服务起来了。
:::
