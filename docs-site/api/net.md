# 网络（HTTP 客户端）

::: warning 来自可选模块，且需要 stdx
本页的函数由**可选模块** `ystyle::xisp_net` 提供，**核心不含**。它依赖 stdx 的 HTTP 客户端，
构建前必须配好 stdx 环境：

```shell
eval "$(cjvs env zsh)" && eval "$(cjvs stdx-env zsh)"
```

```toml
[dependencies]
  "ystyle::xisp" = "0.6.0"
  "ystyle::xisp_net" = "0.6.0"
```

这也是工作区里**唯一**需要平台相关配置的模块；`core` 与其它模块仍然零外部依赖。
:::

## 启用：必须给白名单

```cangjie
import ystyle::xisp.*
import ystyle::xisp_net.*

let interp = LispInterpreter([withNetwork(["api.example.com", "*.example.org"])])
```

网络**默认不注册** —— 比文件读写敏感得多，必须显式放行。

| 白名单写法 | 匹配 | 不匹配 |
|---|---|---|
| `api.example.com` | `api.example.com` | `evil.example.com` |
| `*.example.org` | `a.example.org`、`a.b.example.org` | `example.org` 本身、`a.example.org.evil.net` |

::: danger `withNetwork([])` = 全部拒绝
空数组**不等于**"允许所有"，而是"什么都不允许"。
"没配白名单"绝不能被理解成放行 —— 这是安全默认。
:::

---

## 符号

| 符号 | 签名 |
|---|---|
| `cangjie::http-request` | `(method url [opts])` |
| `cangjie::http-get` | `(url [opts])` |
| `cangjie::http-post` | `(url body [opts])` |
| `cangjie::http-put` / `http-delete` / `http-patch` / `http-head` | `(url [opts])` |
| `cangjie::url-encode` | `(s)` |

```lisp
(let [{:status code :body text}
      (cangjie::http-get "https://api.example.com/users")]
  (if (= code 200) text "failed"))
```

**这里是 fetch 形态，不是 axios**：原语层只做"发请求、给回完整响应"。
baseURL 继承、拦截器链、自动重试这类能力用 Lisp 宏自己写 —— `HashMap` 一等值 +
`let` 解构 + `defmacro` 已经够用，不该固化在桥接层里。

---

## 请求选项

`opts` 就是普通的 `HashMap`：

```lisp
(cangjie::http-post "https://api.example.com/users"
                    "{\"name\":\"x\"}"
                    {:timeout 5000
                     :headers {:content-type "application/json"
                               :authorization "Bearer xxx"}})
```

| key | 类型 | 默认 | 说明 |
|---|---|---|---|
| `:timeout` | 整数（毫秒） | 30000 | 读/写超时 |
| `:headers` | `HashMap` | `{}` | 请求头 |
| `:body` | 字符串 | — | 请求体（`http-post` 也可用第二参数传） |
| `:max-body-bytes` | 整数 | 8 MiB | 响应体上限 |

---

## 响应

```lisp
{:status  200
 :ok      true
 :headers {:content-type "application/json"}
 :body    "{\"name\":\"x\"}"
 :url     "https://api.example.com/users"}
```

- `:status` 状态码，`:ok` 表示 `200..299`；
- `:headers` 的键统一小写，**同名多值给列表**（典型是 `set-cookie`）；
- `:body` 是按 UTF-8 解码的字符串。

配合 [JSON](/api/json) 使用：

```lisp
(cangjie::json-get (hashmap-get (cangjie::http-get url) "body") "data.0.name")
```

---

## 错误分两层

| 情况 | 结果 |
|---|---|
| DNS 失败 / 连接拒绝 / 超时 / TLS 握手失败 | `[NetworkError]` —— 请求**没完成** |
| **HTTP 4xx / 5xx** | **正常数据流**，看 `status` |
| 域名不在白名单 | `[NetworkError] url not allowed: …` |
| URL 缺协议 / 参数类型错 | `[TypeError]` |
| 响应体超上限 | `[NetworkError] response body exceeds …` |

```lisp
;; 404 不是错误，是"请求成功了，服务端说没有"
(let [{:status code} (cangjie::http-get "https://api.example.com/nope")]
  code)                          ; => 404
```

**4xx/5xx 不报错是刻意的**：那是预期内的业务信息，不该打断求值链。
只有"请求根本没完成"才是 `NetworkError`。

---

## 已知边界

- **每次请求新建连接**：`Client` 的线程安全性未确认，而 `spawn` 之后多线程会并发调网络；
  共享实例等于埋一个竞态。代价是每次多一次 TCP/TLS 握手。
- **不跟随重定向**：`3xx` 原样返回（跟随要求每跳都校验域名，首版宁可不跟）。
- **不提供代理选项**：走 stdx 默认行为，读 `http_proxy` / `https_proxy` 环境变量。
- **响应体整包进内存**（受 `:max-body-bytes` 限制），暂不支持流式下载。

---

## 实现说明

::: details 为什么 `--static` 与 OpenSSL 不冲突
曾担心"TLS 要 OpenSSL 3 动态库，而可执行产物用 `--static`"。实测不冲突：
stdx 的 OpenSSL 是**运行时动态加载**的，链接期没有 `libssl.so` 依赖。
一个 `--static` 可执行发真实 HTTPS 请求，构建与运行都成功。
:::

::: details HTTPS 必须显式配 TLS
`ClientBuilder()` 默认不带 TLS 配置，发 HTTPS 会报
`TLS must be configured when HTTPS requests are sent`。
本模块内部统一配 `TlsClientConfig()`，用户无需关心。
:::
