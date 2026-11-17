# ystyle::xisp_net

星枢（Xisp）网络能力：HTTP 客户端。

- **包**：`ystyle::xisp_net`
- **依赖**：`ystyle::xisp`（核心）+ `stdx.net.http` / `stdx.net.tls`（**需要 stdx 环境**）
- **设计**：`docs/plans/2026-11-17-net-design.md`

## ⚠️ 构建前提：需要 stdx

本模块用 stdx 的 HTTP 客户端，构建前**必须**配好 stdx：

```shell
eval "$(cjvs env zsh)" && eval "$(cjvs stdx-env zsh)"
```

**缺 `CANGJIE_STDX_PATH` 时 cjpm 的报错指向错误方向** —— 它会说
"cjpm.toml 里没加依赖"，而真正原因是环境变量不存在。别被它带偏。

这也是**唯一**需要平台相关配置的成员（`bin-dependencies` / `link-option`）；
`core` 与其它成员仍然零外部依赖。

### 与 `--static` 的实测结论

曾经担心"TLS 要 OpenSSL 3 动态库，而可执行产物用 `--static`"会冲突。
**实测不冲突**：stdx 的 OpenSSL 是**运行时动态加载**的（`libcangjie-dynamicLoader-opensslFFI.a`），
链接期没有 `libssl.so` 依赖。实测一个 `--static` 可执行 + 真实 HTTPS 请求（`https://httpbin.org/status/200`）
构建与运行都成功，`ldd` 只显示基础系统库。

## 用法

```cangjie
import ystyle::xisp.*
import ystyle::xisp_net.*

let interp = LispInterpreter([withNetwork(["api.example.com", "*.example.org"])])
```

**默认不注册** —— 网络比文件读写敏感得多，必须显式给白名单。

### 白名单规则

| 写法 | 匹配 | 不匹配 |
|---|---|---|
| `api.example.com` | `api.example.com` | `evil.example.com` |
| `*.example.org` | `a.example.org`、`a.b.example.org` | `example.org` 本身、`a.example.org.evil.net` |

- 匹配目标是 URL 的 **host**（不含协议、端口、路径），避免 `https://evil.com/?x=https://good.com` 这类前缀绕过；
- **后缀通配不匹配主域本身** —— 要连主域就把 `example.org` 也写进去；
- **`withNetwork([])` = 全部拒绝**（不是"允许所有"）。空数组与不启用等价。

## 符号

| 符号 | 签名 |
|---|---|
| `cangjie::http-request` | `(method url [opts])` |
| `cangjie::http-get` | `(url [opts])` |
| `cangjie::http-post` | `(url body [opts])` |
| `cangjie::http-put` / `http-delete` / `http-patch` / `http-head` | `(url [opts])` |
| `cangjie::url-encode` | `(s)` —— percent-encoding，query 拼接必须用 |

**原语层是 fetch 形态，不是 axios**：只做"发请求、给回完整响应"。baseURL 继承、拦截器链、
自动重试这类「有状态配置对象」的能力**留给 Lisp 宏**（`HashMap` 一等值 + `let` 解构 +
`defmacro` 已经够用）。原语层保持薄、正交。

## 请求选项 `opts`

`opts` 就是普通的 `HashMap`（map 字面量已支持）：

```lisp
(cangjie::http-get "https://api.example.com/u"
                   {:timeout 5000
                    :headers {:accept "application/json"
                              :authorization "Bearer xxx"}})
```

| key | 类型 | 默认 | 说明 |
|---|---|---|---|
| `:timeout` | 整数（毫秒） | 30000 | 读/写超时（stdx 没有单独的连接超时） |
| `:headers` | `HashMap` | `{}` | 请求头 |
| `:body` | 字符串 | — | 请求体（`http-post` 也可用第二参数传） |
| `:max-body-bytes` | 整数 | 8 MiB | 响应体上限，超过报错 |

## 响应

响应是一个 `HashMap`：

```lisp
{:status  200
 :ok      true
 :headers {:content-type "application/json"}
 :body    "{\"name\":\"x\"}"
 :url     "https://api.example.com/u"}
```

- `:status` 整数状态码；`:ok` 是 `200..299`；
- `:headers` 的键统一小写；**同名多值给列表**（典型是 `set-cookie`）；
- `:body` 是字符串（按 UTF-8 解码）。

配合 `let` 解构：

```lisp
(let [{:status code :body text} (cangjie::http-get "https://api.example.com/u")]
  (if (= code 200) (use text) (handle code)))
```

## 错误语义：分两层

| 情况 | 结果 |
|---|---|
| DNS 失败 / 连接拒绝 / 超时 / TLS 握手失败 | `Error(NetworkError)` —— 请求**没完成** |
| HTTP **4xx / 5xx** | **正常数据流**，看 `status` |
| 域名不在白名单 | `Error(NetworkError, "url not allowed: …")` |
| URL 缺协议 / 参数类型错 | `Error(TypeError)` |
| 参数个数错 | `Error(ArityMismatch)` |
| 响应体超上限 | `Error(NetworkError, "response body exceeds …")` |

**4xx/5xx 不报错是刻意的**：404 与 500 是预期内的业务信息，不该打断求值链。
这与 fetch 的语义一致，也是"能全程函数式处理"的前提。

`NetworkError` 是 `ErrorType` 的第 9 个变体（2026-11-17 新增）。

## 已知边界

- **每次请求新建 `Client`，用完关闭**。`Client` 的线程安全性未确认，而 `spawn` 之后
  多线程会并发调网络函数；共享实例等于埋一个竞态。代价是每次多一次 TCP/TLS 握手 ——
  对"脚本发几个请求"可接受。要做连接池需先确认线程安全并按线程分配。
- **不跟随重定向**：`3xx` 原样返回。跟随要"每跳都校验 host"，首版宁可不跟。
- **不提供 `:proxy` 选项**：走 stdx 的默认行为（读 `http_proxy` / `https_proxy` 环境变量）。
- **响应体整包进内存**（受 `:max-body-bytes` 限制）。大文件下载需要流式接口，尚未提供。

## 测试

大部分用例**不依赖外网**（URL 解析、白名单匹配、url-encode、沙箱拒绝、参数错误），
默认全跑。真实 HTTP/HTTPS 请求只在 `XISP_NET_TESTS=1` 时跑：

```shell
XISP_NET_TESTS=1 cjpm test --filter '*NetUnitTest*'
```

有代理时"传输层失败"那个用例会跳过 —— 请求被代理接管后拿到的是 502（正常响应），
测不到直连失败，这是环境差异不是实现问题。

## 许可

MIT。依赖的 stdx 为其各自的许可。
