package ystyle::xisp_net

import ystyle::xisp.types.*
import ystyle::xisp.core.eval.*

import stdx.net.http.*
import stdx.net.tls.*

import std.collection.ArrayList
import std.collection.HashMap as StdHashMap

/**
 * 网络能力（能力域 `net`）：HTTP 客户端
 *
 * 设计见 `docs/plans/2026-11-17-net-design.md`。
 *
 * ## 分层：原语层是 fetch 形态，axios 那层留给 Lisp 宏
 *
 * 原语层只做"发请求、给回完整响应"：
 *
 * ```lisp
 * (let [{:status code :body text} (cangjie::http-get "https://api.example.com/u")]
 *   (if (= code 200) (use text) (handle code)))
 * ```
 *
 * baseURL 继承、拦截器链、自动重试这类"有状态配置对象"的能力**不在这里实现** ——
 * 那是宏系统该干的（`HashMap` 一等值 + `let` 解构 + `defmacro` 已经够用）。
 *
 * ## 错误分两层
 *
 * | 情况 | 结果 |
 * |---|---|
 * | DNS / 连接拒绝 / 超时 / TLS 握手失败 | `Error(NetworkError)` —— 请求**没完成** |
 * | HTTP 4xx / 5xx | **正常数据流**，看 `status` 字段 —— 这是一次成功完成的交互 |
 *
 * 4xx/5xx 不报错是刻意的：404 与 500 是预期内的业务信息，不该打断求值链。
 *
 * ## 实现要点
 *
 * - **每次请求新建 `Client`，用完关闭**。`Client` 的线程安全性未确认，而 `spawn`
 *   之后多个线程会并发调网络函数；共享一个实例等于埋一个竞态。代价是每次多一次
 *   TCP/TLS 握手，对"脚本发几个请求"这个场景可接受。将来要做连接池，
 *   需要先确认线程安全并按线程分配。
 * - **HTTPS 必须显式配 `TlsClientConfig`**，否则 stdx 报
 *   `TLS must be configured when HTTPS requests are sent`。
 * - **响应体有大小上限**（默认 8 MiB）。stdx 的客户端是整包缓冲的，不设限时
 *   一个巨大响应就能把进程 OOM。
 * - **代理走 stdx 的默认行为**（读 `http_proxy` / `https_proxy` 环境变量），
 *   本模块不提供 `:proxy` 选项。
 */

/** 网络能力的配置（沙箱门控 + 体积上限） */
public class NetConfig {
    /**
     * 域名白名单：入参是 URL 的 host（不含协议、端口、路径）。
     *
     * `None` 表示**全部拒绝** —— 安全默认，与"空白名单"等价。
     * 宿主必须显式给一个才放行。
     */
    public var urlChecker: ?(String) -> Bool = None

    /** 响应体上限（字节），默认 8 MiB */
    public var maxBodyBytes: Int64 = 8 * 1024 * 1024

    public init() {}
}

/** 从 URL 里取 host（不含 userinfo / 端口 / 路径）；解析不出返回 None */
func hostOf(url: String): ?String {
    // 先剥协议
    var rest = url
    let schemeEnd = url.indexOf("://")
    match (schemeEnd) {
        case Some(i) => rest = url[i + 3..]
        case None => return None
    }
    // 剥 userinfo
    if (let Some(at) <- rest.indexOf("@")) {
        rest = rest[at + 1..]
    }
    // 剥路径
    var hostPort = rest
    if (let Some(slash) <- rest.indexOf("/")) {
        hostPort = rest[..slash]
    }
    if (hostPort.isEmpty()) {
        return None
    }
    // 剥端口（IPv6 的 [::1]:80 形式保留方括号内容）
    if (hostPort.startsWith("[")) {
        if (let Some(close) <- hostPort.indexOf("]")) {
            return Some(hostPort[..close + 1])
        }
        return Some(hostPort)
    }
    if (let Some(colon) <- hostPort.indexOf(":")) {
        hostPort = hostPort[..colon]
    }
    if (hostPort.isEmpty()) {
        return None
    }
    Some(hostPort)
}

/**
 * 域名白名单匹配规则：
 *
 * - 精确匹配 `example.com`
 * - 后缀通配 `*.example.com` 匹配 `a.example.com`、`a.b.example.com`，
 *   **不匹配** `example.com` 本身（要连主域就把它也写进白名单）
 * - 匹配目标是 **host**，不是整条 URL —— 避免 `https://evil.com/?x=https://good.com` 这类前缀绕过
 */
func hostAllowed(host: String, patterns: Array<String>): Bool {
    for (p in patterns) {
        if (p.startsWith("*.")) {
            let suffix = p[1..]
            if (host.endsWith(suffix) && host.size > suffix.size) {
                return true
            }
        } else if (host == p) {
            return true
        }
    }
    false
}

/** 把一个 Lisp 值转成用于请求头的字符串 */
func headerValue(v: LispValue): String {
    match (v) {
        case LispValue.Str(s) => s
        case _ => v.toString()
    }
}

/** 从 opts 里取整数选项 */
func intOpt(opts: LispValue, key: String): ?Int64 {
    match (opts) {
        case LispValue.HashMap(m) =>
            match (m.get(key)) {
                case Some(LispValue.Int(n)) => Some(n)
                case _ => None
            }
        case _ => None
    }
}

/** 从 opts 里取字符串选项 */
func strOpt(opts: LispValue, key: String): ?String {
    match (opts) {
        case LispValue.HashMap(m) =>
            match (m.get(key)) {
                case Some(LispValue.Str(s)) => Some(s)
                case _ => None
            }
        case _ => None
    }
}

/** 从 opts 里取 HashMap 选项 */
func mapOpt(opts: LispValue, key: String): ?StdHashMap<String, LispValue> {
    match (opts) {
        case LispValue.HashMap(m) =>
            match (m.get(key)) {
                case Some(LispValue.HashMap(inner)) => Some(inner)
                case _ => None
            }
        case _ => None
    }
}

/** 把异常转成网络错误值 */
func netError(what: String, e: Exception): LispValue {
    LispValue.Error(XispError(ErrorType.NetworkError, "${what}: ${e.message}"))
}

/**
 * 发一次请求并把响应转成 `HashMap`。
 *
 * 返回 `{:status :ok :headers :body :url}`；传输层失败返回 `Error(NetworkError)`。
 */
func doRequest(method: String, url: String, opts: LispValue, cfg: NetConfig): LispValue {
    // ---- 沙箱门控：先于任何网络动作 ----
    let host = match (hostOf(url)) {
        case Some(h) => h
        case None =>
            return LispValue.Error(XispError(ErrorType.TypeError,
                "invalid URL (expected scheme://host...): ${url}"))
    }
    match (cfg.urlChecker) {
        case Some(checker) =>
            if (!checker(host)) {
                return LispValue.Error(XispError(ErrorType.NetworkError,
                    "url not allowed: ${host}"))
            }
        case None =>
            // 没配白名单 = 全部拒绝（安全默认，不是"全部允许"）
            return LispValue.Error(XispError(ErrorType.NetworkError,
                "url not allowed: ${host} (no urlChecker configured)"))
    }

    // ---- 选项 ----
    let timeoutMs = match (intOpt(opts, "timeout")) {
        case Some(n) => n
        case None => 30000
    }
    let maxBody = match (intOpt(opts, "max-body-bytes")) {
        case Some(n) => n
        case None => cfg.maxBodyBytes
    }
    let body = strOpt(opts, "body")

    try {
        var builder = ClientBuilder()
            .tlsConfig(TlsClientConfig())
            // 首版不跟随重定向：3xx 原样返回，避免"每跳都要校验 host"这件事
            // 在没校验的情况下被悄悄绕过
            .autoRedirect(false)
            .readTimeout(Duration.millisecond * timeoutMs)
            .writeTimeout(Duration.millisecond * timeoutMs)
        let client = builder.build()
        var reqBuilder = HttpRequestBuilder().method(method).url(url)
        match (mapOpt(opts, "headers")) {
            case Some(headers) =>
                for ((k, v) in headers) {
                    reqBuilder = reqBuilder.header(k, headerValue(v))
                }
            case None => ()
        }
        match (body) {
            case Some(b) => reqBuilder = reqBuilder.body(b)
            case None => ()
        }
        let resp = client.send(reqBuilder.build())
        let status = Int64(resp.status)

        // ---- 读响应体，带上限 ----
        let buf = Array<UInt8>(8192, repeat: 0)
        let collected = ArrayList<UInt8>()
        var overflow = false
        while (true) {
            let n = resp.body.read(buf)
            if (n <= 0) {
                break
            }
            if (Int64(collected.size) + n > maxBody) {
                overflow = true
                break
            }
            for (i in 0..n) {
                collected.add(buf[i])
            }
        }
        client.close()
        if (overflow) {
            return LispValue.Error(XispError(ErrorType.NetworkError,
                "response body exceeds ${maxBody} bytes"))
        }

        // ---- 响应头 ----
        // `HttpHeaders <: Iterable<(String, Collection<String>)>`：同名可以有多个值
        // （典型是 Set-Cookie）。单值给字符串、多值给列表，便于直接用 `hashmap-get`。
        let headers = StdHashMap<String, LispValue>()
        for ((k, vs) in resp.headers) {
            let key = k.toAsciiLower()
            let arr = ArrayList<LispValue>()
            for (v in vs) {
                arr.add(LispValue.Str(v))
            }
            if (arr.size == 1) {
                headers[key] = arr[0]
            } else {
                headers[key] = list(arr)
            }
        }

        let out = StdHashMap<String, LispValue>()
        out["status"] = LispValue.Int(status)
        out["ok"] = LispValue.Boolean(status >= 200 && status < 300)
        out["headers"] = LispValue.HashMap(headers)
        out["body"] = LispValue.Str(String.fromUtf8(collected.toArray()))
        out["url"] = LispValue.Str(url)
        LispValue.HashMap(out)
    } catch (e: Exception) {
        netError("request failed (${method} ${url})", e)
    }
}

/** 取第一个参数为字符串；不是则给错误 */
func firstStr(args: ArrayList<LispValue>, fn: String): ?String {
    if (args.size < 1) {
        return None
    }
    match (args[0]) {
        case LispValue.Str(s) => Some(s)
        case _ => None
    }
}

/**
 * 注册网络能力域的符号（`opts` 可选，缺省为空 map）
 */
public func registerNet(env: Environment, cfg: NetConfig): Unit {
    // (cangjie::http-request method url [opts])
    env.define("cangjie::http-request", NativeFunc({ args =>
        if (args.size < 2 || args.size > 3) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "http-request expects 2 or 3 arguments: method url [opts]"))
        }
        let method = match (args[0]) {
            case LispValue.Str(s) => s
            case _ => return LispValue.Error(XispError(ErrorType.TypeError,
                "http-request: method must be a string"))
        }
        let url = match (args[1]) {
            case LispValue.Str(s) => s
            case _ => return LispValue.Error(XispError(ErrorType.TypeError,
                "http-request: url must be a string"))
        }
        let opts: LispValue = if (args.size == 3) { args[2] } else { LispValue.HashMap(StdHashMap<String, LispValue>()) }
        doRequest(method.toAsciiUpper(), url, opts, cfg)
    }))

    // 便利层：http-get / http-post / http-put / http-delete / http-patch / http-head
    // http-post 的第二个参数是 body（字符串），也可用 opts 里的 :body
    env.define("cangjie::http-get", NativeFunc({ args =>
        if (args.size > 2) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "http-get expects 1 or 2 arguments: url [opts]"))
        }
        match (firstStr(args, "http-get")) {
            case Some(url) =>
                let opts: LispValue = if (args.size == 2) { args[1] } else { LispValue.HashMap(StdHashMap<String, LispValue>()) }
                doRequest("GET", url, opts, cfg)
            case None => LispValue.Error(XispError(ErrorType.TypeError,
                "http-get: url must be a string"))
        }
    }))

    env.define("cangjie::http-post", NativeFunc({ args =>
        if (args.size < 2 || args.size > 3) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "http-post expects 2 or 3 arguments: url body [opts]"))
        }
        match (firstStr(args, "http-post")) {
            case Some(url) =>
                let body = match (args[1]) {
                    case LispValue.Str(s) => s
                    case _ => return LispValue.Error(XispError(ErrorType.TypeError,
                        "http-post: body must be a string"))
                }
                // body 已由第二参数给出；opts 里若也写了 :body 以第二参数为准
                var opts: LispValue = if (args.size == 3) { args[2] } else { LispValue.HashMap(StdHashMap<String, LispValue>()) }
                match (opts) {
                    case LispValue.HashMap(m) =>
                        let copy = StdHashMap<String, LispValue>()
                        for ((k, v) in m) {
                            copy[k] = v
                        }
                        copy["body"] = LispValue.Str(body)
                        opts = LispValue.HashMap(copy)
                    case _ =>
                        return LispValue.Error(XispError(ErrorType.TypeError,
                            "http-post: opts must be a hashmap"))
                }
                doRequest("POST", url, opts, cfg)
            case None => LispValue.Error(XispError(ErrorType.TypeError,
                "http-post: url must be a string"))
        }
    }))

    // 其余方法：与 http-get 同形（url [opts]）
    let simple = [
        ("cangjie::http-put", "PUT"),
        ("cangjie::http-delete", "DELETE"),
        ("cangjie::http-patch", "PATCH"),
        ("cangjie::http-head", "HEAD"),
    ]
    for ((sym, method) in simple) {
        env.define(sym, NativeFunc({ args =>
            if (args.size > 2) {
                return LispValue.Error(XispError(ErrorType.ArityMismatch,
                    "${method.toAsciiLower()} expects 1 or 2 arguments: url [opts]"))
            }
            match (firstStr(args, method)) {
                case Some(url) =>
                    let opts: LispValue = if (args.size == 2) { args[1] } else { LispValue.HashMap(StdHashMap<String, LispValue>()) }
                    doRequest(method, url, opts, cfg)
                case None => LispValue.Error(XispError(ErrorType.TypeError,
                    "${method.toAsciiLower()}: url must be a string"))
            }
        }))
    }

    // (cangjie::url-encode s) —— query 拼接必须给，手拼必错
    env.define("cangjie::url-encode", NativeFunc({ args =>
        if (args.size != 1) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "url-encode expects 1 argument: string"))
        }
        match (args[0]) {
            case LispValue.Str(s) => LispValue.Str(urlEncode(s))
            case _ => LispValue.Error(XispError(ErrorType.TypeError,
                "url-encode: argument must be a string"))
        }
    }))
}

/**
 * RFC 3986 的 percent-encoding（保留 unreserved 与保留字符原样）
 *
 * 只编码"不安全"的字节：空格、`"` `<` `>` `#` `%` `{` `}` `|` `\` `^` `~` `[` `]` `` ` ``、
 * 控制字符与非 ASCII。这是 query 值的口径（`&` `=` 等结构字符也编码，
 * 因为它们是**值**的一部分）。
 */
func urlEncode(s: String): String {
    let sb = StringBuilder()
    for (b in s.toArray()) {
        let isUnreserved = (b >= b'a' && b <= b'z') || (b >= b'A' && b <= b'Z') ||
            (b >= b'0' && b <= b'9') || b == b'-' || b == b'_' || b == b'.' || b == b'~'
        if (isUnreserved) {
            sb.append(String.fromUtf8([b]))
        } else {
            let hexChars = ["0", "1", "2", "3", "4", "5", "6", "7",
                            "8", "9", "A", "B", "C", "D", "E", "F"]
            sb.append("%")
            sb.append(hexChars[Int64(b) / 16])
            sb.append(hexChars[Int64(b) % 16])
        }
    }
    sb.toString()
}

/**
 * 网络能力域。
 *
 * `escapable: true`（能触达外部世界）、`hostPolicy: true`（碰宿主网络资源）
 * —— `presetPure` 按属性推导会排除它。
 */
public func netDomain(cfg: NetConfig): LispCapability {
    SimpleCapability("net",
        ["cangjie::http-request", "cangjie::http-get", "cangjie::http-post",
         "cangjie::http-put", "cangjie::http-delete", "cangjie::http-patch",
         "cangjie::http-head", "cangjie::url-encode"],
        { ctx => registerNet(ctx.env, cfg) },
        escapable: true, hostPolicy: true,
        after: ["base"])
}
