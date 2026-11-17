package ystyle::xisp_httpd

import ystyle::xisp.types.*
import ystyle::tachyon.*

import std.collection.ArrayList
import std.collection.HashMap as StdHashMap
import std.net.*

/**
 * `RequestCtx` → Lisp 侧的 `HashMap` 快照
 *
 * ## 为什么必须复制，不能直接暴露 ctx
 *
 * tachyon 的 `RequestCtx` 是**池化复用**的（fasthttp 风格）：handler 返回后对象被回收
 * 并交给下一个请求。Lisp 侧只要有一句 `(define saved req)` 或闭包捕获，之后读到的
 * 就是**别的请求的数据** —— 这种 bug 极难查。
 *
 * 所以这里把需要的东西全部转成 Lisp 值。代价是每请求若干次分配，但 Lisp 层本来就在
 * 解释执行，省这点没有意义；而「ctx 被复用」是正确性问题。
 */
func requestToLisp(ctx: RequestCtx): LispValue {
    let m = StdHashMap<String, LispValue>()

    m["method"] = LispValue.Str(ctx.method().toStr())
    m["path"] = LispValue.Str(ctx.path().toStr())
    m["uri"] = LispValue.Str(ctx.requestURI().toStr())
    m["host"] = LispValue.Str(ctx.host().toStr())

    // raw-query：从不含 query 的 URI 里切出 '?' 之后的部分（保留原始编码）
    let rawUri = ctx.requestURI().toStr()
    match (rawUri.indexOf("?")) {
        case Some(q) =>
            m["raw-query"] = LispValue.Str(rawUri[q + 1..])
        case None => ()
    }

    // query：已解码的键值；重复键后者覆盖（要全部值用 :raw-query 自己拆）
    let qa = ctx.queryArgs()
    let n = qa.count()
    if (n > 0) {
        let qmap = StdHashMap<String, LispValue>()
        for (i in 0..n) {
            qmap[qa.keyAt(i).toStr()] = LispValue.Str(qa.valueAt(i).toStr())
        }
        m["query"] = LispValue.HashMap(qmap)
    }

    // headers：键统一小写（与 xisp_net 的响应头口径一致）
    // visitHeaders 覆盖特殊头（Host / Content-Length / Content-Type / User-Agent /
    // Connection）与普通头，等价于 fasthttp 的 VisitAll
    let hmap = StdHashMap<String, LispValue>()
    ctx.visitHeaders({ name, value =>
        hmap[name.toStr().toAsciiLower()] = LispValue.Str(value.toStr())
        true
    })
    m["headers"] = LispValue.HashMap(hmap)

    let body = ctx.body().toStr()
    if (!body.isEmpty()) {
        m["body"] = LispValue.Str(body)
    }

    match (ctx.remoteAddress()) {
        case Some(addr) => m["remote"] = LispValue.Str(addr.toString())
        case None => ()
    }

    // cookies：从遍历到的 Cookie 头自己拆。
    // 必须走 visitHeaders 的结果而不是 ctx.headerGet("Cookie") —— 后者只查普通头表，
    // 而 Cookie 在 tachyon 里是 HeaderTable 的专用字段。tachyon 0.2.2 起
    // visitHeaders 会带上它（0.2.1 漏了，本模块因此要求 >= 0.2.2）。
    match (hmap.get("cookie")) {
        case Some(LispValue.Str(rawStr)) =>
            if (!rawStr.isEmpty()) {
                let jar = StdHashMap<String, LispValue>()
                for (pair in rawStr.split(";")) {
                    let s = pair.trimAscii()
                    if (s.isEmpty()) {
                        continue
                    }
                    match (s.indexOf("=")) {
                        case Some(eq) =>
                            let k = s[..eq].trimAscii()
                            let v = s[eq + 1..].trimAscii()
                            if (!k.isEmpty()) {
                                jar[k] = LispValue.Str(v)
                            }
                        case None => ()
                    }
                }
                if (jar.size > 0) {
                    m["cookies"] = LispValue.HashMap(jar)
                }
            }
        case _ => ()
    }

    // form：只对表单类 content-type 解析，避免把 JSON body 误当表单
    if (isFormContentType(hmap)) {
        let pa = ctx.postArgs()
        let pn = pa.count()
        if (pn > 0) {
            let fmap = StdHashMap<String, LispValue>()
            for (i in 0..pn) {
                fmap[pa.keyAt(i).toStr()] = LispValue.Str(pa.valueAt(i).toStr())
            }
            m["form"] = LispValue.HashMap(fmap)
        }
    }

    LispValue.HashMap(m)
}

/** content-type 是不是表单类 */
func isFormContentType(headers: StdHashMap<String, LispValue>): Bool {
    match (headers.get("content-type")) {
        case Some(LispValue.Str(ct)) =>
            let lower = ct.toAsciiLower()
            lower.startsWith("application/x-www-form-urlencoded")
        case _ => false
    }
}

/**
 * 把 handler 的返回值写成响应
 *
 * | 返回 | 响应 |
 * |---|---|
 * | `HashMap` | 按 `:status` / `:headers` / `:body`，缺省 200 + `text/plain; charset=utf-8` |
 * | 字符串 | `200` + `text/plain; charset=utf-8` |
 * | `nil` | `204 No Content` |
 * | `Error` | `500`，**错误不进连接、不影响后续请求** |
 * | 其它 | `toString()` 当字符串，`200` |
 */
func writeResponse(ctx: RequestCtx, result: LispValue): Unit {
    match (result) {
        case LispValue.HashMap(m) =>
            var status: UInt16 = 200
            match (m.get("status")) {
                case Some(LispValue.Int(n)) =>
                    if (n > 0 && n < 600) {
                        status = UInt16(n)
                    }
                case _ => ()
            }
            match (m.get("headers")) {
                case Some(LispValue.HashMap(hs)) =>
                    for ((k, v) in hs) {
                        ctx.setHeader(k, headerText(v))
                    }
                case _ => ()
            }
            ctx.setStatusCode(status)
            match (m.get("body")) {
                case Some(b) =>
                    if (status != 204 && status != 304) {
                        ctx.setBodyString(bodyText(b))
                    }
                case None => ()
            }
        case LispValue.Str(s) =>
            ctx.successString("text/plain; charset=utf-8", s)
        case Nil =>
            ctx.setStatusCode(204)
        case LispValue.Error(e) =>
            // 服务端底线：一个 handler 写错不能让整条连接崩，也不能影响别的请求
            ctx.error("500 Internal Server Error\n\n" + e.message, 500)
        case other =>
            ctx.successString("text/plain; charset=utf-8", other.toString())
    }
}

/** 响应头的值：字符串原样，其它走 toString */
func headerText(v: LispValue): String {
    match (v) {
        case LispValue.Str(s) => s
        case _ => v.toString()
    }
}

/** 响应体的值：字符串原样，其它走 toString（方便直接返回 map 当 JSON 片段） */
func bodyText(v: LispValue): String {
    match (v) {
        case LispValue.Str(s) => s
        case _ => v.toString()
    }
}
