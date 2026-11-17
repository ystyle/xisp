package ystyle::xisp_net

import ystyle::xisp.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.core.eval.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import std.env.*

/**
 * 网络能力的测试
 *
 * ## 测试纪律：默认**不依赖外网**
 *
 * 真实请求需要外网与代理，网络抖动会让 CI 假红。所以：
 * - 所有能本地验证的（URL 解析、白名单匹配、url-encode、沙箱拒绝、参数错误）默认全跑；
 * - 真实 HTTP/HTTPS 请求只在 `XISP_NET_TESTS=1` 时跑。
 *
 * ```shell
 * XISP_NET_TESTS=1 cjpm test --filter '*NetLive*'
 * ```
 */
@Test
class NetUnitTest {
    /** 默认不跑真实网络；显式开关 */
    func liveEnabled(): Bool {
        match (getVariable("XISP_NET_TESTS")) {
            case Some(v) => v == "1"
            case None => false
        }
    }

    func evalIn(opts: Array<InterpreterOption>, code: String): String {
        let it = LispInterpreter(opts)
        var r: LispValue = Nil
        for (e in Parser.parseFromString(code)) {
            r = it.evalExpr(e)
        }
        r.toString()
    }

    // ==================== URL 解析（白盒） ====================

    @TestCase
    func testHostOf() {
        println("\n=== URL → host 解析 ===")
        @Assert(hostOf("https://api.example.com/users") == Some("api.example.com"))
        @Assert(hostOf("http://example.com") == Some("example.com"))
        // 端口要剥掉
        @Assert(hostOf("https://example.com:8443/a/b") == Some("example.com"))
        // userinfo 要剥掉
        @Assert(hostOf("https://u:p@example.com/x") == Some("example.com"))
        // IPv6 保留方括号
        @Assert(hostOf("http://[::1]:8080/x") == Some("[::1]"))
        // 没有协议 → 解析不出
        @Assert(hostOf("example.com/a").isNone())
        @Assert(hostOf("").isNone())
        println("✓ 剥协议 / 端口 / userinfo / 路径；IPv6 保留方括号")
    }

    /**
     * 白名单匹配：**精确 or `*.` 后缀，不做隐式通配**
     *
     * 关键安全点：`*.example.com` **不匹配** `example.com` 本身 ——
     * 否则"允许子域"会被理解成"允许主域"，而主域往往是另一个入口。
     */
    @TestCase
    func testHostAllowed() {
        println("\n=== 白名单匹配规则 ===")
        let patterns = ["api.example.com", "*.example.org"]
        @Assert(hostAllowed("api.example.com", patterns))
        @Assert(!hostAllowed("evil.example.com", patterns))
        @Assert(hostAllowed("a.example.org", patterns))
        @Assert(hostAllowed("a.b.example.org", patterns))
        // 后缀通配不匹配主域本身
        @Assert(!hostAllowed("example.org", patterns))
        // 不做前缀意义上的通配
        @Assert(!hostAllowed("api.example.com.evil.net", patterns))
        // 空白名单 = 全不放行
        @Assert(!hostAllowed("anything.com", Array<String>()))
        println("✓ 精确 / 后缀通配 / 主域不自动放行 / 空白名单全拒")
    }

    /** `url-encode` 的 percent-encoding 口径 */
    @TestCase
    func testUrlEncode() {
        println("\n=== url-encode ===")
        @Assert(urlEncode("abc-_.~") == "abc-_.~")
        @Assert(urlEncode("a b") == "a%20b")
        @Assert(urlEncode("a&b=c") == "a%26b%3Dc")
        @Assert(urlEncode("/path?x") == "%2Fpath%3Fx")
        // 非 ASCII 按 UTF-8 字节编码：中 = E4 B8 AD
        @Assert(urlEncode("中") == "%E4%B8%AD")
        @Assert(urlEncode("") == "")
        println("✓ unreserved 原样；空格 / 结构字符 / 中文按 UTF-8 百分号编码")
    }

    // ==================== 装配与沙箱 ====================

    /** 八个符号都装上，且是纯计算的那一项也能用 */
    @TestCase
    func testSymbolsInstalled() {
        println("\n=== 八个网络符号可用 ===")
        let it = LispInterpreter([withQuietMode(), withNetwork(["api.example.com"])])
        for (name in ["cangjie::http-request", "cangjie::http-get", "cangjie::http-post",
                      "cangjie::http-put", "cangjie::http-delete", "cangjie::http-patch",
                      "cangjie::http-head", "cangjie::url-encode"]) {
            let r = it.eval("(procedure? ${name})")
            @Assert(r.toString() == "true")
        }
        // url-encode 不需要网络，可直接验证
        @Assert(it.eval(#"(cangjie::url-encode "a b")"#).toString() == "\"a%20b\"")
        println("✓ 八个符号均为可调用值；url-encode 可用")
    }

    /**
     * **沙箱：空白名单 = 全部拒绝**
     *
     * 这是本模块最重要的安全默认 —— "没配白名单"绝不能被当成"允许所有"。
     */
    @TestCase
    func testEmptyAllowlistDeniesAll() {
        println("\n=== 空白名单 = 全部拒绝 ===")
        let it = LispInterpreter([withQuietMode(), withNetwork(Array<String>())])
        let r = it.eval(#"(cangjie::http-get "https://api.example.com/x")"#)
        let s = r.toString()
        @Assert(s.contains("NetworkError"))
        @Assert(s.contains("url not allowed"))
        println("✓ 未配白名单时请求被拒（不是放行）")
    }

    /** 白名单外的域名被拒，且**在任何网络动作之前**就拒 */
    @TestCase
    func testHostNotInAllowlistDenied() {
        println("\n=== 白名单外域名被拒 ===")
        let it = LispInterpreter([withQuietMode(), withNetwork(["api.example.com"])])
        let r = it.eval(#"(cangjie::http-get "https://evil.example.net/x")"#)
        let s = r.toString()
        @Assert(s.contains("NetworkError"))
        @Assert(s.contains("evil.example.net"))
        // 后缀通配的主域不放行 —— 用"主域"这一侧验证，不碰"子域放行"那一侧：
        // 放行后真的会去连（DNS 解析要几十秒），通配规则本身由白盒 testHostAllowed 覆盖
        let it2 = LispInterpreter([withQuietMode(), withNetwork(["*.example.org"])])
        let r3 = it2.eval(#"(cangjie::http-get "http://example.org/x")"#)
        @Assert(r3.toString().contains("url not allowed"))
        println("✓ 白名单外被拒；后缀通配不放行主域（放行侧由白盒用例覆盖，避免真连导致慢）")
    }

    /** 参数错误 */
    @TestCase
    func testArgumentErrors() {
        println("\n=== 参数错误 ===")
        let it = LispInterpreter([withQuietMode(), withNetwork(["a.com"])])
        @Assert(it.eval("(cangjie::http-get 42)").toString().contains("TypeError"))
        @Assert(it.eval(#"(cangjie::http-request 1 "http://a.com")"#).toString().contains("TypeError"))
        @Assert(it.eval(#"(cangjie::http-request "GET")"#).toString().contains("ArityMismatch"))
        @Assert(it.eval(#"(cangjie::http-get "http://a.com" {} {})"#).toString().contains("ArityMismatch"))
        @Assert(it.eval("(cangjie::url-encode 42)").toString().contains("TypeError"))
        @Assert(it.eval("(cangjie::url-encode)").toString().contains("ArityMismatch"))
        // URL 缺协议 → TypeError（不是 NetworkError：它连 host 都解析不出）
        @Assert(it.eval(#"(cangjie::http-get "a.com/x")"#).toString().contains("TypeError"))
        println("✓ 类型错 TypeError、个数错 ArityMismatch、URL 无协议 TypeError")
    }

    /** 域属性：网络必须被 `presetPure` 排除 */
    @TestCase
    func testDomainIsHostPolicy() {
        println("\n=== 网络域不是纯函数 ===")
        let dom = netDomain(NetConfig())
        @Assert(dom.name == "net")
        @Assert(dom.escapable)
        @Assert(dom.hostPolicy)
        @Assert(dom.symbols.size == 8)
        println("✓ escapable / hostPolicy 均为 true，presetPure 会排除它")
    }

    // ==================== 真实网络（默认跳过） ====================

    /**
     * 真实 HTTP 请求：验证响应形状
     *
     * 只在 `XISP_NET_TESTS=1` 时跑 —— 默认不依赖外网。
     */
    @TestCase
    func testLiveHttp() {
        println("\n=== 真实 HTTP（XISP_NET_TESTS=1 才跑）===")
        if (!this.liveEnabled()) {
            println("· 跳过：未设置 XISP_NET_TESTS=1")
            return
        }
        let it = LispInterpreter([withQuietMode(), withNetworkUnrestricted()])
        // 200
        let ok = it.eval(#"(cangjie::http-get "http://httpbin.org/status/200")"#)
        match (ok) {
            case LispValue.HashMap(m) =>
                @Assert(m["status"].toString() == "200")
                @Assert(m["ok"].toString() == "true")
                @Assert(m.contains("headers"))
                @Assert(m.contains("body"))
                @Assert(m.contains("url"))
            case other => @Fail("期望响应 HashMap，实得 ${other.toString()}")
        }
        // 404 是正常数据流，不是错误
        let notFound = it.eval(#"(cangjie::http-get "http://httpbin.org/status/404")"#)
        match (notFound) {
            case LispValue.HashMap(m) =>
                @Assert(m["status"].toString() == "404")
                @Assert(m["ok"].toString() == "false")
            case other => @Fail("404 应是正常响应，实得 ${other.toString()}")
        }
        println("✓ 200 / 404 都按数据流返回，字段齐全")
    }

    /** 真实 HTTPS：TLS 配置生效（这是 `--static` 与 OpenSSL 共存的关键验证） */
    @TestCase
    func testLiveHttps() {
        println("\n=== 真实 HTTPS（XISP_NET_TESTS=1 才跑）===")
        if (!this.liveEnabled()) {
            println("· 跳过：未设置 XISP_NET_TESTS=1")
            return
        }
        let it = LispInterpreter([withQuietMode(), withNetworkUnrestricted()])
        let r = it.eval(#"(cangjie::http-get "https://httpbin.org/status/200")"#)
        match (r) {
            case LispValue.HashMap(m) => @Assert(m["status"].toString() == "200")
            case other => @Fail("HTTPS 应成功，实得 ${other.toString()}")
        }
        println("✓ HTTPS 可通（TlsClientConfig 生效）")
    }

    /**
     * 传输层失败 → `NetworkError`（连一个必然失败的主机）
     *
     * **有代理时必须跳过**：`http_proxy` / `https_proxy` 存在时 stdx 会把请求交给代理，
     * 而代理对连不上的目标通常回 `502` —— 那就成了"正常响应"，测不到直连失败。
     * 这不是实现问题，是环境差异，所以按环境跳过而不是放宽断言。
     */
    @TestCase
    func testTransportFailureIsNetworkError() {
        println("\n=== 传输层失败报 NetworkError ===")
        if (!this.liveEnabled()) {
            println("· 跳过：未设置 XISP_NET_TESTS=1")
            return
        }
        if (getVariable("http_proxy").isSome() || getVariable("https_proxy").isSome()) {
            println("· 跳过：检测到代理环境变量，请求会被代理接管（回 502），测不到直连失败")
            return
        }
        let it = LispInterpreter([withQuietMode(), withNetworkUnrestricted()])
        let r = it.eval(#"(cangjie::http-get "http://127.0.0.1:1/x" {:timeout 2000})"#)
        let s = r.toString()
        if (!s.contains("NetworkError")) {
            @Fail("应报 NetworkError，实得 ${s}")
        }
        println("✓ 连接被拒 → NetworkError（不是正常响应）")
    }
}
