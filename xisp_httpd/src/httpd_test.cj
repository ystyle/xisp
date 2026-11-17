package ystyle::xisp_httpd

import ystyle::xisp.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp_bc.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import std.net.*

/**
 * HTTP 服务端能力的测试
 *
 * ## 纪律：只用 `127.0.0.1` + 高位端口，不依赖外网
 *
 * 客户端是**手写的最小 HTTP/1.1 请求器**（`TcpSocket` 直接发字节），刻意不引 `xisp_net`
 * —— 那会把 stdx 拖进本模块的测试依赖，而本模块的价值之一就是零 stdx。
 *
 * 每个用例用不同端口，结束都 `httpd-shutdown`，避免端口泄漏影响后续。
 */
@Test
class HttpdTest {
    func evalIn(opts: Array<InterpreterOption>, code: String): String {
        let it = LispInterpreter(opts)
        var r: LispValue = Nil
        for (e in Parser.parseFromString(code)) {
            r = it.evalExpr(e)
        }
        r.toString()
    }

    /** 发一个 GET，返回响应原文（UTF-8 整体解码，能正确带中文） */
    func simpleGet(port: UInt16, path: String, extraHeaders: String): String {
        let raw = "GET ${path} HTTP/1.1\r\nHost: 127.0.0.1\r\n${extraHeaders}Connection: close\r\n\r\n"
        let sock = TcpSocket(IPSocketAddress("127.0.0.1", port))
        sock.connect()
        sock.write(raw.toArray())
        let buf = Array<Byte>(8192, repeat: 0)
        let all = ArrayList<Byte>()
        while (true) {
            let n = sock.read(buf)
            if (n <= 0) {
                break
            }
            for (i in 0..n) {
                all.add(buf[i])
            }
        }
        sock.close()
        String.fromUtf8(all.toArray())
    }

    /** 取响应正文（第一个 \r\n\r\n 之后） */
    func bodyOf(resp: String): String {
        match (resp.indexOf("\r\n\r\n")) {
            case Some(i) => resp[i + 4..]
            case None => ""
        }
    }

    /** 取状态行 */
    func statusLine(resp: String): String {
        match (resp.indexOf("\r\n")) {
            case Some(i) => resp[..i]
            case None => resp
        }
    }

    // ==================== 基础三态 ====================

    /** `HashMap` 返回 → 状态码 / 头 / body 都按写 */
    @TestCase
    func testMapResponse() {
        println("\n=== HashMap 响应 ===")
        let it = LispInterpreter([withQuietMode(), withHttpd(portRange: (19001, 19001))])
        it.eval(#"(define srv (cangjie::httpd-listen 19001 (lambda (req) {:status 201 :headers {:x-test "yes"} :body "created"})))"#)
        let resp = simpleGet(19001, "/x", "")
        @Assert(statusLine(resp).contains("201"))
        @Assert(resp.toAsciiLower().contains("x-test: yes"))
        @Assert(bodyOf(resp) == "created")
        it.eval("(cangjie::httpd-shutdown srv)")
        println("✓ 状态码 / 自定义头 / body 都生效")
    }

    /** 字符串返回 → 200 + text/plain */
    @TestCase
    func testStringResponse() {
        println("\n=== 字符串响应 ===")
        let it = LispInterpreter([withQuietMode(), withHttpd(portRange: (19002, 19002))])
        it.eval(#"(define srv2 (cangjie::httpd-listen 19002 (lambda (req) "plain")))"#)
        let resp = simpleGet(19002, "/", "")
        @Assert(statusLine(resp).contains("200"))
        @Assert(resp.toAsciiLower().contains("text/plain"))
        @Assert(bodyOf(resp) == "plain")
        it.eval("(cangjie::httpd-shutdown srv2)")
        println("✓ 字符串 → 200 text/plain")
    }

    /** `nil` 返回 → 204 */
    @TestCase
    func testNilResponse() {
        println("\n=== nil 响应 ===")
        let it = LispInterpreter([withQuietMode(), withHttpd(portRange: (19003, 19003))])
        it.eval("(define srv3 (cangjie::httpd-listen 19003 (lambda (req) nil)))")
        let resp = simpleGet(19003, "/", "")
        @Assert(statusLine(resp).contains("204"))
        it.eval("(cangjie::httpd-shutdown srv3)")
        println("✓ nil → 204")
    }

    // ==================== 请求快照 ====================

    /**
     * 快照字段完整：method / path / query / headers（含自定义头）/ cookies / form
     */
    @TestCase
    func testRequestSnapshot() {
        println("\n=== 请求快照字段 ===")
        let it = LispInterpreter([withQuietMode(), withHttpd(portRange: (19004, 19004))])
        // handler 把读到的字段拼成一行，便于宿主断言
        it.eval(#"""
            (define srv4
              (cangjie::httpd-listen 19004
                (lambda (req)
                  (str (hashmap-get req :method) "|"
                       (hashmap-get req :path) "|"
                       (hashmap-get (hashmap-get req :query) "a") "|"
                       (hashmap-get (hashmap-get req :headers) "x-custom") "|"
                       (hashmap-get (hashmap-get req :cookies) "sid")))))
            """#)
        let resp = simpleGet(19004, "/p?a=1&b=2", "X-Custom: hello\r\nCookie: sid=abc; t=2\r\n")
        let body = bodyOf(resp)
        if (!body.contains("GET|/p|1|hello|abc")) {
            @Fail("快照字段不符，实得 ${body}")
        }
        it.eval("(cangjie::httpd-shutdown srv4)")
        println("✓ method / path / query / 自定义头 / cookie 都能读到")
    }

    /**
     * **两个请求交替，确认不串数据**
     *
     * 这是「不暴露池化 ctx」的直接验证：如果 handler 里拿到的是复用的 ctx，
     * 第二次请求就会读到第一次的残留。
     */
    @TestCase
    func testNoCrossRequestLeak() {
        println("\n=== 请求之间不串数据 ===")
        let it = LispInterpreter([withQuietMode(), withHttpd(portRange: (19005, 19005))])
        it.eval(#"(define srv5 (cangjie::httpd-listen 19005 (lambda (req) (str (hashmap-get req :path) "::" (hashmap-get (hashmap-get req :query) "v")))))"#)
        // 交替不同 path / 不同 query
        for (i in 0..4) {
            let p = "/p${i}"
            let v = "v${i}"
            let body = bodyOf(simpleGet(19005, "${p}?v=${v}", ""))
            if (body != "${p}::${v}") {
                @Fail("第 ${i} 次请求串数据了：期望 ${p}::${v}，实得 ${body}")
            }
        }
        it.eval("(cangjie::httpd-shutdown srv5)")
        println("✓ 4 次交替请求各自拿到自己的数据")
    }

    // ==================== 错误不崩 ====================

    /**
     * handler 返回 `Error` → 500，**后续请求仍正常**
     *
     * 服务端底线：一个 handler 写错不能让整个服务挂掉。
     */
    @TestCase
    func testHandlerErrorDoesNotKillServer() {
        println("\n=== handler 出错不影响后续请求 ===")
        let it = LispInterpreter([withQuietMode(), withHttpd(portRange: (19006, 19006))])
        it.eval(#"""
            (define srv6
              (cangjie::httpd-listen 19006
                (lambda (req)
                  (if (string=? (hashmap-get req :path) "/boom")
                    (error "kaboom")
                    "ok"))))
            """#)
        let r1 = simpleGet(19006, "/boom", "")
        println("  [debug] /boom 响应: " + statusLine(r1))
        @Assert(statusLine(r1).contains("500"))
        // 同一个服务继续服务
        let r2 = simpleGet(19006, "/fine", "")
        @Assert(statusLine(r2).contains("200"))
        @Assert(bodyOf(r2) == "ok")
        it.eval("(cangjie::httpd-shutdown srv6)")
        println("✓ 出错请求 500，后续请求照常 200")
    }

    // ==================== 并发 ====================

    /** 并发多连接：不崩、不串数据 */
    @TestCase
    func testConcurrentConnections() {
        println("\n=== 并发多连接 ===")
        let it = LispInterpreter([withQuietMode(), withHttpd(portRange: (19007, 19007))])
        it.eval(#"(define srv7 (cangjie::httpd-listen 19007 (lambda (req) (hashmap-get (hashmap-get req :query) "v"))))"#)
        let futures = ArrayList<Future<String>>()
        for (i in 0..8) {
            let v = "c${i}"
            futures.add(spawn { => bodyOf(simpleGet(19007, "/x?v=${v}", "")) })
        }
        for (i in 0..futures.size) {
            let got = futures[i].get()
            let want = "c${i}"
            if (got != want) {
                @Fail("并发请求 ${i} 串数据了：期望 ${want}，实得 ${got}")
            }
        }
        it.eval("(cangjie::httpd-shutdown srv7)")
        println("✓ 8 个并发连接各自拿到自己的数据")
    }

    // ==================== 沙箱 ====================

    /** 特权端口被拒 */
    @TestCase
    func testPrivilegedPortDenied() {
        println("\n=== 特权端口被拒 ===")
        let it = LispInterpreter([withQuietMode(), withHttpd()])
        let r = it.eval("(cangjie::httpd-listen 80 (lambda (req) \"x\"))").toString()
        @Assert(r.contains("RuntimeError"))
        @Assert(r.contains("outside the allowed range"))
        println("✓ 80 端口被拒（默认范围 1024..65535）")
    }

    /**
     * 同一端口可重复监听 —— tachyon 开了 `SO_REUSEPORT`
     *
     * 不是 bug：REUSEPORT 让多个监听者共享端口做负载均衡，所以"端口已被占用"
     * 在 tachyon 之间**不成立**。钉住这个事实，免得以后被当成 bug 修掉。
     *
     * （`httpd-listen` 里的绑定预检仍然保留 —— 对不支持 REUSEPORT 的占用方，
     * 比如别的普通进程，它确实会报 `NetworkError`。那个路径不在这里造，
     * 因为本机没法保证造出"不可共享的占用"。）
     */
    @TestCase
    func testReusePort() {
        println("\n=== 同端口可重复监听（REUSEPORT）===")
        let it = LispInterpreter([withQuietMode(), withHttpd(portRange: (19008, 19008))])
        let r1 = it.eval(#"(define s1 (cangjie::httpd-listen 19008 (lambda (req) "a")))"#).toString()
        let r2 = it.eval(#"(define s2 (cangjie::httpd-listen 19008 (lambda (req) "b")))"#).toString()
        if (!r1.contains("httpd-server") || !r2.contains("httpd-server")) {
            @Fail("REUSEPORT 下两次监听都应成功，实得 ${r1} / ${r2}")
        }
        it.eval("(cangjie::httpd-shutdown s1)")
        it.eval("(cangjie::httpd-shutdown s2)")
        println("✓ 同端口两次监听都成功（REUSEPORT 语义）")
    }

    /** 参数错误 */
    @TestCase
    func testArgumentErrors() {
        println("\n=== 参数错误 ===")
        let it = LispInterpreter([withQuietMode(), withHttpd()])
        @Assert(it.eval("(cangjie::httpd-listen)").toString().contains("ArityMismatch"))
        @Assert(it.eval(#"(cangjie::httpd-listen "8080" (lambda (req) "x"))"#).toString().contains("TypeError"))
        @Assert(it.eval("(cangjie::httpd-listen 8080 42)").toString().contains("TypeError"))
        @Assert(it.eval("(cangjie::httpd-shutdown 42)").toString().contains("TypeError"))
        @Assert(it.eval("(cangjie::httpd-addr 42)").toString().contains("TypeError"))
        // 谓词形态：非 server 给 false 而不是报错
        @Assert(it.eval("(cangjie::httpd-running? 42)").toString() == "false")
        println("✓ 个数错 ArityMismatch、类型错 TypeError、谓词给 false")
    }

    // ==================== 生命周期 ====================

    /** shutdown 后端口释放，且幂等 */
    @TestCase
    func testShutdownReleasesPort() {
        println("\n=== shutdown 释放端口且幂等 ===")
        let it = LispInterpreter([withQuietMode(), withHttpd(portRange: (19009, 19009))])
        it.eval(#"(define srv9 (cangjie::httpd-listen 19009 (lambda (req) "x")))"#)
        @Assert(it.eval("(cangjie::httpd-running? srv9)").toString() == "true")
        @Assert(it.eval("(cangjie::httpd-addr srv9)").toString() == "\"127.0.0.1:19009\"")
        it.eval("(cangjie::httpd-shutdown srv9 2000)")
        @Assert(it.eval("(cangjie::httpd-running? srv9)").toString() == "false")
        // 幂等：再关一次不报错
        it.eval("(cangjie::httpd-shutdown srv9)")
        // 端口应已释放，同端口能再起
        let again = it.eval(#"(define srv9b (cangjie::httpd-listen 19009 (lambda (req) "y")))"#).toString()
        if (!again.contains("httpd-server")) {
            @Fail("同端口无法重新监听，实得 ${again}")
        }
        it.eval("(cangjie::httpd-shutdown srv9b)")
        println("✓ 关闭后有 running?=false、可重复关闭、端口可复用")
    }

    // ==================== 三后端 ====================

    /** handler 在 AST / BC / JIT 下行为一致 */
    @TestCase
    func testAllBackends() {
        println("\n=== 三后端一致 ===")
        var port: UInt16 = 19010
        for (backend in ["AST", "BC", "JIT"]) {
            let opts = ArrayList<InterpreterOption>()
            opts.add(withQuietMode())
            opts.add(withHttpd(portRange: (port, port)))
            if (backend != "AST") {
                opts.add(withBytecodeCompiler())
            }
            if (backend == "JIT") {
                opts.add(withJit())
            }
            let it = LispInterpreter(opts.toArray())
            it.eval("(define s (cangjie::httpd-listen ${port} (lambda (req) (str \"[\" (hashmap-get req :path) \"]\"))))")
            let body = bodyOf(simpleGet(port, "/hi", ""))
            if (body != "[/hi]") {
                @Fail("[${backend}] 期望 [/hi]，实得 ${body}")
            }
            it.eval("(cangjie::httpd-shutdown s)")
            port += 1
        }
        println("✓ AST / BC / JIT 下 handler 行为一致")
    }

    /** 域属性：服务端不是纯函数 */
    @TestCase
    func testDomainIsHostPolicy() {
        println("\n=== 服务端域属性 ===")
        let dom = httpdDomain(HttpdConfig())
        @Assert(dom.name == "httpd")
        @Assert(dom.escapable)
        @Assert(dom.hostPolicy)
        @Assert(dom.symbols.size == 4)
        println("✓ escapable / hostPolicy 均为 true，presetPure 会排除它")
    }
}
