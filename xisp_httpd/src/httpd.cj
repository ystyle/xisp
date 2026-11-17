package ystyle::xisp_httpd

import ystyle::xisp.types.*
import ystyle::xisp.core.eval.*
import ystyle::tachyon.*

import std.collection.ArrayList
import std.collection.HashMap as StdHashMap
import std.net.*
import std.sync.*
import std.time.*

/**
 * HTTP 服务端能力（能力域 `httpd`）
 *
 * 设计见 `docs/plans/2026-11-17-httpd-design.md`。
 *
 * ```lisp
 * (define srv
 *   (httpd-listen 8080
 *     (lambda (req)
 *       {:status 200 :body (str "hello " (hashmap-get (hashmap-get req :query) "name"))})))
 * (httpd-shutdown srv)
 * ```
 *
 * ## 用 tachyon 而不是 stdx.net.http
 *
 * tachyon 仅依赖 `std.*`（**不需要 stdx 环境**），每连接一协程，稳态内存约为
 * stdx 方案的 1/9。它不做 TLS / HTTP/2 / WebSocket —— 那些该由前置 LB 终结，
 * 后端走 HTTP/1.1。
 *
 * ## 请求是快照，不是 ctx
 *
 * tachyon 的 `RequestCtx` 池化复用，handler 返回后就被下一个请求改写。Lisp 侧
 * 拿到的是**复制出来的 `HashMap`**（见 `convert.cj`）—— 捕获也不会串数据。
 *
 * ## 每请求一个求值器
 *
 * 每连接一协程 ⇒ 多个连接会并发调 handler，而 `Evaluator` 有大量"当前正在求值什么"
 * 的可变字段。所以每个请求用 `ctx.newEvaluator()`（继承宿主配置）造一个新的，
 * 共享同一棵 `Environment` 树；服务启动时对根环境调一次 `shareAcrossThreads()`。
 */

/** 服务端配置：宿主策略，脚本改不了 */
public class HttpdConfig {
    /** 绑定地址。默认只绑回环 —— 对外监听必须宿主显式改 */
    public var bind: String = "127.0.0.1"

    /** 允许脚本使用的端口下界（默认拒绝特权端口） */
    public var minPort: UInt16 = 1024

    /** 允许脚本使用的端口上界 */
    public var maxPort: UInt16 = 65535

    /** 请求体上限（tachyon `maxRequestBodySize`） */
    public var maxBodyBytes: Int64 = 4 * 1024 * 1024

    /** 并发连接上限（tachyon `concurrency`） */
    public var maxConns: Int64 = 16384

    public var readTimeout: ?Int64 = None
    public var writeTimeout: ?Int64 = None
    public var idleTimeout: ?Int64 = None

    public init() {}
}

/** handler 与句柄共享的运行期状态 */
class ServerState {
    /** 1 = 正在关停（handler 据此提前返回 503，长循环据此中断） */
    let closing = AtomicInt64(0)

    /** 1 = 仍在监听 */
    let live = AtomicInt64(1)
}

/** 服务句柄（`LispExternal`） */
public class HttpdServer <: LispExternal {
    let server: Server
    let state: ServerState
    let addr: String

    init(server: Server, state: ServerState, addr: String) {
        this.server = server
        this.state = state
        this.addr = addr
    }

    public prop typeName: String {
        get() {
            "httpd-server"
        }
    }

    public func display(): String {
        "#<httpd-server ${addr}>"
    }

    /** 优雅关闭；幂等 */
    func shutdown(timeoutMs: ?Int64): Unit {
        // 先置关停标记：handler 里的长循环靠它中断
        state.closing.store(1)
        match (timeoutMs) {
            case Some(ms) =>
                server.shutdownGracefully(Duration.millisecond * ms)
            case None =>
                server.shutdown()
        }
        state.live.store(0)
    }

    func isRunning(): Bool {
        state.live.load() != 0
    }
}

/** 处理一次请求：转快照 → 新求值器 → 调 handler → 写响应 */
func handleRequest(ctx: RequestCtx, handler: LispValue,
                   newEvaluator: () -> Evaluator,
                   isInterrupted: () -> Bool,
                   state: ServerState): Unit {
    if (state.closing.load() != 0) {
        ctx.error("503 Service Unavailable", 503)
        return
    }
    try {
        let req = requestToLisp(ctx)
        let ev = newEvaluator()
        // 中断：宿主的中断条件 **或** 本服务正在关停
        ev.setInterruptChecker({ => isInterrupted() || state.closing.load() != 0 })
        let args = ArrayList<LispValue>()
        args.add(req)
        let result = ev.applyFunction(handler, args)
        writeResponse(ctx, result)
    } catch (e: Exception) {
        // 服务端底线：一个 handler 出错不能让连接崩，更不能影响别的请求
        ctx.error("500 Internal Server Error\n\n${e.message}", 500)
    }
}

/** 从 opts 里取整数 */
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

/** 取时间参数 → ?Duration */
func durOpt(opts: LispValue, key: String): ?Duration {
    match (intOpt(opts, key)) {
        case Some(ms) => Some(Duration.millisecond * ms)
        case None => None
    }
}

/** 应用 opts 覆盖（只允许调优项，绑定地址与端口范围是宿主策略） */
func applyOpts(server: Server, opts: LispValue): Unit {
    match (intOpt(opts, "max-body-bytes")) {
        case Some(n) => server.maxRequestBodySize = n
        case None => ()
    }
    match (intOpt(opts, "max-conns")) {
        case Some(n) => server.concurrency = n
        case None => ()
    }
    match (durOpt(opts, "read-timeout")) {
        case Some(d) => server.readTimeout = Some(d)
        case None => ()
    }
    match (durOpt(opts, "write-timeout")) {
        case Some(d) => server.writeTimeout = Some(d)
        case None => ()
    }
    match (durOpt(opts, "idle-timeout")) {
        case Some(d) => server.idleTimeout = Some(d)
        case None => ()
    }
}

/**
 * `(cangjie::httpd-listen port handler [opts])` —— **非阻塞**，返回服务句柄
 */
func listenHttpd(args: ArrayList<LispValue>, cfg: HttpdConfig,
                 newEvaluator: () -> Evaluator,
                 isInterrupted: () -> Bool,
                 env: Environment): LispValue {
    if (args.size < 2 || args.size > 3) {
        return LispValue.Error(XispError(ErrorType.ArityMismatch,
            "httpd-listen expects 2 or 3 arguments: port handler [opts]"))
    }
    let portN = match (args[0]) {
        case LispValue.Int(n) => n
        case _ => return LispValue.Error(XispError(ErrorType.TypeError,
            "httpd-listen: port must be an integer"))
    }
    if (portN < Int64(cfg.minPort) || portN > Int64(cfg.maxPort)) {
        return LispValue.Error(XispError(ErrorType.RuntimeError,
            "httpd-listen: port ${portN} is outside the allowed range " +
            "${cfg.minPort}..${cfg.maxPort}"))
    }
    let port = UInt16(portN)
    // handler 必须是可调用值；不校验的话错误要到第一个请求才暴露
    if (!args[1].isProcedure()) {
        return LispValue.Error(XispError(ErrorType.TypeError,
            "httpd-listen: handler must be a procedure"))
    }
    let handler = args[1]
    let opts: LispValue = if (args.size == 3) { args[2] } else { LispValue.Nil }

    // 预检端口：端口被占用要立刻报错。中间隔着 spawn 的话，绑定失败只会在
    // 协程里打一行 stderr，脚本却以为服务起来了。
    let addr = "${cfg.bind}:${port}"
    try {
        let probe = TcpServerSocket(bindAt: IPSocketAddress(cfg.bind, port))
        probe.close()
    } catch (e: Exception) {
        return LispValue.Error(XispError(ErrorType.NetworkError,
            "httpd-listen: cannot bind ${addr}: ${e.message}"))
    }

    let state = ServerState()
    env.shareAcrossThreads()
    let srv = Server({ ctx: RequestCtx =>
        handleRequest(ctx, handler, newEvaluator, isInterrupted, state)
    })
    srv.maxRequestBodySize = cfg.maxBodyBytes
    srv.concurrency = cfg.maxConns
    match (cfg.readTimeout) {
        case Some(ms) => srv.readTimeout = Some(Duration.millisecond * ms)
        case None => ()
    }
    match (cfg.writeTimeout) {
        case Some(ms) => srv.writeTimeout = Some(Duration.millisecond * ms)
        case None => ()
    }
    match (cfg.idleTimeout) {
        case Some(ms) => srv.idleTimeout = Some(Duration.millisecond * ms)
        case None => ()
    }
    applyOpts(srv, opts)

    let handle = HttpdServer(srv, state, addr)
    let bindHost = cfg.bind
    // 后台跑监听循环：起个服务不该要求脚本懂并发
    spawn {
        =>
        try {
            srv.listenAndServe(bindHost, port)
        } catch (e: Exception) {
            state.live.store(0)
            eprintln("[xisp_httpd] listen ${addr} failed: ${e.message}")
        }
    }
    // 等端口**真的**可连再返回。tachyon 的 listenAndServe 是"绑定+循环"一体的，
    // 没有"绑定完成"回调；不等的话调用方紧接着发请求会连不上（而且端口占用这类
    // 失败会藏进协程里，脚本却以为服务起来了）。
    if (!waitUntilConnectable(bindHost, port, state)) {
        state.closing.store(1)
        state.live.store(0)
        return LispValue.Error(XispError(ErrorType.NetworkError,
            "httpd-listen: ${addr} did not become reachable in time"))
    }
    LispValue.External(handle)
}

/**
 * 轮询到端口可连（最多 2 秒，每 20ms 一次）
 *
 * 探测本身会产生一个空连接，tachyon 会正常读空然后关闭 —— 无害。
 */
func waitUntilConnectable(host: String, port: UInt16, state: ServerState): Bool {
    var i = 0
    while (i < 100) {
        if (state.live.load() == 0) {
            return false
        }
        try {
            let probe = TcpSocket(IPSocketAddress(host, port))
            probe.connect()
            probe.close()
            return true
        } catch (_: Exception) {
            sleep(Duration.millisecond * 20)
        }
        i += 1
    }
    false
}

/** 取参数里的 `HttpdServer` */
func serverArg(args: ArrayList<LispValue>, idx: Int64, fn: String): ?HttpdServer {
    if (idx >= args.size) {
        return None
    }
    match (args[idx]) {
        case LispValue.External(ext) =>
            match (ext) {
                case s: HttpdServer => Some(s)
                case _ => None
            }
        case _ => None
    }
}

/** 注册 `httpd-shutdown` / `httpd-addr` / `httpd-running?`（`httpd-listen` 见上） */
public func registerHttpd(ctx: CapabilityContext, cfg: HttpdConfig): Unit {
    ctx.register("cangjie::httpd-listen", { args =>
        listenHttpd(args, cfg, ctx.newEvaluator, ctx.isInterrupted, ctx.env)
    })

    ctx.register("cangjie::httpd-shutdown", { args =>
        if (args.size < 1 || args.size > 2) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "httpd-shutdown expects 1 or 2 arguments: server [timeout-ms]"))
        }
        let srv = match (serverArg(args, 0, "httpd-shutdown")) {
            case Some(s) => s
            case None => return LispValue.Error(XispError(ErrorType.TypeError,
                "httpd-shutdown: expected a httpd-server"))
        }
        let timeout: ?Int64 = if (args.size == 2) {
            match (args[1]) {
                case LispValue.Int(n) => Some(n)
                case _ => return LispValue.Error(XispError(ErrorType.TypeError,
                    "httpd-shutdown: timeout must be an integer (milliseconds)"))
            }
        } else {
            None
        }
        srv.shutdown(timeout)
        LispValue.Nil
    })

    ctx.register("cangjie::httpd-addr", { args =>
        if (args.size != 1) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "httpd-addr expects 1 argument: server"))
        }
        match (serverArg(args, 0, "httpd-addr")) {
            case Some(s) => LispValue.Str(s.addr)
            case None => LispValue.Error(XispError(ErrorType.TypeError,
                "httpd-addr: expected a httpd-server"))
        }
    })

    ctx.register("cangjie::httpd-running?", { args =>
        if (args.size != 1) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "httpd-running? expects 1 argument: server"))
        }
        match (serverArg(args, 0, "httpd-running?")) {
            case Some(s) => LispValue.Boolean(s.isRunning())
            case None => LispValue.Boolean(false)
        }
    })
}

/**
 * HTTP 服务端能力域。
 *
 * `escapable: true`（能对外提供访问）、`hostPolicy: true`（占端口）
 * —— `presetPure` 按属性推导会排除它。
 *
 * 注意 `httpd-listen` **在** symbols 里（它是普通注册函数，不是求值器拦截的形式），
 * 与 `spawn` 不同 —— 后者是②类、不注册到环境，域装配动不了它。
 */
public func httpdDomain(cfg: HttpdConfig): LispCapability {
    SimpleCapability("httpd",
        ["cangjie::httpd-listen", "cangjie::httpd-shutdown",
         "cangjie::httpd-addr", "cangjie::httpd-running?"],
        { ctx => registerHttpd(ctx, cfg) },
        escapable: true, hostPolicy: true,
        after: ["base"])
}
