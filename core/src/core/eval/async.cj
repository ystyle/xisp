package ystyle::xisp.core.eval

import ystyle::xisp.types.*

import std.collection.ArrayList
import std.sync.*

/**
 * 异步能力：`spawn` + `Future`（能力域 `async`）
 *
 * 设计见 `docs/plans/2026-11-17-async-design.md`。
 *
 * ## 分层
 *
 * | 符号 | 层 | 说明 |
 * |---|---|---|
 * | `spawn` | **②求值器拦截** | 实参必须在**新线程里**求值，故不能是普通函数 |
 * | `future-get` / `future-try-get` / `future-done?` / `future-pending?` / `future-cancel` | ③注册函数 | 吃 `Future` 值，无控制流语义 |
 *
 * ## 失败的处理（与 0.6.0 的错误语义对齐）
 *
 * `LispValue.Error` 一旦**参与求值链就中止**，这是全仓统一的语义，`spawn` 不能例外。
 * 于是"`future-get` 返回错误值但不中止调用方"是**做不到**的 —— 错误值连作为实参
 * 传给谓词都做不到（实参求值出错就中止了）。所以失败处理走两条路：
 *
 * | 想要 | 用什么 |
 * |---|---|
 * | 失败就中止（fail-fast，默认） | `(future-get f)` —— 子线程的错误原样抛给调用方 |
 * | 失败要自己处置 | `(future-ok? f)` 先判断，`(future-error f)` 取错误文本（字符串） |
 *
 * ```lisp
 * (let [f (spawn (may-fail))]
 *   (if (future-ok? f)
 *     (use (future-get f))
 *     (handle (future-error f))))
 * ```
 *
 * `future-ok?` / `future-error` 会**阻塞到任务完成**（它们内部就是 `get()`），
 * 拿到的是 `Error` 值但只返回 `Bool` / 字符串，故不触发中止。
 *
 * ## 环境语义：共享环境树 + 写入加锁
 *
 * 子线程**共享**父线程的环境树（`(spawn (set! counter ...))` 对父可见），
 * 由 `Environment.shareAcrossThreads()` 懒分配读写锁保证安全。每个线程有**自己的**
 * `Evaluator`（它持有十余个"当前正在求值什么"的可变字段，共享必错）。
 *
 * ## 子线程一律走 AST
 *
 * 子线程**不注入字节码/JIT 后端**。三条理由，都不是性能取舍：
 *
 * 1. `BytecodeVM` 有 3 个类级 `static` 字段（`functionTable` / `globalRefCache` /
 *    `funcIds`），并发写会损坏 `ArrayList` 与 `HashMap`；
 * 2. 其中 `functionTable` / `funcIds` 承载**跨线程语义** —— `Closure` 变体里存的是
 *    VM 内部的函数 id，跨 VM 传递后 id 对不上。改成实例字段会打破"闭包可传递"；
 * 3. `jitCurEnv` 等是包级 `static var`，JIT 完全不能并发。
 *
 * 而 AST 的闭包是 `Procedure(参数, 体, 环境)` —— 纯数据，天然跨线程可用。
 * 所以子线程走 AST 是**正确性要求**，不只是保守。
 */

/** `Future` 是可选模块能力：用 `std.core` 的 `Future<T>` 承载，包成外部值给 Lisp 层 */
public class LispFuture <: LispExternal {
    public let inner: Future<LispValue>

    public init(f: Future<LispValue>) {
        this.inner = f
    }

    public prop typeName: String {
        get() {
            "future"
        }
    }

    /**
     * 文本表示。已完成给结果摘要，未完成给 `pending`。
     *
     * `tryGet()` 不阻塞：线程未完成时返回 `None`。
     */
    public func display(): String {
        match (this.inner.tryGet()) {
            case Some(v) => "#<future done ${v.toString()}>"
            case None => "#<future pending>"
        }
    }
}

extend Evaluator {
    /**
     * 把本求值器的**配置**复制到 `target`（供子线程的求值器继承）。
     *
     * 刻意不复制：`backend`（子线程一律 AST）、各类缓存与深度计数器（天然独立）。
     * 复制的是"宿主策略"与"当前模块上下文"——它们决定子线程里的可见性与权限，
     * 漏掉任何一个，子线程就会绕过沙箱或找不到模块。
     */
    public func copyConfigTo(target: Evaluator): Unit {
        target.maxStackDepth = this.maxStackDepth
        target.maxMacroExpandDepth = this.maxMacroExpandDepth
        target.funcChecker = this.funcChecker
        target.pathChecker = this.pathChecker
        target.moduleLoadChecker = this.moduleLoadChecker
        target.currentFilePath = this.currentFilePath
        target.moduleRegistry = this.moduleRegistry
        target.moduleLoader = this.moduleLoader
        target.moduleParser = this.moduleParser
        target.currentModule = this.currentModule
        target.currentLoader = this.currentLoader
        target.concurrencyEnabled = this.concurrencyEnabled
    }

    /**
     * `(spawn expr)` —— 在新线程求值 `expr`，返回 `Future` 外部值。
     *
     * 中断接入：子线程的中断检查器 = 「本线程收到取消请求」或「父线程的中断回调」。
     * 仓颉的 `Future.cancel()` 只发请求、不停止线程，线程要靠
     * `Thread.hasPendingCancellation` 自查 —— 这正好是 xisp 已有的协作式中断模型，
     * 所以复用既有的 4 处检查点即可，不需要新增。
     */
    func evalSpawn(expr: LispValue): LispValue {
        if (!this.concurrencyEnabled) {
            return LispValue.Error(XispError(ErrorType.RuntimeError,
                "spawn is disabled (concurrency was turned off by the host)"))
        }
        // 必须恰好一个表达式：`(spawn)` 或 `(spawn a b)` 都是调用错误
        let cell = match (expr) {
            case Cons(c) => c
            case _ => return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "spawn expects exactly 1 expression"))
        }
        if (!cell.cdr.isNil()) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "spawn expects exactly 1 expression"))
        }
        let body = cell.car
        // 共享环境链：懒分配读写锁，之后该链上的每次绑定访问都走锁
        this.env.shareAcrossThreads()
        let sharedEnv = this.env
        let parentChecker = this.interruptChecker
        let parent = this

        let fut: Future<LispValue> = spawn {
            =>
            let child = Evaluator(sharedEnv)
            parent.copyConfigTo(child)
            let checker: () -> Bool = {
                =>
                if (Thread.currentThread.hasPendingCancellation) {
                    true
                } else {
                    match (parentChecker) {
                        case Some(c) => c()
                        case None => false
                    }
                }
            }
            child.setInterruptChecker(checker)
            child.eval(body)
        }
        LispValue.External(LispFuture(fut))
    }
}

/** 从参数里取 `LispFuture`；不是则返回 `None` */
func futureArg(args: ArrayList<LispValue>, idx: Int64): ?LispFuture {
    if (idx >= args.size) {
        return None
    }
    match (args[idx]) {
        case LispValue.External(ext) =>
            match (ext) {
                case f: LispFuture => Some(f)
                case _ => None
            }
        case _ => None
    }
}

func notAFutureError(fnName: String): LispValue {
    LispValue.Error(XispError(ErrorType.TypeError,
        "${fnName}: expected a future value"))
}

/**
 * 注册异步能力域的符号（不含 `spawn` —— 它是②类求值器拦截，不进环境）
 */
public func registerAsync(env: Environment): Unit {
    // (future-get f) / (future-get f timeout-ms) —— 阻塞等待
    env.define("future-get", NativeFunc({ args =>
        if (args.size != 1 && args.size != 2) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "future-get expects 1 or 2 arguments: future [timeout-ms]"))
        }
        match (futureArg(args, 0)) {
            case Some(f) =>
                if (args.size == 1) {
                    return f.inner.get()
                }
                let ms = match (args[1]) {
                    case LispValue.Int(n) => n
                    case _ =>
                        // 非整数超时：按无超时处理会掩盖调用错误，直接报类型错
                        return LispValue.Error(XispError(ErrorType.TypeError,
                            "future-get: timeout must be an integer (milliseconds)"))
                }
                if (ms <= 0) {
                    return f.inner.get()
                }
                try {
                    f.inner.get(Duration.millisecond * ms)
                } catch (e: TimeoutException) {
                    // 等待方超时：顺手向对方发取消请求（否则它会继续跑完，白烧 CPU）
                    f.inner.cancel()
                    LispValue.Error(XispError(ErrorType.RuntimeError, "future timeout"))
                }
            case None => notAFutureError("future-get")
        }
    }))

    // (future-try-get f) —— 非阻塞；未完成给 nil
    env.define("future-try-get", NativeFunc({ args =>
        if (args.size != 1) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "future-try-get expects 1 argument: future"))
        }
        match (futureArg(args, 0)) {
            case Some(f) =>
                match (f.inner.tryGet()) {
                    case Some(v) => v
                    case None => Nil
                }
            case None => notAFutureError("future-try-get")
        }
    }))

    // (future-done? f) / (future-pending? f)
    env.define("future-done?", NativeFunc({ args =>
        if (args.size != 1) {
            return LispValue.Boolean(false)
        }
        match (futureArg(args, 0)) {
            case Some(f) =>
                match (f.inner.tryGet()) {
                    case Some(_) => LispValue.Boolean(true)
                    case None => LispValue.Boolean(false)
                }
            case None => LispValue.Boolean(false)
        }
    }))

    env.define("future-pending?", NativeFunc({ args =>
        if (args.size != 1) {
            return LispValue.Boolean(false)
        }
        match (futureArg(args, 0)) {
            case Some(f) =>
                match (f.inner.tryGet()) {
                    case Some(_) => LispValue.Boolean(false)
                    case None => LispValue.Boolean(true)
                }
            case None => LispValue.Boolean(false)
        }
    }))

    // (future-ok? f) —— 阻塞到完成，返回是否成功（false = 子线程产生了错误）
    // 内部拿到 Error 值但只返回 Bool，故不会让调用方中止。
    env.define("future-ok?", NativeFunc({ args =>
        if (args.size != 1) {
            return LispValue.Boolean(false)
        }
        match (futureArg(args, 0)) {
            case Some(f) =>
                match (f.inner.get()) {
                    case LispValue.Error(_) => LispValue.Boolean(false)
                    case _ => LispValue.Boolean(true)
                }
            case None => LispValue.Boolean(false)
        }
    }))

    // (future-error f) —— 阻塞到完成；失败返回错误文本（字符串），成功返回 nil
    env.define("future-error", NativeFunc({ args =>
        if (args.size != 1) {
            return Nil
        }
        match (futureArg(args, 0)) {
            case Some(f) =>
                match (f.inner.get()) {
                    case LispValue.Error(e) => LispValue.Str(e.toString())
                    case _ => Nil
                }
            case None => Nil
        }
    }))

    // (future-cancel f) —— 发取消请求（不强制终止，对方在检查点自行收尾）
    env.define("future-cancel", NativeFunc({ args =>
        if (args.size != 1) {
            return LispValue.Error(XispError(ErrorType.ArityMismatch,
                "future-cancel expects 1 argument: future"))
        }
        match (futureArg(args, 0)) {
            case Some(f) =>
                f.inner.cancel()
                Nil
            case None => notAFutureError("future-cancel")
        }
    }))
}

/**
 * 异步能力域。
 *
 * - `escapable: true` —— `spawn` 打破"一次只有一个求值线程"的沙箱假设；
 * - `hostPolicy: true` —— 起线程是宿主资源。
 *
 * 两者都让它被 `presetPure` 按属性推导排除。
 *
 * **注意 `spawn` 不在 `symbols` 里**：它是②类求值器拦截（与 `map`/`eval` 同层），
 * 不注册到环境，故域装配动不了它。要禁用并发用 `withNoConcurrency()`。
 */
public func asyncDomain(): LispCapability {
    SimpleCapability("async",
        ["future-get", "future-try-get", "future-done?", "future-pending?", "future-cancel",
         "future-ok?", "future-error"],
        { ctx => registerAsync(ctx.env) },
        escapable: true, hostPolicy: true,
        after: ["base"])
}
