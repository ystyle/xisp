package ystyle::xisp_tests

import ystyle::xisp.*
import ystyle::xisp_bc.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList
import std.sync.*

/**
 * 异步能力（`spawn` + `Future`）
 *
 * 设计见 `docs/plans/2026-11-17-async-design.md`。
 *
 * ## 测试纪律：**不依赖真实耗时**
 *
 * 不用 `sleep` 做时序等待 —— 那会让 CI 抖动、并且在慢机器上假失败。
 * 需要"确定已完成"时就 `future-get`（阻塞等待），之后 `done?` 必然为真。
 * 因此本文件**不**断言"刚 spawn 完必然是 pending"这类竞态状态。
 */
@Test
class AsyncTest {
    func evalIn(opts: Array<InterpreterOption>, code: String): String {
        let it = LispInterpreter(opts)
        var r: LispValue = Nil
        for (e in Parser.parseFromString(code)) {
            r = it.evalExpr(e)
        }
        r.toString()
    }

    func quiet(): Array<InterpreterOption> {
        [withQuietMode()]
    }

    // ==================== 基本语义 ====================

    /** `spawn` 在新线程求值，`future-get` 取回结果 */
    @TestCase
    func testSpawnBasic() {
        println("\n=== spawn + future-get ===")
        let opts = this.quiet()
        @Assert(this.evalIn(opts, "(future-get (spawn (+ 1 2)))") == "3")
        @Assert(this.evalIn(opts, "(future-get (spawn (* 3 4)))") == "12")
        @Assert(this.evalIn(opts, #"(future-get (spawn "hi"))"#) == "\"hi\"")
        @Assert(this.evalIn(opts, "(future-get (spawn (list 1 2 3)))") == "(1 2 3)")
        // 多个并发任务，结果都能取回
        @Assert(this.evalIn(opts,
            "(let [f1 (spawn (+ 1 1)) f2 (spawn (+ 2 2))] (+ (future-get f1) (future-get f2)))") == "6")
        println("✓ 基本求值、多任务并发取值")
    }

    /**
     * **子线程的 `set!` 对父环境可见**（环境是共享的，不是快照）
     *
     * 这是设计 §5 的核心语义决策。若改成"快照"，`(spawn (set! counter ...))`
     * 对外不可见 —— Lisp 用户看到 `spawn` 会以为"这段代码在别处同时跑"，
     * 而不是"跑在另一个宇宙"。
     */
    @TestCase
    func testSetVisibleToParent() {
        println("\n=== 子线程 set! 对父环境可见 ===")
        let opts = this.quiet()
        @Assert(this.evalIn(opts,
            "(begin (define c 0) (future-get (spawn (set! c 42))) c)") == "42")
        // 子线程 define 的新名字，父线程也能看到（写入的是同一个环境对象）
        @Assert(this.evalIn(opts,
            "(begin (future-get (spawn (define g 7))) g)") == "7")
        println("✓ set! 与 define 都写进共享环境")
    }

    /**
     * **子线程的失败怎么处置**
     *
     * `LispValue.Error` 一旦参与求值链就中止，这是全仓统一的语义，`spawn` 不能例外 ——
     * 所以"`future-get` 返回错误值但不中止调用方"**做不到**（错误值连作为实参传给
     * 谓词都做不到，实参求值就中止了）。失败处置走两条路：
     *
     * - `future-get`：fail-fast，子线程的错误原样抛给调用方；
     * - `future-ok?` / `future-error`：它们内部 `get()` 拿到 Error 值，但只返回
     *   `Bool` / 字符串，故不触发中止 —— 这是容错路径。
     */
    @TestCase
    func testFailureHandling() {
        println("\n=== 子线程失败的两种处置 ===")
        let opts = this.quiet()
        // 容错路径：可判定、可取出错误文本，且不中止
        @Assert(this.evalIn(opts, "(future-ok? (spawn (error \"boom\")))") == "false")
        let errText = this.evalIn(opts, "(future-error (spawn (error \"boom\")))")
        if (!errText.contains("RuntimeError")) {
            @Fail("future-error 应含 RuntimeError，实得 ${errText}")
        }
        // 成功路径
        @Assert(this.evalIn(opts, "(future-ok? (spawn 42))") == "true")
        @Assert(this.evalIn(opts, "(future-error (spawn 42))") == "nil")
        // 组合：先判断再取值
        @Assert(this.evalIn(opts,
            "(let [f (spawn (error \"x\"))] (if (future-ok? f) (future-get f) \"handled\"))") ==
            "\"handled\"")
        // fail-fast：future-get 把错误抛给调用方
        @Assert(this.evalIn(opts, "(future-get (spawn (error \"boom\")))").contains("RuntimeError"))
        // 未绑定变量这类求值期错误同样被捕获在 Future 里
        @Assert(this.evalIn(opts,
            "(future-error (spawn (+ 1 undefined-var)))").contains("UnboundVariable"))
        println("✓ future-ok? / future-error 容错；future-get 仍 fail-fast")
    }

    // ==================== Future 操作 ====================

    /** `type-of` / 谓词 */
    @TestCase
    func testFutureTypeAndPredicates() {
        println("\n=== Future 的类型与状态谓词 ===")
        let opts = this.quiet()
        @Assert(this.evalIn(opts, "(type-of (spawn 1))") == "\"future\"")
        @Assert(this.evalIn(opts, "(external? (spawn 1))") == "true")
        @Assert(this.evalIn(opts, "(external-type (spawn 1))") == "\"future\"")
        // get 之后状态确定：完成、不 pending、try-get 有值
        @Assert(this.evalIn(opts,
            "(let [f (spawn 42)] (future-get f) (future-done? f))") == "true")
        @Assert(this.evalIn(opts,
            "(let [f (spawn 42)] (future-get f) (future-pending? f))") == "false")
        @Assert(this.evalIn(opts,
            "(let [f (spawn 42)] (future-get f) (future-try-get f))") == "42")
        println("✓ type-of 报 future；get 之后 done?=true / pending?=false / try-get 有值")
    }

    /**
     * `future-get` 的超时参数
     *
     * 保证超时的办法：等一个永远不会完成的任务。这里用"子线程等自己"造不出来，
     * 所以用**已完成**的任务验证"超时不影响正常结果"，再验证 `timeout <= 0`
     * 等同于无超时。真正的超时分支无法在不引入时序依赖的前提下稳定复现，
     * 故只断言"超时参数被接受且不破坏正常路径"。
     */
    @TestCase
    func testFutureGetWithTimeout() {
        println("\n=== future-get 带超时参数 ===")
        let opts = this.quiet()
        // 任务已完成，给个宽裕超时 → 正常拿到结果
        @Assert(this.evalIn(opts,
            "(let [f (spawn (+ 1 2))] (future-get f 60000))") == "3")
        // timeout <= 0 按无超时处理
        @Assert(this.evalIn(opts,
            "(let [f (spawn (+ 2 3))] (future-get f 0))") == "5")
        @Assert(this.evalIn(opts,
            "(let [f (spawn (+ 2 3))] (future-get f -1))") == "5")
        println("✓ 超时参数可用，<=0 等于不设限")
    }

    /**
     * 取消：`future-cancel` 只发请求，不强制终止
     *
     * 断言的是"取消后仍能正常取回结果且不 hang"，**不**断言"任务一定提前停下" ——
     * 那取决于子线程跑到哪一步，属于竞态，无法在不引入 sleep 的前提下稳定验证。
     * 协作式中断的检查点本身由 `error_propagation_test` 与 `sandbox_test` 覆盖。
     */
    @TestCase
    func testFutureCancel() {
        println("\n=== future-cancel ===")
        let opts = this.quiet()
        // 取消一个已完成的任务：无害
        @Assert(this.evalIn(opts,
            "(let [f (spawn 1)] (future-get f) (future-cancel f) (future-get f))") == "1")
        // 取消一个长任务：必须能返回（不 hang、不崩）
        let r = this.evalIn(opts,
            "(let [f (spawn (for-each (lambda (i) nil) (range 0 2000)))] (future-cancel f) (future-get f))")
        // 结果可能是 nil（跑完了）或中断错误（被停在检查点）——两者都算正确
        if (!(r == "nil" || r.contains("interrupted"))) {
            @Fail("取消后结果异常：${r}")
        }
        println("✓ 取消已完成任务无害；取消长任务能返回")
    }

    // ==================== 参数与错误 ====================

    /** `spawn` 必须恰好一个表达式 */
    @TestCase
    func testSpawnArity() {
        println("\n=== spawn 参数个数 ===")
        let opts = this.quiet()
        @Assert(this.evalIn(opts, "(spawn)").contains("ArityMismatch"))
        @Assert(this.evalIn(opts, "(spawn 1 2)").contains("ArityMismatch"))
        println("✓ (spawn) / (spawn a b) 都报 ArityMismatch")
    }

    /** 非 Future 参数 → `TypeError` */
    @TestCase
    func testNonFutureArgument() {
        println("\n=== 非 Future 参数报 TypeError ===")
        let opts = this.quiet()
        for (code in ["(future-get 42)", "(future-try-get 42)",
                      "(future-cancel 42)"]) {
            let r = this.evalIn(opts, code)
            if (!r.contains("TypeError")) {
                @Fail("${code} 应报 TypeError，实得 ${r}")
            }
        }
        println("✓ future-get / try-get / cancel 对非 Future 都报 TypeError")
    }

    /** `withNoConcurrency()` 关掉 spawn */
    @TestCase
    func testConcurrencyDisabled() {
        println("\n=== withNoConcurrency 关掉 spawn ===")
        let opts: Array<InterpreterOption> = [withQuietMode(), withNoConcurrency()]
        @Assert(this.evalIn(opts, "(spawn 1)").contains("disabled"))
        // 其它能力不受影响
        @Assert(this.evalIn(opts, "(+ 1 2)") == "3")
        @Assert(this.evalIn(opts, "(length (list 1 2))") == "2")
        println("✓ spawn 被拒，普通求值不受影响")
    }

    // ==================== 前置改造的验证 ====================

    /**
     * **版本号递增必须原子**（前置改造 1）
     *
     * `Environment.defineVersion` 是全局共享的绑定版本号，多线程同时 `define` 会
     * 并发递增。若用 `value = value + 1`（读-改-写），会丢递增 —— 后果是某线程的
     * 缓存该失效却没失效，**继续用旧绑定（语义错，不只是慢）**。
     *
     * 这里直接压 `ArithVersionCell.bump()`：4 线程 × 20000 次，总数必须精确。
     */
    @TestCase
    func testVersionCellIsAtomic() {
        println("\n=== 版本号并发递增不丢 ===")
        let cell = ArithVersionCell()
        let threads = ArrayList<Future<Unit>>()
        let perThread: Int64 = 20000
        let threadCount: Int64 = 4
        for (i in 0..threadCount) {
            threads.add(spawn {
                =>
                for (j in 0..perThread) {
                    cell.bump()
                }
            })
        }
        for (f in threads) {
            f.get()
        }
        let expect = perThread * threadCount
        if (cell.value != expect) {
            @Fail("期望 ${expect}，实得 ${cell.value}（说明有递增丢失）")
        }
        println("✓ 4 线程 × ${perThread} 次 = ${cell.value}，无丢失")
    }

    /**
     * **spawn 会给环境链打共享标记**（前置改造 2）
     *
     * 标记后才懒分配读写锁。没 spawn 过时必须是 `false` ——
     * 那是"零开销"的证据：未标记的环境每次绑定访问只多一次 `Option` 判空。
     */
    @TestCase
    func testSpawnMarksEnvironmentShared() {
        println("\n=== spawn 后环境被标记为跨线程共享 ===")
        let it = LispInterpreter([withQuietMode()])
        if (it.getEnvironment().isSharedAcrossThreads()) {
            @Fail("未 spawn 时不应标记")
        }
        let r = it.eval("(future-get (spawn 1))")
        @Assert(r.toString() == "1")
        if (!it.getEnvironment().isSharedAcrossThreads()) {
            @Fail("spawn 后应标记")
        }
        // 再 spawn 一次不重复分配（shareAcrossThreads 短路）
        it.eval("(future-get (spawn 2))")
        @Assert(it.getEnvironment().isSharedAcrossThreads())
        println("✓ 未 spawn 不标记（零开销）；spawn 后标记且幂等")
    }

    /** 共享标记会传播到祖先环境（子线程要读外层变量） */
    @TestCase
    func testSharePropagatesToParents() {
        println("\n=== 共享标记传播到祖先链 ===")
        let root = Environment()
        let child = root.createChild()
        let grandchild = child.createChild()
        @Assert(!root.isSharedAcrossThreads())
        grandchild.shareAcrossThreads()
        @Assert(grandchild.isSharedAcrossThreads())
        if (!child.isSharedAcrossThreads()) {
            @Fail("父环境应一并标记")
        }
        if (!root.isSharedAcrossThreads()) {
            @Fail("祖先环境应一并标记")
        }
        println("✓ 标记沿 parent 链传到底")
    }

    // ==================== 三后端 ====================

    /**
     * 三个后端下 `spawn` 行为一致
     *
     * 子线程**一律走 AST**（`BytecodeVM` 有类级 `static` 表、`Closure` 里存 VM 内部
     * 函数 id，跨线程不成立），所以 BC / JIT 下 `spawn` 由编译器回退给 AST ——
     * 父线程这一层的回退代价只是"取表达式"，微乎其微。
     */
    @TestCase
    func testAllBackends() {
        println("\n=== 三后端一致 ===")
        let cases = [
            ("(future-get (spawn (+ 1 2)))", "3"),
            ("(begin (define c 0) (future-get (spawn (set! c 9))) c)", "9"),
            ("(let [f1 (spawn 1) f2 (spawn 2)] (+ (future-get f1) (future-get f2)))", "3"),
            ("(type-of (spawn 1))", "\"future\""),
        ]
        for (backend in backendNames()) {
            let opts = ArrayList<InterpreterOption>()
            opts.add(withQuietMode())
            if (backend != "AST") {
                opts.add(withBytecodeCompiler())
            }
            if (backend == "JIT") {
                opts.add(withJit())
            }
            for ((code, expect) in cases) {
                let r = this.evalIn(opts.toArray(), code)
                if (r != expect) {
                    @Fail("[${backend}] ${code} 期望 ${expect}，实得 ${r}")
                }
            }
        }
        println("✓ AST / BC / JIT 三后端结果一致")
    }

    /**
     * 字节码下 `spawn` 调用编译闭包 —— 靠「编译闭包降级」落地
     *
     * 字节码后端的 `Closure` 里存的是 VM 内部函数 id，而子线程的求值器一律是 AST
     * （跨线程安全），原先会直接报 `AST: closure without bytecode backend`。
     * 修法是 `applyProcedure` 在无后端时用 `decompiler` 把闭包降级成
     * `Procedure(参数名, AST 体, 捕获环境)` 再调。
     *
     * 这条用例钉住的就是那条路径：没有它，机制坏了也发现不了（HTTP handler 的用例
     * 只覆盖了「经 ctx.newEvaluator 的求值器」，不覆盖 spawn 的子线程）。
     */
    @TestCase
    func testSpawnCompiledClosure() {
        println("\n=== 字节码下 spawn 调用编译闭包 ===")
        let bc = this.evalIn([withQuietMode(), withBytecodeCompiler()],
            "(begin (define (f x) (* x 2)) (future-get (spawn (f 21))))")
        @Assert(bc == "42")
        // 高阶函数里传编译闭包给 spawn 的形式（闭包作为值跨线程）
        let hof = this.evalIn([withQuietMode(), withBytecodeCompiler()],
            "(begin (define (g h) (future-get (spawn (h 10)))) (define (inc x) (+ x 1)) (g inc))")
        @Assert(hof == "11")
        println("✓ BC 下编译闭包在子线程里能正常调用（自动降级成 AST 闭包）")
    }
}
