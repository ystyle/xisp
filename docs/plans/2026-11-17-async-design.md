# Xisp 异步（spawn + Future）设计

> **状态**：设计已定，**未实施**
> **前置**：§4 的两项前置改造（阻塞项，必须先做）
> **后续**：`xisp_net` 落地时要处理共享 Client 的线程安全（见 [`2026-11-17-net-design.md`](2026-11-17-net-design.md) §6）
> **相关**：[`2026-10-11-capability-domains-design.md`](2026-10-11-capability-domains-design.md) · [`2026-11-17-json-design.md`](2026-11-17-json-design.md)

---

## 1. 目标与非目标

**目标**：让脚本能并行执行多个求值任务，并把结果取回来。

```lisp
(let [f1 (spawn (slow-compute 1))
      f2 (spawn (slow-compute 2))]
  (+ (future-get f1) (future-get f2)))
```

**非目标（本轮不做）**

- **`async` / `await` 语法**：xisp 不做协程/状态机改写。`spawn` + `Future` 已够表达并发（这也是 `docs/design.md` 既定的模型）；
- 线程池 / 调度器（首版一个 spawn 一个仓颉线程）；
- 跨进程、分布式 actor；
- 结构化并发（structured concurrency）的强约束（父退出时强制收割子任务）—— 首版只在 `LispInterpreter` 关闭时收割。

---

## 2. 为什么它必须排在网络之前

| 判据 | 说明 |
|---|---|
| 改的是核心 | 求值器、环境容器、中断、三后端 —— 早做早暴露，改错了发布后是破坏性变更 |
| 网络蹭它 | 网络模块一旦有共享 `Client` 实例，多线程复用就是必答题；异步先做，网络落地时一次设计对 |
| 零外部依赖 | `spawn` 是语言关键字、`Future<T>` 在 `std.core`、锁在 `std.sync` —— 不碰 `CANGJIE_STDX_PATH` |
| 未知更少 | 网络有 TLS vs `--static` 这个可能推翻方案的风险；异步只有语义要定，没有技术未知 |

---

## 3. 归属：core 内，做成能力域 `async`

`spawn` / `Future` 是 `std.core` 与语言关键字的能力，**不引入外部依赖**，故属 core（不是模块问题）。

但**要做成可装配的域**：

```cangjie
SimpleCapability("async",
    ["spawn", "future-get", "future-try-get", "future-done?", "future-pending?", "future-cancel"],
    { ctx => registerAsync(ctx.env) },
    escapable: true,      // 能绕过单线程假设 → 保守标 true
    hostPolicy: true,     // 起线程是宿主资源
    after: ["base"])
```

`escapable: true` 的理由：沙箱假设"一次只有一个求值线程"，`spawn` 直接打破它。`presetPure` 按属性推导会排除它 —— 正确。

---

## 4. 前置改造（阻塞项）

这三项不做，异步的语义就是错的，不是"慢一点"的问题。

### 4.1 `Environment.defineVersion` 是 `static var`（**语义会错**）

```cangjie
// core/src/types/types.cj:39
public static var defineVersion: ArithVersionCell = ArithVersionCell()
```

它是**全局绑定版本号**，用于让所有帧的 `globalRefs` 缓存失效。多线程下两个问题：

1. **`value += 1` 非原子** —— `ArithVersionCell` 是个裸可变类，无锁。两个线程同时读-改-写会丢一次递增；
2. 丢递增 → 某线程的缓存**该失效却没失效** → 查到旧绑定，**语义错**（不是慢）。

同源问题：`Environment.arithVersion`（每环境一个 `ArithVersionCell`，但子环境共享父的那个实例），`Evaluator` 的 `arithGuardVersion` / `deferGuardVersion` 缓存。

**修法（二选一，倾向 B）**

| 方案 | 说明 |
|---|---|
| A：加锁 | 每次 `define` / `set!` 都锁一次。正确但热路径有开销 |
| **B：改成线程局部** | 版本号本就是"让本线程的缓存失效"的工具，跨线程共享它没有意义。`ThreadLocal` 或每 `Evaluator` 一个计数器 |

选 B：语义更准确（缓存是 per-Evaluator 的，失效信号也该 per-Evaluator），且**热路径零开销**。

### 4.2 `jitCurEnv` 是包级全局变量（**JIT 完全不能多线程**）

```cangjie
// xisp_bc/src/jit_runtime.cj:25
var jitCurEnv: Environment = Environment()
```

`jit_runtime.cj:251` 还在 `jitCurEnv = env` 直接改它。两个线程同时跑 JIT 代码会互相踩这个全局环境 —— 结果是把 A 线程的变量解析到 B 线程的环境里。

同类的全局状态还有 `jit_codegen.cj` 的 `entryTablePtr` / `resolveFnAbs` / `budgetCellAddr` / `bailCellAddr`。

**首版修法（选 A）**

| 方案 | 说明 |
|---|---|
| **A：spawn 体内禁用 JIT** | 子线程的 `ExecutionBackend` 强制回退到 BC 或 AST。简单、安全、可测 |
| B：把 `jitCurEnv` 等改成线程局部 | 能真正多线程跑 JIT，但要审一遍所有 `static var` 的用法，且要验 JIT 产物可重入 |

选 A。B 留作后续（要单独评估：JIT 生成的机器码是否可重入、`entryTable` 是否可并发写）。

**这个限制要在文档里明说**：子线程里 JIT 静默回退 —— 不是"JIT 不能用"，而是"子线程用 BC"。

### 4.3 `Environment.bindings` 是非并发容器

```cangjie
private var bindings: HashMap<String, LispValue>   // std.collection.HashMap
```

多线程下同时读写会坏（不只是数据不一致，`HashMap` 写时 rehash 会让并发读崩溃）。

**修法：懒分配读写锁**

- 平时（没有 spawn 存在的环境树）**不分配锁**，`lookup` / `set` 走原路径，**零开销**；
- 某环境被 `spawn` 共享时，给它和它的祖先链打标记并分配一把 `ReadWriteLock`；
- 打标记后，该环境的 `lookup` 走读锁、`set`/`define` 走写锁。

代价：标记之后该环境的所有访问有锁开销。可接受 —— 只有真用了 spawn 的环境才付。

---

## 5. 环境语义：**共享环境树 + 写入加锁**

这是最关键的语义决策。三种方案：

| 方案 | 语义 | 取舍 |
|---|---|---|
| **A（采用）共享 + 写锁** | `(spawn (set! counter (+ counter 1)))` 对父环境**可见** | 最接近用户直觉；代价是 §4.3 的锁 |
| B 快照（spawn 时克隆环境链） | 子线程的写**不传出**，像 Erlang 进程 | 安全、无锁；但 `set!` 对外不可见会让人困惑，且克隆成本高 |
| C 共享读、禁止跨线程写 | 写共享作用域直接报错 | 最安全；但限制太硬，`(spawn ...)` 里连累加器都不能用 |

**选 A**。理由：

1. Lisp 用户看到 `(spawn ...)`，直觉是"这段代码在别处同时跑"，不是"跑在另一个宇宙"；
2. B 的克隆在有闭包 + 引用类型（`HashMap`）时语义很怪 —— 克隆出来的是同一个 `HashMap` 引用，"快照"是假的；
3. C 会让 `spawn` 变得几乎没用（任何 `set!` 都报错）；
4. 竞态由锁兜住，用户要更严格可以用 §9 的严格模式。

**每线程一个 `Evaluator`，共享同一棵 `Environment` 树。** 因为 `Evaluator` 有 10+ 个可变字段：

```
currentFilePath / moduleRegistry / moduleLoader / moduleParser / currentModule
/ currentLoader / backend / evalDepth / compileSuspended
/ arithGuardVersion+cache / deferGuardVersion+cache
```

这些全是"当前正在求值什么"的状态，跨线程共享必错。

---

## 6. API

### 6.1 修正 `docs/design.md` 的写法

`design.md` §5 写的是 `(future.get)` / `(future.get(timeout: 5000))` / `(future.cancel)` —— **这是方法调用语法，xisp 没有成员访问**（已核实：全仓无 `member-access`）。改成一组一等函数：

```lisp
(spawn expr)              ; → Future，expr 在**新线程里**求值
(future-get f)            ; 阻塞等待 → 结果值
(future-get f 5000)       ; 带超时（毫秒）→ 结果值 或 Error(Timeout)
(future-try-get f)        ; 非阻塞 → 结果值 或 nil（未完成时）
(future-done? f)          ; → Bool
(future-pending? f)       ; → Bool
(future-cancel f)         ; 发取消请求（不强制终止，见 §8）
```

`Future` 用 `LispValue.External` 承载（0.6.0 已有的接缝），实现：

| 接口 | 用途 |
|---|---|
| `LispExternal` | `typeName` → `"future"`，`display()` → `#<future done>` |
| `LispExternalCallable` | 让 `(f)` 等价于 `(future-get f)` —— 便捷写法，不是必需 |

### 6.2 `spawn` 必须是特殊形式（②类）

**这是必须的**：`(spawn expr)` 里 `expr` 要在**新线程**求值。若 `spawn` 是普通函数，实参会在当前线程先求值，spawn 就没意义了。

故 `spawn` 属**②求值器拦截**（与 `map` / `eval` / `apply` 同层）：

- 在 `trySpecialForm` 的派发表里新增 `case "spawn"`；
- 按 §3 的"可替换"原则，**用户 `define` 同名可顶掉**（进让位集合）；
- 字节码后端要有一条对应的编译路径（或走 `compileSuspended` 回退 —— 但那样每次 spawn 都回退整树，性能差，应在编译器里特化）。

`future-*` 则是③类注册函数（吃 Future 值，无控制流语义）。

---

## 7. 错误语义（与 0.6.0 对齐）

| 情况 | 结果 |
|---|---|
| `expr` 体内产生 `Error` | 中止**该线程**的求值链，`Future` 持有该 `Error`；`(future-get f)` **返回这个 Error 值**（不抛、不再包一层） |
| `future-get` 超时 | `Error(XispError(RuntimeError, "future timeout"))` |
| 取消后再 `future-get` | 返回该线程结束时留下的值（可能是 `Error`，也可能是部分结果） |
| 对非 Future 调 `future-get` | `Error(TypeError, "expected a future")` |
| `spawn` 时线程创建失败 | `Error(RuntimeError, "…")` —— 在**当前**线程直接返回，不产生 Future |

**`future-get` 拿到 Error 后不"中止调用方"** —— 这是一个刻意的例外：Error 是 Future 的**结果**，不是"当前链"的错误。调用方要自己判断：

```lisp
(let [r (future-get f)]
  (if (error? r) (handle r) (use r)))
```

这一条要写进 `docs-site/api/`，否则会与"错误一律中止"的原则打架。理由：并发任务失败是**预期内的信息**，不是求值链断了。

---

## 8. 取消与中断

仓颉的 `Future.cancel()` **只发终止请求，不停止线程** —— 线程要自己查 `Thread.currentThread.hasPendingCancellation`。

这正是 xisp 已有的**协作式中断**模型，直接对接：

| 现有机制 | 现状 |
|---|---|
| `Evaluator.interruptChecker: ?(() -> Bool)` | 宿主注入一个检查回调 |
| 检查点 | 4 处：求值入口、`NativeFunc` 调用前后、高阶函数迭代体、BC VM 指令派发 |
| 中断表示 | `Error(RuntimeError, INTERRUPTED_MESSAGE)` |

**接入方式**：子线程的 `Evaluator.setInterruptChecker` 注入一个组合检查器：

```
查该线程的取消标记（future-cancel 设置）
  || 查 Thread.currentThread.hasPendingCancellation
  || 查该线程自己的超时截止时间（future-get 的 timeout 也走这里）
```

于是**不需要新增检查点** —— 已有的 4 处自动生效。`future-cancel` 只是让那个回调开始返回 `true`。

注意：`future-get` 的 timeout 是**等待方的超时**，不等于**被等方的超时**。首版两者都接同一个标记（`future-get` 超时后顺手取消对方），文档里要写明。

---

## 9. 沙箱与严格模式

`spawn` 打破"一次只有一个求值线程"的假设，所以沙箱不能只是"允许/禁止"：

| 模式 | 行为 |
|---|---|
| 默认 | 允许 spawn，环境树加锁（§5 方案 A） |
| `withNoConcurrency()`（新增） | `spawn` 未注册 → `UndefinedFunction` |
| 可选：`withStrictConcurrency()` | 允许 spawn，但子线程写共享作用域 → `Error(RuntimeError, "cross-thread write")`（§5 方案 C） |

`withSandbox()` **不**自动禁 spawn —— 沙箱管的是"能不能逃逸/碰宿主资源"，并发本身不是逃逸。但要在 `withSandbox` 的文档里提醒：并发 + 无超时 = 可能在跑 30 秒的任务，建议配 `withTimeout`。

---

## 10. 三后端

| 后端 | 多线程可行性 | 依据 |
|---|---|---|
| AST | ✅ 每线程一个 `Evaluator` 即可 | `Evaluator` 状态是实例字段（除 §4.1 的 static 版本号） |
| BC | ✅ 原则上可以 | `BytecodeVM.registers: ArrayList<LispValue>` 是**实例字段**，每线程一个 VM 实例即可 |
| **JIT** | ❌ **首版禁用** | `jitCurEnv` 等是包级 `static var`（§4.2） |

**必须实测**（不能只看代码）：

1. BC VM 多实例并发跑同一段编译产物，结果是否与串行一致；
2. JIT 在子线程里被禁用的实现是否可靠（是不是所有路径都回退了）；
3. 三后端在 spawn 场景下的错误语义是否一致（加进 `BackendDiffTest` 的语料）。

---

## 11. 待定与风险

| # | 项 | 现状 | 处置 |
|---|---|---|---|
| A1 | 环境加锁后的性能 | 未测 | 先测「无 spawn 时零开销」与「有 spawn 时 lookup 代价」，写进基准 |
| A2 | JIT 能否改成可多线程 | 只知道 `jitCurEnv` 挡路 | 首版禁用；后续单独评估（不在本设计范围） |
| A3 | 线程数上限 | 未定 | 首版不设限（一个 spawn 一个 OS 线程），文档里提醒"不要 spawn 十万个" |
| A4 | 解释器关闭时未收割的子线程 | 未定 | 首版：`LispInterpreter.close()` 时 join 所有活跃 Future；不 join 的子线程会造成进程僵死 |
| A5 | `spawn` 在编译后端的特化 | 未做 | 若走 `compileSuspended` 整树回退，性能会很差（对照宏的历史：BC 下慢 12 倍）——**要像宏那样做预展开式特化**，不能只回退 |

A5 值得强调：宏曾因为"回退整树"在 BC 下慢 12 倍，最后靠编译前展开解决。`spawn` 同理，不能靠回退。

---

## 12. 验收标准

1. §4 的三项前置改造各自有测试：`defineVersion` 并发递增不丢、JIT 在子线程回退、环境锁在无 spawn 时零开销；
2. 同步语义：
   - `(spawn (+ 1 2))` → `future-get` = `3`；
   - `(spawn (set! counter (+ counter 1)))` 之后 `counter` 递增（方案 A 的可观测证据）；
   - `(spawn (error "x"))` → `future-get` 返回该 `Error`，**调用方不中止**；
3. 取消：`(future-cancel f)` 后长循环能在合理时间内停下（用 `(spawn (loop-forever))` 测）；
4. 三后端行为一致（AST / BC 各跑一遍，JIT 预期走回退）；
5. `docs-site/api/` 新增并发章节；`docs/design.md` §5 的 `(future.get)` 写法订正为 `(future-get f)`；
6. `xisp_tests` 里新增用例**不依赖真实耗时**（用 `hashmap` 做同步点，不用 sleep），保证 CI 稳定。

---

## 13. 与既有文档的关系

- `docs/design.md` §5「异步支持 (spawn + Future<T>)」：模型正确，但**方法调用语法要订正**（§6.1）；
- `task.md` §4.5：三条待办（spawn 宏 / Future 操作 / 线程工具）**照旧成立**，加一条"§4 的前置改造"；
- 能力域文档 §6.3「异步域的定位」：结论一致（属 core + 运行期装配），本设计把它落成具体的域声明。
