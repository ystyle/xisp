# 并发（spawn / Future）

Xisp 的并发模型是 **`spawn` + `Future`**（不是 `async` / `await`）：
`(spawn expr)` 在新线程里对 `expr` 求值，返回一个 `Future` 值，用 `future-get` 取结果。

```lisp
(let [f (spawn (+ 1 2))]
  (future-get f))          ; => 3
```

多个任务并行：

```lisp
(let [f1 (spawn (+ 1 1))
      f2 (spawn (+ 2 2))]
  (+ (future-get f1) (future-get f2)))   ; => 6
```

::: tip 由 core 提供，不属可选模块
`spawn` 与 `future-*` 是核心能力（用 `std.core` 的线程与 `Future`），不需要额外依赖。
它们属于能力域 `async`，`presetPure` 按属性推导会排除它（并发不是"纯函数"）。
:::

---

## 环境是共享的，不是快照

子线程**共享**父线程的环境。`set!` 与 `define` 对父线程可见：

```lisp
(begin
  (define c 0)
  (future-get (spawn (set! c 42)))
  c)                       ; => 42
```

这是刻意的设计：Lisp 用户看到 `spawn`，直觉是"这段代码在别处同时跑"，
而不是"跑在另一个宇宙"。竞态由环境锁兜住（见下方"实现说明"）。

**每个线程有自己的求值器**，但共享同一棵环境树 —— 求值器里有十几个
"当前正在求值什么"的可变字段，跨线程共享必错。

---

## 取结果的几种方式

| 形式 | 行为 |
|---|---|
| `(future-get f)` | 阻塞等待，返回结果；**任务失败时把错误抛给调用方** |
| `(future-get f 5000)` | 最多等 5000 毫秒；超时返回错误并**顺手向任务发取消请求** |
| `(future-try-get f)` | 不阻塞；任务未完成返回 `nil` |
| `(future-done? f)` | 是否已完成（`Bool`） |
| `(future-pending? f)` | 是否还在跑（`Bool`） |
| `(future-ok? f)` | **阻塞到完成**，返回任务是否成功（`Bool`） |
| `(future-error f)` | **阻塞到完成**，失败返回错误文本（字符串），成功返回 `nil` |
| `(future-cancel f)` | 发取消请求（不强制终止，见下方） |

`timeout` 给 `0` 或负数等同于不设限。

::: warning 不要在取结果之前依赖 done? / pending?
刚 `spawn` 完，任务大概率还没跑完 —— 但这是**竞态**，不是保证。

```lisp
(future-done? (spawn 1))   ; 可能是 true 也可能是 false
```

需要确定状态时先 `future-get`（阻塞），之后状态就确定了：

```lisp
(let [f (spawn 42)]
  (future-get f)
  (future-done? f))        ; => true
```
:::

---

## 失败怎么处置

`Error` 一旦参与求值就中止，这是全语言的统一语义，`spawn` **不能例外**。
所以"把错误当普通值返回"是做不到的 —— 错误值连作为实参传给谓词都做不到。
失败处置走两条路：

### 一、fail-fast（默认）：让调用方也停下

```lisp
(future-get (spawn (error "boom")))   ; => [RuntimeError] "boom"，调用方中止
```

这是默认路径，适合"子任务失败就该整体失败"的场景。

### 二、容错：先判断，再取值

```lisp
(let [f (spawn (may-fail))]
  (if (future-ok? f)
    (use (future-get f))
    (handle (future-error f))))
```

`future-ok?` 与 `future-error` 会**阻塞到任务完成**，但它们内部拿到错误值后只返回
`Bool` / 字符串，所以不会让调用方中止。

```lisp
(future-ok? (spawn (error "boom")))     ; => false
(future-error (spawn (error "boom")))   ; => "[RuntimeError] \"boom\""
(future-ok? (spawn 42))                 ; => true
(future-error (spawn 42))               ; => nil
```

---

## 取消是协作式的

`future-cancel` **只是发一个请求**，不强制终止线程。任务在下一个检查点
（求值入口、高阶函数迭代体、宿主函数调用前后等）自行收尾：

```lisp
(let [f (spawn (for-each (lambda (i) nil) (range 0 10000000)))]
  (future-cancel f)
  (future-get f))        ; 尽快返回；可能是中断错误，也可能是已完成的结果
```

**结果不确定是正常的**：任务可能已经跑完了，也可能被停在检查点。
如果需要"一定能被打断"，任务体内应该有足够的检查点（长循环写在 `for-each` /
`map` 等内建里就有）。

---

## 关闭并发

```cangjie
let interp = LispInterpreter([withNoConcurrency()])
```

`spawn` 会返回 `Error(RuntimeError, "spawn is disabled …")`，其它能力不受影响。

::: tip 为什么不能用域装配关掉 spawn
`spawn` 是②类符号（求值器拦截），实参必须在**新线程里**求值，所以它不注册到环境 ——
域装配动不了它。这也是它没被放进 `async` 域的 `symbols` 里的原因。
:::

---

## 实现说明（给要读源码的人）

::: details 子线程为什么一律走 AST

即使主线程开了字节码或 JIT，`spawn` 的子线程也走 AST。三条理由都不是性能取舍：

1. `BytecodeVM` 有三个类级 `static` 字段（函数表 / 全局引用缓存 / 函数 id 映射），
   并发写会损坏 `ArrayList` 与 `HashMap`；
2. 其中函数表承载**跨线程语义** —— 字节码的闭包里存的是 VM 内部的函数 id，
   跨 VM 传递后 id 对不上；改成实例字段会打破"闭包可传递"；
3. JIT 的若干运行时状态是包级 `static var`，完全不能并发。

而 AST 的闭包是"参数 + 体 + 环境"，纯数据，天然跨线程可用。

代价：`spawn` 这一层在字节码/JIT 下由编译器回退给 AST。但父线程这边只做
"取表达式 + 起线程 + 返回 Future"，回退开销微乎其微。
:::

::: details 环境锁是懒分配的
没有用过 `spawn` 的解释器，环境里**没有锁** —— 每次绑定访问只多一次可空判断。
只有被 `spawn` 共享过的环境（及其祖先链）才分配读写锁：

- 读（`lookup` / `contains` / `getKeys`）走读锁，多读并发；
- 写（`define` / `set!`）走写锁；
- 沿父环境链递归查找时每层各自加解锁（读锁可重入），不会死锁；
- `set!` 的父链递归发生在**锁外**，避免"持本层锁再请求父层锁"的交叉死锁。
:::

---

## 参见

- [错误语义](control) —— 为什么错误不能当值处理
- [选项系统](/integration/options) —— `withNoConcurrency` / `withTimeout` / `withSandbox`
