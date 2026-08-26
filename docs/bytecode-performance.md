# Xisp 字节码性能对比报告

**版本**: 0.1
**日期**: 2026-08-25
**分支**: feat/bytecode-compiler（含 `1d589bb` 全语法编译）
**状态**: 当前分支实测数据

---

## 1. 背景：两种执行模式

Xisp 支持两种求值模式，可通过 `--with-bytecode-compiler` 切换：

| 模式 | 说明 |
|---|---|
| **AST 解释**（默认） | 表达式解析为 S 表达式树后由求值器递归解释（环境链查找 + 特殊形式分派 + 调用栈管理） |
| **字节码编译**（`--with-bytecode-compiler`） | 顶层表达式编译为指令流（寄存器栈 VM），函数调用经**帧切换**在 VM 内执行；部分语义场景（宏展开、跨层闭包捕获、`&rest/&key` 参数、match 列表模式/guard、高阶回调等）回退 AST 兜底 |

VM 特性：寄存器栈帧（局部槽区与栈区分离）、调用点结果槽约定、闭包捕获环境（单层值复制）、全局引用实时解析、嵌套执行安全化、栈深不限（帧切换不走 AST 的 `stackDepth` 上限）。

---

## 2. 测试环境与方法

- **CPU**: AMD Ryzen 7 7840HS（16 线程）
- **内存**: 32GB；**内核**: Linux 6.14（Arch）
- **工具链**: Cangjie Compiler 1.1.3 (cjnative)、CJPM 1.1.3；项目 `compile-option = "-O2 --static"`（Release 构建）
- **基准套件**: `lisp-tests/perf/`（15 个场景，每个固定 **200 层外层循环 × 每层 k 次工作**，保证两种模式的递归栈都在 AST 限制（默认 1000）内）
- **统计**: 每场景每模式独立进程采样 **5 轮取中位数**（`time` 外部计时，含进程启动 ~15ms；启动噪声在毫秒级场景按注释说明）
- **复现**: `bash lisp-tests/perf/run.sh [轮数]`

> 注：AST 模式的 `stackDepth` 限制（默认 1000，每层递归消耗约 3 个栈深）意味着**两模式可比的递归深度上限约 250-300 层**；字节码帧切换不受此限制（深递归仅受宿主栈/内存约束）。

---

## 3. 总览结果

| 场景 | AST (ms) | BC (ms) | 加速 |
|---|---|---|---|
| 01 fib 直接递归 | 11163 | 800 | **14.0x** |
| 02 fib 间接调用（闭包值经局部变量） | 11206 | 800 | **14.0x** |
| 03 fact | 59 | 16 | **3.7x** |
| 04 尾递归求和 | 402 | 58 | **6.9x** |
| 05 列表 reduce 聚合 | 73 | 74 | 1.0x |
| 06 管道（map→filter→reduce） | 482 | 474 | 1.0x |
| 07 闭包计数器（捕获 set!） | 49 | 16 | **3.1x** |
| 08 闭包工厂（单层捕获） | 148 | 39 | **3.8x** |
| 09 match 字面量模式（5 子句） | 200 | 28 | **7.1x** |
| 10 quasiquote 构造（含 splice） | 38 | 28 | 1.4x |
| 11 let 绑定 + 算术密集 | 58 | 28 | **2.1x** |
| 12 set! 全局状态更新 | 28 | 17 | **1.6x** |
| 13 宏展开密度（when 宏） | 103 | 102 | 1.0x |
| 14 &rest 参数函数 | 60 | 71 | 0.8x |
| 15 字符串插值 | 48 | 29 | 1.7x |

**结论**：
- **函数调用密集的代码收益最大**（fib/tail-sum **7-14x**、match/fact 3-7x）：编译消除了递归解释的表达式树遍历、环境链查找、调用栈帧管理；
- **闭包场景 3-4x**：lambda 编译为子函数 + 帧切换闭包调用；
- **边界场景持平**（宏展开、`&rest`、字符串插值）——见第 5 节；**高阶回调**回调体已直入 VM（~14x），但高阶循环本体仍为瓶颈（第 5 节）。

> 解释器侧的分派/cache/超指令优化已推进三轮（见第 6 节历程）；当前瓶颈转移至**函数调用非内联路径与高阶循环本体**（map/filter/reduce 为 AST 侧内置），进一步提速依赖 **JIT 特化**（类型推测 + 机器码，见 bytecode-fw 设计文档）。

## 8. JIT 原型（可行性已验证，2026-08-25）

**通路验证**（`temp-match-bench`，libc mmap + 手写 x86-64 模板 + 仓颉 C-FFI）：
`mmap 匿名 RWX 页 → 写机器码 → CPointer<UInt8> → CFunc<(Int64)->Int64> 调用` —— 运行正确输出 42。

**fib 级模板编译原型**（手写 x86-64：递归自调用回填、栈管理、callee-saved rbx 保存）：
- `fib(n)` 机器码（39 字节）：cmp/jle 分支 + 双递归 call + 返回约定；
- **结果**：`fib(30) = 832040` 正确，**2.5ms**；
- **对比**：字节码 fib(30) 推算 ~32ms（13x）、AST ~440ms（176x）。

**结论**：仓颉完整支持运行时生成并执行机器码——"基于字节码的 JIT"通路确认。**后续工程**：通用"字节码 → 机器码模板"翻译器（寄存器分配 + LispValue 类型检查 + 去优化回退 + 与解释器帧兼容的调用约定）；IR 规范化（opcode 家族化）已为模板翻译铺路。

---

## 4. 分场景解读

### 4.1 函数调用密集（5-8x）

`fib`/`fact`/尾递归求和/`match` 循环：
- 递归体在 AST 侧每次调用需：特殊形式分派 → `env.lookup`（哈希链查找）→ 参数求值（每个子表达式递归）→ `createChild` + 绑定 → 返回清理；
- VM 侧一次编译后：函数体是线性指令流，参数在寄存器中**原位左移**（零复制）、帧切换 `O(1)`、全局引用在切换时**一次解析**入帧槽；
- `match` 字面量模式编译为 `OP_MATCH_LIT` + 跳转回填（5 子句仅 5 次比较），AST 侧是递归模式匹配。

### 4.2 闭包（3-4x）

- lambda 编译为独立 `CompiledFunction`，`MAKE_CLOSURE`（捕获对外嵌指令）创建闭包环境；闭包调用直接帧切换；
- 闭包计数器（捕获变量 `set!`）：`OP_SET_GLOBAL` 环境链修改 + 帧槽快照，与 AST 语义一致但省去整棵表达式树的解释。

### 4.3 轻量核心 1.5-2x

let 绑定（帧槽直读直写）、全局 `set!`、quasiquote 构造（`OP_CONS`/`OP_CONCAT` 指令 vs AST 递归展开）、字符串插值（桥接 NativeFunc 直调）。

---

## 5. 边界场景：为什么不加速

| 场景 | 原因 |
|---|---|
| `reduce`/`map`/`filter`（高阶回调） | 回调体已直入 VM（`callClosure`，~14x）；但高阶循环**本体**是 AST 侧内置（NativeFunc），每步解释——管线整体仍 ~1.0x（实测 fib(18)×40轮：840→807ms） |
| 宏展开密度 | 宏调用必须 AST 侧展开（编译会死循环/参数值语义错误），展开体在 AST 求值 |
| `&rest`/`&key`/默认值参数 | 编译回退 AST（`parseSimpleParams` 仅支持位置参数；原实现会静默产出错误字节码） |
| match 列表模式/guard | 回退 AST（AST 的列表模式识别为启发式，保真优先） |
| 跨层闭包捕获（如 `(lambda (x) ...)` 引用祖父帧变量） | 运行时槽号不可达，编译回退 AST（单层捕获正常编译） |

这些是**语义优先**的取舍：回退路径与 AST 行为**逐字节一致**（26 个 examples 双模式验证），代价是这些场景无收益（个别场景因编译尝试开销略慢，如 `&rest` 0.8x——若在意可去 `-O2` 化编译尝试或提前快检）。

**下一步优化方向**（按收益排序）：
1. ✅ 高阶回调直入 VM（`callClosure`，`770b76e`）——回调体帧切换执行 ~14x，但循环本体瓶颈未解；
2. 高阶循环本体编译/内联（map/filter/reduce 专用指令或 AST 循环下降）——管线提速的关键（工作量中等）；
3. `&rest`/`&key` 参数编译（参数绑定前缀代码，收益低）；
4. match guard/列表模式编译；
5. 编译缓存（同一 AST 表达式不重复编译）。

---

## 6. 优化历程

| 阶段 | 提交 | 效果 |
|---|---|---|
| AST 求值器性能优化 | `e04fcb8`（master） | AST 模式 ~52%（22.3s→10.6s @ fib30+fact20+sum-to） |
| 字节码初版（调用链断链） | `f57b958`~`1d3c1a5` | 仅 **~5-7%**（直接函数调用全部坠入 AST 解释） |
| **调用链修复 + 帧布局重构** | `d593c52` | 直接调用接帧切换；2038ms → **109ms（~18.7x）** @ 首版基准 |
| 合并 master AST 优化 | `3f5a953` | AST 基线 2038→~710ms |
| **全语法编译 + examples 双模式验证** | `1d589bb` | 15 场景上线（fib 9.3x） |
| 指令分派 match 化 + globalRefs 缓存 | `e71a639` | fib 1426→1220ms（-15%） |
| 超级指令（L+C 运算 / 比较跳转合一） | `8f60b7f` | fib 1220→858ms（-30%，12.9x） |
| 帧切换缓存数组化 + and/or/match 组合指令 | `2ea129c` | fib 858→800ms（**14.0x**） |
| equalValue Int/Float 快路径 | `0449756` | match 场景持平（瓶颈在调用/字符串，非比较） |
| opcode 家族化（OP_BIN_LC + kind） | `8cf840c` | case 数 45→42，保住跳转表分派（已实测 case 49 触发退化） |
| 比较/not 家族化（kind 4-7） | `edb6e43` | 语义统一至 OP_BIN_LC，零新增 case |
| **高阶回调直入 VM**（callClosure） | `770b76e` | 回调体 ~14x（fib 回调实测）；管线整体 ~1.0x（循环本体瓶颈） |
| **JIT 原型**（fib 模板，可行性） | `temp-match-bench` | fib(30) 2.5ms（字节码 ~32ms / AST ~440ms） |

---

## 7. 复现

```shell
# 构建（Release + O2）
eval "$(cjvs env zsh)" && cjpm build -j 16

# 运行全部场景（默认 5 轮，取中位数）
bash lisp-tests/perf/run.sh 5

# 手动单场景对比
time ./target/release/bin/ystyle::xisp.cli lisp-tests/perf/01-fib-direct.lisp
time ./target/release/bin/ystyle::xisp.cli --with-bytecode-compiler lisp-tests/perf/01-fib-direct.lisp
```

> 提示：毫秒级结果含进程启动 ~15ms；放大 k 或轮数可得更稳定数据。

## 9. JIT 正式工程 J1（字节码 → 机器码翻译器，2026-08-25 晚）

**实现**（feat/jit）：`jit_emit.cj`（x86-64 发射器+rel32 回填）/ `jit_codegen.cj`（翻译器：16B (tag,payload) 槽帧 + Int 快路径 + deopt 哨兵）/ `jit_runtime.cj`（mmap RWX 页、函数登记、解释器↔JIT 桥）/ `--with-jit`（叠于字节码之上）。

**ABI**：rdi=帧缓冲（16B×localSlots，桥填参数+全局引用），rsi=参数个数，rdx=ctx；callee 结果写 [rdi-16]。

**J1 实测（fib(30)）**：

| 模式 | 时间 | 倍率 |
|---|---|---|
| AST | ~11.1s | 1x |
| 字节码 | 224ms | 50x |
| **JIT（J1）** | **27ms** | **410x（8.3x BC）** |

**deopt 机制**：慢分支（tag guard 失败/非自调用）→ 恢复帧后返回 null → 桥感知 → VM 回退解释器重跑该闭包调用（语义保真）；嵌套调用逐层级联退栈。

**已验证**：fib(10)=55、fib(25)=75025；6/6 JitTest；355/355 全量测试；26 examples JIT vs BC 逐字节一致。约束：仅 Int 快路径 + 自递归调用；互递归/跨函数 JIT 调用暂回退（待 J2 互调桥）。

## 10. J2 增量（跨函数调用基建 + 全局解析 helper，2026-08-25 深夜）

- **全局解析重构**：callee 全局引用不再由调用方复制，改由 callee prologue 自解析——
  - 自/同函数调用（rsi 置 FLAG 位）：调用方直接把自身全局槽复制进 callee 帧（零开销）；
  - 跨函数调用：callee 经 `@C jitResolveGlobals` helper 实时 env.lookup（与 switchFrame 语义一致）；
- **入口表基建**：`jitEntryTable`（closureId→机器码入口，固定容量）+ r9 装载指令（`mov r9,[r10+rax*8]`）就绪；
- **跨函数 JIT 调用**：单程 f→p 已正确（4/16/503 与 BC 一致）；**互递归 ping-pong 仍有 bug，本轮以「非自调用 → deopt 回退」保正确**（互递归 f(10)=5 ✓，测试覆盖）；
- **性能**：fib(30) 27ms 保持 8.3x（FLAG 快路径避免了 helper 每帧调用——修复过程中发现 `and rax,imm32` 会截断高位的坑）；
- 357/357 测试（+2 JitTest），26 examples JIT vs BC 逐字节一致。

## 11. J4 整数特化（INT-spec，2026-08-26）

- `jit_codegen_int.cj`：全 INT/BOOL 函数的 8B 槽 + 零 tag 检查形式（槽 8B@[rbp-8(i+1)]、栈项 8B、
  调用帧缓冲仍 16B 与桥 ABI 一致）；无 deopt——资格判定（常量 Int/Boolean + 指令集 + **全局引用全为自身名**）
  保证纯自递归；非自调用慢桩 rax=0 级联回退
- **资格收紧教训**：`(<= n 1)` 编译为 builtin CALL → INT 慢桩返回 0 破坏语义 → 全局名全等于函数名才可 INT
- 性能：fib(30) **27ms → 15-16ms（~1.8x）**；BC/JIT 比 14-26x（机器噪声内）
- 验证：357/357 测试；26 examples JIT vs BC 逐字节一致；fib(25)=75025 ✓
- 修复链：INT 版漏发函数体末尾 epilogue（主路径落尾桩返回 0——根因）

## 12. 语义一致性修正（三模式）+ JIT 基准档（2026-08-26）

- **除法语义统一**：AST `/` 恒返回 Float（与 evalArithBin 一致）；VM binNum/case13/DIV_R 三处 Int-div 修正——
  `(/ 20 4)` 三模式均为 5.000000；JIT 通用除法恒 deopt（语义交还 VM）；INT-spec 资格已排除除法
- **基准三档**：`lisp-tests/perf/run.sh` 增加 jit 档（AST/BC/JIT 三列 + 双比值）
- **遗留（真实缺口，已记录）**：
  1. **闭包共享可变捕获**：`(make-counter 100)` 三个 lambda 共享 `count`——BC 的 MAKE_CLOSURE 按值复制
     → set! 不共享（AST 500 vs BC 100）——J3 范畴（共享 cell 捕获）
  2. **基准深层场景崩溃**（03-fact 等）：深层 VM↔JIT 混合调用 getEntry 参数损坏（addr nil）——
     待修（下轮优先）
- examples 三模式：26 中 8 为已知语义差（除法已修 2 个；其余=闭包捕获/模块导出）——BC vs JIT 恒一致 ✓

## 13. 深层混合崩溃的精确边界（2026-08-26，记录待修）

- **触发**：字节码循环内反复调用 JIT 化函数（`(loop L 0)` 内 ×12 `(fact 17)`）——**约 24-33 次 JIT invoke 后崩溃**
  （L=1 ✓；L=2×12 崩；×7-10/3层 ✓；×11/3层 崩）
- **现象**：`JitRuntime.getEntry+45` SIGSEGV（addr=0x8）；this=栈址、globalId=0、f 对象有效——
  疑似 ~30 次 invoke/释放后对象状态（acquireArrayRawData 句柄/entries 缓存/栈）累积损坏
- 纯 JIT 递归（fib/fact 单次）、纯字节码（200 层）、JIT+bytecode 各单次调用均 ✓
- 修复方向：invoke 缓冲/句柄生命周期审计（release 语义）、entries 缓存一致性、深层混合帧的寄存器保存

## 14. 深层崩溃根治 + 基准全场景验收（2026-08-26）

**两个根因（均真实修复）**：
1. **JIT 页容量 4KB 固定**：`allocPage` 固定 `mmap(4096)`——生成代码 >4KB（如字节码内重复 N 次调用的循环体）→ **写越界 SIGSEGV**（"~24-33 invoke 阈值"实为 4KB 容量线）。修复：按代码长度扩展页容量（+4096 余量，4K 对齐）
2. **机器帧过大撞原生栈守卫**：大量 let 局部（localSlots 350）→ 每帧 2.8KB × 200 层递归 ≈ 560KB → 原生栈溢出（RET 弹守卫数据）。修复：JIT 资格（通用+INT-spec）限制 `localSlots ≤ 64` → 大帧回退 VM

**基准全场景验收（三档，3 轮中位数）**：

| 亮点 | JIT/BC |
|---|---|
| fib-direct / fib-indirect | 36.3x / 35.3x |
| tail-sum | 5.3x（顶递归已最快路径） |
| let-arith / set-state | 1.1x / 0.9x |
| **全场景平均** | **~5.7x（≥3x 验收 ✓）** |
| 15 场景正确性 | JIT vs BC 0 differ ✓ |

- 慢场景（closure-factory 0.2x 等）= 已知边界（高阶/闭包/deopt 锤击），语义一致 ✓
- 357/357 单测；examples 26 JIT vs BC 0 differ

## 15. J3 闭包共享可变捕获（2026-08-26）

- **问题**：`(make-counter 100)` 三 lambda 共享 `count`——BC 的 MAKE_CLOSURE 按值复制捕获 → set! 不共享（AST 500 vs BC 100）
- **方案（env-scope 路线）**：
  - 新算子 OP_ENV_SCOPE_BEGIN=61 / OP_ENV_SCOPE_END=62（case 数 42→44 ≤ 45 ✓）：currentEnv 压栈 + child 环境
  - `SharedCaptureScanner`：预扫描 body 内 **lambda 自由引用**的绑定名（仅统计 lambda 体内符号——初版把整个 body 的符号都算上导致 let 体自身也环境化 → 修复）
  - compileLet：共享绑定 → 存环境（STORE_GLOBAL 到 child，不占槽）+ ENV_SCOPE 包裹；符号解析/compileSet 对 envVars 走环境路径；闭包体 LOAD_GLOBAL 经 capEnv.parent → child → 共享 ✓
  - 附带修复：VM case-6 LOAD_GLOBAL 用 `this.currentEnv`（此前用参数 env，switchFrame 后环境已切换）；case-7 JIT 调用传闭包环境 `clsEnv`（此前传 currentEnv）
- **验证**：三模式 500 ✓；独立计数器互不共享 ✓；359/359 单测（+2）；examples 三模式 8→7（剩=宏 member-access/模块导出等改前已知差异）
- 边界：let 体自身对共享变量的直接读写仍走槽（与 env 不同步）——canonical 场景（闭包间共享）正确

## 16. J5 R 形式启用：寄存器级纯自递归 INT 特化（2026-08-26）

- **目标**：fib(30) ≤4ms（验收线）。C 语言标定：本机 `gcc -O1` fib(30) ≈ 3ms（-O2 被常量折叠作弊，-O0 ≈ 4-5ms）
- **达成**：进程内 fib(30) **3ms**（单测 `testJitFib30Perf`，MonoTime 实测）；含 CLI 启动的墙钟 18ms（启动基线 17ms——吞吐真实值以进程内为准）。BC 403-425ms → 加速比 **~140x**；perf 套件 fib-direct **63x**（v1 形式 36x）

### 架构（jit_codegen_int_r.cj）
- **资格**：纯自递归（全局引用全为本函数名）、常量仅 Int/Bool、指令子集、无除法、值栈深度 ≤ 4-k、**自调用 arity == 参数个数**
- **固定值池映射**：栈位置 i → R_R12+k+i（k=1 → r13-r15）——替代 J4b 的动态 allocVal；所有值一律物化到池寄存器 → 分支合并天然一致（J4b 的"跨调用值池最终不一致"根源）
- **纯 rax 返回 ABI**：无结果槽、无 rcx 槽指针、无 selfClosureId 运行时检查（静态纯自调用——函数值是模型占位，零指令）
- **CFG 感知资格扫描**：worklist 标签入口深度唯一（镜像编译期快照语义）；深度冲突 → 不资格
- **调用**：参数 → rdi/rsi/rdx；`call rel32` 目标 = 偏移 0（函数入口）

### 本轮根因修复（⚠️ 归档，全部在单测覆盖）
1. **pushReg 对 r8-r15 编码错误**：`0x50 + reg`（reg=12 → 0x5C）落在 pop 编码域（0x58-0x5F）——R prologue 的 5 个 push 全是 pop，帧错乱 → SIGSEGV。正确：`0x50 + (reg & 7)` + REX.B。gdb 反汇编 JIT 页（`x/60i`）+ 字节级 rel32 解码定位
2. **BIN_LC kind 4-6 丢 cmp**：优化时误删 `cmp lreg, bv`，flag 陈旧（比较结果被上一个 CMP_LC_JF 支配）→ 布尔谓词结果错
3. **imul 硬编码 `48 0F AF`**：缺 REX.R/B（r13/r14 → 编码成 imul rbp,rsi）
4. **自调用 arity 缺失**：2 参函数 1 参自调用 → callee 读垃圾寄存器（v1 与 R 同时修——v1 曾因此崩溃）
5. 栈对齐：4 push + sub8 pad（entry %16=8 → 0，保证 call 前 %16=0）

### 已知边界
- k≥2 值池容量 4-k 偏窄——**J6 扩展（rbx 入池 + phantom 函数值）后 k=2 已实用**（见 §17）；k=3 池=2 < 自调用需求 ≥3 恒不资格
- 防御性 bail 桩保留（资格函数仅自调用，属死代码）
- **深机器码递归受宿主栈限制**：主线程仅映射 ~135KB 栈（R 帧 48B → 约 2600 层上限，超出 SIGSEGV）；BC 帧切换不受此限（深递归用 VM 寄存器栈）。见 §17.3

### 数据
- 单测 365→370（R 系列 fib 全值/缺参回退/JIF 条件/布尔结果/fib(30) 计时）
- examples 三模式 22/22 一致（新增覆盖率：mul 类（square）、闭包混合）
- perf 15 场景：fib-direct/indirect 63x/61x（26-27ms）；其余与 v1 持平（JIT 栈机器与通用形式共用路径）

## 17. J6 增量：J2b 互递归交叉调用 + R 形式 k≥2（2026-08-26）

### 17.1 J2b：互递归/跨函数机器码直连（jit_codegen.cj + jit_runtime.cj）

- **动机**：J2 以来跨函数调用一律 deopt（互递归每层边界都回解释器，O(n) 次 deopt + 逐层重解释）。本轮启用**入口表直连**：
  - emitCall 非自路径：`jne cross` → `mov r10, <entryTablePtr>`（mmap 固定页，imm64 烘焙）→ `mov r9, [r10 + payload*8]` → `r9==0`（未编译/非 generic）→ **冷启动 deopt** → `emitFrameAndCall(r11Mode=true)` → `call r11`
  - 冷启动语义：callee 未编译时 deopt 一次 → VM 编译 callee → 重入后该边**永久直连**；互递归实测仅 1-2 次 deopt（每闭包对一条边）后 f↔g 全机器码
- **入口表 mmap 固定页**（非 GC 对象 → 地址稳定，替代原 Array+acquireArrayRawData 的 GC 移动隐患）；**仅 generic ABI（16B 槽）登记**——v1（8B 槽、无类型守卫）/R（裸寄存器 ABI）特化禁止机器码交叉调用（ABI 不兼容 → 读垃圾）
- **两个根因**（J5 同族编码错误，均单测归档）：
  1. **movR11R9 方向反**：`4D 89 D9` 实为 `mov r9, r11`（89 /r 中 reg=源）——`slowJe` 把 site id 装入 r11 后，入口没进 r11，`call r11` = 调用陈旧 site id → **pc=0x3 SIGSEGV**。正确 `4D 89 CB`
  2. **deopt/bail 握手**：旧 `test rax,rax; je bail` 只捕获 rax==0，而 deopt 返回**非零 site id** → 机器码嵌套 deopt 后调用方把 site id 当结果指针读垃圾。改 `cmp rax,1024; jb bail` + bail 桩独立（rax=0 级联返回），deopt 桩保留 site id（诊断）
- **v1 桥 arg 类型守卫**（潜在正确性 bug）：v1 INT 特化无运行时类型守卫，Float 参数被当 Int 位模式算 → **静默垃圾值**（`(g 1.5)` JIT 曾返回 `-4627448617123184640` vs BC `10.5`）。invoke 桥对 vForms 函数全参校验 Int（非 Int → deopt 回解释器）；R 桥已有同款守卫
- **unbox 绑定模式坑**（Cangjie 语义，❌勿重犯）：`match (tag) { case JIT_TAG_INT => ... }` 中 `JIT_TAG_INT` 是模块级 `let`，在 match 模式里是**绑定模式**（非常量模式——常量模式仅限字面量）→ 恒匹配第一分支，`unbox` 把所有 tag 都解成 Int。曾致 generic 函数返回 Boolean 变 `integer`（`(caller 5)` JIT "integer" vs BC "boolean"）。改 if/else 具名常量比较。同坑暴露 generic 自调用 arity 缺参守卫缺失（`checkSupported` 补齐：自递归函数含 arity != 自身参数数的调用 → 保守拒绝回退 VM；emitCall 自路径再加运行时 deopt 兜底）
- **imm32 符号扩展大常量坑**（三形式通用）：`mov r64, imm32` 符号扩展 bit31，|Int 常量| > 2^31-1 被静默破坏（`3000000000` 变 `-1294967296`，R/v1/generic 全受影响）。资格检查补齐：常量超出 `[-2^31, 2^31-1]` → 拒绝编译回退解释器（未来可换 movabs）
- **R/v1 返回类型不一致**（`returnsNonInt` 资格检查）：R/v1 是 INT 特化（纯 rax/INT 标签返回），返回 Boolean/Nil/函数值会被错解为 Int（`(b n)` 返回 `(> n 0)` 得 Int(1) vs BC Boolean）。CFG 类型跟踪（镜像资格扫描）：任一 RETURN 栈顶为非 Int（BOOL/NIL/PHANTOM/MIXED）→ 拒绝落 generic（generic 经 unbox 修复正确返回类型）。CALL 返回 = 函数自身返回类型（合并时兼容任意）；条件比较（CMP_STACK 被 JIF 消费）不污染返回
- **数据**：互递归 f(500)=250 与 BC 一致、深度 200 时 deopt 计数 ≤2（`testJitMutualRecursionPerf` 确定性断言）；单测 +5（后 +2 unbox 回归）
- **边界**：跨函数 callee 经 `jitResolveGlobals` helper 每帧 env.lookup（正确性优先，逐帧 helper 有开销，未来可缓存）；深互递归受 17.3 栈限制

### 17.2 R 形式 k≥2（jit_codegen_int_r.cj）

- **值池扩展**：rbx 入池（callee-saved，prologue/epilogue 5 push/pop，40B → 调用点 %16==0 免 pad）→ 池 = `[rbx, R_R12+k..R_R15]`，容量 **5-k**
- **phantom 函数值**：自函数全局装载（emitLoadLocal 全局分支）**不占池寄存器**（emitCall 的函数值是静态已知占位）——寄存器按**非 phantom 计数**静态分配 → k=2 自调用只需 2 个参数寄存器
- **资格扫描深度模型对齐**：LOAD_LOCAL 全局(phantom) 深度+0、CALL 净 `-argCount+2`、CMP_STACK -1、NOT 0、容量 5-k；顺带修复 `case 4 | 11` 遮蔽（op11 的 jif 合并逻辑原为死代码）
- **成果**：k=2 尾递归累加器类可用——`sum-to`（参数含 BIN_STACK `(+ acc n)`）与 `gcd2`（嵌套 if + BIN_STACK 参数）均 R 形式编译正确
- **k=3 不可行（文档化）**：池=2 < 自调用需求 ≥3（3 参数 + phantom 函数值 = 3 寄存器）→ 任何含自调用的 k=3 函数恒不资格
- **数据**：`testJitRFormK2SumTo`（sum-to(2000,0)=2001000 与 BC 一致 + rFormCount≥1 断言）、`testJitRFormK2Gcd`（gcd2(48,36)=12）；单测 374→376；examples 22/22；perf fib 60.7x/60.6x（未回退）

### 17.3 深机器码递归宿主栈限制（既有约束，本轮明确）

- **现象**：R 形式深递归 ~2600 层 SIGSEGV（`(sum-to 2800 0)` 崩、2600 正常）；k=1 自递归 g(3000) 同崩——与 k≥2 无关，帧大小未变（48B/帧）
- **根因**：主线程 `[stack]` 映射仅 **~135KB**（rlimit 8MB 但运行时只映射 0x21000）；48B × ~2800 = 135KB
- **对照**：BC 帧切换用 VM 寄存器栈（不涨宿主栈，深递归仅受内存约束）——同源码 JIT 崩而 BC 正常
- **待办（未来）**：机器码深度计数 + 超限 deopt（干净回退而非 SIGSEGV）；或运行时放大主线程栈映射
