# JIT 正式工程：字节码 → 机器码翻译器设计

**版本**: 0.1（设计草案）
**日期**: 2026-08-25
**状态**: 待评审（评审通过前不实现）

---

## 1. 背景与目标

xisp 字节码模式现状（2026-08-25）：fib(30) 字节码 ~32ms，AST ~440ms，**JIT 模板原型 2.5ms**（`temp-match-bench`，手写 x86-64：递归自调用回填 + 栈管理 + callee-saved 保存，已验证 `mmap RWX → CPointer<UInt8> 写码 → CFunc<(Int64)->Int64> 调用` 通路）。

解释循环的开销构成（每指令固定成本）：
- 指令取指 + `match(Int64)` 跳转表分派；
- 寄存器栈为内存数组：`registers[sp]` 读/写每次数组边界检查 + 索引寻址；
- 盒装值 `LispValue` 枚举：算术需 tag 检查、拆箱、运算、装箱；
- 帧定位（`bp + offset`）与 `savedFrames` 元组管理。

**JIT 目标**：把 `CompiledFunction` 的字节码翻译为 x86-64 机器码，消除上述全部固定成本，保留解释器作为**语义回退面**（未覆盖指令、宏、边界场景仍走字节码/AST）。

**量化验收目标**（后续可修正）：
- fib(30)：JIT ≤ 4ms（≥8x BC / ≥100x AST），理想 2.5-3ms；
- 15 场景基准套件全跑，JIT 相对 BC 平均 ≥3x；
- 行为与 AST 逐字节一致（examples + 基准双/三模式校验）；
- 全量单元测试通过，新增 JIT 专项测试（见第 7 节）。

---

## 2. 核心设计决策

### 2.1 翻译模型：编译期全量 JIT（无运行期 profiling）

- xisp 是嵌入式脚本：顶层表达式编译一次、执行一次，**没有运行期"热点"概念**，不需要 tier 化或采样 profiler。
- 编译期对每个 `CompiledFunction` 直接生成机器码；翻译失败（不支持的指令/模式）→ 该函数标记 `jit-fallback`，解释器原路径执行（与现有"编译失败回退 AST"同构）。
- 每个函数**字节码永不删除**：guard/慢路径/调试都依赖它。

### 2.2 值表示与类型策略（分层特化）

> 关键约束：`LispValue` 是仓颉 `enum`，其**内存布局未经文档化**，机器码无法安全读取其 tag。这是本工程最大的前置风险（见 2.6 J0 之一）。

- **J 层 1（首版，J0 修订后）**：
  - JIT 槽 = `(tag: Int64, payload: Int64)` 16 字节，位于 JIT 原生栈帧槽区（**不是** LispValue 盒；见 2.6）；
  - JIT 寄存器只用 caller-saved（rax/rcx/rdx/rsi/rdi/r8-r11），局部槽常驻**原生栈槽区**；
  - 整数算术用 **tag 检查快路径**：两操作数均为 `JIT_TAG_INT` → payload 原生 `add/sub/mul`（`div` 走慢路径防除零）→ 写结果槽；否则跳慢路径 helper（与 VM `binNum` 相同语义，可抛异常）；
  - 边界经 Cangjie bridge `box/unbox`（match 实现），机器码不触 LispValue 布局。
- **J 层 2（特化，若 J 层 1 不达标）**：翻译期**简单类型推理**（槽/常在的整数传播），整数不盒化、直接以 `Int64` 在寄存器/i64 栈槽流动，仅逃逸点（跨调用、存全局、返回未知类型上下文）盒化。
- 布尔/比较：`lt/gt/eq` 快路径直接比较双 Int 盒的 payload，结果生成 Boolean 盒（复用 `OP_NORM_FALSE` 语义）。

### 2.3 调用约定

```
// JIT 函数 ABI（SysV x86-64 扩展）
// 参数 1..6:  rdi, rsi, rdx, rcx, r8, r9（盒值指针）
// 参数 7..N:  原生栈 8 字节对齐
// 返回:       rax（盒值指针）
// 帧:         原生栈帧；局部槽区按 CompiledFunction.localSlots 布局（与 VM bp=1000 布局同构，
//           解释器互调时可平移）；保存 rbp/rbx/r12-r15（callee-saved 惯例）
```

- **JIT → JIT**：零转换，直接 `call`（相对地址回填，递归入口自引用回填——原型已验证）。
- **JIT → 解释器/内置**（`CALL`/`CALL_GLOBAL` 目标非 JIT 化函数）：桥接 stub——盒参数打包 → 调 Cangjie helper（现有 `callFunction`/`applyProcedure` 逻辑或 `vm.callClosure`）→ 取回结果盒。
- **解释器 → JIT**：VM 的 `switchFrame`/`callFunction` 路径发现目标函数已 JIT → 直接调 `JitFunc.entry`（盒参数按 ABI 传参，返回盒入调用点结果槽）。
- **闭包捕获（MAKE_CLOSURE）**：J3 阶段支持；先回退（VM 已有可变长度捕获，语义保真）。

### 2.4 慢路径与回退

- 每指令的通用语义封装为 `extern` 的 Cangjie helper（模块级函数，接收 `unsafe` 指针/盒），机器码在快路径失败时 `call` 之；helper 复用现有 VM 语义代码，**单一语义源**；
- 未翻译指令 → 函数级回退（`jit-fallback`）而非指令级；
- 运行时异常（除零、类型错误）：helper 抛仓颉异常 → 不影响 JIT 页（异常穿过机器码帧：`call` 帧栈在异常 unwinding 时需可跳过——**需验证仓颉异常穿过 CFunc 机器码帧的行为**，J0 验证项）。

### 2.5 盒分配器

- 为 `LispValue` 盒提供**小块 bump 分配器**（固定块 + 链表，类似线程局部分配器）；JIT 快路径内联分配（指针 bump + 阈值比较）；
- 首版允许简单化：`malloc`/仓颉分配器（慢路径直接调用），快路径 bump 优化留到 J5 性能阶段。**需确认**：仓颉对象能否被机器码"借用内存"——J0 验证项（备选：JIT 专用字节数组池 + 盒为 `CPointer` 包装的结构，见 2.6）。

### 2.6 前置风险与验证（J0，已实测完成 2026-08-25）

> **J0 结论先行**：核心风险全部澄清，且 `LispValue` 布局风险**被设计消除**。

| 原风险 | J0 实测结论（temp-match-bench 原型） |
|---|---|
| 仓颉 `enum`（LispValue）内存布局未文档化 | **已消除**：JIT 内部改为自有 (tag, payload) 双字表示（tag 编码与 LispValue 无关），边界经 Cangjie `box/unbox`（match 实现），JIT 永不读 LispValue 原始字节 |
| `CFunc` 取址 + 机器码调用 `@C` helper | ✅ 实测：`CFunc→CPointer` 取址、`call rax`、3/4 参数 rdi/rsi/rdx/rcx 传递完全正确；helper 访问模块全局状态正常 |
| 仓颉异常穿过机器码 `call` 帧 | ✅ 实测：helper 抛异常 → 外层 `catch` 成功、进程存活。**慢路径可直接抛异常**，无需错误码通道 |
| 栈对齐 | ⚠️ 实测：`sub rsp,8` 错位调用必崩（SIGSEGV）→ 发射器保证 16B 对齐是硬约束（SysV 规则，`push rbp` 后天然对齐） |
| **新发现**：异常穿越后 callee-saved 脏寄存器 | ⚠️ 异常跳过 JIT 帧时其保存的 rbx/r12-r15 不会恢复 → **JIT 帧规范：只用 caller-saved 寄存器（rax/rcx/rdx/rsi/rdi/r8-r11），callee-saved 一律不入 JIT 持久帧**（fib 原型的 rbx 用法需改为 r8 或栈槽） |

**J0 修订后的值表示（替代草案 2.2）**：
- JIT 槽 = 16 字节 `(tag: Int64, payload: Int64)`，位于 JIT 原生栈帧槽区；
- 整数快路径：槽 tag == `JIT_TAG_INT` → payload 原生运算 → 写结果槽；否则跳慢路径 helper（语义单一源，可抛异常）；
- 解释器→JIT / JIT→解释器边界：Cangjie bridge 用 `match` 完成 box/unbox（`LispValue → (tag,payload)` 与反向），机器码只认 (tag,payload)。

### 2.7 与既有工作衔接

- **不依赖** `bytecode-fw` 拆分（方案 A 独立）；JIT 落地于现有 `src/core/eval/`，未来可随 VM 一并平移；
- IR 复用现有 `CompiledFunction`/`OpCode`，**不新增指令**（已有 42 case 跳转表约束保持只影响解释器；JIT 发射器按 opcode 模板表分派，无 case 数约束）；
- CLI：新增 `--with-jit`（叠于 `--with-bytecode-compiler` 之上；未开字节码时无效）。

---

## 3. 架构

```
src/core/eval/
├── jit_emit.cj      JitEmitter：字节序缓冲、标签/回填（rel8/rel32）、重定位（call 目标）
├── jit_codegen.cj   JitCodegen：CompiledFunction → 机器码模板（每 opcode 一个 emit 函数）
├── jit_runtime.cj   JitRuntime：mmap/mprotect 页、JitFunc 注册表（按函数 identity）、
│                    JIT 调用入口（ABI 适配）、慢路径 helper（extern 语义）
├── jit_bridge.cj    解释器↔JIT：switchFrame/callFunction 的 JIT 分支、applyProcedure 的
│                    JIT 分支（与 callClosure 并列）
└── vm.cj            改动：JitRuntime 挂载（BytecodeVM 持有 Option<JitRuntime>）
```

**关键类型草案**：

```cangjie
public class JitFunc {
    public let id: Int64              // CompiledFunction.identity
    public let entry: CFunc<(CPointer<Unit>) -> CPointer<Unit>>  // 通用入口（ABI 适配在 stub 内）
    public let page: CPointer<Unit>
}

public class JitRuntime {
    public let funcs: HashMap<Int64, JitFunc>
    public func tryCompile(f: CompiledFunction): Option<JitFunc>
    public func invoke(func_: CompiledFunction, args: ArrayList<LispValue>, env: Environment): LispValue
}
```

**发射器接口**：

```cangjie
public class JitEmitter {
    public func emit8(v: UInt8)
    public func emit32(v: Int64)                    // rel32 占位
    public func here(): Int64                        // 当前偏移
    public func label(): Int64                       // 跳转目标
    public func patchRel32(at: Int64, target: Int64)
    public func callRel32(target: Int64)             // 函数入口回填（递归/前向）
}
```

---

## 4. 指令覆盖规划（按阶段）

| 指令类 | J1 | J2 | J3 | J4 |
|---|---|---|---|---|
| 常量/局部/全局装载（PUSH_CONST/LOAD_LOCAL/LOAD_GLOBAL/） | ✅ | | | |
| 栈操作（PUSH_NIL/TRUE/FALSE/POP/DUP/SWAP） | ✅ | | | |
| 算术/比较家族化 OP_BIN_LC（kind 0-7） | ✅ 快路径+慢路径 | | | ✅ 类型推理特化 |
| 跳转/条件（JUMP/JIF/JMP_TABLE） | ✅ 回填 | | | |
| CALL/CALLEE（直接调用） | ✅ JIT→JIT | ✅ JIT→解释器桥 | | |
| CALL_GLOBAL/全局函数入口 | ✅ | | | |
| 返回（RETURN/HALT） | ✅ | | | |
| match 字面量 OP_MATCH_LIT_KEEP | ✅ | | | |
| OP_AND_JF/OP_OR_JF/OP_NORM_FALSE | ✅ | | | |
| MAKE_CLOSURE/CLOSURE 捕获 | | | ✅ | |
| SET_GLOBAL/SET_LOCAL | ✅ | | | |
| 特殊形式回退（宏/&rest/fmt） | 不回退：函数级 | | | |

---

## 5. 分阶段计划与验收

| 阶段 | 内容 | 估 | 验收 |
|---|---|---|---|
| **J0 布局/风险验证** | ✅ 完成（2026-08-25，temp-match-bench）：@C helper 取址/调用/全局访问/异常穿越/栈对齐/callee-saved 约束全部实测；LispValue 布局风险消除 | 1-2 天 | 见 2.6 结论表 |
| **J1 翻译器骨架 + 直接调用** | 发射器/标签回填；核心指令模板；JIT→JIT 递归调用；`--with-jit` 管线接入；解释器→JIT 入口 | 3-5 天 | fib(30) 正确且 ≤8ms；基准首个场景 JIT>0 增益 |
| **J2 全指令覆盖 + 互调桥** | 剩余指令模板；JIT→解释器/内置桥；全局引用解析入 JIT 帧 | 3-5 天 | 15 场景全跑正确；平均 ≥3x BC；examples 三模式一致 |
| **J3 闭包捕获** | MAKE_CLOSURE 模板（捕获 vlen 语义平移）；applyProcedure JIT 分支 | 2-3 天 | 闭包基准（3-4x BC 场景）不回归且提速 |
| **J4 整数特化** | 翻译期类型推理、未盒化传播、慢路径+错误槽 | 3-5 天 | fib(30) ≤4ms；基准 ≥4x BC |
| **J5 稳定/性能/文档** | 盒分配器 bump 化；错误通道（异常/除零）；页生命周期；基准套件 jit 模式；文档+记忆 | 2-3 天 | 全量测试 + 基准回测；文档成文 |

总计约 **2-3 周**（仓颉不熟悉指令编码的风险已在 J0 前置）。

---

## 6. 测试策略（TDD）

- 每阶段新增 `JitTest`（`src/jit_test.cj`）：`evalWithJit` 辅助（bytecode + jit 叠层）；
- 语义一致性类：随机/固定表达式集 **AST vs BC vs JIT 三模式结果相等**（复用现有双模式基础设施，加第三模式）；
- 指令级：每模板一个用例（含回退路径：整数快路径失败走慢路径结果一致）；
- 性能回归：基准套件 `lisp-tests/perf/run.sh` 增加 jit 档（`--with-bytecode-compiler --with-jit`）；
- 异常：除零/类型错误/越界在 JIT 下与 AST 一致（错误通道）；递归深栈（10 万层）不崩。

---

## 7. 实施流程与分支

- 评审通过后：`feat/jit` 分支（Feature Branch + Squash Merge 流程）；
- 每阶段独立提交 + 该阶段测试全绿后才进下一阶段；阶段完成更新 `task.md`/`cangjie-mem`/工作日志；
- J0/J1 先行，产出实测数据（fib 口径）后再评审是否进入 J2-J5（数据驱动，避免过度工程）。

---

## 附：与 bytecode-fw 设计的关系

bytecode-fw（A 拆分）是**可复用性**目标；JIT 是**性能**目标。两者独立实施，均复用现有 IR；若 A 先做，JIT 的翻译器/运行时随 VM 平移进 fw；若 JIT 先做，A 的泛型化需保留 JIT 挂钩点（`JitRuntime` 作为 VM 宿主注入项）。
