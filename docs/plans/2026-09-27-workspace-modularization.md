# Xisp 工作区化（模块划分）设计

> **状态**：**已实施并发布**（0.5.0，2026-09-28）—— 本文档**冻结为记录**，只描述**仓颉侧模块划分**。
>
> **本文档已不再承载**（拆分后各自独立演进，请勿在此追加）：
>
> | 拆出的主题 | 现文档 |
> |---|---|
> | Lisp 侧运行期装配、register 分层、`External` 接缝、域装配与中断注入（原 §4、§10.1–10.3、10.5、10.6） | [`2026-10-11-capability-domains-design.md`](2026-10-11-capability-domains-design.md) |
> | 科学计算扩展族（原 §10.4、10.7–10.10） | [`2026-10-11-sci-family-design.md`](2026-10-11-sci-family-design.md)（**规划中**） |
>
> 下文保留原有的 §1–§9 编号（**外部引用 `§3.2` / `§3.3` / `§5.2` 依赖这些编号**），
> §4 与 §10 改为指向新文档的占位节。

**版本**: 0.1（设计草案）· **日期**: 2026-09-27 · **目标版本**: 0.5.0（已发布）

---

## 1. 问题

Xisp 的定位是「仓颉嵌入式 Lisp 脚本语言」，但**当时的交付粒度与这个定位不匹配**：
它是一个 cjpm 模块里的 10 个子包，任何宿主只要 `import ystyle::xisp.*`，就会整体拿到
解释器 + REPL + 行编辑器 + 终端控制（POSIX/Windows）+ CLI 入口 + 字节码编译器 + JIT + 示例。

本设计解决的是**仓颉侧（编译期）**这件事：

| 轴 | 机制 | 回答的问题 | 章节 |
|---|---|---|---|
| **仓颉侧（编译期）** | cjpm 模块 / `[workspace]` | 这段代码及其外部依赖，**有没有进这个二进制** | §3、§5 |

另外两条轴（运行期能力装配、外部值地基）已拆到能力域文档，见文档头的对照表。
两条轴的关系是：**能靠运行期装配解决的问题，不要靠拆模块解决；
拆模块只用于"依赖图必须变"的场景**（独立外部依赖 / 平台特化 / 独立演进节奏）。

**结果**：阶段 1 / 阶段 2 均已实施，**0.5.0 已发布中心仓**（5 个包 + tag `v0.5.0`），
跨仓依赖已同步（`atelier` / `cordis_xisp` 升 `0.5.0`）。

---

## 2. 现状事实（均有证据）

### 2.1 依赖粒度是「模块」，不是「子包」

cjpm 手册「模块配置文件说明」：`name = "模块名及模块 root 包名"`；
`[workspace]` 的合法字段只有 `members` / `build-members` / `test-members` /
`compile-option` / `override-compile-option` / `link-option` / `target-dir`。

- 仓颉**没有** Cargo 那样的 feature / 可选依赖；
- `--cfg` + `profile.customized-option` 只能透传条件编译变量，且只能由**模块作者**在**本模块**设定
  ——消费者无法用它裁剪被依赖模块的代码；
- 因此「让嵌入方只拿 core」**只能**靠拆成多个 cjpm 模块。

### 2.2 包依赖图（严格单向，本身就是好切法）

```
types  ←  parser  ←  core.module  ←  core.eval  ←  core.builtin  ←  bridge
                                                        ↑
                                        repl  →  terminal（零 xisp 依赖）
```

（按各包 `import` 实测；`core.eval → core.module` 是因为 `ModuleLoader <: Loader` 的实现要求值器，
两套模块代码是"抽象 / 实现"关系，不是重复实现。）

### 2.3 体积构成（非测试 LOC）

| 分组 | 文件 | 行数 |
|---|---|---|
| 基础层 | `types` 1016 + `parser` 1203 | 2219 |
| **AST 解释器系** | core/evaluator/special_forms/macro/pattern_match/helpers/higher_order/arithmetic_fast | **3989** |
| 模块加载（core.eval 内） | module.cj / module_loader.cj / script_loader.cj | 1469 |
| `core.module`（抽象层） | module / module_parser / module_source / loader / dependency_resolver | 1865 |
| `core.builtin` | 11 个文件 | 2109 |
| `bridge` | 4 个文件 | 740 |
| 根包（interpreter + options） | | 1342 |
| **字节码** | compiler.cj / vm.cj / code.cj | **3322** |
| **JIT** | jit_codegen*.cj / jit_emit.cj / jit_runtime.cj | **2608** |
| `repl` | REPL / 行编辑 / 补全 / 高亮 / 历史 | 1363 |
| `terminal` | POSIX / Windows 实现 | 537 |
| `cli` | main.cj | 223 |
| `examples` | 仓颉示例 11 个 | 1419 |

### 2.4 字节码 / JIT 是「运行期开关」，不是编译期裁剪

`--with-bytecode-compiler`、`--with-jit` 都是 `cli/main.cj` 解析的命令行参数
（`withBytecodeCompiler()` / `withJit()` 选项）——也就是说 **5930 行执行后端恒定编进任何宿主二进制**。

### 2.5 JIT 带 `rwx` 映射，嵌入式沙箱场景是硬伤

`jit_runtime.cj:104` `mmap(..., prot=7, flags=0x22)`（`READ|WRITE|EXEC` + `PRIVATE|ANONYMOUS`），
且 `@When[os == "Linux" && arch == "x86_64"]`。受限宿主 / W^X 策略 / seccomp / OHOS 等环境
不应把这段带进进程，而现在**无法选择**。

### 2.6 AST ↔ 字节码的耦合是一道「窄缝」，可接口化

好消息：它已经是**注入式**的，不是缠绕。

| 位置 | 内容 |
|---|---|
| `evaluator.cj:44-45` | `compiler: Option<Compiler>` / `vm: Option<VM>`，默认 `None` |
| `options.cj:486-500` | `withBytecodeCompiler()` / `withJit()` 负责构造并注入 |
| `core.cj:40-45` | 唯一编译分派点：`c.compile(expr)` → `v.execute(code, env)` |
| `higher_order.cj:510` | `vm.callClosure(id, args, closureEnv)` |
| `higher_order.cj:513` | 直读 `BytecodeVM.functionTable` ← **唯一需要额外开接口的地方** |
| `vm.cj:90` | `makeSubEvaluator()` → VM 回退 AST（方向 bc → core，天然正确） |

结论：拆出执行后端**可行**，需要把 `Compiler` / `VM` 两个具体类型换成接口，
并把 `Code` 类型上移（或泛化）。

> 更完整的耦合点清单与逐项拆解方式见 [`2026-08-25-bytecode-fw-design.md`](2026-08-25-bytecode-fw-design.md)
> §2.2 与 §3——那边是这件事的专题设计，本文 §3.2 只记决策。

### 2.7 Lisp 侧的运行期装配骨架**已经存在**

- `type InterpreterOption = (LispInterpreter) -> Unit`，已有 31 个 `withXxx` 选项；
- `BuiltinFunctions.registerAll(env)`（全量）与 `registerPure(env, output!)`（纯函数子集，cordis_xisp / atelier 在用）；
- `Bridge.registerFunc` / `registerFuncWithNS` / `registerStdIO` / `registerStdFS` / `registerStdCollection`。

**缺的不是机制，而是"域"这个抽象**：现在只有 all / pure 两档，外加若干零散开关（§5.3 有清单）。

`registerPure` 的文档注释已经把关键约束写死了，本设计必须继承它：

> 权限检查对特殊形式无效——`eval` / `apply` / `map` / `filter` / `reduce` / `for-each`
> 会先于检查执行，且函数值可被当作参数传递，因此「注册后过滤」不可靠，
> **只能用「不注册」来表达禁止**。

即：**能力域的粒度必须是"注册集合"，不能是"调用拦截"。**

> 运行期装配的后续设计（域模型、register 分层、`registerPure` 的归宿）见
> [`2026-10-11-capability-domains-design.md`](2026-10-11-capability-domains-design.md) §3–§5；
> 本节仅作为"骨架已存在"的事实记录。

### 2.8 两个真实消费者只用到了 core 的一小块

| 消费者 | 实际 import | 被迫连带吃下 |
|---|---|---|
| `cordis_xisp`（当时依赖 `ystyle::xisp 0.4.1`） | `types` / `parser` / `core.eval` / `core.builtin.BuiltinFunctions` / `bridge.Bridge` | REPL、终端、CLI、BC、JIT、examples、cjpminfo |
| `atelier`（当时依赖 `ystyle::xisp 0.4.1`） | 同上 + `terminal.runeDisplayWidth`（仅 1 个函数） | 同上全部 |

> 拆分后两者均已升到 `0.5.0`（`atelier` 另改了 4 处 `ystyle::xisp.terminal.*` → `ystyle::xisp_terminal.*`）。

`atelier` 那行为一个终端宽度函数而拖进整个解释器，**正是"模块边界缺失"逼出来的借道**，
也说明 `terminal` 事实上已是通用库。

---

## 3. 仓颉侧：模块划分

### 3.0 归属判定规则

| 能力特征 | 处理方式 | 例子 |
|---|---|---|
| 零外部依赖 + 核心语义 | 留 core，**无需开关** | `if` / `lambda` / 算术 / 表 |
| 零外部依赖 + 宿主策略 | 留 core，**运行期开关**（§5） | IO / FS / 模块加载 / 输出通道 |
| **有外部依赖**（stdx / FFI / C 工具链） | **拆 cjpm 模块** | net/http、tls、compress |
| 平台特化（`rwx` / 平台 syscall） | **拆 cjpm 模块** + `@When` | jit、terminal |

### 3.1 阶段 1：6 个成员（低风险，解决"被 REPL/CLI/示例拖累"）

**关键技巧：core 的模块名仍然叫 `xisp`。**
模块名 = root 包名，只要它不变，`ystyle::xisp.*` 全部保持不变 → **两个消费者的 import 一行都不用改**
（`terminal` 那处除外）。这几乎是零成本的兼容。

| 目录 | 模块名 | 包名 | 内容 | LOC | 发中心仓 |
|---|---|---|---|---|---|
| `core/` | `xisp` | `ystyle::xisp`（+`types`/`parser`/`core.*`/`bridge`）**不变** | AST 解释器 + 内建 + 模块系统 + 桥接 + `LispInterpreter`/`options` | ~13733 | ✅ |
| `terminal/` | `xisp_terminal` | `ystyle::xisp_terminal` | 终端能力（零 Lisp 依赖，atelier 已当通用库用） | 537 | ✅ |
| `repl/` | `xisp_repl` | `ystyle::xisp_repl` | REPL / 行编辑 / 补全 / 高亮 / 历史 + `runRepl()` | 1363 | ✅ |
| `cli/` | `xisp_cli` | `ystyle::xisp_cli` | `main` | 223 | ❌ 可执行包不上中心仓 |
| `xisp_examples/` | `xisp_examples` | `ystyle::xisp_examples.*` | 仓颉示例（原 `src/examples`）。目录名**不叫 `examples/`**：仓库根已有 Lisp 示例目录 `examples/`（`.lisp`），重名会冲突 | 1419 | ❌ |
| `xisp_tests/` | `xisp_tests` | `ystyle::xisp_tests` | 契约 / 集成测试（原 root 包的 16 个测试文件迁入） | — | ❌ |

> `xisp` 的 13733 行**含**阶段 2/3 才拆出去的字节码（3322）与 JIT（2608）；两者拆出后核心约 7803 行。

依赖方向：

```
xisp_terminal      （无 xisp 依赖）
      ↑
xisp_repl  ──────→  xisp
      ↑              ↑
xisp_cli  ───────────┘
xisp_examples ──────→ xisp（+ 各能力域）
```

阶段 1 的**唯一 API 破坏**：`LispInterpreter.runREPL()` / `runSimpleREPL()` 移出 core
（`interpreter.cj:9` 是 core 对 `repl` 的唯一依赖，`:741`/`:750` 是唯一用法），
改由 `xisp_repl` 提供会话入口。两个消费者都没用这两个方法。

#### 3.1.1 测试组织（两层）

| 层 | 位置 | 能测什么 |
|---|---|---|
| **包内白盒测试** | 与被测包同目录（如 `core/src/parser/parser_test.cj`） | 该包的 `internal` —— 仓颉 `internal` 是**包级**（当前包及子包） |
| **契约 / 集成测试** | `xisp_tests` 独立模块 | 只用 public API；跨模块集成（阶段 2/3 的 BC / JIT 集成测试也归这里） |

搬迁时实测：原 root 包的 16 个测试文件迁入 `xisp_tests` **只需改 `package` 声明 + 补一行 `import ystyle::xisp.*`**
（它们本来就是宿主视角的契约测试——抽查与试编译都确认只用 public API）。

`xisp_tests` / `xisp_examples` 以 `*_tests` / `*_examples` 结尾，被 cjpm-ws **自动排除发布**。

> `bytecode_test.cj` / `jit_test.cj` 是**集成测试**：它们依赖 `Evaluator` / `LispValue` / `xisp_bc` 的实现类，
> 因此留在 `xisp_tests`（该模块额外依赖 `ystyle::xisp_bc`），而不是跟随后端模块走——
> 后端模块自身的测试应由"最小假宿主"驱动（08-25 §3.4）。

### 3.2 阶段 2：拆出 `ystyle::xisp_bc`（BC/JIT 恒定编入）—— ✅ 已实施（2026-09-28）

**形态**：`compiler` / `vm` / `jit_*`（5930 行）搬到独立模块 `ystyle::xisp_bc`（依赖核心）；
**IR 留在核心**（`CompiledCode` / `CompiledFunction` / `OP_*`）——它是核心与后端之间的**协议**，
放核心才能让接缝签名直接使用 `CompiledCode`（否则要引入不透明句柄或提前泛型化）。

| 项 | 结果 |
|---|---|
| 模块 | `xisp_bc`（依赖 `ystyle::xisp`），5 个发布成员之一 |
| 核心接缝 | `ExecutionBackend` 接口（`core/src/core/eval/backend.cj`）：`compile` / `execute` / `callClosure` / `setInterruptChecker` |
| 注入点 | `LispInterpreter.setExecutionBackend(backend)`；选项 `withBytecodeCompiler()` / `withJit()` 移入 `xisp_bc` |
| 装配 | `xisp_bc/src/backend.cj` 的 `BytecodeExecutionBackend` 组合编译器与 VM |
| JIT | 与 VM **同模块**，因此**不需要跨模块执行钩子**（`JitRuntime.shared` / `functionTable` 都成了模块内访问） |
| 反向依赖 | 零：核心不认识 `BytecodeVM` / `BytecodeCompiler` / `JitRuntime` |
| 破坏性变更 | `LispInterpreter.setCompiler/setVM` → `setExecutionBackend`；`withBytecodeCompiler` / `withJit` 的 import 来源变为 `ystyle::xisp_bc` |

**关键简化（相对 08-25 草案）**：因为 `xisp_bc` 依赖核心（而不是反过来），只有 **core → BC 一个方向**需要接口注入，
且核心侧总共只有 6 处引用点（`evaluator` 字段与 setter、`core.cj` 分派、`higher_order.cj` 的闭包回调、
`interpreter.cj` 注入、`options.cj` 选项）。

**验收（探针实测）**：只依赖 `ystyle::xisp` 的探针二进制中 `Bytecode:` 字面量 **0**、JIT 字面量 **0**
（对照 CLI：1 / 5）；core 内已无任何 VM / 编译器 / JIT 代码（仅保留 IR 常量与数据结构）。

**关于"通用 kit"**：把 VM 给 xisp 之外的语言复用（`bytecode_kit`）需要先做两件事——
IR 泛型化、把 VM 的指令语义外置为宿主接口（详见 §3.3）。本次**不做**，理由见 §3.3 末尾。

### 3.3 后续：通用 kit 的演进路径（原"阶段 3"已取消）

JIT 已随 VM 一起进 `xisp_bc`（同模块、无需钩子），原"阶段 3 拆 `xisp_jit`"**不再需要**。

若将来确要让 VM 给 xisp 之外的语言复用（`bytecode_kit`），路径是：

1. **IR 泛型化**：`CompiledFunction` / `CompiledCode` 的常量池与 `body` 参数化为 `T`
   （`paramInfos: ArrayList<ParamInfo>` 需一并处理——它是语言语义，宜随值语义一起外置）；
2. **值语义外置**：把 `vm.cj` 的 **139 条指令语义**与约 **470 行独立值语义方法**
   （`binNum` / `compareXxx` / `equalValue` / `callProcedure*` / cons 操作…）抽成宿主接口
   （约 30~60 个方法），`BytecodeVM` 只保留取指—译码—分派与帧管理；
3. 把通用部分搬到新模块 `bytecode_kit`，`xisp_bc` 保留语言前端（`compiler.cj`）与值语义实现。

**为什么本次不做**：`bytecode_kit` 的唯一增量收益是"给别的语言复用"，而代价是一次涉及 139 条指令的高风险重构
（含性能重验）；而"宿主可选 BC/JIT"这一主诉求在 §3.2 已达成。等真出现 xisp 之外的宿主时再做，
届时有真实宿主驱动接口设计。

**留门现状**：

| 已留 | 未留 |
|---|---|
| 接缝已按 `ExecutionBackend` 抽象（非 `LispValue` 专用签名散落各处） | IR 仍是 `LispValue` 特化 |
| `BytecodeVM.functionTable` 的访问已收在模块内 | VM 语义仍在 VM 内（未外置） |
| IR 与核心同包，泛型化时可统一改 | `ParamInfo` 仍在 IR 里 |

### 3.4 目录布局

```
xisp/                      # workspace 根 = 产品
├── cjpm.toml              # 仅 [workspace] members
├── workspace.toml         # cjpm-ws 清单：产品版本 / 共享依赖 / 发布范围
├── core/  terminal/  repl/  cli/  examples/
│   └── {cjpm.toml, src/}
├── xisp_bc/         # 阶段 2 新增：模块 ystyle::xisp_bc（字节码编译器 + 寄存器栈 VM + JIT）
├── docs/  docs-site/  examples/  lisp-tests/  scripts/    # 仓库级共用资源原地不动
```

（仓库根 `examples/*.lisp` 是 Lisp 示例，与 `src/examples` 的仓颉示例是两回事，不动。
字节码后端与 JIT 合在一个模块（`xisp_bc`）：JIT 需要 VM 的内部状态，同模块内直接访问即可，无需跨模块钩子。）

---

## 4. Lisp 侧：能力域（按需装配）——**已迁出**

> 本节（域模型、`registerPure` 约束、域清单与现状、外部依赖域 stdx 实测、网络域分层、异步域定位）
> 已迁至 [`2026-10-11-capability-domains-design.md`](2026-10-11-capability-domains-design.md)：
> 域模型见其 §5，域清单见其 §5.2，stdx 实测与网络/异步域见其 §6。
>
> 保留本节号，是为了不破坏下文与外部对 `§4.4` / `§4.5` 的既有引用。

## 5. 工程化：cjpm-ws

### 5.1 清单与命令

```shell
cjpm ws init          # 扫描成员生成 workspace.toml（默认只预览，-w 落盘）
cjpm ws check         # 只读校验（版本 lockstep / 发布批次），退出码 1 可作 CI 闸门
cjpm ws sync -w       # 把清单的 version / 共享依赖写进各成员 cjpm.toml
cjpm ws bump 0.5.0    # 整产品推进版本（默认 dry-run）
cjpm ws bundle / publish / tag
```

### 5.2 两条硬要求（**动手前必须先落**）

1. **成员 `cjpm.toml` 声明 `organization`——这是组织模块的常规要求，不是拆分带来的新东西。**
   xisp 的 `cjpm.toml` 本来就有 `organization = "ystyle"`（它发布为 `ystyle::xisp` 这种组织模块），
   各成员照写即可，依赖键写 `"ystyle::<成员名>"`。
   （漏写时 cjpm 会把它当外部 registry 依赖去查索引而 404——但这是组织模块本来就不该漏的，
   不是"需要留意的坑"。）

2. **`bin-dependencies` / `link-option` 必须由每个要发布的成员自己写，workspace 根那份不顶用。**
   实测（cjpm 1.1.3，探针 probe4，lib 成员 import `stdx.crypto.digest`）：

   | 场景 | 根写了 | 成员自己写 | 结果 |
   |---|---|---|---|
   | workspace 根 `cjpm build` | ✅ | ❌ | ✅ 本地构建成功（根对成员生效） |
   | 成员目录内 `cjpm bundle` | ✅ | ❌ | ❌ `root package 'ystyle::probe_lib4' imports package 'stdx.crypto.digest' ... but it is not added as a dependency in cjpm.toml` |
   | 成员目录内 `cjpm bundle` | ✅ | ✅ | ✅ `cjpm bundle success` |

   解包产物 `.cjp` 可见：**制品里的 `cjpm.toml` 就是成员自己那份**（含它自己声明的 `bin-dependencies`），
   根那份根本不在制品里。后果：

   - **发布方**：每个用到 stdx 的成员都要重复声明 —— cordis-cj 现状即此：根 7 条 + 6 个成员各 7 条 = **42 处重复**；
   - **消费者**：正因为制品里带着，消费者不必重复写（能力域文档 §6.1 实测一）；
   - **cjpm-ws 目前不管这类字段**（只管 version 与 `[dependencies]` 版本对齐），这份样板只能靠约定 + 检查兜。

3. **workspace 内本地解析完全不校验版本号**：故意写不存在的 `9.9.9` 依然 `cjpm build success`。
   版本漂移构建期发现不了 → **必须让 `cjpm ws check` 进 CI**
   （xisp 目前只有 docs 部署 workflow，没有构建 CI，本次一并补）。

### 5.3 版本与命名约定

- 拆分是破坏性变更：`0.4.1 → 0.5.0`，**所有已发布成员共用同一版本号**（lockstep）；
- 示例 / 测试成员**必须带 `_examples` / `_tests` 后缀**（`xisp_examples`，不能叫 `examples`），
  否则 cjpm-ws 的自动排除失效，会把它们当发布成员；
- `xisp_cli` 不匹配这两类，需在 `[workspace.publish] exclude` 里显式排除
  （可执行包不上中心仓）。

### 5.4 制品范围

各成员的 `include` / `exclude` 要重写，且**一律锚定**（`/src` 而非 `src`）。
现在的 `core/cjpm.toml` 里 `include = ["src", "LICENSE"]` 未锚定，是既有隐患。

---

## 6. 迁移步骤（可执行清单）

1. `git checkout -b feat/workspace-modularization`；
2. 建 workspace 根：`cjpm.toml([workspace])` + `cjpm ws init -w` 生成 `workspace.toml`；
3. `git mv` 搬迁源码（保历史）：`src/{types,parser,core,bridge}` + `interpreter.cj` + `options.cj` → `core/src/`；
   `repl/` → `repl/src/`；`terminal/` → `terminal/src/`；`cli/` → `cli/src/`；`examples/` → `examples/src/`；
4. 测试随包走（`*_test.cj` 留在各自模块内）；
5. 各成员 `cjpm.toml`：补 `organization`、`name`、版本、`include`/`exclude`；
   把原 `[package.package-configuration]` 里的 executable 条目迁到 `xisp_examples`；
6. 改包声明：`ystyle::xisp_repl` → `ystyle::xisp_repl`、`.terminal` → `ystyle::xisp_terminal`、
   `.cli` → `ystyle::xisp_cli`、`.examples.*` → `ystyle::xisp_examples.*`；**core 侧包名不动**；
7. 把 `runREPL` / `runSimpleREPL` 从 `LispInterpreter` 移到 `xisp_repl`；
8. stdx 的 `bin-dependencies` **留到引入第一个能力域模块（`xisp_net`）时，由该成员自己写全平台那份**
   —— 不要只写在 workspace 根（成员目录内 bundle/publish 不读根，见 §5.2 实测二）。
   阶段 1 各成员都不引 stdx，本步暂不动；
9. `cjpm build` + `cjpm test`（workspace 全量）跑绿；
10. 更新 README / docs-site 中的产物路径（`ystyle::xisp_cli` → `ystyle::xisp_cli`，8 处引用）
    与 `xisp_examples/src/` 路径；
11. 补构建 CI：`cjpm ws check` + `cjpm build` + `cjpm test`；
12. 合入 master，走 `cjpm ws bump 0.5.0` → `cjpm ws bundle` → 私有仓预演 → 中心仓。

---

## 7. 跨仓改动

| 仓库 | 改动 |
|---|---|
| `cordis-cj/cordis_xisp` | 无（`ystyle::xisp.*` 包名不变）；升级依赖版本至 `0.5.0` |
| `atelier/cli` | `ystyle::xisp_terminal.*` → `ystyle::xisp_terminal.*`（`tui.cj:3`、`md_render.cj:4`、`tui_test.cj:14`、`tui_activity_test.cj:15`）；升级 `0.5.0` |

---

## 8. 风险与未决问题

| # | 项 | 说明 |
|---|---|---|
| R1 | ~~阶段 2 动 core 内部~~ | ✅ 已消解（2026-09-28）：接缝实际只有 6 处引用点（小于草案估计）；BC/JIT 集成测试（约 970 行）作为回归网全部通过。08-25 的 `closureProcedure` / 执行钩子**不再需要**——JIT 与 VM 同模块 |
| R2 | workspace 版本不校验 | 靠 `cjpm ws check` 进 CI 兜；**不做这道闸门就等于没有 lockstep** |
| R3 | `terminal` 独立成库 | 它事实上是通用终端库（atelier 已当通用库用），长期看是否该独立成产品/另仓，值得单独评估 |
| Q1 | 阶段 2 / 3 | **已完成**（2026-09-28）：阶段 2 拆出 `xisp_bc`（BC + JIT 同模块，§3.2）；原阶段 3 取消。通用 kit（`bytecode_kit`）留作演进，路径见 §3.3 |
| Q2 | `xisp_cli` 是否也走 lockstep | ✅ 已解决：`xisp_cli` **纳入发布**（中心仓是源码发布，可执行包同样可分发，消费方 `cjpm install ystyle::xisp_cli-<ver>`），5 个发布成员共同 lockstep；`workspace.toml` 的 `exclude` 已清空 |
| Q3 | core 的 `cjpminfo` 依赖 | ✅ 已解决（2026-09-28）：`@ModuleVer` 随 `XISP_VERSION` 一并移到 `xisp_repl`，core 依赖清零（见 §9.1） |
| Q4 | 能力域粒度 | **已迁出** → 能力域文档 风险 C1 |
| R4 | stdx 环境变量 | **已迁出** → 能力域文档 §6.1（含两条实测）。要点：`CANGJIE_STDX_PATH` 路径三段随安装方式而变，CI/容器/新机器默认不存在；"core 与其它成员不得 import stdx"是硬约束 |
| R5 | 多成员样板重复 | **已迁出** → 能力域文档 §6.1 / 风险 C8。要点：`bin-dependencies` / `link-option` 必须在每个要发布的成员里各写全份（cordis-cj 实测 42 处重复），cjpm-ws 目前不同步这类字段；缓解："只有能力域成员才允许出现 stdx 配置" + CI 校验各成员该段一致 |
| Q5 | 网络域走哪条路 | **已迁出** → 能力域文档 §6.2 / 风险 C10 |

---

## 9. 验收标准

1. `cjpm build` / `cjpm test` 在 workspace 根全绿，成员数与依赖图符合 §3.1；
2. 一个只依赖 `ystyle::xisp` 的探针工程，**不配置 `CANGJIE_STDX_PATH`**、不链接 terminal/REPL，
   可正常创建解释器、注册桥接函数、执行脚本（对应用户侧"嵌入式"的最低要求）；
3. 阶段 2 完成后，只依赖 `ystyle::xisp` 的探针二进制**不含字节码后端符号**（3322 行）；
   阶段 3 完成后进一步不含 JIT 符号（2608 行）；
4. `cjpm ws check` 在 CI 中作为闸门生效；
5. `atelier` / `cordis_xisp` 升级后构建通过。

### 9.1 阶段 1 实施记录（2026-09-28）

| 项 | 结果 |
|---|---|
| 成员 | `core`(xisp) · `terminal`(xisp_terminal) · `repl`(xisp_repl) · `cli`(xisp_cli) · `xisp_examples` · `xisp_tests` |
| 构建 | `cjpm build` 通过 |
| 测试 | `cjpm test` **427 / 427 通过**（含迁入 `xisp_tests` 的 16 个契约测试） |
| 产物 | `ystyle::xisp_cli` + `ystyle::xisp_examples.*` 共 11 个可执行件；冒烟 `(println (+ 1 2))` → `3` |
| lockstep | `cjpm ws check` 通过（发布成员 `xisp` / `xisp_terminal` / `xisp_repl` 均为 v0.5.0，拓扑序 xisp → xisp_terminal → xisp_repl） |
| CI | 新增 `.github/workflows/ci.yml`（cjpm-ws 闸门 + build + test） |

**实施中的两个决策 / 发现**：

1. **core 变成零外部依赖**：`@ModuleVer` 的版本常量原本在 `types` 包，core 因此依赖 `cjpminfo`；
   但版本号只有界面用途（4 个使用点里 3 处在 repl / cli），故把 `XISP_VERSION` 与 banner 一并移到
   `xisp_repl`，core 去掉 `cjpminfo` → **依赖清零**（§8 Q3 提前解决）。
   - **cjpminfo 必须 ≥ 1.2.1**：1.2.0 只从 `PWD` 定位清单，workspace 下会命中根的
     `[workspace]` `cjpm.toml`（无 `[package]` 段）→ `@ModuleVer` 展开成**空串**；
     1.2.1 改用 cjc 的 `-p` 定位"当前正在编译的包"。
2. **`LispInterpreter.printBanner()` 不再保留**：banner 文本与版本号都归界面模块，
   core 只留 `shouldShowBanner()`（配置查询）。

**待收口**：跨仓 `atelier/cli` 的 4 处 `ystyle::xisp.terminal.*` → `ystyle::xisp_terminal.*`（并升 0.5.0），
`cordis_xisp` 仅升版本；README / docs-site 里的产物路径已同步（`ystyle::xisp.cli` → `ystyle::xisp_cli`）。

### 9.2 阶段 2 实施记录（2026-09-28）

| 项 | 结果 |
|---|---|
| 新成员 | `xisp_bc`（`compiler` / `vm` / `jit_*` 共 5930 行），5 个发布成员之一 |
| 留在核心 | IR（`code.cj`：`OP_*` + `CompiledFunction` + `CompiledCode`）与 `ExecutionBackend` 接口 |
| 核心改动点 | 6 处：`evaluator` 字段与注入方法、`core.cj` 编译分派、`higher_order.cj` 闭包回调、`interpreter.cj` 注入、`options.cj` 移除两个选项 |
| 需暴露的符号 | `OP_*` 常量加 `public`（IR 是协议）；`Evaluator.evalBegin` 加 `public`（VM 回退 AST 时按 begin 语义求值） |
| 构建 / 测试 | `cjpm build` 通过；`cjpm test` **427 / 427** |
| 工作区校验 | `cjpm ws check` 通过；拓扑序 `xisp → xisp_bc → xisp_terminal → xisp_repl → xisp_cli` |
| **产物验收** | 只依赖 `ystyle::xisp` 的探针：`Bytecode:` 字面量 **0**、JIT 字面量 **0**（CLI 对照：1 / 5） |

**破坏性变更**：`LispInterpreter.setCompiler` / `setVM` → `setExecutionBackend`；
`withBytecodeCompiler()` / `withJit()` 的 import 来源变为 `ystyle::xisp_bc`（CLI 已同步）。

---

## 10. 科学计算扩展族（xisp-sci）的架构位置 ——**已迁出**

> 本节（`External` 接缝、协议设计、语义决策、破坏面实测、分族与依赖分层、
> 命名冲突、外部 C 库、落地形态、A/B/C 方案比选）已拆为两份：
>
> | 原内容 | 现文档 |
> |---|---|
> | 外部值接缝、协议族、语义决策、破坏面、命名与符号冲突、域装配与中断注入 | [`2026-10-11-capability-domains-design.md`](2026-10-11-capability-domains-design.md) §7、§8 |
> | 科学计算族的依赖分层、C 库、落地形态、数组归属比选 | [`2026-10-11-sci-family-design.md`](2026-10-11-sci-family-design.md)（**规划中**） |
>
> 该族排在很后面（用户明确"会很后面才开始"），故 sci 文档当前只固化约束，不开工。
