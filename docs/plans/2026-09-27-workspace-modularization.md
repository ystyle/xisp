# Xisp 工作区化与能力域拆分设计

**版本**: 0.1（设计草案）
**日期**: 2026-09-27
**状态**: 设计评审
**目标版本**: 0.5.0

---

## 1. 问题

Xisp 的定位是「仓颉嵌入式 Lisp 脚本语言」，但**当前的交付粒度与这个定位不匹配**：
它是一个 cjpm 模块里的 10 个子包，任何宿主只要 `import ystyle::xisp.*`，就会整体拿到
解释器 + REPL + 行编辑器 + 终端控制（POSIX/Windows）+ CLI 入口 + 字节码编译器 + JIT + 示例。

本设计要解决的是两件**正交**的事，外加一件"只有 core 能给"的地基工程：

| 轴 | 机制 | 回答的问题 | 章节 |
|---|---|---|---|
| **仓颉侧（编译期）** | cjpm 模块 / `[workspace]` | 这段代码及其外部依赖，**有没有进这个二进制** | §3、§5 |
| **Lisp 侧（运行期）** | `InterpreterOption` 符号装配 | 同一个二进制里，**宿主给不给脚本这个能力** | §4 |
| **扩展地基（core 独占）** | `LispValue` 的外部值接缝 | 外部模块能不能**拥有自己的值类型** | §10 |

第三件事的驱动是**科学计算扩展族**（`xisp_array` / `xisp_linalg` / `xisp_stats` /
`xisp_signal` / `xisp_io` / `xisp_sci`）：它们不是"给不给能力"的问题，而是要往解释器里
引入**全新的值类型**（N 维数组）。而现状的 `LispValue` 是封闭枚举、且没有外部对象变体，
**这一族现在无法落地**（§10.1）。

一个能力域的完整边界 = 两条轴上的共同决定。这也是本设计的主线：
**能靠运行期装配解决的问题，不要靠拆模块解决；拆模块只用于"依赖图必须变"的场景。**

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

### 2.8 两个真实消费者只用到了 core 的一小块

| 消费者 | 实际 import | 被迫连带吃下 |
|---|---|---|
| `cordis_xisp`（依赖 `ystyle::xisp 0.4.1`） | `types` / `parser` / `core.eval` / `core.builtin.BuiltinFunctions` / `bridge.Bridge` | REPL、终端、CLI、BC、JIT、examples、cjpminfo |
| `atelier`（依赖 `ystyle::xisp 0.4.1`） | 同上 + `terminal.runeDisplayWidth`（仅 1 个函数） | 同上全部 |

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

### 3.1 阶段 1：5 个成员（低风险，解决"被 REPL/CLI/示例拖累"）

**关键技巧：core 的模块名仍然叫 `xisp`。**
模块名 = root 包名，只要它不变，`ystyle::xisp.*` 全部保持不变 → **两个消费者的 import 一行都不用改**
（`terminal` 那处除外）。这几乎是零成本的兼容。

| 目录 | 模块名 | 包名 | 内容 | LOC | 发中心仓 |
|---|---|---|---|---|---|
| `core/` | `xisp` | `ystyle::xisp`（+`types`/`parser`/`core.*`/`bridge`）**不变** | AST 解释器 + 内建 + 模块系统 + 桥接 + `LispInterpreter`/`options` | ~13733 | ✅ |
| `terminal/` | `xisp_terminal` | `ystyle::xisp_terminal` | 终端能力（零 Lisp 依赖，atelier 已当通用库用） | 537 | ✅ |
| `repl/` | `xisp_repl` | `ystyle::xisp_repl` | REPL / 行编辑 / 补全 / 高亮 / 历史 + `runRepl()` | 1363 | ✅ |
| `cli/` | `xisp_cli` | `ystyle::xisp_cli` | `main` | 223 | ❌ 可执行包不上中心仓 |
| `examples/` | `xisp_examples` | `ystyle::xisp_examples.*` | 仓颉示例（原 `src/examples`） | 1419 | ❌ |

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

### 3.2 阶段 2：拆出 `ystyle::bytecode_kit`（解决"BC 恒定编入"）

**技术设计不在本文**：接口草案（`ValueHost<T>` / `RegisterVM<T>` / `FnUnit<T>` / `Codegen`）、
耦合点清单、验收标准，全部见 [`2026-08-25-bytecode-fw-design.md`](2026-08-25-bytecode-fw-design.md) §2–§3。
本文只记**决策**：

| 项 | 决定 |
|---|---|
| 模块名 | **`ystyle::bytecode_kit`**（中性名，不含 `xisp`）——将来若被第三方复用**不必改名**；改名会破坏所有消费者 |
| 与既有设计的关系 | 即 08-25 的**方案 A**。本文的"阶段 2"与它**是同一件事**，以那边为准 |
| 实施范围 | **只做方案 A**。方案 B（语义规则化）/ C（语法驱动生成器）**不做**，种子留在 08-25 |
| 实现深度 | **特化优先**：`ValueHost<LispValue>` / `BytecodeVM<LispValue>`（避免每指令 trait 分派），泛型化留白 |
| core 侧接口形状 | 按 `ValueHost<…>` 的**形状**定义（而非 `Evaluator` / `Compiler` 具体类）；特化只是"类型参数先填死"，将来放开是机械改动 |
| 边界 | `compileDefine` 等**前端语义留在 xisp**（08-25 §3.3 已划对）；kit 内不含任何 Lisp 语义 |

**本文此前那个 `ExecutionBackend` 草案作废**，以 08-25 的 `ValueHost<T>` 为准——二者职责重叠，
不该存在两套接口。同时，08-25 §3.2 的 `ValueHost<T>` 少了两项 xisp 侧需要的能力，**拆分时应补齐**
（需回写该文档）：

1. `closureProcedure(id)` —— 替代现在 `higher_order.cj:513` 直读 `BytecodeVM.functionTable`；
2. **执行钩子** —— JIT 需要在指令派发处插快速路径（`vm.cj:599`），kit 要给宿主留这个接入点。

- 收益：不依赖 `bytecode_kit` 的宿主**不引入字节码后端**（compiler 1539 + vm 1675 + code 108 = 3322 行）；
- 风险：动了 core 内部，但回归网较厚（`bytecode_test.cj` 478 行 + `jit_test.cj` 489 行）。

### 3.3 阶段 3：拆出 `xisp_jit`（把机器码生成从默认二进制里拿掉）

JIT **不属于 `bytecode_kit`**：它生成的是**针对 `LispValue` 内存布局**的机器码，与 xisp 的值表示强绑定，
不具备通用性。它依赖 `bytecode_kit`（经 §3.2 的执行钩子接入）+ `xisp`（值表示）：

```
xisp_jit  →  bytecode_kit + xisp
```

收益的准确口径：**不启用 JIT 时本来就不会触发 `mmap`**（`rwx` 在 JIT 运行时初始化时才申请），
所以拆出的主要收益是**体积与信任面**——不引入 2608 行机器码生成代码，也就没有"被误启用"的可能。
平台面：JIT 仅 Linux x86-64（`@When`），其它平台本来就会回退字节码。

**可选**：若阶段 2 之后确认没人需要"要 BC 不要 JIT"，本阶段可跳过（JIT 留在 xisp 内）。

### 3.4 目录布局

```
xisp/                      # workspace 根 = 产品
├── cjpm.toml              # 仅 [workspace] members
├── workspace.toml         # cjpm-ws 清单：产品版本 / 共享依赖 / 发布范围
├── core/  terminal/  repl/  cli/  examples/
│   └── {cjpm.toml, src/}
├── bytecode_kit/          # 阶段 2 新增：模块名 ystyle::bytecode_kit
├── jit/                   # 阶段 3 可选：模块名 ystyle::xisp_jit
├── docs/  docs-site/  examples/  lisp-tests/  scripts/    # 仓库级共用资源原地不动
```

（仓库根 `examples/*.lisp` 是 Lisp 示例，与 `src/examples` 的仓颉示例是两回事，不动。
`bytecode_kit` 的目录名与模块名一致，但**模块名故意不含 `xisp`**——见 §3.2。）

---

## 4. Lisp 侧：能力域（按需装配）

### 4.1 结论先说

**Lisp 内建函数的"按需加载"不需要拆 cjpm 模块**——它是运行期符号装配，与 §3 的编译期依赖图正交。
只有当某个域**的实现需要外部依赖或平台特化**时，才两侧一起做（域模块 + 装配选项）。

### 4.2 域模型

一个能力域 = 三个要素：

1. **注册集合**（哪些顶层符号）——靠"不注册"表达禁止（§2.7 的约束）；
2. **实现位置**（core 内 / 独立模块）；
3. **默认开关状态**（解释器默认给不给）。

域在 core 内部时，直接用现有 `registerXxx` 同包函数组合即可；
域在独立模块时，需要一个跨模块接缝（草案）：

```cangjie
// ystyle::xisp（core 侧，草案）
public interface LispCapability {
    prop name: String                 // 域标识，如 "net"
    prop symbols: Array<String>       // 本域注册的顶层符号，供沙箱审计与自省
    func install(interp: LispInterpreter): Unit
}

public func withCapability(cap: LispCapability): InterpreterOption
public func withCapabilities(caps: Array<LispCapability>): InterpreterOption
```

`xisp_net` 依赖 core 并提供 `NetCapability`，core **完全不认识 net** —— 依赖方向正确，
且这套机制对阶段 2/3 的 BC、JIT 同样适用（成熟后可与 §3.2 的 `ValueHost` 注入点统一为同一套宿主接缝）。

### 4.3 域清单与现状

**A. core 域（编译进 core；目前大多无开关，建议补上按域装配）**

| 域 | 符号示例 | 现注册点 |
|---|---|---|
| base | `nil`、关键字别名 | `registerAll` 头部 / `env.registerKeywordAlias` |
| arithmetic | `+ - * / mod abs min max` | `registerArithmetic` / `registerModulo` / `registerMath` |
| comparison | `= < > <= >=` | `registerComparison` |
| list | `car` / `cdr` / `cons` / `length` | `registerListOps` |
| higher-order | `map` / `filter` / `reduce` / `apply` / `for-each` | `registerHigherOrder` |
| logic | `and` / `or` / `not` | `registerLogic` |
| predicates | `null?` / `number?` | `registerPredicates` |
| hashmap | `hashmap-*` | `registerHashMap` / `registerRedisAliases` |
| macros | 内建宏 | `BuiltinMacros.registerAll` |
| aliases | 现代 / Redis 别名 | `registerModernAliases` |
| string & output | `str` / `print` / `println` | `registerStringOps` / `registerPrint` / `registerOutput` |

**B. 宿主策略域（core 内，运行期开关，已实现）**

| 域 | 现开关 |
|---|---|
| stdio 桥接 | `withStdIO()` |
| fs 桥接 | `withStdFS()` |
| collection 桥接 | `withStdCollection()` |
| 模块加载 | `withNoModuleLoad()` / `withAllowedModulePaths()` / `withModuleMode()` |
| 文件写 | `withNoFileWrite()` / `withAllowedPaths()` |
| 函数黑/白名单 | `withBlockedFunctions()` / `withAllowedFunctions()` |
| 输出通道 | `withOutputFn()` |
| 纯函数子集 | `BuiltinFunctions.registerPure(env, output!)` |

**C. 只靠拆模块能解决的域（有外部依赖 / 平台特化）**

| 域 | 模块 | 外部依赖 | 状态 |
|---|---|---|---|
| bytecode | `bytecode_kit` | 无外部依赖（通用基座，零 Lisp 语义） | 已实现，待拆（§3.2） |
| jit | `xisp_jit` | 无外部依赖；含 `rwx` 与 Linux x86-64 平台特化 | 已实现，待拆（§3.3） |
| net（http / websocket / tls，见 §4.5） | `xisp_net` | stdx.net.* / stdx.crypto | 规划 |
| data（compress / serialization / json） | `xisp_data` | stdx.compress.* / stdx.serialization | 规划 |
| async | 见 §4.5 | 可能零外部依赖 | 规划 |

### 4.4 外部依赖域：stdx 只能走二进制依赖，及其真实代价

**结论：stdx 的三种"源码依赖"名义上存在，实际不可用。**（评审反馈 + 工作区 20+ 项目一致实践）

| 方式 | 官方文档说法 | 实际 |
|---|---|---|
| git / 本地 / 中心仓源码依赖 | 支持 | 需把 stdx 连同它依赖的第三方 C 库一起编译（python / cmake(≥3.16.5,<4) / ninja / openssl≥3 / clang 15.x / Windows 下 mingw-w64）；中心仓方式还得不到可静态依赖的产物，部署复杂度直接上台阶 |
| **二进制依赖（`CANGJIE_STDX_PATH`）** | — | **唯一好用**，对齐 SDK 自带的预编译产物 |

所以 stdx 一律按二进制依赖处理：

```toml
[target.x86_64-unknown-linux-gnu.bin-dependencies]
  path-option = ["${CANGJIE_STDX_PATH}"]
# … 各平台各列一遍（参考 cordis-cj/cjpm.toml）
```

#### 实测一：`bin-dependencies` **会传递给消费者**（好消息）

独立模块 `probe_app3` 依赖另一个独立模块 `probe_lib`（后者 `import stdx.crypto.digest`
并声明了 `bin-dependencies` + `link-option`），而 `probe_app3` 自己**什么都不声明**：

| 宿主构建环境 | 结果 |
|---|---|
| 有 `CANGJIE_STDX_PATH` | ✅ `cjpm build success` |
| 无 `CANGJIE_STDX_PATH` | ❌ `root package 'ystyle::probe_lib' imports package 'stdx.crypto.digest' ... but it is not added as a dependency in cjpm.toml` |

即：**宿主不必抄 stdx 的样板，但环境变量必须在**。
（前提是**被依赖的库自己声明了** —— 制品里的 `cjpm.toml` 带着它，见 §5.2 实测二。）
并且那条报错**指向错误方向**（说"cjpm.toml 里没加依赖"，实际缺的是环境变量），诊断成本高。

#### 实测二：环境变量的路径写法因人而异（本设计要正面处理的问题）

`cjvs stdx-env zsh` 只导出**一个**变量，路径由三段拼成：

```
CANGJIE_STDX_PATH=/home/<user>/.config/cjvs/stdx/<stdx-version>/<platform-triple>/static/stdx
```

而这三段在不同安装方式下都不一样：cjvs 管理 / SDK 手装 / DevEco 自带，路径深度各不相同；
cjvs 下 `CANGJIE_HOME` 甚至是**每次会话变化的临时目录**。于是"写死一个路径"在团队内不可行，
只能依赖变量——而变量在 CI / 容器 / 新机器上默认不存在。

**设计对策**（写进实施清单）：

1. **把 stdx 关进能力域模块，core 与其它成员一律不得 `import stdx.*`。**
   这样"不需要网络/压缩的宿主"**完全不接触这个变量**——这是拆模块最实际的收益；
2. **由用到 stdx 的成员各自完整声明** `bin-dependencies` / `link-option`（全平台各一条）。
   **不要只写在 workspace 根**：根那份只对"在根目录跑本地构建"生效，
   在成员目录内 `bundle` / `publish` 时**不读根**（实测，见 §5.2 实测二）；
3. **能力域模块的 README 必须写明**：构建环境需先 `eval "$(cjvs stdx-env zsh)"`（或等价配置），
   并**明确警告 cjpm 的报错会指向 cjpm.toml、掩盖环境变量缺失**；
4. 选做：提供探测/体检脚本（`CANGJIE_STDX_PATH` 存在且目录内有 `libstdx.*` 才放行），
   接进 CI，避免拿 cjpm 那条误导性报错去排查。

**FFI 链接选项同样不外泄**：用到带 FFI 的 stdx 子库（`stdx.net.tls`、`stdx.crypto.x509` 等）
需按官方文档补 `-lstdx.net.tlsFFI -lcangjie-dynamicLoader-opensslFFI` 之类 —— 关在域模块内。

### 4.5 网络域的分层选型（决定要不要外部依赖）

网络能力**不是非 stdx 不可**，按协议层次分三档：

| 档 | 能力 | 依赖 | 归属 |
|---|---|---|---|
| 传输层 | TCP / UDP / UDS socket | **`std.net`（标准库，零外部依赖）** | 可留 core 或零依赖模块，运行期装配 |
| 应用层 | HTTP / WebSocket / TLS | `stdx.net.*` + `stdx.crypto`（FFI + 环境变量） | **必须拆 `xisp_net`** |
| 备选 | 第三方纯仓颉 HTTP 库（中心仓已有若干，如 `soulsoft_net_http` 等） | 待确认其自身是否依赖 stdx | **待调研**，判据：该库是否纯 `std.net` 实现 —— 若是，网络域也能做到零环境变量 |

（已查证：`std.net` 只有传输层 UDP/TCP/UDS，HTTP 与 TLS 都在 stdx。）

### 4.6 异步域的定位（不急于拆）

仓颉的 `async` / `await` 是**语言级**能力（std.core），不引入外部依赖，因此：

- **脚本语法层的 async 求值语义** → 属 core（需要 `Evaluator` 支持，不是模块问题）；
- **与宿主事件循环 / 线程池对接** → 属**宿主策略域**（运行期注入运行时），也不需要拆模块；
- **若采用 `stdx.actors`** → 转为**外部依赖域**，拆 `xisp_async` 模块。

即：异步的归属**取决于实现选型**，建议在选型确定前不预设模块，先用运行期装配表达。

---

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
   - **消费者**：正因为制品里带着，消费者不必重复写（§4.4 实测一）；
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
6. 改包声明：`ystyle::xisp.repl` → `ystyle::xisp_repl`、`.terminal` → `ystyle::xisp_terminal`、
   `.cli` → `ystyle::xisp_cli`、`.examples.*` → `ystyle::xisp_examples.*`；**core 侧包名不动**；
7. 把 `runREPL` / `runSimpleREPL` 从 `LispInterpreter` 移到 `xisp_repl`；
8. stdx 的 `bin-dependencies` **留到引入第一个能力域模块（`xisp_net`）时，由该成员自己写全平台那份**
   —— 不要只写在 workspace 根（成员目录内 bundle/publish 不读根，见 §5.2 实测二）。
   阶段 1 各成员都不引 stdx，本步暂不动；
9. `cjpm build` + `cjpm test`（workspace 全量）跑绿；
10. 更新 README / docs-site 中的产物路径（`ystyle::xisp.cli` → `ystyle::xisp_cli`，8 处引用）
    与 `src/examples/` 路径；
11. 补构建 CI：`cjpm ws check` + `cjpm build` + `cjpm test`；
12. 合入 master，走 `cjpm ws bump 0.5.0` → `cjpm ws bundle` → 私有仓预演 → 中心仓。

---

## 7. 跨仓改动

| 仓库 | 改动 |
|---|---|
| `cordis-cj/cordis_xisp` | 无（`ystyle::xisp.*` 包名不变）；升级依赖版本至 `0.5.0` |
| `atelier/cli` | `ystyle::xisp.terminal.*` → `ystyle::xisp_terminal.*`（`tui.cj:3`、`md_render.cj:4`、`tui_test.cj:14`、`tui_activity_test.cj:15`）；升级 `0.5.0` |

---

## 8. 风险与未决问题

| # | 项 | 说明 |
|---|---|---|
| R1 | 阶段 2 动 core 内部 | AST↔VM 接口化虽窄，但涉及求值主路径，需靠既有 bytecode/jit 测试网（约 970 行）兜底；另需回写 08-25 文档（补 `closureProcedure` 与执行钩子，见 §3.2） |
| R2 | workspace 版本不校验 | 靠 `cjpm ws check` 进 CI 兜；**不做这道闸门就等于没有 lockstep** |
| R3 | `terminal` 独立成库 | 它事实上是通用终端库（atelier 已当通用库用），长期看是否该独立成产品/另仓，值得单独评估 |
| Q1 | 阶段 2 / 3 是否本次做 | **决策已定**（§3.2/§3.3）：阶段 2 拆 `bytecode_kit`、**只做方案 A**；阶段 3 `xisp_jit` 视需要。剩余待定的是**排期**（是否与阶段 1 同批） |
| Q2 | `xisp_cli` 是否也走 lockstep | 不发布则版本会漂移，需明确"跟随产品版本但不上中心仓" |
| Q3 | core 的 `cjpminfo` 依赖 | 现在 `types` 用 `@ModuleVer`；若要"零外部依赖核心"，可在阶段 2 用 `build.cj` 生成版本常量替掉 |
| Q4 | 能力域粒度 | core 域的按域装配是否需要（如"只要算术+表，不要高阶函数"），需要真实宿主诉求来定 |
| R4 | stdx 环境变量 | `CANGJIE_STDX_PATH` 的路径三段（stdx 版本 / 平台三元组 / static-dynamic）随安装方式而变，CI/容器/新机器默认不存在；对策见 §4.4，其中"core 与其它成员不得 import stdx"是硬约束 |
| R5 | 多成员样板重复 | `bin-dependencies` / `link-option` 必须在每个要发布的成员里各写全份（cordis-cj 实测 42 处重复），且 cjpm-ws 目前不同步这类字段。缓解：约定"只有能力域成员才允许出现 stdx 配置" + CI 校验各成员该段一致 |
| Q5 | 网络域走哪条路 | 传输层（`std.net`，零依赖）现在就可用；应用层（HTTP/TLS）走 stdx 还是第三方纯仓颉库，见 §4.5，需按"是否引入 stdx"这一判据调研后再定 |

---

## 9. 验收标准

1. `cjpm build` / `cjpm test` 在 workspace 根全绿，成员数与依赖图符合 §3.1；
2. 一个只依赖 `ystyle::xisp` 的探针工程，**不配置 `CANGJIE_STDX_PATH`**、不链接 terminal/REPL，
   可正常创建解释器、注册桥接函数、执行脚本（对应用户侧"嵌入式"的最低要求）；
3. 阶段 2 完成后，只依赖 `ystyle::xisp` 的探针二进制**不含字节码后端符号**（3322 行）；
   阶段 3 完成后进一步不含 JIT 符号（2608 行）；
4. `cjpm ws check` 在 CI 中作为闸门生效；
5. `atelier` / `cordis_xisp` 升级后构建通过。

---

## 10. 科学计算扩展族（xisp-sci）的架构位置

> 本节回答"拆 core 之后，`array` / `linalg` / `stats` / `signal` / `io` / `sci` 这一族怎么放"，
> 并指出**只有 core 能提供、且必须现在就提供**的那道接缝。

### 10.1 硬约束：现在的 core 无法承载"数组"这种值

`LispValue` 是**封闭枚举**（`types/types.cj`），共 15 个变体：

```
Nil / Boolean / Int / Float / Str / Symbol / Cons
Procedure / ProcedureFromParams / Macro / MacroFromParams
NativeFunc / Closure / HashMap / Error
```

**没有任何"外部 / 宿主对象"变体。** 而 `bridge` 里的 `LispConvertible` / `LispDeserializable` /
`ExtendLispValue` 是**转换协议**——它们把仓颉类型转成上面 15 种之一（或从其一转回），
**不能新增变体**。

后果：`xisp_array` 的 N 维数组**只能**退化成 `HashMap` 或 `Cons`（逐元素一个 `LispValue`）：

| | 想要 | 退化的后果 |
|---|---|---|
| 内存 | 连续 `Array<Float64>` + shape / strides | 1e6 元素 = 1e6 个装箱 `LispValue` |
| 视图 / 切片 | 零拷贝 view | 只能整份复制 |
| BLAS / LAPACK 互操作 | 把指针直接交给 C | 无法表达 |
| 性能 | 与 numpy 同级的目标 | 数量级退化，科学计算失去意义 |

**所以必须由 core 提供一道"外部值"接缝**——这是整份设计中唯一"只有 core 能给"的东西。

### 10.2 接缝形态：为什么必须是「core 加一个变体 + 一个协议」

**先回答"是不是直接做到 core 里"：是，但 core 里只增加两样东西——**

1. `LispValue` 的**一个**变体：`External(LispExternal)`；
2. **一个**协议接口（约 20~40 行）。

数组的内存布局、广播规则、视图、BLAS 调用、dtype 系统，**一行都不进 core**。core 保持"不认识数组"。

**为什么绕不开**（逐个排除替代方案）：

| 方案 | 结论 |
|---|---|
| 用 `HashMap` / `Cons` 表示数组 | ❌ 1e6 元素 = 1e6 个装箱 `LispValue`，连续缓冲与原生身份全丢 |
| 用 `LispConvertible` / `LispDeserializable` | ❌ 它们是"映射到现有 15 个变体"，**不能新增变体** |
| 把 `LispValue` 从 `enum` 改成 `interface` | ❌ 全量重构 + 每个值一次虚分派，比现在更慢 |
| 用 `Any` 关联值（变体 `Host(Any)`） | ⚠️ 仍要加变体，但丢类型安全（任何值都能塞）；协议方案严格更优 |
| 把数组类型做进 core | ❌ core 被科学计算绑架，与"嵌入式精简核心"直接矛盾 |
| **变体 `External(LispExternal)`** | ✅ 唯一同时满足**类型安全 / core 零依赖 / 零拷贝**的形态 |

**语言层实测**（cjpm 1.1.3，最小工程，已清理）：

- 接口可以作为枚举关联值 ✅；
- `non-exhaustive patterns` 是**编译错误**（不是警告）→ 加变体必然被编译器逐处定位，不会漏改（好事）；
- **踩到的语法细节**：枚举构造器名**不能与接口名同名** —— `| Ext(Ext)` 会被解析成类型模式，
  报 `enum pattern's parameters size is wrong`。所以变体名与协议名要区分（`External` / `LispExternal` 没问题）。

#### 10.2.1 协议设计：最小必需 + 能力接口（可组合）

```cangjie
// ystyle::xisp.types（core 侧，草案）

/** 必需：core 的通用逻辑（print / type-of / 错误信息）只依赖这两项 */
public interface LispExternal {
    prop typeName: String        // "ndarray"，供 type-of / 谓词 / 错误信息
    func display(): String       // repr；**实现方负责截断**（见 §10.2.2 第 6 条）
}

/** 以下按需实现，core 内建按"是否实现"分派，互不牵连 */
public interface LispExternalEquatable <: LispExternal {
    func equals(other: LispExternal): Bool
}
public interface LispExternalIndexable <: LispExternal {
    func length(): Int64
    func at(index: Int64): LispValue      // 供 (length x) / (nth x i) 等通用内建
}
public interface LispExternalComparable <: LispExternal {
    func compare(other: LispExternal): Int64
}
public interface LispExternalCallable <: LispExternal {
    func invoke(args: ArrayList<LispValue>): LispValue
}
```

设计原则：**core 只强制最小集**，其余能力用接口探测（`is LispExternalIndexable`）渐进支持。
扩展方负担最小；将来新增能力接口也不会破坏已有实现（它们只是不实现新接口）。

#### 10.2.2 语义决策（现在就定，别留到实现时随手定）

| # | 议题 | 建议 | 理由 |
|---|---|---|---|
| 1 | `=` 对 External 的语义 | **同一性 + 结构浅比较**（shape / dtype / 共享缓冲），**不做逐元素**；逐元素比较用显式 `array-equal?` | `=` 在 Lisp 里返回 Bool，藏在 `(if (= a b) …)` 里的 O(n) 比较是昂贵意外 |
| 2 | truthy | **External 恒为真**（对象存在即真） | 避免"空数组是假"这类跨域分歧；判空用显式谓词 |
| 3 | 可变 External 作 `HashMap` 键 | **禁止**（未实现 `LispExternalEquatable` 就不能作键） | 可变对象哈希不稳定，会静默破坏 `HashMap` |
| 4 | FFI 分配的资源 | 实现方自行实现 `Resource`；**core 不引入 `close` 语义** | core 不该知道谁持有外部内存 |
| 5 | 序列化 | External **不参与** JSON / 模块序列化；需要时实现 `LispConvertible.toLisp()` 显式转换 | 保持"序列化只处理内建变体"的封闭性 |
| 6 | `display()` 长度 | 由实现方截断 | core 若统一截断，科学计算想打全量就没法表达 |

#### 10.2.3 中断接缝：外部模块怎么拿到检查器

core 已有 `setInterruptChecker`，但 `Evaluator.isInterrupted()` 是**实例成员**，外部模块手里
只有自己的对象，拿不到求值器。若不解决，就是"linalg 跑 30 秒、宿主杀不掉"（§10.6）。

方案：**在域装配时显式注入**（不用全局状态）：

```cangjie
// core 侧（草案）
public class CapabilityContext {
    public let env: Environment
    public let isInterrupted: () -> Bool                                  // 长循环里周期性调用
    public let register: (String, (ArrayList<LispValue>) -> LispValue) -> Unit
    public let registerType: (String) -> Unit                             // 声明本域产生的外部类型名，供沙箱审计
}

public interface LispCapability {
    prop name: String
    prop symbols: Array<String>
    func install(ctx: CapabilityContext): Unit
}
```

契约（写进模块 README 与域声明）：

> 外部模块中任何 O(元素数) 以上的循环，必须周期性调用 `ctx.isInterrupted()`，
> 为真时以 `Error(RuntimeError, INTERRUPTED_MESSAGE)` 中止。

### 10.3 破坏面实测：不是"全生态大改"，而是 **~24 处补分支**

> **更正**：本节此前写"core 内 133 处 match / 470 个分支"——那是**涉及 `LispValue` 的 match 总数**，
> 不是**需要修改的数量**（绝大多数已有 `case _` 兜底，或匹配的其实是 `Option` / `Token` 等）。
> 下面是按"模式里出现 LispValue 变体、且块内无 `case _`"重新统计的结果：

| 范围 | 未兜底的 LispValue match | 位置 |
|---|---|---|
| core | **12** | `core/eval/module.cj` 7 · `types/types.cj` 2 · `core/eval/macro.cj` 1 · `pattern_match.cj` 1 · `higher_order.cj` 1 |
| 下游生产代码 | **0** | atelier / cordis_xisp 的 `runtime.cj`、`json_bridge.cj` 均不受影响 |
| 下游测试代码 | **12** | 全部集中在 `atelier/src/ptc/json_bridge_test.cj` |
| **合计** | **~24** | 每处补一个 `case External(_) => …` |

而且 `non-exhaustive patterns` 是**编译错误**（实测），编译器会逐处指出，**不会漏改**。

**结论：加变体的成本远低于此前估计，建议并入 0.5.0**（与拆模块共用同一次生态同步）。

### 10.4 分族与依赖分层（严格单向）

```
        xisp（core：LispValue / External 协议 / 域装配 / 中断接缝）
              ↑
        xisp_array            ← 唯一地基：dtype / shape / strides / 广播 / 视图
        ↑    ↑    ↑    ↑
   xisp_io  xisp_linalg  xisp_stats  xisp_signal
        ↑    ↑    ↑    ↑
            xisp_sci           ← 门面：聚合 + 高层 API，不含重实现
```

**规则（防止长成大泥球）**：

1. `array` 是唯一地基，**只有它可以被同族其它模块依赖**；
2. `io` / `linalg` / `stats` / `signal` **互相不得依赖**（各自只依赖 `array` + core）；
3. `sci` 只做聚合 / re-export 与跨域工作流，不重复实现；
4. 全族**不得反向依赖** core 以外的语言本体设施（`repl` / `cli` / `bc`）。

### 10.5 命名与符号冲突

- **模块名不能含连字符**（cjpm 要求合法标识符）：仓库可以叫 `xisp-sci`，但模块必须是
  `xisp_array` / `xisp_linalg` / …，包名 `ystyle::xisp_array`；
- 科学计算会引入大量顶层符号（`matmul` / `mean` / `fft` / `shape`），与 core 内建**重名风险高**。
  建议：域注册时声明命名空间或统一前缀，并**把符号清单作为域声明的一部分**
  （§4.2 的 `LispCapability.symbols`）。好处有三：沙箱可审计、文档可自动生成、
  重名能在装载时被检出，而不是运行时静默覆盖。

### 10.6 与 core 的接缝共三件 + 建议实施顺序

| # | 接缝 | 设计位置 | 状态 |
|---|---|---|---|
| 1 | `External` 变体 + `LispExternal` 协议族 | §10.2.1 | **待做**，建议并入 0.5.0 |
| 2 | 域装配 `LispCapability` + `CapabilityContext`（中断注入、类型声明） | §10.2.3、§4.2 | **待做** |
| 3 | 外部模块的**中断义务**（长循环必须查中断） | §10.2.3 | 机制已有，契约待写进模块 README |

> 第 3 条最容易被忽略：core 已有 `setInterruptChecker`（`evaluator.cj:188`），
> `INTERRUPTED_MESSAGE` 也已是 public（cordis / atelier 在用）。但沙箱 `timeout` 与宿主取消
> **只能打断求值热路径**——`xisp_linalg` 里一个跑 30 秒的分解循环若不主动查中断，宿主就**杀不掉它**。

**建议实施顺序**（若决定做）：

1. `types` 包加协议族 + 变体 → 编译驱动修 core 内那 12 处，补单元测试；
2. core 通用语义按能力接口分派：`print` / `type-of` / `=` / `length` / `nth` / 谓词；
3. `LispCapability` + `CapabilityContext`（中断注入、类型声明）；
4. **先用一个测试用假扩展**（如 `ExternalDemo`：一个极简的外部计数器值）端到端验证
   —— 注册域 → 产生 External → `print` / `=` / 中断全部走通；
5. 之后才开 `xisp_array`（它是唯一地基，必须先稳）。

### 10.7 外部 C 库（LAPACK / BLAS / FFTW）

- 走 `[ffi.c]` + `link-option`，且按 §5.2 的实测结论**每个成员各自声明**（workspace 根不顶用）；
- 与 stdx 同理：这些配置要**关在对应成员内**，不污染 core 与同族其它成员；
- 待决：性能后端的形态。cjpm 没有可选依赖，所以"纯仓颉实现 vs C 加速"只能三选一：
  ① 只做纯仓颉（零依赖、性能受限）；② 拆 `xisp_linalg_native` 之类的加速模块（API 分叉）；
  ③ 同模块内运行时探测（编译期仍需 C 库在场）。**建议先 ①，把 ③ 留作后路。**

### 10.8 科学计算族的落地形态

**已定**（评审结论）：科学计算族**一定是新的 cjpm 模块**（`ystyle::xisp_array` / `…_linalg` / …），
不会挤进语言本体的模块里——这与 §3.0 的判定规则一致（独立依赖、独立平台特性、独立演进）。

**暂缓**：它们是各自独立仓库、独立 workspace，还是合成一个 `xisp-sci` workspace，
等这些模块真有代码、能看清发布节奏与 C 依赖时再定。届时参考下表：

| 方案 | 说明 | 取舍 |
|---|---|---|
| A 拆成两个 workspace | `xisp`（语言本体）+ `xisp-sci`（array / io / linalg / stats / signal / sci） | 发布节奏、外部依赖（LAPACK / FFTW）、目标用户都不同；跨 workspace 写精确版本号 |
| B 同一个 workspace | 十余个成员共用 lockstep 版本 | 语言本体改一行 bug 也要整族发版；C 依赖会渗透进语言本体的发布流程 |
| C 每族独立仓库 | 演进最自由 | 版本 / CI / 发版协调成本最高 |

### 10.9 待决问题

| # | 问题 | 现状 |
|---|---|---|
| S1 | 0.5.0 是否**一并加 `External` 变体** | 成本已实测（~24 处，§10.3），**建议并入**；待你拍板 |
| S2 | 变体与协议的命名 | 建议变体 `External` + 协议 `LispExternal`（注意二者不能同名，§10.2） |
| S3 | 科学计算族的仓库 / workspace 划分 | **暂缓**（见 §10.8），已定"一定是新的 cjpm 模块" |
| S4 | linalg / signal 的性能后端形态 | 见 §10.7，建议先纯仓颉 |
| S5 | 符号命名策略：命名空间（`(import sci.linalg)`）还是前缀（`linalg-`） | 待定，与 `LispCapability.symbols` 声明配套 |
| S6 | 是否把"LispValue 的 match 必须有外部值分支"写成 lint / 检查 | 加变体时编译器能强制一次，但**将来再加变体还会重演**；建议留一条 CI 检查 |
| S7 | 数组的归属：External / 专用 `Array` 变体 / 两者并存（§10.10） | **待拍板**，判据见 §10.10 |

### 10.10 方案比选：数组的归属（回答"把 array 做成 core 呢"）

先说清："array 做成 core"有**两种完全不同的含义**，混在一起谈会得出错误结论：

- **1a**：core 里有 `Array` 变体 **+ 数组的完整实现**（数据布局 / 广播 / reduce / 打印）
- **1b**：core 里只有 `Array` 变体 **+ 抽象接口**，**实现在 `xisp_array`**

#### 1a 的代价（量化）

| 项 | 估计 | 依据 |
|---|---|---|
| 纯仓颉 NDArray 核心（dtype / shape / strides / 视图 / 广播 / reduce / 打印） | **3000~5000 行** | 与现有**字节码后端**同量级（`compiler.cj` 1539 + `vm.cj` 1675 = 3214 行） |
| core 当前非测试规模 | 13733 行 | 见 §2.3 |
| 于是 core 体积 | **+25%~35%** | |
| C 后端（BLAS / LAPACK） | **无处安放** | 一旦进 core，core 就有 C 依赖，与"零依赖嵌入式核心"直接冲突，且 §5.2 的"每成员自带 bin-dependencies"会在 core 上重演 |
| 宿主"不要数组" | **不可能** | 变体与实现都在 core 模块里 |

另外：**每新增一个一等类型**（将来还有 dataframe / graph / stream…）都要改 enum + 全生态补 `case`。

生态现状：中心仓**没有成熟的通用 NDArray 库**（`linalg4cj` 下载 2、`cangjie_tpc::matrix4cj` 下载 6），
所以数组大概率要自己写——代码量落在 xisp 侧，归属是可控决策。

#### 关键洞察：「一等公民待遇」**不来自专用变体**

它来自 **core 的通用内建按能力接口分派**。只要 `length` / `nth` / `print` / `=` / `type-of`
会探测能力接口（§10.2.1），数组**用 `External` 接入也能获得近乎一等的观感**：

```lisp
(length arr)   ; core 探测 LispExternalIndexable
(print  arr)   ; core 调 display()
(= a b)        ; core 探测 LispExternalEquatable
(type-of arr)  ; → "ndarray"
```

专用变体**唯一不可替代的收益是编译器特化**（字节码 / JIT 为数组生成操作码或快速路径）。
所以要问的是：**xisp 的 JIT 会不会为数组做特化？**
- 会 → 专用变体（乃至 1b）值得；
- 不会（数组运算都走 `xisp_array` 自己的仓颉实现，JIT 不介入）→ 专用变体收益很小。

#### 三方案对比

| | A：只加 `External` | B：`External` + `Array` 变体（抽象在 core、实现在 `xisp_array`） | C：`Array` 完整实现进 core |
|---|---|---|---|
| core 新增 | 1 变体 + 协议族 | 2 变体 + 数组抽象接口 | 2 变体 + 协议族 + 3000~5000 行实现 |
| core 体积 | +~40 行 | +~100 行 | **+25%~35%** |
| 通用内建支持数组 | 靠能力接口（可做到） | 直接 | 直接 |
| 编译器特化数组 | ❌ | ✅ | ✅ |
| C 后端位置 | `xisp_linalg` | `xisp_linalg` / `xisp_array` | **无处安放** |
| 宿主可否不要数组 | ✅ | ✅ | ❌ |
| 将来新增一等类型 | 零破坏 | 每个都要改 enum + 补 case | 同左 |
| 本次破坏面 | ~24 处 | ~24 处（**边际成本≈0，同一批**） | ~24 处 + 实现自身 |

#### 结论与推荐

- **C 不推荐**：把"实现"和"身份"一起搬进 core，代价是 core 膨胀 + C 后端无处安放 + 宿主失去选择权。
- **A 与 B 的差别很小**，判据只有一条：**JIT 是否要为数组特化**。
- 需要注意：因为 0.5.0 **无论如何都要为 `External` 走一遍 ~24 处补 `case`**，
  "顺带加 `Array` 变体"的**边际成本几乎为零**（每个 match 多写一个分支而已）；
  若推迟到以后再升 B，届时下游可能已经写了更多 `LispValue` 匹配，破坏面会变大。
  → **若确定把科学计算当产品主线，倾向 B**（一次做够）；若想保持 core 概念最少，选 A。

#### 边界规则（若选 B）

| 归 `Array`（专用变体） | 归 `External`（通用接缝） |
|---|---|
| **所有科学计算都依赖的地基**：NDArray 本身 | 某个领域的对象：linalg 的分解结果、stats 的分布、signal 的滤波器、io 的句柄 |

#### 无论 A/B/C，数组的**实现**都不该进 `xisp`（core 模块）

| | 实现在 `xisp` | 实现在 `xisp_array` |
|---|---|---|
| 宿主不要数组 | ❌ 必吃 | ✅ 不依赖即可 |
| 变体安全性 | — | ✅ core 里的 `Array` 变体**永远构造不出来**（没人实现接口） |
| 与 §10.8"科学计算一定是新模块" | 冲突 | 一致 |
