# 模块与依赖

Xisp 是一个 **cjpm 工作区**（`[workspace]`），由 7 个成员模块组成，每个成员都是可单独依赖的包。
宿主**按需依赖**：只要表达式求值能力就只依赖核心；要 REPL 或字节码 / JIT 再各自追加。

---

## 成员一览

| 模块 | 包 | 内容 | 依赖 |
|---|---|---|---|
| `xisp` | `ystyle::xisp` | AST 解释器 / 内建函数 / 模块系统 / 仓颉桥接 | **无**（零外部依赖） |
| `xisp_bc` | `ystyle::xisp_bc` | 字节码编译器 / 寄存器栈 VM / JIT | `xisp` |
| `xisp_terminal` | `ystyle::xisp_terminal` | 终端能力（raw mode、字符显示宽度） | 无 |
| `xisp_repl` | `ystyle::xisp_repl` | 交互式 REPL（行编辑 / 补全 / 高亮 / 历史） | `xisp`、`xisp_terminal` |
| `xisp_cli` | `ystyle::xisp_cli` | 命令行入口（可执行） | `xisp`、`xisp_repl`、`xisp_bc` |
| `xisp_examples` | `ystyle::xisp_examples.*` | 仓颉侧示例（**不发布**） | `xisp`、`xisp_repl` |
| `xisp_tests` | `ystyle::xisp_tests` | 契约 / 集成测试（**不发布**） | `xisp`、`xisp_bc` |

依赖方向单向，无环：

```
xisp_terminal          xisp（核心：零外部依赖，不含字节码与 JIT）
      ↑                  ↑
xisp_repl ───────────────┤
      ↑                  │
xisp_cli ────────────────┤
                         │
xisp_bc ─────────────────┘   （可选后端）
```

---

## 宿主该依赖哪些

| 你要什么 | 依赖 |
|---|---|
| 只求值 Lisp 表达式 / 注册桥接函数 | `ystyle::xisp` |
| 再加交互式 REPL | `+ ystyle::xisp_repl`（自动带上 `xisp_terminal`） |
| 再加字节码 / JIT 加速 | `+ ystyle::xisp_bc` |
| 只要终端能力（与 Lisp 无关） | `ystyle::xisp_terminal` 单独可用 |

```toml
[dependencies]
  "ystyle::xisp" = "0.5.0"
  "ystyle::xisp_repl" = "0.5.0"   # 需要 REPL 时
  "ystyle::xisp_bc" = "0.5.0"     # 需要字节码 / JIT 时
```

::: tip 核心是零依赖的
`ystyle::xisp` 不依赖任何第三方包，也不含任何字节码 / JIT 代码。
**不依赖 `xisp_bc` 的宿主，产物里就没有字节码与 JIT**——JIT 的可写可执行内存（`rwx`）
代码路径也就不会出现在那个进程里。
:::

---

## 字节码与 JIT 是可选的

编译器、寄存器栈 VM 与 JIT 都在 `ystyle::xisp_bc`，通过核心定义的 `ExecutionBackend` 接缝注入：

```cangjie
import ystyle::xisp.*
import ystyle::xisp_bc.*

// 只启用字节码（不支持的表达式自动回退 AST）
let interp = LispInterpreter([withBytecodeCompiler()])

// 字节码 + JIT（JIT 叠在字节码之上；仅 Linux x86-64，其它平台自动降级）
let fast = LispInterpreter([withBytecodeCompiler(), withJit()])

// 或者手动注入后端
interp.setExecutionBackend(BytecodeExecutionBackend())
```

不注入后端时**全程 AST 解释**。选项细节见 [选项系统](options)，
后端接缝与 IR 的归属见仓库内 `docs/plans/2026-09-27-workspace-modularization.md` §3.2。

---

## 从源码构建

仓库根即工作区根：

```bash
cjpm build            # 构建全部成员
cjpm test             # 运行全部成员的测试
cjpm build -m core    # 只构建核心（-m 指定成员）
cjpm ws check         # 版本一致性闸门（多包产品共用版本号）
```

产物：

```
target/release/bin/ystyle::xisp_cli                    # 命令行入口
target/release/bin/ystyle::xisp_examples.<示例名>       # 仓颉侧示例
```
