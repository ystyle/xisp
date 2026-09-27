# ystyle::xisp_bc

星枢字节码执行后端（**可选依赖**）：字节码编译器、寄存器栈 VM、JIT。

- **依赖**：`ystyle::xisp`（核心：IR / 值类型 / 求值器接缝）
- **接缝**：实现核心的 `ExecutionBackend`（位于 `ystyle::xisp.core.eval`），
  核心在求值入口按需调用；**核心不依赖本模块**——不依赖本模块的宿主不会引入任何字节码 / JIT 代码
- **选项**：`withBytecodeCompiler()`、`withJit()`（原本位于核心的 `options.cj`）
- **JIT 平台**：仅 Linux x86-64（`@When`），其余平台为 stub，`withJit()` 自动降级为字节码

## 与核心的分工

| 层 | 位置 | 内容 |
|---|---|---|
| IR（协议） | 核心 `ystyle::xisp.core.eval` | `CompiledFunction` / `CompiledCode` / `OP_*` —— 核心与本模块之间的约定 |
| 后端（本模块） | `ystyle::xisp_bc` | 编译器、寄存器栈 VM、JIT 代码生成与运行时 |
| 前端语义 | 核心 | 特殊形式 / 宏 / 模块系统（编译器的输入与 AST 回退路径） |

## 演进说明

本模块当前是 **xisp 特化**的（VM 与编译器直接使用 `LispValue`）。
若要演化为可给其它语言复用的通用 VM 基座，需要两件事：

1. IR 泛型化（`FnUnit<T>` / 常量池为 `T`）；
2. 把 VM 的指令语义（139 条指令的值操作、约 470 行独立值语义方法）外置为宿主接口，
   `BytecodeVM` 只保留取指—译码—分派与帧管理。

设计记录见 `docs/plans/2026-08-25-bytecode-fw-design.md` 与
`docs/plans/2026-09-27-workspace-modularization.md` §3.2。
