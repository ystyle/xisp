# ystyle::xisp

星枢（Xisp）核心模块：嵌入式 Lisp 脚本语言的解释器、内建函数、模块系统与仓颉桥接。

- **包**：`ystyle::xisp`（root）、`ystyle::xisp.types` / `.parser` / `.core.eval` / `.core.builtin` / `.core.module` / `.bridge`
- **依赖**：无（零外部依赖）
- **宿主入口**：`LispInterpreter` + `InterpreterOption`
- **不含**：REPL / 终端控制 / CLI 入口 / 仓颉侧示例 / **字节码与 JIT** —— 各自独立成模块，宿主按需依赖
- **字节码接缝**：IR（`CompiledCode` / `CompiledFunction` / `OP_*`）与 `ExecutionBackend` 由核心定义，
  实现在 `ystyle::xisp_bc`；不注入后端时全程 AST 解释，核心不引入任何字节码代码

```cangjie
import ystyle::xisp.*

main(): Int64 {
    let interp = LispInterpreter([withStdIO()])
    println(interp.eval("(+ 1 2)"))
    0
}
```

## 扩展模块的中断义务（契约）

> 适用对象：**任何通过 `LispCapability` 安装能力域的模块**（含将来的 `xisp_array` /
> `xisp_linalg` 等）。core 提供机制，履行义务的是扩展方。

- 域在 `install` 时拿到的 `CapabilityContext` 带 `isInterrupted: () -> Bool`；
- **任何与元素数 / 迭代步数相关的循环，都必须周期性调用它**，返回 `true` 时立即
  中止并返回错误值（`LispValue.Error(XispError(RuntimeError, INTERRUPTED_MESSAGE))`
  或等价），**不要跑完再返回**；
- 为什么必须：宿主用它实现超时与协作式取消。一个跑 30 秒的矩阵分解若不查中断，
  宿主就**杀不掉**它（Lisp 侧没有可捕获的异常可打断）；
- 粒度建议：每 N 次迭代（N 取 1k~100k 量级）查一次，热路径不要逐次查；
- 中断以**值**表达、不是抛异常（与全仓错误模型一致），因此调用方拿到的是
  `Error`，宿主可据 `INTERRUPTED_MESSAGE` 识别。

core 自身已有 4 处检查点：求值入口、`NativeFunc` 调用前后、`map`/`filter`/`reduce`/
`for-each` 迭代体、字节码 VM 指令派发（`xisp_bc` 经 `setInterruptChecker` 接入）。
`withTimeout` / `withSandbox` 即建立在此之上。

---

工作区结构与模块划分见仓库根 `README.md` 与 `docs/plans/2026-09-27-workspace-modularization.md`；
能力域装配与扩展接缝见 `docs/plans/2026-10-11-capability-domains-design.md`。
