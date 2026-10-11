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

工作区结构与模块划分见仓库根 `README.md` 与 `docs/plans/2026-09-27-workspace-modularization.md`；
能力域装配与扩展接缝见 `docs/plans/2026-10-11-capability-domains-design.md`。
