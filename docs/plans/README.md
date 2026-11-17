# 设计计划索引

> `docs/plans/` 是**历史记录**：写的是当时的决定与勘察，不随实现回改正文。
> 但「这个设计现在处于什么阶段」是元信息，会漂——所以每份文档开头都有状态行，
> 老文档额外加了 `【现状（YYYY-MM-DD 标注）】`，本页按状态汇总。
>
> **改代码时若发现某行状态过时，顺手改状态行即可，不要去改正文里的历史决策。**

---

## 已实施并发布（当前代码就是它）

| 文档 | 内容 | 落地形态 |
|---|---|---|
| [2026-11-17-json-design.md](2026-11-17-json-design.md) | JSON 能力（取路径 + 解析 + 序列化） | 模块 `ystyle::xisp_json`（0.7.0 起，序列化 0.8.0 起） |
| [2026-11-17-async-design.md](2026-11-17-async-design.md) | 并发：`spawn` + `Future` | 核心内置（0.7.0 起，零外部依赖） |
| [2026-11-17-net-design.md](2026-11-17-net-design.md) | HTTP 客户端 | 模块 `ystyle::xisp_net`（0.7.0 起，需 stdx） |
| [2026-11-17-httpd-design.md](2026-11-17-httpd-design.md) | HTTP 服务端 | 模块 `ystyle::xisp_httpd`（0.8.0 起，基于 tachyon，零 stdx） |
| [2026-10-11-capability-domains-design.md](2026-10-11-capability-domains-design.md) | 能力域分层与扩展接缝 | 核心（0.6.0 起）：`LispCapability` / `External` + 5 个能力接口 |
| [2026-09-27-workspace-modularization.md](2026-09-27-workspace-modularization.md) | 工作区化（模块划分 / lockstep / 制品范围） | 10 个 cjpm 成员（0.5.0 起） |
| [2026-08-25-jit-engineering.md](2026-08-25-jit-engineering.md) | JIT 正式工程（字节码 → 机器码） | `xisp_bc/src/jit_*.cj`；fib 基准 AST 21779ms / BC 3785ms / **JIT 40ms** |
| [2026-08-02-perf-detail-design.md](2026-08-02-perf-detail-design.md) | 求值器细节优化（match / 别名 / 守卫） | 核心求值器 |
| [2026-08-02-docs-site-design.md](2026-08-02-docs-site-design.md) | 文档站点重构 | `docs-site/`（VitePress，GitHub Pages 部署） |
| [2026-07-29-vm-optimization-design.md](2026-07-29-vm-optimization-design.md) | VM 架构优化（槽位索引 + 寄存器机） | `xisp_bc` 的 `BytecodeVM` |
| [2026-07-29-bytecode-compiler-design.md](2026-07-29-bytecode-compiler-design.md) | 字节码编译器 | 模块 `ystyle::xisp_bc`（0.3.0 起） |
| [2026-07-29-architecture-refactoring-design.md](2026-07-29-architecture-refactoring-design.md) | 架构重构与字节码路线图 | 路线图已走完（工作区化 → 后端拆分 → 全语法编译 → JIT） |
| [2026-07-17-repl-enhancement-design.md](2026-07-17-repl-enhancement-design.md) | REPL 增强 | 模块 `ystyle::xisp_repl` |
| [2026-07-07-loader-design.md](2026-07-07-loader-design.md) | Loader 接口 | `core/src/core/module/`（含 `moduleLoadChecker` 权限钩子） |

---

## 方案已定，待实施

| 文档 | 内容 | 卡在哪 |
|---|---|---|
| [2026-08-25-bytecode-fw-design.md](2026-08-25-bytecode-fw-design.md) | Bytecode-Kit：把字节码基座拆出来，让别的语言也能复用 | **方案 A 已定**（拆 `bytecode_kit`），方案 B / C 不做。未开工 |

---

## 规划中（只固化了约束，不是开工计划）

| 文档 | 内容 | 前置 |
|---|---|---|
| [2026-10-11-sci-family-design.md](2026-10-11-sci-family-design.md) | 科学计算族（`xisp_array` / `io` / `linalg` / `stats` / `signal` / `sci`） | ① `External` 接缝 **已落地**（0.6.0）；② 真实用法与性能目标 **缺**——决定要不要为数组做编译器特化 |

> **仓库划分仍标为暂缓**（§4 的三个方案）。倾向：独立仓库 + 独立 workspace（`xisp-sci`），
> 理由是 C 依赖（LAPACK / FFTW）不该渗透进语言本体的发布流程。

---

## 约定

1. **正文不回改** —— 历史上想错的地方、被实测推翻的假设都留着，它们本身是信息；
2. **状态行要维护** —— 实施完就改开头那几行，别让它停在"待评审"；
3. **新计划命名** `YYYY-MM-DD-主题-design.md`，并在本页加一行；
4. 实施记录写在文档末尾的「实施记录」节（订正了什么、与设计稿的差异、验证数据），
   而不是散在提交信息里。
