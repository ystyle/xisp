# 星枢

一个仓颉嵌入式 Lisp 脚本语言

## 设计文档
- 设计文档在 `docs/design.md`
- 核心必要功能列表在 `docs/core.md`

## Git 工作流

采用 **Feature Branch + Squash Merge** 流程：

```shell
# 1. 从 master 切出新功能分支
git checkout master
git pull --rebase
git checkout -b feat/功能名

# 2. 在分支上开发，多次提交
git add . && git commit -m "feat(xxx): ..."

# 3. 开发完成后，切回 master 合并（squash）
git checkout master
git merge --squash feat/功能名
git commit -m "feat(xxx): 功能描述"

# 4. 删除已合并的分支
git branch -D feat/功能名
```

**原则**：
- `master` 分支始终可编译、可发布
- 功能分支 squash 后只有 1 个提交到 master
- 提交信息遵循 Conventional Commits

## 仓颉语言

- 在仓颉 api 和手册使用 `cangjie_docs` 工具查找，不要猜 api 和语法
- match 的 case 后不能接 `{}`，case 后直接写多行表达式

## 任务指南

- **不要考虑时间，不要简化算法，不要简化测试，按最好的来搞**
- 完成任务后更新 `task.md`
- 实现功能后把总结记录到 `cangjie-mem` 项目级记忆里
- 新功能、新特性必须写单元测试
- 以仓颉单元测试为主，测试发现的新问题要添加用例到仓颉单元测试里
- 提交前必须确保所有测试通过，最好执行 lint/typecheck

## 常用命令

```shell
# 编译
cjpm build

# 运行
./target/release/bin/ystyle::xisp_cli

# 测试
cjpm test
cjpm test --show-all-output --filter 'ParserTest.*'

# 清理
cjpm clean
```

## 项目结构

cjpm 工作区（`[workspace]`）：每个成员是独立发布的包。模块划分与依赖方向见
`docs/plans/2026-09-27-workspace-modularization.md`（已实施并随 0.5.0 发布）；
运行期能力装配与扩展接缝见 `docs/plans/2026-10-11-capability-domains-design.md`。

```
.
├── cjpm.toml        # [workspace] 成员清单
├── workspace.toml   # cjpm-ws 产品清单（统一版本 / 发布范围）
├── core/            # 模块 ystyle::xisp —— 解释器 / 内建 / 模块系统 / 桥接（零外部依赖）
│   └── src/{types,parser,core,bridge}/ + interpreter.cj + options.cj
├── xisp_bc/         # 模块 ystyle::xisp_bc —— 字节码编译器 / 寄存器栈 VM / JIT（可选后端）
├── terminal/        # 模块 ystyle::xisp_terminal —— 终端能力（零 Lisp 依赖）
├── repl/            # 模块 ystyle::xisp_repl —— REPL；会话入口 runRepl / runSimpleRepl
├── cli/             # 模块 ystyle::xisp_cli —— 命令行入口（发布）
├── xisp_examples/   # 模块 ystyle::xisp_examples —— 仓颉示例（不发布）
├── xisp_tests/      # 模块 ystyle::xisp_tests —— 契约 / 集成测试（不发布）
├── docs/            # 设计文档、语法手册、集成指南
├── docs-site/       # VitePress 用户文档
├── examples/        # Lisp 示例文件（.lisp）
├── lisp-tests/      # Lisp 集成测试
├── task.md          # 任务进度管理
├── AGENTS.md        # 本文件
└── CLAUDE.md        # 旧版（已废弃）
```

**测试归属**：白盒测试与被测包同目录（可访问该包 `internal`）；
只用 public API 的契约测试与跨模块集成测试放 `xisp_tests/`。

**多包命令**：版本一致性闸门 `cjpm ws check`（workspace 内的本地依赖不校验版本号）；
单个成员构建 `cjpm build -m <member>`。
