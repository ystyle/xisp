# Xisp 文档站点重构设计

日期: 2026-08-02
分支: feat/docs-site

## 背景

当前 `docs/` 是混合目录，同时包含：
- **用户文档**：`syntax/`（01-basics/02-modern/03-macros）、`integration/`、`repl-guide.md`
- **agent/开发文档**：`design.md`、`core.md`、`plans/`、`bugs/`、`module-source-design.md`

且文档与实现存在多处偏差（详见下）。目标：复用 refine 项目的 VitePress 模式，
将用户文档重构为静态站点，agent/开发文档与用户文档物理分离。

## 方案

复用 refine 的 VitePress 目录模式：

| 目录 | 用途 | 对应 refine |
|------|------|------------|
| `docs-site/` | VitePress 站点源码（用户文档） | `refine/docs-site/` |
| `docs-site/.vitepress/config.mjs` | 导航/侧边栏/搜索配置 | 同 |
| `docs-site/package.json` | docs:dev / docs:build / docs:preview | 同 |
| `docs/dist` | 构建产物（outDir: ../docs/dist） | `refine/docs/dist/` |
| `docs/` | agent/开发文档（保留，移除用户文档部分） | `refine/docs/` |

构建命令：
```bash
cd docs-site
npm run docs:dev      # 本地预览 http://localhost:5173
npm run docs:build    # 构建到 ../docs/dist
npm run docs:preview  # 预览构建产物
```

## 站点结构（docs-site/）

```
docs-site/
  package.json
  .vitepress/config.mjs
  index.md                        # 首页（hero + features）
  guide/                          # 语法指南
    01-quickstart.md              # 快速开始
    02-basics.md                  # 基础语法
    03-modern.md                  # 现代语法
    04-macros.md                  # 宏系统
    05-modules.md                 # 模块系统
    06-unicode.md                 # Unicode/中文支持
  api/                            # 内置函数参考
    index.md
    arithmetic.md                 # 算术/比较
    list.md                       # 列表操作
    hashmap.md                    # 哈希映射
    predicates.md                 # 谓词
    logic.md                      # 逻辑
    string.md                     # 字符串
    control.md                    # 流程控制
  integration/                    # 嵌入集成
    embedding.md                  # LispInterpreter
    options.md                    # 选项系统
    sandbox.md                    # 沙箱
    bridge.md                     # 桥接
```

## 文档与实现差异修正清单（重构时同步修正）

基于实测验证（`/tmp/opencode/doc_check*.lisp`）：

1. **`when`/`unless` 宏**：实现仅支持单表达式，else 分支用 `0` 非 `nil`。
   文档声称支持多表达式 body + 返回 nil。→ 修复实现（见任务）
2. **`member`**：core.md 声称存在，实际未注册。→ 从文档移除或补实现
3. **`#{...}` 集合**：02-modern 声称是 HashSet 且 `contains?` 可用。
   实际解析为普通列表，无 HashSet 类型。→ 修正文档
4. **`contains?`**：仅支持 hashmap（Redis 风格），不支持集合。→ 修正文档
5. **字符串插值**：实际语法是 `#"Hello #{name}"`；design.md 写的 `#"Hello {name}"` 错误。→ 修正
6. **布尔值输出**：实际 `true/false`，部分文档写 `#t/#f`。→ 统一为实际输出
7. **match 守卫语法**：`(x when guard)` 单元素模式与 `((x y) when ...)` 列表模式。→ 修正
8. **`round`**：`(round 3.14159 2)` → `3.140000`（保留浮点格式）。→ 修正示例
9. **`hashmap` 输出格式**：`{:name "张三"}` 显示为 `{name "张三"}`（无冒号）。→ 修正示例
10. **`try/catch`**：core.md 声称支持，实际 `try` 未定义。→ 从文档移除
11. **`spawn`/Future**：design.md 标记未实现，task.md 也未完成。→ 明确标注未实现
12. **性能优化阶段**：design.md 阶段4 标"待实现"，实际已大幅优化。→ 更新状态
13. **`(n) when (number? n)` 描述值匹配**：实测返回 nil，语法不成立。→ 修正/移除

## 实施步骤

1. 创建 `docs-site/`，复制 refine 的 package.json / config.mjs 骨架
2. 修复 `when`/`unless` 宏实现（多表达式 + nil）
3. 编写 guide/（重构现有 01/02/03 + 修正差异）
4. 编写 api/（从 appendix-std-symbols.md 提炼）
5. 编写 integration/（复用现有 integration/*.md）
6. 更新 `docs/`：移除用户文档部分，保留 agent 文档
7. `npm run docs:build` 验证构建通过
8. 全量测试 + benchmark + 更新 task.md

## 验证

- `npm run docs:build` 构建成功，无死链
- 所有 Lisp 示例经实际运行验证（doc_check*.lisp）
- 318 单元测试全过
