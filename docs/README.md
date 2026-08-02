# Xisp 文档

星枢（Xisp）文档分为两部分：**用户文档**（`docs-site/`，VitePress 站点）和**开发/设计文档**（本目录）。

## 📖 用户文档（docs-site/）

面向开发者的完整文档站点，部署在 GitHub Pages：

```bash
cd docs-site
npm run docs:dev      # 本地预览 http://localhost:5173
npm run docs:build    # 构建到 ../docs/dist
```

| 章节 | 内容 |
|------|------|
| [快速开始](../docs-site/guide/01-quickstart) | 5 分钟上手 |
| [基础语法](../docs-site/guide/02-basics) | 数据类型、特殊形式、内置函数 |
| [现代语法](../docs-site/guide/03-modern) | 字面量、解构、管道、模式匹配 |
| [宏系统](../docs-site/guide/04-macros) | 元编程和代码生成 |
| [模块系统](../docs-site/guide/05-modules) | 代码组织和命名空间 |
| [Unicode 支持](../docs-site/guide/06-unicode) | 中文关键字 |
| [API 参考](../docs-site/api/) | 全部内置函数 |
| [集成指南](../docs-site/integration/) | 嵌入、选项、沙箱、桥接 |

---

## 🔧 开发/设计文档（本目录）

面向维护者和贡献者：

- [核心功能设计](core.md) - 数据类型、求值器、内置函数
- [架构设计](design.md) - 分层架构、语法规范、路线图
- [模块来源设计](module-source-design.md) - 模块系统扩展性设计
- [设计计划](plans/) - 功能设计与实现计划
- [Bug 调查](bugs/) - 问题排查记录
