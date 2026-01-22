# Xisp 文档

星枢（Xisp）完整技术文档

## 📖 文档导航

### 语法文档

Lisp 语法、语义和现代化特性：

- [Lisp 核心功能](syntax/core.md) - 数据类型、求值器、内置函数
- [设计文档](syntax/design.md) - 架构设计和技术选型
- [现代化语法](syntax/modern_syntax.md) - 向量、哈希、插值等
- [解构绑定](syntax/destructuring.md) - 模式匹配和解构
- [管道操作符](syntax/pipeline.md) - -> 线程宏

**快速入门**: 查看 [examples/README.md](../examples/README.md) 获取 Lisp 脚本示例

---

### 集成文档

在仓颉项目中嵌入和使用 Xisp：

- [桥接层 API](integration/bridge.md) - Lisp 与仓颉互操作
- [选项系统](integration/options_system.md) - 解释器配置选项
- [沙箱系统](integration/sandbox.md) - 安全执行环境和权限控制

**快速入门**: 查看 [src/examples/](../src/examples/) 获取仓颉示例代码

---

### Unicode 支持

多语言编程和国际化支持：

- [中文支持详细文档](unicode/chinese_support.md) - 完整的中文关键字说明
- [中文快速开始](unicode/chinese_quickstart.md) - 中文编程入门

**相关文档**: [Unicode 支持概述](../UNICODE_SUPPORT.md)

---

## 🚀 快速开始指南

### Lisp 脚本开发者

1. 阅读 [Lisp 核心功能](syntax/core.md) 了解基本语法
2. 查看 [examples/](../examples/) 运行示例脚本
3. 参考 [现代化语法](syntax/modern_syntax.md) 学习高级特性

### 仓颉开发者

1. 阅读 [桥接层 API](integration/bridge.md) 了解互操作
2. 查看 [src/examples/](../src/examples/) 运行示例代码
3. 参考 [选项系统](integration/options_system.md) 配置解释器

### 国际化开发者

1. 阅读 [中文快速开始](unicode/chinese_quickstart.md)
2. 查看 [examples/chinese_demo.lisp](../examples/chinese_demo.lisp)
3. 参考 [中文支持详细文档](unicode/chinese_support.md)

---

## 📚 文档分类

### 按主题分类

| 主题 | 文档 | 说明 |
|------|------|------|
| 核心语法 | [syntax/core.md](syntax/core.md) | 数据类型、求值器、特殊形式 |
| 现代化特性 | [syntax/modern_syntax.md](syntax/modern_syntax.md) | 向量、哈希、插值、解构、管道 |
| 解构绑定 | [syntax/destructuring.md](syntax/destructuring.md) | let 模式匹配详解 |
| 管道操作 | [syntax/pipeline.md](syntax/pipeline.md) | -> 线程宏详解 |
| 桥接层 | [integration/bridge.md](integration/bridge.md) | Lisp 与仓颉互操作 API |
| 选项系统 | [integration/options_system.md](integration/options_system.md) | 解释器配置和选项 |
| 沙箱安全 | [integration/sandbox.md](integration/sandbox.md) | 安全执行环境 |
| 中文支持 | [unicode/chinese_support.md](unicode/chinese_support.md) | 中文关键字和多语言 |
| 中文入门 | [unicode/chinese_quickstart.md](unicode/chinese_quickstart.md) | 中文编程快速入门 |
| 架构设计 | [syntax/design.md](syntax/design.md) | 技术选型和设计思路 |

### 按使用场景分类

#### REPL 交互式编程
- [Lisp 核心功能](syntax/core.md) - 基础语法和内置函数
- [现代化语法](syntax/modern_syntax.md) - 现代化语法特性
- [中文快速开始](unicode/chinese_quickstart.md) - 中文编程入门
- [examples/](../examples/) - Lisp 脚本示例

#### 仓颉嵌入式集成
- [桥接层 API](integration/bridge.md) - 互操作接口
- [选项系统](integration/options_system.md) - 配置选项
- [沙箱系统](integration/sandbox.md) - 安全执行
- [src/examples/](../src/examples/) - 仓颉示例代码

#### 高级特性
- [解构绑定](syntax/destructuring.md) - 模式匹配
- [管道操作符](syntax/pipeline.md) - 函数组合
- [设计文档](syntax/design.md) - 架构设计

---

## 🔗 相关链接

- [项目 README](../README.md) - 项目概述
- [examples/README.md](../examples/README.md) - Lisp 脚本示例
- [src/examples/](../src/examples/) - 仓颉示例代码
- [task.md](../task.md) - 开发任务和路线图

---

**版本**: 0.1.0 MVP
**最后更新**: 2026-01-22
