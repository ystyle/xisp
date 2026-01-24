# Xisp 示例代码集合

欢迎来到 Xisp 示例代码库！这里包含了从入门到实战的完整学习材料。

## 🚀 快速开始

### 5 分钟快速体验

```bash
./xisp examples/01-basics/01_quick_start.lisp
```

你将快速了解 Xisp 的核心特性：
- ✅ 基本算术运算
- ✅ 变量定义
- ✅ 列表操作
- ✅ 函数定义
- ✅ 高阶函数
- ✅ 模式匹配
- ✅ 现代语法

### 学习路线图

```
┌─────────────────────────────────────────────────────────────┐
│                    Xisp 学习路径                              │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  🟢 初级 (35分钟)                                            │
│  └─ 01-basics → 02-intermediate                              │
│                                                              │
│  🟡 有经验 (65分钟)                                           │
│  └─ 03-advanced → 04-macros → 05-modules                     │
│                                                              │
│  🔴 专家 (45分钟)                                             │
│  └─ 06-interop → 07-real-world                               │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## 📚 目录结构

### 01-basics - 基础教程 🟢

**适合人群**：Lisp 初学者
**学习时间**：35 分钟
**前置知识**：无

| 文件 | 主题 | 时间 |
|------|------|------|
| [01_quick_start.lisp](./01-basics/01_quick_start.lisp) | 5分钟快速体验 | 5分钟 |
| [02_tutorial.lisp](./01-basics/02_tutorial.lisp) | 完整教程（12主题） | 30分钟 |

**学习内容**：
- 基本算术运算
- 变量定义与作用域
- 列表操作
- 函数定义与调用
- 条件判断与递归
- 高阶函数入门

→ [详细说明](./01-basics/README.md)

---

### 02-intermediate - 中级特性 🟡

**适合人群**：掌握基础语法的开发者
**学习时间**：55 分钟
**前置知识**：完成 01-basics

| 文件 | 主题 | 时间 |
|------|------|------|
| [01_pattern_matching.lisp](./02-intermediate/01_pattern_matching.lisp) | 模式匹配 | 15分钟 |
| [02_destructuring.lisp](./02-intermediate/02_destructuring.lisp) | 解构绑定 | 10分钟 |
| [03_guard_clauses.lisp](./02-intermediate/03_guard_clauses.lisp) | 守卫条件 | 10分钟 |
| [04_modern_syntax.lisp](./02-intermediate/04_modern_syntax.lisp) | 现代语法集合 | 20分钟 |

**学习内容**：
- 强大的模式匹配系统
- 简洁的解构绑定
- 优雅的守卫条件
- 现代化语法特性

→ [详细说明](./02-intermediate/README.md)

---

### 03-advanced - 高级特性 🔴

**适合人群**：熟悉 Lisp 的开发者
**学习时间**：50 分钟
**前置知识**：完成 02-intermediate

| 文件 | 主题 | 时间 |
|------|------|------|
| 01_closures.lisp | 闭包与函数组合 | 20分钟 |
| 02_higher_order.lisp | 高阶函数深度 | 15分钟 |
| [03_thread_macro.lisp](./03-advanced/03_thread_macro.lisp) | 管道操作符 | 10分钟 |
| [04_apply_demo.lisp](./03-advanced/04_apply_demo.lisp) | Apply 函数 | 5分钟 |

**学习内容**：
- 闭包与词法作用域
- 函数组合与柯里化
- 管道式编程
- 元编程基础

→ [详细说明](./03-advanced/README.md)

---

### 04-macros - 宏系统 🔴

**适合人群**：想要深入理解 Lisp 的开发者
**学习时间**：65 分钟
**前置知识**：完成 03-advanced

| 文件 | 主题 | 时间 |
|------|------|------|
| [01_macro_simple.lisp](./04-macros/01_macro_simple.lisp) | 简单宏（无反引号） | 15分钟 |
| [02_macro_basics.lisp](./04-macros/02_macro_basics.lisp) | 基础宏（反引号语法） | 20分钟 |
| [03_macro_advanced.lisp](./04-macros/03_macro_advanced.lisp) | 高级宏特性 | 30分钟 |

**学习内容**：
- 宏与函数的区别
- 反引号和逗号语法
- 常用宏模式
- 宏的调试技巧

**为什么学习宏？**
宏是 Lisp 的"超能力"：
- 扩展语言语法
- 消除代码重复
- 创建 DSL
- 编译时计算

→ [详细说明](./04-macros/README.md)

---

### 05-modules - 模块系统 🟡

**适合人群**：需要组织大型项目的开发者
**学习时间**：30 分钟
**前置知识**：完成 02-intermediate

| 文件/目录 | 主题 | 时间 |
|-----------|------|------|
| [01_basic_demo.lisp](./05-modules/01_basic_demo.lisp) | 基础模块演示 | 10分钟 |
| [modules_demo/](./05-modules/modules_demo/) | 完整模块示例 | 20分钟 |

**学习内容**：
- 模块导入与导出
- 相对路径导入
- 项目结构组织
- 依赖管理

→ [详细说明](./05-modules/README.md)

---

### 06-interop - 互操作和桥接 🟡

**适合人群**：需要与仓颉生态集成的开发者
**学习时间**：25 分钟
**前置知识**：完成 01-basics

| 文件 | 主题 | 时间 |
|------|------|------|
| 01_cangjie_bridge.lisp | 仓颉标准库调用 | 15分钟 |
| 02_file_io.lisp | 文件 I/O | 10分钟 |

**学习内容**：
- 调用仓颉标准库
- 文件读写操作
- 目录遍历
- 错误处理

→ [详细说明](./06-interop/README.md)

---

### 07-real-world - 实际应用 🔴

**适合人群**：准备开发实际项目的开发者
**学习时间**：45 分钟
**前置知识**：完成 05-modules

| 目录/文件 | 主题 | 说明 |
|----------|------|------|
| [ystyle/](./07-real-world/ystyle/) | 第三方库 | zlog, utils |
| 01_logger.lisp | 日志使用示例 | 使用 zlog |
| 02_data_processing.lisp | 数据处理 | 真实场景 |

**应用场景**：
- 日志记录
- 数据处理
- 文件操作
- 脚本自动化

→ [详细说明](./07-real-world/README.md)

---

### legacy - 旧版示例 ⚠️

保留用于向后兼容和参考。

**注意**：这些示例可能使用过时语法，建议参考新分类目录。

→ [详细说明](./legacy/README.md)

---

## 🔧 运行方式

### 运行单个文件

```bash
./xisp examples/01-basics/01_quick_start.lisp
```

### REPL 交互式学习

```bash
./xisp
```

在 REPL 中加载文件：

```lisp
xisp> ,load examples/01-basics/02_tutorial.lisp
```

### 运行指定目录的所有示例

```bash
# 运行所有基础教程
for file in examples/01-basics/*.lisp; do
    ./xisp "$file"
done
```

---

## 💡 难度标识

- 🟢 **初级**：适合初学者，无前置知识要求
- 🟡 **中级**：需要掌握基础语法
- 🔴 **高级**：需要深入理解 Lisp 和函数式编程

---

## 📊 学习统计

| 类别 | 示例数量 | 总时长 |
|------|---------|--------|
| 基础教程 | 2 | 35分钟 |
| 中级特性 | 4 | 55分钟 |
| 高级特性 | 4 | 50分钟 |
| 宏系统 | 3 | 65分钟 |
| 模块系统 | 2 | 30分钟 |
| 互操作 | 2 | 25分钟 |
| 实际应用 | 3 | 45分钟 |
| **总计** | **20+** | **~5小时** |

---

## ❓ 常见问题

### Q: 完全没有 Lisp 经验，从哪里开始？

**A**: 从 `01-basics` 开始：
1. 先运行 `01_quick_start.lisp`（5分钟）
2. 再学习 `02_tutorial.lisp`（30分钟）
3. 然后进入 `02-intermediate`

### Q: 有其他 Lisp 经验，如何学习？

**A**:
- 如果熟悉 Common Lisp：直接看 `02-intermediate` 和 `04-macros`
- 如果熟悉 Clojure：重点关注 `03-advanced` 和 `07-real-world`
- 如果熟悉 Scheme：查看 `05-modules` 和 `06-interop`

### Q: 示例文件中的代码可以复制使用吗？

**A**: 当然可以！所有示例代码都是自由使用的。你可以：
- 直接复制到你的项目
- 修改和扩展
- 作为学习参考

### Q: 如何调试示例代码？

**A**:
1. 在 REPL 中逐步运行
2. 使用 `println` 查看中间结果
3. 使用 `,load` 加载文件
4. 参考错误提示修改

---

## 📖 更多资源

### 项目文档

- [项目 README](../README.md) - 项目概述
- [任务追踪](../task.md) - 开发计划
- [核心设计](../docs/core.md) - 核心功能说明
- [设计文档](../docs/design.md) - 详细设计
- [宏系统语法](../docs/syntax/macros.md) - 宏语法手册
- [模块系统](../docs/modules.md) - 模块系统文档

### 仓颉示例程序

仓颉示例程序位于 `src/examples/` 目录：

```bash
# 查看可用示例
ls src/examples/

# 编译
cjpm build

# 运行示例
./target/release/bin/ystyle::xisp.examples.match_demo
```

---

## 🐛 问题反馈

发现示例有问题？欢迎：

- 提交 Issue
- 发起 Pull Request
- 在讨论区交流

---

## 📝 更新日志

### v0.2.0 (2026-01-24)

- ✨ 重新组织示例目录结构
- ✨ 添加难度标识和学习时间
- ✨ 创建学习路线图
- ✨ 拆分大文件为主题分类
- ✨ 新增快速体验示例
- ✨ 改进 README 文档

### v0.1.0 (2026-01-23)

- 🎉 初始版本
- 📚 基础教程
- 📚 宏系统示例
- 📚 现代语法特性

---

**版本**: 0.2.0
**最后更新**: 2026-01-24
**维护者**: Xisp Team

---

**🎉 开始你的 Xisp 之旅吧！**
