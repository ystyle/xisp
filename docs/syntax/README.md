# Xisp 语法权威指南

欢迎使用 Xisp 语法权威指南！这是 Xisp 语言的官方语法文档，所有语法特性均以本文档为准。

---

## 📚 文档结构

本指南分为三个部分，按学习路径组织：

### 1️⃣ [基础语法](01-basics.md) **[30分钟]**

**目标读者**: Lisp 初学者

**内容概览**:
- 数据类型（原子、列表、向量、哈希映射）
- 特殊形式（define、lambda、if、quote、begin、set!）
- 基础函数（算术、比较、列表操作、谓词）
- 函数定义与应用（高阶函数、apply）
- 条件判断（if、嵌套 if、逻辑组合）
- 作用域与绑定（let、let*）
- 传统语法兼容

**学习目标**:
- ✅ 掌握 Xisp 核心语法
- ✅ 能够编写基础 Lisp 程序
- ✅ 理解词法作用域和闭包

---

### 2️⃣ [现代语法特性](02-modern.md) **[40分钟]**

**目标读者**: 需要编写现代化代码的开发者

**前置知识**: [基础语法](01-basics.md)

**内容概览**:
- 别名系统（first/rest、begin 等）
- 字面量语法（向量、哈希映射、哈希集合）
- 字符串插值（`#{}` 语法）
- 解构绑定（向量解构、嵌套解构）
- 管道操作符（`->` thread-first）
- 模式匹配（match 表达式、守卫条件）
- 综合示例（数据处理、配置解析、状态管理）

**学习目标**:
- ✅ 掌握现代化编程风格
- ✅ 编写简洁、易读的代码
- ✅ 使用字面量语法和数据结构

---

### 3️⃣ [宏系统](03-macros.md) **[50分钟]**

**目标读者**: 需要元编程的开发者

**前置知识**: [基础语法](01-basics.md)、[现代语法特性](02-modern.md)

**内容概览**:
- 宏基础（什么是宏、宏 vs 函数）
- 反引号语法（`` ` ``, `,`, `,@`）
- 宏展开（macroexpand、macroexpand-all）
- 内置宏（when、unless、let*、if-let、when-let*、condb 等）
- 高级宏示例（自定义控制结构、DSL）
- 最佳实践（避免变量捕获、保持简单）
- 实现原理（语法转换、求值过程）

**学习目标**:
- ✅ 理解宏的原理和用途
- ✅ 能够编写自己的宏
- ✅ 掌握元编程技巧

---

## 🚀 快速开始

### 5 分钟快速体验

```lisp
; 1. 定义变量和函数
(define name "Xisp")
(define (square x) (* x x))

; 2. 使用现代语法
(define numbers [1 2 3 4 5])

; 3. 使用管道操作符
(-> numbers
    (map square)
    (filter even?)
    length)
; => 2

; 4. 使用字符串插值
(println #"Hello, #{name}!")
; => Hello, Xisp!
```

### 学习路径

```
┌──────────────────────────────────────────────┐
│         Xisp 学习路径（约2小时）               │
├──────────────────────────────────────────────┤
│                                              │
│  🟢 初级 (30分钟)                             │
│  └─ 01-basics.md                             │
│     数据类型、特殊形式、基础函数               │
│                                              │
│  🟡 中级 (40分钟)                             │
│  └─ 02-modern.md                             │
│     别名、字面量、解构、管道、模式匹配          │
│                                              │
│  🔴 高级 (50分钟)                             │
│  └─ 03-macros.md                             │
│     宏系统、元编程、代码生成                   │
│                                              │
└──────────────────────────────────────────────┘
```

---

## 💡 设计原则

### 现代优先

Xisp 主推现代化语法，代码更简洁、易读：

```lisp
; ✅ 推荐：现代语法
(first lst)
(rest lst)

; ⚠️ 兼容：传统语法
(car lst)
(cdr lst)
```

### 向后兼容

完全支持传统 Lisp 语法，与其他 Lisp 方言互通：

```lisp
; Common Lisp 风格
(defun square (x)
  (* x x))

; Xisp 风格（等价）
(define (square x)
  (* x x))
```

### 实用主义

语法设计以实用性为导向：

```lisp
; 简洁的解构
(let [[x y & rest] data]
  (process x y rest))

; 清晰的管道
(-> data
    validate
    transform
    save)
```

---

## 📖 其他资源

### 示例代码

- [基础教程](../../examples/01-basics/) - 5分钟快速体验、完整教程
- [中级特性](../../examples/02-intermediate/) - 模式匹配、解构、守卫条件
- [高级特性](../../examples/03-advanced/) - 闭包、高阶函数、管道操作符
- [宏系统](../../examples/04-macros/) - 简单宏、基础宏、高级宏

### 设计文档

- [核心功能](../core.md) - 核心功能列表
- [设计文档](../design.md) - 详细设计和架构
- [模块系统](../modules.md) - 模块系统文档

### 集成文档

- [Bridge 桥接](../integration/bridge.md) - 仓颉互操作
- [选项系统](../integration/options_system.md) - 选项配置
- [集成指南](../integration/) - 完整集成文档

---

## 🎯 语法规范

### 命名约定

- **变量和函数**: 使用连字符（kebab-case）
  ```lisp
  (define my-var 10)
  (define (my-function x) ...)
  ```

- **常量**: 使用全部大写
  ```lisp
  (define MAX-SIZE 100)
  ```

- **谓词函数**: 以 `?` 结尾
  ```lisp
  (number? x)
  (string? s)
  (list? lst)
  ```

- **转换函数**: 以 `->` 分隔
  ```lisp
  (string->number "123")
  (number->string 123)
  ```

### 代码风格

```lisp
; ✅ 好的风格
(define (process-data data)
  (-> data
      validate
      transform
      save))

; ❌ 避免
(define (process-data data)
  (save (transform (validate data))))
```

---

## 🔧 工具支持

### REPL

```bash
# 启动 REPL
./target/release/bin/ystyle::xisp.cli

# 在 REPL 中
xisp> (define x 10)
xisp> (square x)
xisp> (macroexpand '(when test then))
```

### 运行文件

```bash
# 运行单个文件
./target/release/bin/ystyle::xisp.cli examples/01-basics/01_quick_start.lisp

# 在 REPL 中导入文件
xisp> (import "examples/01-basics/02_tutorial.lisp")
```

---

## 📝 文档版本

- **当前版本**: 1.0.0
- **最后更新**: 2026-01-24
- **维护者**: Xisp Team
- **许可证**: 与 Xisp 项目相同

---

## 🤝 贡献

发现文档问题或想要改进？

- 提交 Issue
- 发起 Pull Request
- 在讨论区交流

---

## 🎉 开始学习

选择你的起点：

- 🟢 **初学者**: 从 [基础语法](01-basics.md) 开始
- 🟡 **有经验**: 直接学习 [现代语法特性](02-modern.md)
- 🔴 **高级用户**: 深入 [宏系统](03-macros.md)

**祝你学习愉快！**
