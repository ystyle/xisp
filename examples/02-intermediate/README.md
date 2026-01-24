# 02 - 中级特性

恭喜你完成了基础教程！现在让我们学习 Xisp 的现代化语法特性。

## 📚 文件列表

| 文件 | 主题 | 难度 | 预计时间 |
|------|------|------|----------|
| [01_pattern_matching.lisp](./01_pattern_matching.lisp) | 模式匹配 | 🟡 中级 | 15 分钟 |
| [02_destructuring.lisp](./02_destructuring.lisp) | 解构绑定 | 🟡 中级 | 10 分钟 |
| [03_guard_clauses.lisp](./03_guard_clauses.lisp) | 守卫条件 | 🟡 中级 | 10 分钟 |
| [04_modern_syntax.lisp](./04_modern_syntax.lisp) | 现代语法集合 | 🟡 中级 | 20 分钟 |

## 🎯 学习目标

完成本部分后，你将掌握：

- ✅ 模式匹配的强大功能
- ✅ 解构绑定简化代码
- ✅ 守卫条件增强控制流
- ✅ 现代化语法提升开发效率

## 📖 各文件说明

### 01_pattern_matching.lisp - 模式匹配

**内容**：
- 基础模式匹配
- 列表模式匹配
- Rest 参数匹配
- 守卫条件
- 嵌套模式匹配

**为什么重要**：模式匹配是现代 Lisp 的核心特性，让代码更清晰、更安全

**运行**：
```bash
./xisp examples/02-intermediate/01_pattern_matching.lisp
```

### 02_destructuring.lisp - 解构绑定

**内容**：
- 列表解构
- 向量解构
- 嵌套解构
- Rest 参数解构
- 实际应用场景

**为什么重要**：解构绑定让你从复杂数据结构中提取值，代码更简洁

**运行**：
```bash
./xisp examples/02-intermediate/02_destructuring.lisp
```

### 03_guard_clauses.lisp - 守卫条件

**内容**：
- 守卫条件基础
- 多守卫组合
- 守卫与模式匹配结合
- 错误处理模式

**为什么重要**：守卫条件提供优雅的错误处理和数据验证方式

**运行**：
```bash
./xisp examples/02-intermediate/03_guard_clauses.lisp
```

### 04_modern_syntax.lisp - 现代语法集合

**内容**：
- 字面量语法（向量、哈希映射、哈希集合）
- 字符串插值
- 解构绑定
- 管道操作符
- 实际应用示例

**为什么重要**：这些现代特性让代码更接近现代语言，提升开发体验

**运行**：
```bash
./xisp examples/02-intermediate/04_modern_syntax.lisp
```

## 💡 学习建议

### 推荐学习顺序

1. **先学模式匹配**（01_pattern_matching.lisp）
   - 这是最重要的特性，其他特性都会用到

2. **再学解构绑定**（02_destructuring.lisp）
   - 与模式匹配配合使用

3. **然后学守卫条件**（03_guard_clauses.lisp）
   - 增强模式匹配的能力

4. **最后综合应用**（04_modern_syntax.lisp）
   - 将所有特性整合使用

### 学习技巧

- ✅ 在 REPL 中逐步尝试每个示例
- ✅ 修改示例代码，观察结果变化
- ✅ 尝试组合多个特性
- ✅ 思考在实际项目中如何应用

## ⚡ 快速对比

| 特性 | 传统写法 | 现代写法 | 优势 |
|------|---------|---------|------|
| 模式匹配 | 嵌套 if/match | match 表达式 | 清晰、安全 |
| 解构 | (first x) (second x) | (match (list a b) ...) | 简洁 |
| 守卫 | 嵌套 if | match + when | 优雅 |
| 管道 | 嵌套函数调用 | (==> x f g) | 可读 |

## 📚 下一步

完成中级特性后，建议继续学习：

- → [03-advanced - 高级特性](../03-advanced/) （闭包、高阶函数）
- → [04-macros - 宏系统](../04-macros/) （元编程）

## ❓ 常见问题

### Q: 必须按顺序学习吗？
**A**: 建议按顺序，但如果你有其他 Lisp 方言经验，可以跳过熟悉的内容。

### Q: 这些特性是 Common Lisp 的吗？
**A**: 不完全是。Xisp 融合了 Clojure、Racket 等现代 Lisp 方言的特性。

### Q: 什么时候使用这些特性？
**A**:
- 模式匹配：处理复杂条件逻辑
- 解构：从数据结构提取值
- 守卫：数据验证和错误处理
- 现代语法：提升代码可读性

---

**🚀 让我们开始学习中级特性吧！**
