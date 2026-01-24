# 04 - 宏系统

学习 Lisp 最强大的特性：元编程。

## 📚 文件列表

| 文件 | 主题 | 难度 | 预计时间 |
|------|------|------|----------|
| [01_macro_simple.lisp](./01_macro_simple.lisp) | 简单宏（无反引号） | 🟡 中级 | 15 分钟 |
| [02_macro_basics.lisp](./02_macro_basics.lisp) | 基础宏（反引号语法） | 🟡 中级 | 20 分钟 |
| [03_macro_advanced.lisp](./03_macro_advanced.lisp) | 高级宏特性 | 🔴 高级 | 30 分钟 |

## 🎯 学习路径

### 阶段 1：理解宏的工作原理
学习 `01_macro_simple.lisp`，了解：
- 宏与函数的区别
- 如何手工构造表达式
- 宏展开的过程

### 阶段 2：掌握现代宏语法
学习 `02_macro_basics.lisp`，掌握：
- 反引号（`` ` ``）语法
- 逗号（`,`）和逗号-at（`,@`）
- 常用宏模式（when, unless, cond, let*）

### 阶段 3：高级宏编程
学习 `03_macro_advanced.lisp`，深入：
- let* 和 if-let
- when-let* 和守卫条件
- condb 宏
- 宏的调试技巧

## 💡 为什么学习宏？

宏是 Lisp 的"超能力"：

1. **扩展语言**：添加新的语法结构
2. **消除重复**：自动生成样板代码
3. **DSL**：创建领域特定语言
4. **编译时计算**：在编译时执行代码

## 🚀 运行示例

```bash
# 简单宏
./xisp examples/04-macros/01_macro_simple.lisp

# 基础宏
./xisp examples/04-macros/02_macro_basics.lisp

# 高级宏
./xisp examples/04-macros/03_macro_advanced.lisp
```

## ⚠️ 学习建议

- ✅ 按顺序学习，不要跳过
- ✅ 使用 `macroexpand` 查看宏展开结果
- ✅ 在 REPL 中实验宏定义
- ✅ 先理解，再应用

---

*宏是 Lisp 的灵魂，掌握它你将拥有无与伦比的力量！*
