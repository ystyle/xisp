# 05 - 模块系统

学习如何组织大型 Lisp 项目。

## 📚 文件列表

| 文件/目录 | 主题 | 难度 | 预计时间 |
|---------|------|------|----------|
| [01_basic_demo.lisp](./01_basic_demo.lisp) | 基础模块演示 | 🟡 中级 | 10 分钟 |
| [modules_demo/](./modules_demo/) | 完整模块示例 | 🟡 中级 | 20 分钟 |

## 🎯 学习目标

- 理解模块系统
- 使用 import 导入模块
- 使用 export 导出符号
- 相对路径导入
- 管理项目结构

## 🚀 运行示例

```bash
# 基础演示
./xisp examples/05-modules/01_basic_demo.lisp

# 完整示例
./xisp examples/05-modules/modules_demo/demo.lisp
```

## 📖 模块系统基础

### 导入模块

```lisp
(import module-name)
```

### 导出符号

```lisp
(export symbol1 symbol2)
```

### 相对导入

```lisp
(import "./pkg2")
(import "../parent-pkg")
```

## 💡 最佳实践

1. **按功能组织模块**：每个模块一个文件
2. **明确导出接口**：只导出必要的符号
3. **使用相对导入**：便于项目重构
4. **避免循环依赖**：保持模块图清晰

---

*模块系统让你的代码井井有条！*
