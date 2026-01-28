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
- 模块间依赖和符号绑定

## 🚀 运行示例

```bash
# 基础演示（需先设置搜索路径）
XISP_PATH=./examples/05-modules/modules_demo ./target/release/bin/ystyle::xisp.cli examples/05-modules/01_basic_demo.lisp

# 完整示例
XISP_PATH=./examples/05-modules/modules_demo ./target/release/bin/ystyle::xisp.cli examples/05-modules/modules_demo/demo.lisp
```

## 📖 模块系统基础

### 导入模块

```lisp
(import pkg1)             ; 按模块名导入（通过搜索路径查找）
(import ystyle::log)      ; 带组织名的模块
```

### 导出符号

```lisp
(export symbol1 symbol2)
```

### 模块间引用

模块内部使用模块名引用其他模块，不支持文件路径导入：

```lisp
;; pkg2/main.lisp
(import pkg1)             ; ✅ 通过模块名导入
(pkg1.add 1 2)

(import "./utils.lisp")   ; ❌ 模块内部不支持文件路径导入
```

REPL 或 CLI 脚本中可以使用文件导入：

```lisp
(import "./utils.lisp")   ; ✅ 仅 REPL/CLI 脚本可用
```

## 💡 最佳实践

1. **按功能组织模块**：每个模块一个文件
2. **明确导出接口**：只导出必要的符号
3. **模块内引用用模块名**：不使用文件路径
4. **避免循环依赖**：保持模块图清晰

---

*模块系统让你的代码井井有条！*
