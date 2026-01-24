# 模块系统演示

本目录演示了 Xisp 模块系统的功能，包括相对路径导入。

## 目录结构

```
modules_demo/
├── module.lisp        # 主模块元数据
├── demo.lisp          # 顶层演示文件
├── pkg1/              # 包 1（工具函数）
│   ├── module.lisp    # 包元数据
│   └── utils.lisp     # 工具函数实现
└── pkg2/              # 包 2（使用 pkg1）
    ├── module.lisp    # 包元数据
    └── main.lisp      # 主文件（导入 pkg1）
```

## 相对路径导入语法

### 1. 当前目录导入

```lisp
(import "./local.module")
```

从当前文件所在目录导入 `local.module` 模块。

### 2. 上级目录导入

```lisp
(import "../parent.module")
```

从当前文件所在目录的上级目录导入 `parent.module` 模块。

### 3. 完整模块名导入

```lisp
(import pkg1)
(import pkg2)
```

使用完整的模块名导入模块（通过 XISP_PATH 搜索）。

## 工作原理

### 路径解析

1. **当前目录 (`./`)**: 从当前文件所在目录开始查找
2. **上级目录 (`../`)**: 从当前文件所在目录的上级目录开始查找
3. **模块名解析**: 相对路径会被转换为绝对路径，然后查找 `package.lisp`

### 示例

假设当前文件是 `modules_demo/pkg2/main.lisp`:

```lisp
;; 导入 pkg1 模块（通过搜索路径）
(import pkg1)

;; 使用 pkg1 的功能
(define result (pkg1.add 10 20))
```

## 限制

1. 模块导入需要设置 `XISP_PATH` 环境变量或在搜索路径中
  - 默认搜索路径为: `~/.xisp/modules` 和 当前目录 `.xisp/modules`
2. 每个模块目录必须有 `module.lisp` 文件
3. 模块名必须与目录名一致

## 运行演示

```bash
# 设置模块搜索路径并运行演示
XISP_PATH=examples/05-modules/modules_demo ./target/release/bin/ystyle::xisp.cli examples/05-modules/modules_demo/demo.lisp
```

**说明**：
- `XISP_PATH` 环境变量指定模块搜索路径
- `modules_demo` 目录包含 `pkg1` 和 `pkg2` 两个模块
- `demo.lisp` 演示如何导入和使用这些模块

## 演示内容

1. **模块导入** - 导入 pkg2 模块
2. **跨模块调用** - pkg2 调用 pkg1 的功能
3. **符号导出** - 使用 `export` 导出公共符号
4. **模块依赖** - pkg2 通过依赖声明使用 pkg1

## 相关文档

- [模块系统完整文档](../../docs/modules.md)
- [包结构说明](../../docs/modules.md#包结构)
- [导入语法](../../docs/modules.md#导入语法)

