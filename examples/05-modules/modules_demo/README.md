# 模块系统演示

本目录演示了 Xisp 模块系统的功能，展示模块间的依赖和符号导出。

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

## 模块导入

模块之间通过模块名引用，不依赖文件路径：

```lisp
(import pkg1)        ; 通过模块名导入
(import ystyle::log)  ; 带组织名的模块
```

模块文件（如 `pkg2/main.lisp`）内部使用模块名导入其他模块：

```lisp
;; pkg2/main.lisp
(import pkg1)        ; ✅ 通过搜索路径查找 pkg1
(pkg1.greet "World")
```

## 搜索路径

模块按名称搜索，默认路径：

- `~/.xisp/modules/`（用户全局目录）
- `XISP_PATH` 环境变量（`:` 分隔）

运行演示时需设置 `XISP_PATH`：

```bash
XISP_PATH=./examples/05-modules/modules_demo ./target/release/bin/ystyle::xisp.cli examples/05-modules/modules_demo/demo.lisp
```

## 演示内容

1. **模块导入** - 导入 pkg2 模块
2. **跨模块调用** - pkg2 调用 pkg1 的功能
3. **符号导出** - 使用 `export` 导出公共符号
4. **模块依赖** - pkg2 通过依赖声明使用 pkg1

## 相关文档

- [模块系统完整文档](../../docs/modules.md)

