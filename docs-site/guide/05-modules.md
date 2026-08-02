# 模块系统

**阅读时间**: 25 分钟

Xisp 的代码组织分为两个层面：**模块系统**（以 `module.lisp` 为入口的命名空间隔离）和 **文件导入**（简单文件脚本）。

本指南的示例基于 xisp 仓库自带的**教学模块** `examples/05-modules/modules_demo`，并提供"自己搭建"的完整步骤。

---

## 加载模式

Xisp 有两种加载模式，由 Loader 策略控制：

| 模式 | Loader | 使用场景 | 支持 `(import pkg)` | 支持 `(import "./xxx")` |
|------|--------|---------|-------------------|----------------------|
| **脚本模式** | ScriptLoader | REPL、CLI 脚本 | ✅ | ✅ |
| **模块模式** | ModuleLoader | 模块内部、嵌入式 | ✅ | ❌ |

两种模式的 `(import module-name)` 行为完全一致，都通过 `ModuleSource` 接口解析。

---

## 准备工作：下载源码

模块示例依赖 xisp 仓库自带的 `examples/05-modules/modules_demo` 教学模块。请先克隆源码：

```bash
git clone https://gitcode.com/ystyle/xisp.git
cd xisp

# 编译
cjpm build
```

后续运行示例时，用 `XISP_PATH` 指向教学模块目录：

```bash
XISP_PATH=./examples/05-modules/modules_demo ./target/release/bin/ystyle::xisp.cli script.lisp
```

---

## 教学模块结构

`examples/05-modules/modules_demo/` 是一个完整的教学模块，展示了模块的组织方式：

```
modules_demo/
├── module.lisp              ; 模块声明（顶层）
├── demo.lisp                ; 演示入口脚本
├── pkg1/                    ; 包 1（工具函数，无依赖）
│   ├── module.lisp
│   └── utils.lisp           ; 导出 greet/add/multiply/get-info
└── pkg2/                    ; 包 2（依赖 pkg1）
    ├── module.lisp
    └── main.lisp            ; 导入 pkg1，导出 calculate/greet 等
```

### 直接运行演示

```bash
XISP_PATH=./examples/05-modules/modules_demo \
  ./target/release/bin/ystyle::xisp.cli \
  examples/05-modules/modules_demo/demo.lisp
```

输出展示了完整的模块导入、函数调用和跨包依赖：

```
=== Xisp 模块系统演示 ===
1. 导入 pkg2 模块
模块导入成功
2. 调用 pkg2 中导出的函数
Greetings from pkg2!
pkg1 says:
Hello from pkg1! 你好, Xisp!
...
```

---

## 导入语法

### 按模块名导入

```lisp
(import pkg2)          ; 导入模块（通过 XISP_PATH 搜索）
(pkg2.greet "Xisp")    ; 用 pkg2. 前缀访问
(pkg2.calculate 10 20)
```

### 别名导入

```lisp
(import (pkg2 :as mypkg))
(mypkg.greet "Xisp")   ; 用别名访问
```

### 限定导入（only）

只导入指定符号：

```lisp
(import (only pkg2 greet))
(pkg2.greet "Xisp")    ; 只暴露 greet，其他符号不可见
```

### 符号前缀

导入后，用 `模块名.符号名` 的方式调用：

```lisp
(import pkg1)
(pkg1.add 1 2)          ; => 3
(pkg1.multiply 3 4)     ; => 12
(pkg1.get-info)         ; => "This is pkg1 - a utility package"
```

::: tip 未导出的符号不可见
pkg1 里的 `internal-helper` 未导出，`(pkg1.internal-helper)` 会报 `UndefinedFunction`。
:::

---

## export - 控制可见性

模块文件内用 `export` 声明对外可见的符号：

```lisp
;; pkg1/utils.lisp
(export greet add multiply get-info)   ; 导出，外部可访问

(define (greet name) ...)
(define (add a b) ...)

(define (internal-helper) ...)          ; 不导出，模块私有
```

---

## 跨包依赖

pkg2 依赖 pkg1，在模块内部用模块名导入：

```lisp
;; pkg2/main.lisp
(import pkg1)          ; ✅ 模块内部用模块名导入

(define (calculate x y)
  (pkg1.add x y))      ; 调用 pkg1 的函数
```

依赖关系在 `module.lisp` 中声明：

```lisp
(module pkg2
  (version "1.0.0")
  (dependencies
    (pkg1 "1.0.0")))
```

::: warning 模块内部不支持文件导入
```lisp
(import "./utils.lisp")   ; ❌ 模块内部不支持
```
模块内引用其他模块必须用模块名导入。
:::

---

## module.lisp 元数据

每个模块目录必须有 `module.lisp`：

```lisp
(module pkg1
  (version "1.0.0")
  (description "Package 1 - Utility functions")
  (author "Xisp Demo"))
```

第三方模块还需 `organization` 字段：

```lisp
(module log
  (version "0.2.0")
  (organization "ystyle")
  (description "Logging library"))
```

::: warning
`(module ...)` 声明由模块加载器在模块目录上下文中解析，**不能直接作为脚本运行**（直接执行会报 `UndefinedFunction: 'module'`）。它是模块元数据声明文件。
:::

---

## 自己搭建一个模块

不依赖仓库预置模块，从头创建你自己的模块：

### 步骤 1：创建目录结构

```
myapp/
├── module.lisp              ; 模块声明
└── utils.lisp               ; 工具函数
```

### 步骤 2：编写 module.lisp

```lisp
(module myapp
  (version "0.1.0")
  (description "My application")
  (author "Me"))
```

### 步骤 3：编写 utils.lisp

```lisp
;; 导出 square 函数
(export square)

(define (square x)
  (* x x))
```

### 步骤 4：使用你的模块

```lisp
;; test.lisp
(import myapp)
(println (myapp.square 5))   ; => 25
```

### 步骤 5：运行

```bash
XISP_PATH=./myapp ./target/release/bin/ystyle::xisp.cli test.lisp
```

::: tip XISP_PATH 搜索路径
模块按 `XISP_PATH`（冒号分隔的路径列表）搜索。当前工作目录 `.` 不会自动加入，需显式指定 `XISP_PATH=.`。
:::

---

## 文件导入（(import "./xxx")）

仅用于 **REPL 和 CLI 脚本**（ScriptLoader 模式），不适用于模块内部。

| 语法 | 说明 | 符号前缀 |
|------|------|---------|
| `(import "./utils.lisp")` | 加载单个文件 | 无前缀 |
| `(import "./helpers")` | 加载目录包 | 有前缀（目录名） |

```lisp
;; REPL 或脚本中
(import "./utils.lisp")        ; 文件，无前缀
(processData "test")

(import "./helpers")           ; 目录包，有前缀
(helpers.validateEmail "...")
```

---

## 最佳实践

1. **模块内引用用模块名**：`(import myapp.helpers)`
2. **文件导入只在 REPL/脚本用**：`(import "./utils.lisp")`
3. **用 `export` 控制可见性**，只暴露公共 API
4. **模块目录自动加载**，不需要手动 import 内部文件

---

## 下一步

- [Unicode 支持](06-unicode) - 中文关键字
- [API 参考](../api/) - 全部内置函数
