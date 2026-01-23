# Xisp 模块系统

Xisp 提供了简洁优雅的模块系统，支持代码组织、命名空间隔离和依赖管理。

## 核心概念

### 目录 = 包

```
ystyle/
└── log.zlog/
    ├── package.lisp      ; 包元数据
    ├── core.lisp         ; 包代码
    └── file.lisp
```

**核心理念**：
- 每个目录就是一个包
- 自动加载目录下所有 `.lisp` 文件（忽略 `.` 开头的文件）
- 包名是路径的最后一个组件

---

## 命名空间

### 命名空间语法

使用 `::` 作为命名空间分隔符：

```lisp
;; 标准库（无组织前缀）
(import std::io)
(import std::io::file)

;; 第三方包（带组织前缀）
(import ystyle::log.zlog)
(import myorg::utils.string)
```

### 结构

```
组织名::包名
```

- **`::` 前**：组织名（可选）
- **`::` 后**：包名（可以用 `.` 分层级）

| 导入语句 | 组织名 | 包名 | 目录 |
|---------|-------|------|------|
| `std::io` | 无 | `io` | `std/io/` |
| `std::io.file` | 无 | `io.file` | `std/io/file/` |
| `ystyle::log.zlog` | `ystyle` | `log.zlog` | `ystyle/log.zlog/` |
| `myorg::utils.string` | `myorg` | `utils.string` | `myorg/utils/string/` |

---

## package.lisp - 包元数据

### 基本格式

每个包目录下必须有 `package.lisp` 文件：

```lisp
(package ystyle.log.zlog
  (version "0.2.0")
  (description "Logging library")
  (author "ystyle")
  (homepage "https://github.com/ystyle/log")
  (license "MIT"))

(export init shutdown write)
```

### 应用（有依赖）

```lisp
(package myapp
  (version "1.0.0")
  (description "My application")
  (author "Me")

  (dependencies
    (ystyle.log.zlog "0.2.0")
    (std.io "latest")
    (myorg.utils "1.0.0")))
```

### 字段说明

| 字段 | 说明 | 必需 |
|------|------|------|
| `name` | 包名 | ✅ |
| `version` | 版本号 | ✅ |
| `description` | 描述 | ⚠️ |
| `author` | 作者 | ⚠️ |
| `homepage` | 主页 | ⚠️ |
| `license` | 许可证 | ⚠️ |
| `dependencies` | 依赖列表 | ⚠️ |
| `export` | 导出符号 | ⚠️ |

---

## 导入语法

### 基础导入

```lisp
;; 标准库
(import std::io)
(import std::io::file)

;; 第三方包
(import ystyle::log.zlog)
(import myorg::utils.string)
```

### 别名导入

```lisp
(import ystyle::log.zlog :as zlog)
(zlog.init "myapp")
```

### 限定导入

```lisp
(import (only std::io.file read write))
(io.read "/tmp/test.txt")
(io.write "/tmp/out.txt" "hello")
```

### 相对路径导入

```lisp
;; 当前目录
(import ./lib.utils)
(utils.process data)

;; 上级目录
(import ../lib.parser)
(parser.parse code)
```

---

## 包名规则

### 规则

**包名 = 路径的最后一个组件**：

```lisp
(import std::io)              ; → io
(import std::io::file)         ; → file
(import ystyle::log.zlog)     ; → log.zlog
(import myorg::utils.string)  ; → string
```

### 使用示例

```lisp
(import ystyle::log.zlog)
(zlog.init "myapp")          ; 使用 log.zlog 作为包名
(zlog.write "Hello")

(import myorg::utils.string)
(string.reverse "hello")     ; 使用 string 作为包名
```

---

## 目录结构

### 全局模块目录

```
~/.xisp/modules/
├── std/                      ; 标准库
│   └── io/
│       ├── package.lisp
│       └── file.lisp
│
├── ystyle/                   ; 组织：ystyle
│   └── log.zlog/
│       ├── package.lisp
│       ├── core.lisp
│       └── file.lisp
│
└── myorg/                    ; 组织：myorg
    └── utils/
        ├── package.lisp
        └── string.lisp
```

### 项目目录

```
myapp/
├── package.lisp              ; 项目声明（可选）
├── main.lisp
└── lib/
    └── helpers/
        ├── package.lisp
        └── core.lisp
```

---

## 文件加载规则

### 自动加载

**规则**：
- ✅ 自动加载目录下所有 `.lisp` 文件
- ✅ 按文件名排序加载
- ❌ 忽略 `.` 开头的文件

### 示例

```
ystyle/log.zlog/
├── package.lisp             ; ✅ 加载
├── core.lisp               ; ✅ 加载
├── file.lisp               ; ✅ 加载
├── .backup.lisp            ; ❌ 忽略
├── .old.lisp               ; ❌ 忽略
└── .tmp.lisp               ; ❌ 忽略
```

---

## 版本管理

### 目录结构

```
~/.xisp/modules/ystyle/log.zlog/
├── package.lisp             ; version = "0.2.0" (默认)
├── core.lisp
└── file.lisp

~/.xisp/modules/ystyle/log.zlog@0.1.0/
├── package.lisp             ; version = "0.1.0"
└── core.lisp
```

### 使用方式

#### 1. 默认导入（最常用）

```lisp
;; REPL 或无配置的项目
(import ystyle::log.zlog)
; → 自动使用默认版本（无版本号目录）
```

#### 2. 项目配置（固定版本）

**项目根目录：`package.lisp`**（可选）

```lisp
(package myapp
  (version "1.0.0")
  (dependencies
    (ystyle.log.zlog "0.2.0")   ; 固定版本
    (std.io "latest")             ; 显式最新
    (myorg.utils "1.0.0")))
```

**使用时**：
```lisp
(import ystyle::log.zlog)
; → 使用 package.lisp 中声明的版本（0.2.0）
```

#### 3. 相对路径导入

```lisp
;; 导入本地模块
(import ./lib.helpers)
(import ../lib.parser)
```

---

## 依赖传递

### 依赖链

```
myapp@1.0.0
  └── ystyle.log.zlog@0.2.0
      └── std.io@1.0.0
```

**实现**：
- 每个包的 `package.lisp` 声明自己的依赖
- 加载包时递归加载其依赖
- 版本冲突使用项目声明或默认版本

---

## export - 导出符号

### 语法

```lisp
(export symbol1 symbol2 ...)
```

### 示例

```lisp
;; std/io/file/package.lisp
(package std.io.file
  (version "1.0.0")
  (author "Xisp Team"))

(export read write exists? append)

(define (read path) ...)
(define (write path content) ...)
```

**要点**：
- `export` 声明的符号可以被包外访问
- 未 `export` 的符号是包私有的
- 每个 `.lisp` 文件可以有自己的 `export` 声明

---

## 完整示例

### 目录结构

```
~/.xisp/modules/
├── std/
│   └── io/
│       ├── package.lisp
│       ├── file.lisp
│       └── net.lisp
│
└── ystyle/
    └── log.zlog/
        ├── package.lisp
        ├── core.lisp
        └── console.lisp

myapp/
├── package.lisp
├── main.lisp
└── lib/
    └── utils/
        └── string.lisp
```

### 包文件

**std/io/file/package.lisp**：
```lisp
(package std.io.file
  (version "1.0.0")
  (author "Xisp Team"))

(export read write appendToFile exists?)

(define (read path) "读取文件")
(define (write path content) "写入文件")
```

**ystyle/log.zlog/package.lisp**：
```lisp
(package ystyle.log.zlog
  (version "0.2.0")
  (author "ystyle")
  (dependencies
    (std.io "1.0.0")))

(export init shutdown write)

(define (init) "初始化")
```

**myapp/package.lisp**：
```lisp
(package myapp
  (version "1.0.0")
  (dependencies
    (ystyle.log.zlog "0.2.0")
    (std.io "latest")))
```

### 使用

```lisp
;; main.lisp

;; 导入标准库
(import std::io::file)
(file.read "/tmp/test.txt")

;; 导入第三方包
(import ystyle::log.zlog)
(zlog.init "myapp")

;; 导入本地包
(import ./lib.utils)
(utils.process data)
```

---

## 层级访问

### 规则

**只能访问当前层级包的符号**：

```lisp
;; std/io/file/package.lisp
(export readToEnd)

;; 使用
(import std::io.file)
(file.readToEnd path)        ; ✅ 可以
; (std.io.file.readToEnd)   ; ❌ 不能跨层级
```

---

## 错误处理

### 包不存在

```lisp
(import nonexist.package)
; ❌ 错误：找不到 nonexist.package 目录
```

### 缺少 package.lisp

```lisp
(import mypackage)
; ❌ 错误：Missing package.lisp in mypackage/
```

### 符号未导出

```lisp
;; std/io/file/package.lisp
(define (internalHelper) ...)  ; 未 export

;; main.lisp
(import std.io::file)
(file.internalHelper)
; ❌ 错误：internalHelper 未导出（包私有）
```

### 组织名不匹配

```lisp
;; ystyle/log.zlog/package.lisp
(package ystyle.log.zlog
  (version "0.2.0"))  ; org = "ystyle"

;; 错误导入
(import wrongorg::log.zlog)
; ❌ 错误：Package 'log.zlog' belongs to 'ystyle', not 'wrongorg'
```

---

## 符号命名规则

### 变量/函数名

**允许的字符**：
- 字母、数字
- `-`, `_`, `.`, `+`, `*`, `/`, `?`, `!`, `=`

**示例**：
```lisp
(define my-var 10)
(define log_file "app.log")
(define myFunc 100)
(define zlog.init true)
```

**禁止**：
```lisp
(define log:file "app.log")     ; ❌ 不能用 :
(define log::file "app.log")    ; ❌ 不能用 ::
```

### 命名空间

**只在 import 中使用 `::`**：
```lisp
(import ystyle::log.zlog)   ; ✅ 正确
(import std::io::file)       ; ✅ 正确

;; 代码中使用
(zlog.init)                 ; ✅ 正确
(file.read)                 ; ✅ 正确
```

---

## 文件忽略规则

### 忽略的文件

```
package/
├── package.lisp             ; ✅ 加载
├── core.lisp               ; ✅ 加载
├── .backup.lisp            ; ❌ 忽略
├── .old.lisp               ; ❌ 忽略
└── .tmp.lisp               ; ❌ 忽略
```

**规则**：
- ✅ 普通 `.lisp` 文件：加载
- ❌ 以 `.` 开头的文件：忽略

---

## 最佳实践

### 1. 包命名

```lisp
;; 推荐简短有意义的包名
(import utils.string)         ; ✅ 好
(import ystyle.log.zlog)      ; ✅ 好
(import company.project.app.utils)  ; ❌ 太深
```

### 2. 符号导出

```lisp
;; 只导出公共 API
(export publicFunction helperFunction)
(define (publicFunction) ...)   ; 导出
(define (helperFunction) ...)   ; 导出
(define (internalHelper) ...)   ; 不导出，包私有
```

### 3. 导入方式

```lisp
;; 明确导入需要的符号
(import (only std::io.file read write))

;; 使用别名避免冲突
(import std.io.file :as file)
(import myorg.io.file :as org-file)
```

### 4. 文件组织

```
package/
├── package.lisp           ; 包元数据
├── api.lisp               ; 公共接口，导出符号
├── internal.lisp          ; 内部实现，不导出
└── .backup.lisp          ; 备份，忽略
```

---

## 与其他语言对比

| 特性 | Xisp | Node.js | Rust | Go |
|------|------|---------|------|-----|
| 元数据文件 | `package.lisp` | `package.json` | `Cargo.toml` | `go.mod` |
| 导入语法 | `(import ...)` | `require(...)` | `use ...` | `import ...` |
| 命名空间 | `::` | `/` | `::` | `/` |
| 导出关键字 | `export` | `exports` | `pub` | 无（首字母大写） |
| 依赖管理 | `dependencies` | `dependencies` | `dependencies` | `require` |

---

## 快速参考

### 导入语法

| 语法 | 说明 |
|------|------|
| `(import pkg::subpkg)` | 导入包 |
| `(import pkg::subpkg :as alias)` | 别名导入 |
| `(import (only pkg::subpkg sym1 sym2))` | 限定导入 |
| `(import ./local)` | 相对路径 |

### export 语法

```lisp
(export symbol1 symbol2 ...)
```

### package.lisp 语法

```lisp
(package org.name
  (version "0.1.0")
  (dependencies
    (org.dep "version"))
  (export sym1 sym2))
```

---

## 相关资源

- **示例程序**：
  - `examples/modules_demo.lisp` - 完整演示
- **实现代码**：
  - `src/core/module.cj` - 模块系统核心
  - `src/core/eval_module.cj` - import/export 特殊形式
  - `src/core/evaluator.cj` - 集成模块系统
- **配置文件**：
  - `package.lisp` - 项目/包元数据
