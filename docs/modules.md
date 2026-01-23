# Xisp 模块系统

Xisp 提供了简洁优雅的模块系统，支持代码组织、命名空间隔离和依赖管理。

## 快速开始

### 基本概念

```
~/.xisp/modules/
ystyle/                   ; 组织目录
└── log/                 ; git 仓库：github.com/ystyle/log
    ├── package.lisp     ; 包元数据（定义整个包，类似 package.json）
    ├── core.lisp        ; 根目录也可以有代码文件
    └── zlog/           ; 子目录
        ├── core.lisp
        └── file.lisp
```

**核心理念**：
- **`.` 和 `::` 都表示目录层级**
- `package.lisp` 在包根目录（类似 Node.js 的 package.json）
- 自动加载包根目录及其子目录的 `.lisp` 文件（忽略 `.` 开头的文件）

### 导入示例

```lisp
;; 第三方包
(import ystyle::log.zlog)   ; → ystyle/log/package.lisp

;; 使用时包名是最后一级（zlog）
(zlog.init "myapp")
```

---

## 命名空间与分隔符

### 分隔符规则

| 分隔符 | 用途 | 示例 |
|--------|------|------|
| `.` | 包名使用 `.` 分隔层级 | `log.zlog` |
| `::` | `::` 分隔组织和包名 | `ystyle::log.zlog` |

**导入示例**：
```lisp
(import ystyle::log.zlog)    ; → ystyle/log/package.lisp
(import myorg::utils.string) ; → myorg/utils/package.lisp
```

**目录结构对应**：
```
~/.xisp/modules/
└── ystyle/
    └── log/
        ├── package.lisp
        └── zlog/      → (import ystyle::log.zlog)
```

### 符号命名规则

**变量/函数名允许的字符**：
- 字母、数字、`-`、`_`、`.`、`+`、`*`、`/`、`?`、`!`、`=`

```lisp
(define my-var 10)         ; ✅
(define log_file "app")    ; ✅
(define file.read "data")  ; ✅
(define log:file "app")    ; ❌ 不能用 :
(define log::file "app")   ; ❌ 不能用 ::
```

**符号中允许 `.`**，但导入语句中的 `.` 只用于分隔包层级。

---

## package.lisp - 包元数据

### 基本格式

每个第三方包目录下必须有 `package.lisp` 文件：

```lisp
(package log.zlog
  (version "0.2.0")
  (organization "ystyle")
  (description "Logging library")
  (author "ystyle")
  (homepage "https://github.com/ystyle/log")
  (license "MIT"))
```

**要点**：
- **包名不含组织前缀**：`log.zlog` 而不是 `ystyle.log.zlog`
- **organization 字段**：指定组织名（第三方包必需）
- **导入时组合**：`(import ystyle::log.zlog)`

### 字段说明

| 字段 | 说明 | 必需 |
|------|------|------|
| `name` | 包名（不含组织前缀） | ✅ |
| `version` | 版本号 | ✅ |
| `organization` | 组织名（第三方包） | ✅ |
| `description` | 描述 | ⚠️ |
| `author` | 作者 | ⚠️ |
| `homepage` | 主页 | ⚠️ |
| `license` | 许可证 | ⚠️ |
| `dependencies` | 依赖列表 | ⚠️ |

### 依赖声明

```lisp
(package myapp
  (version "1.0.0")
  (description "My application")
  (author "Me")

  (dependencies
    (ystyle::log.zlog "0.2.0")))
```

**依赖格式**：`(组织::包名 "version")` - 使用 `::` 分隔组织和包名

---

## 导入语法

| 语法 | 说明 |
|------|------|
| `(import std.io.file)` | 导入标准库包 |
| `(import org::pkg.subpkg)` | 导入第三方包 |
| `(import org::pkg :as alias)` | 别名导入 |
| `(import (only pkg sym1 sym2))` | 限定导入 |
| `(import "./local")` | 相对路径导入 |

### 别名导入

```lisp
(import ystyle::log.zlog :as zlog)
(zlog.init "myapp")
```

### 限定导入

```lisp
(import (only std.io.file read write))
(file.read "/tmp/test.txt")
(file.write "/tmp/out.txt" "hello")
```

### 相对路径导入

```lisp
(import "./lib/utils")
(import "../lib/parser")
```

---

## 包名与符号使用

### 包名规则

**包名 = 导入路径的最后一个组件**：

```lisp
(import std.io)           → 包名是 `io`
(import std.io.file)      → 包名是 `file`
(import ystyle::log.zlog) → 包名是 `zlog`
```

### 使用示例

```lisp
;; 导入
(import std.io.file)
(import ystyle::log.zlog)

;; 使用（包名.函数名）
(file.read "/tmp/test.txt")
(file.write "/tmp/out" "data")
(zlog.init "myapp")
(zlog.write "Hello")
```

### 层级访问

**只能访问当前层级包的符号**：

```lisp
(import std.io.file)
(file.read path)           ; ✅ 可以
(std.io.file.read path)    ; ❌ 不能跨层级访问
```

---

## export - 导出符号

### 语法

```lisp
(export symbol1 symbol2 ...)
```

### 示例

```lisp
(export read write exists?)

(define (read path) "读取文件")
(define (write path content) "写入文件")
(define (internalHelper) ...)  ; 未导出，包私有
```

**要点**：
- `export` 声明的符号可以被包外访问
- 未 `export` 的符号是包私有的
- 每个 `.lisp` 文件可以有自己的 `export` 声明

---

## 目录结构

### 全局模块目录

```
~/.xisp/modules/
├── ystyle/                   ; 组织：ystyle
│   └── log/                 ; git 仓库：github.com/ystyle/log
│       ├── package.lisp
│       ├── core.lisp
│       └── zlog/
│           ├── core.lisp
│           └── file.lisp
│
└── myorg/                    ; 组织：myorg
    └── utils/
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

**规则**：
- ✅ 自动加载目录下所有 `.lisp` 文件
- ✅ 按文件名排序加载
- ❌ 忽略 `.` 开头的文件

**示例**：
```
log/
├── package.lisp    ; ✅ 加载
├── core.lisp      ; ✅ 加载
├── zlog/
│   ├── core.lisp  ; ✅ 加载
│   └── file.lisp  ; ✅ 加载
├── .backup.lisp   ; ❌ 忽略
└── .tmp.lisp      ; ❌ 忽略
```

---

## 版本管理

### 目录结构

```
~/.xisp/modules/ystyle/log/
├── package.lisp        ; version = "0.2.0" (默认)
├── core.lisp
└── zlog/
    ├── core.lisp
    └── file.lisp

~/.xisp/modules/ystyle/log@0.1.0/
├── package.lisp        ; version = "0.1.0"
├── core.lisp
└── zlog/
    ├── core.lisp
    └── file.lisp
```

### 使用方式

**1. 默认导入（最常用）**：
```lisp
(import ystyle::log.zlog)  ; 使用默认版本（无版本号目录）
```

**2. 项目配置（固定版本）**：
```lisp
;; 项目根目录 package.lisp
(package myapp
  (version "1.0.0")
  (dependencies
    (ystyle::log "0.2.0")))  ; 固定版本
```

**3. 相对路径导入**：
```lisp
(import "./lib.helpers")
(import ../lib.parser")
```

---

## 依赖传递

```
myapp@1.0.0
  └── ystyle::log@0.2.0
      └── std.io@1.0.0
```

**实现**：
- 每个包的 `package.lisp` 声明自己的依赖
- 加载包时递归加载其依赖
- 版本冲突使用项目声明或默认版本

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
;; package.lisp
(define (internalHelper) ...)  ; 未 export

;; main.lisp
(import mypackage)
(mpackage.internalHelper)
; ❌ 错误：internalHelper 未导出（包私有）
```

### 组织名不匹配

```lisp
;; ystyle/log/package.lisp
(package log
  (organization "ystyle"))

;; 错误导入
(import wrongorg::log.zlog)
; ❌ 错误：Package 'log.zlog' belongs to organization 'ystyle', not 'wrongorg'
```

---

## 最佳实践

### 1. 包命名

```lisp
(import utils.string)         ; ✅ 简短有意义
(import ystyle::log.zlog)     ; ✅ 组织::包名清晰
(import company.project.app.utils)  ; ❌ 层级太深
```

### 2. 符号导出

```lisp
;; 只导出公共 API
(export publicFunction helperFunction)
(define (publicFunction) ...)   ; 导出
(define (internalHelper) ...)   ; 不导出，包私有
```

### 3. 导入方式

```lisp
;; 明确导入需要的符号
(import (only std.io.file read write))

;; 使用别名避免冲突
(import std.io.file :as file)
(import myorg::io.file :as org-file)
```

### 4. 文件组织

```
package/
├── package.lisp     ; 包元数据
├── api.lisp         ; 公共接口，导出符号
├── internal.lisp    ; 内部实现，不导出
└── .backup.lisp     ; 备份，忽略
```

---

## 完整示例

### 目录结构

```
~/.xisp/modules/
└── ystyle/
    └── log/               ; git 仓库：github.com/ystyle/log
        ├── package.lisp
        ├── core.lisp
        └── zlog/
            ├── core.lisp
            └── file.lisp

myapp/
├── package.lisp
├── main.lisp
└── lib/
    └── utils/
        └── string.lisp
```

### 包文件

**ystyle/log/package.lisp**：
```lisp
(package log
  (version "0.2.0")
  (organization "ystyle"))
```

**ystyle/log/core.lisp**（代码文件示例）：
```lisp
(export init shutdown write)

(define (init) "初始化")
(define (write msg) "写入日志")
```

**myapp/package.lisp**：
```lisp
(package myapp
  (version "1.0.0")
  (dependencies
    (ystyle::log "0.2.0")))
```

**myapp/main.lisp**：
```lisp
;; 导入第三方包
(import ystyle::log.zlog)
(zlog.init "myapp")
(zlog.write "Hello World")

;; 导入本地包
(import ./lib.utils)
(utils.process data)
```

---

## 与其他语言对比

| 特性 | Xisp | Node.js | Rust | Go |
|------|------|---------|------|-----|
| 元数据文件 | `package.lisp` | `package.json` | `Cargo.toml` | `go.mod` |
| 导入语法 | `(import ...)` | `require(...)` | `use ...` | `import ...` |
| 分隔符 | `.` 和 `::` | `/` | `::` | `/` |
| 导出关键字 | `export` | `exports` | `pub` | 无（首字母大写） |

---

## 相关资源

- **示例程序**：`examples/modules_demo.lisp` - 完整演示
- **实现代码**：
  - `src/core/module.cj` - 模块系统核心
  - `src/core/eval_module.cj` - import/export 特殊形式
  - `src/core/evaluator.cj` - 集成模块系统
- **配置文件**：`package.lisp` - 项目/包元数据
