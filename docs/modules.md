# Xisp 模块系统

Xisp 提供了简洁优雅的模块系统，支持代码组织、命名空间隔离和依赖管理。

## 快速开始

### 基本概念（对齐仓颉术语）

| 术语 | 定义 | 标志 | 示例 |
|------|------|------|------|
| **模块** | 有 `package.lisp` 的项目/子项目 | 存在 `package.lisp` | `ystyle/log/` |
| **包** | 模块内的目录组织 | 模块内的子目录 | `log/zlog/` |
| **文件** | 单个 `.lisp` 源文件 | `.lisp` 文件 | `utils.lisp` |

**核心理念**：
- **`.` 和 `::` 在文件系统都表示目录层级**
- **`package.lisp`** 定义模块（类似仓颉的 `cjpm.toml`）
- **自动加载**模块/包目录下所有 `.lisp` 文件（忽略 `.` 开头的文件）
- **绝对导入**：只导入模块（必须有 `package.lisp`）
- **相对导入**：导入文件或目录包（不需要 `package.lisp`）

### 导入示例

```lisp
;; 绝对导入 - 导入外部模块
(import ystyle::log.zlog)      ; → ystyle/log/zlog/package.lisp
(import pkg1)                  ; → 搜索路径/pkg1/package.lisp

;; 相对导入 - 导入本地文件/目录包
(import "./math.add")          ; → math/add/ 目录（包）
(import "./utils.lisp")        ; → utils.lisp 文件
(import "./helpers/core.lisp") ; → helpers/core.lisp 文件

;; 使用,最后一级是符号前缀
(zlog.init "myapp")            ; 模块导入：有前缀
(add.calculate 1 2)            ; 目录包导入：有前缀
(processData "test")           ; 文件导入：无前缀
```

---

## 命名空间与分隔符

### 分隔符规则

| 分隔符 | 用途 | 示例 |
|--------|------|------|
| `.` | 模块名/相对路径使用 `.` 分隔层级 | `log.zlog` |
| `::` | 绝对导入时 `::` 分隔组织和模块名 | `ystyle::log.zlog` |

**绝对导入示例**：
```lisp
(import ystyle::log.zlog)      ; → ystyle/log/zlog/package.lisp
(import myorg::utils.string)   ; → myorg/utils/string/package.lisp
(import pkg1)                  ; → 搜索路径/pkg1/package.lisp
```

**相对导入示例**：
```lisp
(import "./math.add")          ; → math/add/ 目录包
(import "./utils.lisp")        ; → utils.lisp 文件
(import "../helpers/core")     ; → ../helpers/core.lisp 文件
```

**目录结构对应**：
```
~/.xisp/modules/
└── ystyle/
    └── log/
        ├── package.lisp       ; 模块声明
        └── zlog/              ; 子模块
            ├── package.lisp   ; 子模块声明
            └── core.lisp
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

**符号中允许 `.`**，但导入语句中的 `.` 只用于分隔层级。

---

## 导入语法详解

### 绝对导入 vs 相对导入

| 导入类型 | 语法 | 加载目标 | 是否需要 package.lisp | 符号前缀 |
|---------|------|---------|---------------------|---------|
| **绝对导入-模块** | `(import pkg1)` | 模块目录 | ✅ 必需 | ✅ 有（模块名） |
| **绝对导入-子模块** | `(import org::pkg.sub)` | 子模块目录 | ✅ 必需 | ✅ 有（子模块名） |
| **相对导入-目录包** | `(import "./math.add")` | 目录 | ❌ 不需要 | ✅ 有（目录名） |
| **相对导入-文件** | `(import "./utils.lisp")` | 单文件 | ❌ 不需要 | ❌ 无前缀 |

### 判断规则

**绝对导入**（Symbol 类型）：
- 只能导入模块（必须有 `package.lisp`）
- 通过搜索路径查找
- 有命名空间隔离（模块名.符号名）

**相对导入**（String 类型）：
- 导入文件或目录包（不需要 `package.lisp`）
- 基于当前文件路径解析
- **以 `.lisp` 结尾** → 单文件导入（无前缀）
- **不以 `.lisp` 结尾** → 目录包导入（有前缀）

### 绝对导入示例

```lisp
;; 导入第三方模块
(import ystyle::log.zlog)
(zlog.init "myapp")          ; ✅ zlog. 前缀

;; 导入本地模块（通过搜索路径）
(import pkg1)
(pkg1.greet "test")          ; ✅ pkg1. 前缀

;; 导入子模块
(import std.io.file)
(file.read "/tmp/test.txt")  ; ✅ file. 前缀
```

### 相对导入示例

```lisp
;; 导入目录包 → 有前缀
(import "./math.add")
(add.calculate 1 2)          ; ✅ add. 前缀

(import "./string.format")
(format.format "hello")      ; ✅ format. 前缀

;; 导入单文件 → 无前缀
(import "./utils.lisp")
(processData "test")         ; ✅ 无前缀，直接导入符号

(import "./helpers/consts.lisp")
(getMaxSize())               ; ✅ 无前缀

(import "./math/core.lisp")
(calculate 1 2)              ; ✅ 无前缀
```

### 高级导入语法

**别名导入**：
```lisp
(import ystyle::log.zlog :as zlog)
(zlog.init "myapp")
```

**限定导入**：
```lisp
(import (only pkg1 greet hello))
(pkg1.greet "test")          ; ✅ 只导入指定符号
```

---

## package.lisp - 模块元数据

### 基本格式

每个模块目录下必须有 `package.lisp` 文件：

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
- **模块名不含组织前缀**：`log.zlog` 而不是 `ystyle.log.zlog`
- **organization 字段**：指定组织名（第三方模块必需）
- **导入时组合**：`(import ystyle::log.zlog)`

### 字段说明

| 字段 | 说明 | 必需 |
|------|------|------|
| `name` | 模块名（不含组织前缀） | ✅ |
| `version` | 版本号 | ✅ |
| `organization` | 组织名（第三方模块） | ✅ |
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

**依赖格式**：`(组织::模块名 "version")` - 使用 `::` 分隔组织和模块名

---

## 符号前缀规则

### 前缀规则总结

**统一规则**：符号前缀 = **路径的最后一级**

| 导入语句 | 加载路径 | 符号前缀 |
|---------|---------|---------|
| `(import pkg1)` | `pkg1/package.lisp` | `pkg1` |
| `(import org::math.calc)` | `org/math/calc/package.lisp` | `calc` |
| `(import "./math.add")` | `math/add/` | `add` |
| `(import "./utils.lisp")` | `utils.lisp` | **无前缀** |

### 使用示例

```lisp
;; 导入外部模块 - 有前缀
(import ystyle::log.zlog)
(zlog.init "myapp")          ; ✅ zlog. 前缀
(zlog.write "Hello")

;; 导入本地模块 - 有前缀
(import pkg1)
(pkg1.greet "test")          ; ✅ pkg1. 前缀

;; 导入目录包（相对）- 有前缀
(import "./math.add")
(add.calculate 1 2)          ; ✅ add. 前缀

;; 导入单文件（相对）- 无前缀
(import "./utils.lisp")
(processData "test")         ; ✅ 无前缀，直接导入符号
```

### 层级访问规则

**只能访问当前层级模块/包的符号**：

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
;; pkg1/utils.lisp
(export read write exists?)

(define (read path) "读取文件")
(define (write path content) "写入文件")
(define (internalHelper) ...)  ; 未导出，包私有
```

**要点**：
- `export` 声明的符号可以被模块/包外访问
- 未 `export` 的符号是私有的
- 每个 `.lisp` 文件可以有自己的 `export` 声明
- **文件导入**（相对导入）的符号直接导入到当前作用域

---

## 目录结构

### 全局模块目录

```
~/.xisp/modules/
├── ystyle/                   ; 组织：ystyle
│   └── log/                 ; 模块（有 package.lisp）
│       ├── package.lisp
│       ├── core.lisp
│       └── zlog/            ; 子模块（有 package.lisp）
│           ├── package.lisp
│           ├── core.lisp
│           └── file.lisp
│
└── myorg/                    ; 组织：myorg
    └── utils/               ; 模块
        └── package.lisp
        └── string/          ; 子模块
```

### 项目目录

```
myapp/                         ; 模块（有 package.lisp）
├── package.lisp              ; 模块声明
├── main.lisp
├── helpers/                  ; 包（目录组织，不需要 package.lisp）
│   ├── consts.lisp          ; 文件
│   └── validate.lisp        ; 文件
└── math/
    ├── core.lisp            ; 文件
    └── stats/               ; 目录包（相对导入）
        └── average.lisp     ; 文件
```

---

## 文件加载规则

### 模块/包的加载规则

**规则**：
- ✅ 自动加载目录下所有 `.lisp` 文件
- ✅ 按文件名排序加载（字母顺序）
- ❌ 忽略 `.` 开头的文件
- ❌ `package.lisp` 不作为代码文件执行

**示例**：
```
log/
├── package.lisp    ; ✅ 元数据文件，不执行
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

**3. 相对路径导入**（不适用版本管理）：
```lisp
(import "./math.add")      ; 本地目录包
(import "./utils.lisp")    ; 本地文件
```

---

## 依赖传递

```
myapp@1.0.0
  └── ystyle::log@0.2.0
      └── std.io@1.0.0
```

**实现**：
- 每个模块的 `package.lisp` 声明自己的依赖
- 加载模块时递归加载其依赖
- 版本冲突使用项目声明或默认版本

---

## 错误处理

### 模块不存在

```lisp
(import nonexist.package)
; ❌ 错误：找不到 nonexist.package 模块（缺少 package.lisp）
```

### 缺少 package.lisp

```lisp
(import mypackage)
; ❌ 错误：Missing package.lisp in mypackage/
; 提示：如果要导入文件，请使用相对导入 (import "./mypackage.lisp")
```

### 相对导入遇到模块

```lisp
;; 假设 subdir/ 有 package.lisp
(import "./subdir")
; ❌ 错误：Cannot import module 'subdir' using relative import.
;        Use (import subdir) or (import org::subdir) instead.
```

### 符号未导出

```lisp
;; utils.lisp
(define (internalHelper) ...)  ; 未 export

;; main.lisp
(import "./utils.lisp")
(internalHelper)
; ❌ 错误：internalHelper 未导出（文件私有）
```

### 组织名不匹配

```lisp
;; ystyle/log/package.lisp
(package log
  (organization "ystyle"))

;; 错误导入
(import wrongorg::log)
; ❌ 错误：Module 'log' belongs to organization 'ystyle', not 'wrongorg'
```

---

## 最佳实践

### 1. 模块命名

```lisp
(import utils.string)         ; ✅ 简短有意义
(import ystyle::log.zlog)     ; ✅ 组织::模块名清晰
(import company.project.app.utils)  ; ❌ 层级太深
```

### 2. 符号导出

```lisp
;; 只导出公共 API
(export publicFunction helperFunction)
(define (publicFunction) ...)   ; 导出
(define (internalHelper) ...)   ; 不导出，私有
```

### 3. 导入方式选择

**绝对导入**：导入外部模块/公共库
```lisp
(import ystyle::log.zlog)
(import pkg1)
```

**相对导入**：导入项目内辅助文件
```lisp
(import "./utils.lisp")       ; 工具函数
(import "./helpers.consts")   ; 常量定义
(import "./math.core")        ; 数学函数
```

**避免符号冲突**：
```lisp
;; 使用限定导入
(import (only "./math/stats.lisp" average median))
```

### 4. 文件组织

**模块（有 package.lisp）**：
```
log/
├── package.lisp     ; 模块元数据
├── api.lisp         ; 公共接口，导出符号
├── internal.lisp    ; 内部实现，不导出
└── .backup.lisp     ; 备份，忽略
```

**项目目录（相对导入）**：
```
myapp/
├── main.lisp
├── utils.lisp       ; 工具函数（无前缀导入）
├── helpers/
│   ├── consts.lisp  ; 常量（无前缀导入）
│   └── validate.lisp ; 验证函数（无前缀导入）
└── math/
    └── stats/       ; 目录包（有 stats. 前缀）
        └── average.lisp
```

---

## 完整示例

### 目录结构

```
~/.xisp/modules/
└── ystyle/
    └── log/               ; git 仓库：github.com/ystyle/log
        ├── package.lisp   ; 模块声明
        ├── core.lisp
        └── zlog/          ; 子模块
            ├── core.lisp
            └── file.lisp

myapp/
├── package.lisp           ; 项目模块声明
├── main.lisp
├── utils.lisp            ; 工具文件
└── math/
    └── stats/            ; 目录包（相对导入）
        └── average.lisp
```

### 模块文件

**ystyle/log/package.lisp**：
```lisp
(package log
  (version "0.2.0")
  (organization "ystyle"))
```

**ystyle/log/zlog/core.lisp**：
```lisp
(export init shutdown write)

(define (init name) "初始化")
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
;; 导入外部模块（绝对导入）- 有前缀
(import ystyle::log.zlog)
(zlog.init "myapp")
(zlog.write "Hello World")

;; 导入目录包（相对导入）- 有前缀
(import "./math.stats")
(stats.average [1 2 3])

;; 导入单文件（相对导入）- 无前缀
(import "./utils.lisp")
(processData "test")
```

---

## 与其他语言对比

| 特性 | Xisp | Go | Rust | Node.js |
|------|------|-----|------|---------|
| 元数据文件 | `package.lisp` | `go.mod` | `Cargo.toml` | `package.json` |
| 绝对导入 | `(import pkg)` | `import "pkg"` | `use pkg;` | `require("pkg")` |
| 相对导入 | `(import "./file.lisp")` | `import "./file"` | N/A | `require("./file")` |
| 分隔符 | `.` 和 `::` | `/` | `::` | `/` |
| 导出关键字 | `export` | 首字母大写 | `pub` | `exports` |
| 符号前缀 | 包名.符号名 | 包名.符号名 | 包名::符号名 | - |

**设计理念**：
- 类似 **Go**：导入后使用 `包名.符号名` 访问
- 相对导入：类似 C 的 `#include`，直接导入符号到当前作用域

---

## 相关资源

- **示例程序**：`examples/modules_demo/` - 完整演示
- **实现代码**：
  - `src/core/module.cj` - 模块系统核心
  - `src/core/eval_module.cj` - import/export 特殊形式
  - `src/core/module_loader.cj` - 文件加载器
  - `src/core/package_parser.cj` - package.lisp 解析器
- **配置文件**：`package.lisp` - 模块元数据
