# Xisp 模块系统

Xisp 提供了简洁优雅的模块系统，支持代码组织、命名空间隔离和依赖管理。

## 快速开始

### 基本概念（对齐仓颉术语）

| 术语 | 定义 | 标志 | 示例 |
|------|------|------|------|
| **模块** | 有 `module.lisp` 的项目/子项目 | 存在 `module.lisp` | `ystyle/log/` |
| **包** | 模块内的目录组织 | 模块内的子目录 | `log/zlog/` |
| **文件** | 单个 `.lisp` 源文件 | `.lisp` 文件 | `utils.lisp` |

**核心理念**：
- **`.` 和 `::` 在文件系统都表示目录层级**
- **`module.lisp`** 定义模块（类似仓颉的 `cjpm.toml`）
- **自动加载**模块/包目录下所有 `.lisp` 文件（忽略 `.` 开头的文件）
- **绝对导入**：只导入模块（必须有 `module.lisp`）
- **相对导入**：导入文件或目录包（不需要 `module.lisp`）

**使用规则**：
- 导入后，**最后一级是符号前缀**
- 相对导入文件时**无前缀**

### 示例项目结构

```
myapp/                         ; 项目模块（有 module.lisp）
├── module.lisp                ; 模块声明
├── main.lisp                  ; 主程序
├── utils.lisp                 ; 工具文件
├── helpers/                   ; 包（普通目录）
│   ├── consts.lisp           ; 常量定义
│   └── validate.lisp         ; 验证函数
└── math/                      ; 包
    ├── core.lisp             ; 核心数学函数
    └── stats/                ; 子包
        └── average.lisp      ; 平均值计算
```

**说明**：本文档将基于 `myapp` 项目讲解模块系统的各个方面。

---

## 命名空间与分隔符

### 分隔符规则

| 分隔符 | 用途 | 示例 |
|--------|------|------|
| `.` | 模块名/相对路径使用 `.` 分隔层级 | `log.zlog` |
| `::` | 绝对导入时 `::` 分隔组织和模块名 | `ystyle::log.zlog` |

**绝对导入示例**：
```lisp
(import ystyle::log)           ; → ystyle/log/ 模块
(import ystyle::log.zlog)      ; → ystyle/log/zlog/ 包
(import pkg1)                  ; → 搜索路径/pkg1/ 模块
```

**相对导入示例**（基于 myapp 项目）：
```lisp
(import "./utils.lisp")          ; → utils.lisp 文件
(import "./helpers")             ; → helpers/ 包
(import "./math.stats")          ; → math/stats/ 包
```

**目录结构对应**：
```
myapp/                         ; 当前项目
├── main.lisp                  ; 假设当前文件
├── utils.lisp                 ; (import "./utils.lisp")
├── helpers/                   ; (import "./helpers")
└── math/
    └── stats/                 ; (import "./math.stats")
```

**第三方模块目录结构**：


```
~/.xisp/modules/
└── ystyle/
    └── log/                   ; (import ystyle::log)
        ├── module.lisp
        └── zlog/              ; (import ystyle::log.zlog)
```

第三方模块默认搜索路径为:
- `.xisp/modules/` 当前项目的目录
- `~/.xisp/modules/` 全局的HOME目录
- `repl`可以使用环境变量`XISP_PATH`自定义搜索路径，多个用`:`冒号分隔
- `嵌入式仓颉`可以使用`withAllowedPaths`选项添加指定目录

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

| 导入类型 | 语法 | 加载目标 | 是否需要 module.lisp | 符号前缀 |
|---------|------|---------|---------------------|---------|
| **绝对导入-模块** | `(import pkg1)` | 模块目录 | ✅ 必需 | ✅ 有（模块名） |
| **绝对导入-模块的包** | `(import org::pkg.sub)` | 模块内的包指向的目录 | ❌ 不需要 | ✅ 有（包名） |
| **相对导入-目录包** | `(import "./math.stats")` | 目录 | ❌ 不需要 | ✅ 有（目录名） |
| **相对导入-文件** | `(import "./utils.lisp")` | 单文件 | ❌ 不需要 | ❌ 无前缀 |

### 判断规则

**绝对导入**（Symbol 类型）：
- 只能导入模块（必须有 `module.lisp`）
- 通过搜索路径查找
- 有命名空间隔离（模块名.符号名）

**相对导入**（String 类型）：
- 导入文件或目录包（不需要 `module.lisp`）
- 基于当前文件路径解析
- **以 `.lisp` 结尾** → 单文件导入（无前缀）
- **不以 `.lisp` 结尾** → 目录包导入（有前缀）

### 绝对导入示例

```lisp
;; 导入第三方模块
(import ystyle::log)
(log.init "myapp")             ; ✅ log. 前缀

;; 导入本地模块（通过搜索路径）
(import pkg1)
(pkg1.greet "test")           ; ✅ pkg1. 前缀
```

### 相对导入示例（基于 myapp 项目）

```lisp
;; 导入文件 → 无前缀
(import "./utils.lisp")
(processData "test")          ; ✅ 无前缀，直接导入符号

;; 导入包 → 有前缀
(import "./helpers")
(helpers.validateEmail "test@example.com")  ; ✅ helpers. 前缀

(import "./math.stats")
(stats.average [1 2 3])       ; ✅ stats. 前缀
```

### 高级导入语法

**别名导入**：
```lisp
(import ystyle::log :as log)
(log.init "myapp")
```

**限定导入**：
```lisp
(import (only ystyle::log init write))
(log.init "myapp")
(log.write "Hello")
```

---

## module.lisp - 模块元数据

### 基本格式

每个模块目录下必须有 `module.lisp` 文件。

**myapp/module.lisp**：
```lisp
(module myapp
  (version "1.0.0")
  (description "My application")
  (author "Me")

  (dependencies
    (ystyle::log "0.2.0")))
```

**ystyle/log/module.lisp**（第三方模块）：
```lisp
(module log
  (version "0.2.0")
  (organization "ystyle")
  (description "Logging library")
  (author "ystyle"))
```

**要点**：
- **模块名 = 当前目录名**：`log`（不是 `log.zlog` 或 `ystyle.log`）
- **organization 字段**：指定组织名（第三方模块必需）
- **导入时组合**：`(import ystyle::log)` 导入模块，`(import ystyle::log.zlog)` 导入模块下的包

### 字段说明

| 字段 | 说明 | 必需 |
|------|------|------|
| `name` | 模块名（=当前目录名） | ✅ |
| `version` | 版本号 | ✅ |
| `organization` | 组织名（第三方模块） | ✅ |
| `description` | 描述 | ⚠️ |
| `author` | 作者 | ⚠️ |
| `homepage` | 主页 | ⚠️ |
| `license` | 许可证 | ⚠️ |
| `dependencies` | 依赖列表 | ⚠️ |

### 依赖声明

**myapp/module.lisp**：
```lisp
(module myapp
  (version "1.0.0")
  (description "My application")
  (author "Me")

  (dependencies
    (ystyle::log "0.2.0")))
```

**依赖格式**：`(组织::模块名 "version")` - 使用 `::` 分隔组织和模块名

---

## 符号前缀规则

### 前缀规则总结

**统一规则**：符号前缀 = **路径的最后一级**

| 导入语句 | 加载路径 | 符号前缀 |
|---------|---------|---------|
| `(import ystyle::log)` | `ystyle/log/module.lisp` | `log` |
| `(import pkg1)` | `pkg1/module.lisp` | `pkg1` |
| `(import "./helpers")` | `helpers/` | `helpers` |
| `(import "./utils.lisp")` | `utils.lisp` | **无前缀** |

### 使用示例（基于 myapp）

```lisp
;; main.lisp

;; 导入第三方模块 - 有前缀
(import ystyle::log)
(log.init "myapp")            ; ✅ log. 前缀

;; 导入项目内包（相对）- 有前缀
(import "./helpers")
(helpers.validateEmail "...")  ; ✅ helpers. 前缀

;; 导入项目内文件（相对）- 无前缀
(import "./utils.lisp")
(processData "test")          ; ✅ 无前缀
```

### 层级访问规则

**只能访问当前层级模块/包的符号**：

```lisp
(import "./math.stats")
(stats.average data)           ; ✅ 可以
(math.stats.average data)     ; ❌ 不能跨层级访问
```

---

## export - 导出符号

### 语法

```lisp
(export symbol1 symbol2 ...)
```

### 示例（基于 myapp）

**myapp/helpers/consts.lisp**：
```lisp
(export MAX_SIZE MIN_VALUE)

(define MAX_SIZE 1000)
(define MIN_VALUE 0)
(define INTERNAL_CONSTANT 42)  ; 未导出，包私有
```

**myapp/main.lisp**：
```lisp
(import "./helpers.consts")
(println MAX_SIZE)        ; ✅ 可以访问
(println MIN_VALUE)       ; ✅ 可以访问
(println INTERNAL_CONSTANT)  ; ❌ 错误：未导出
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
│   └── log/                 ; 模块（有 module.lisp）
│       ├── module.lisp
│       ├── core.lisp
│       └── zlog/            ; 包（普通目录，无 module.lisp）
│           ├── core.lisp
│           └── file.lisp
│
└── myorg/                    ; 组织：myorg
    └── utils/               ; 模块
        └── module.lisp
        └── string/          ; 包
```

### 项目目录

```
myapp/                         ; 模块（有 module.lisp）
├── module.lisp              ; 模块声明
├── main.lisp
├── helpers/                  ; 包（目录组织，不需要 module.lisp）
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
- ❌ `module.lisp` 不作为代码文件执行

**示例**：
```
log/
├── module.lisp    ; ✅ 元数据文件，不执行
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

模块目录可以包含多个版本，默认版本在根目录，其他版本以 `@版本号` 命名：

```
~/.xisp/modules/ystyle/log/         ; 默认版本 (0.2.0)
~/.xisp/modules/ystyle/log@0.1.0/   ; 指定版本 (0.1.0)
```

目录结构同 `目录结构` 章节。

### 使用方式

**1. 默认导入（最常用）**：
```lisp
(import ystyle::log.zlog)  ; 使用默认版本（无版本号目录）
```

**2. 项目配置（固定版本）**：
```lisp
;; 项目根目录 module.lisp
(module myapp
  (version "1.0.0")
  (dependencies
    (ystyle::log "0.2.0")))  ; 固定版本
```

**3. 相对路径导入**（不适用版本管理）：
```lisp
(import "./math.stats")      ; 本地目录包
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
- 每个模块的 `module.lisp` 声明自己的依赖
- 加载模块时递归加载其依赖
- 版本冲突使用项目声明或默认版本

---

## 错误处理

### 模块不存在

```lisp
(import nonexist.package)
; ❌ 错误：找不到 nonexist.package 模块（缺少 module.lisp）
```

### 缺少 module.lisp

```lisp
(import mypackage)
; ❌ 错误：Missing module.lisp in mypackage/
; 提示：如果要导入文件，请使用相对导入 (import "./mypackage.lisp")
```

### 相对导入遇到模块

```lisp
;; 假设 subdir/ 有 module.lisp
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
;; ystyle/log/module.lisp
(module log
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
(import ystyle::log.zlog)     ; ✅ 织::模块.包名清晰
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
**相对导入**：导入项目内辅助文件

详细示例参见 `导入语法详解` 章节。

### 4. 文件组织

**模块（有 module.lisp）**：
```
log/
├── module.lisp     ; 模块元数据
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
~/.xisp/modules/              ; 全局模块目录
└── ystyle/
    └── log/                  ; log 模块
        ├── module.lisp
        ├── core.lisp
        └── zlog/             ; 包
            └── file.lisp

myapp/                        ; 项目目录
├── module.lisp              ; 模块声明
├── main.lisp
├── utils.lisp
├── helpers/
│   ├── consts.lisp
│   └── validate.lisp
└── math/
    ├── core.lisp
    └── stats/
        └── average.lisp
```

### 模块文件

**myapp/module.lisp**：
```lisp
(module myapp
  (version "1.0.0")
  (description "My application")
  (author "Me")
  (dependencies
    (ystyle::log "0.2.0")))
```

**myapp/helpers/consts.lisp**：
```lisp
(export MAX_SIZE MIN_VALUE)

(define MAX_SIZE 1000)
(define MIN_VALUE 0)
```

**myapp/helpers/validate.lisp**：
```lisp
(export validateEmail)

(define (validateEmail email) "验证邮箱")
```

**myapp/math/stats/average.lisp**：
```lisp
(export average)

(define (average numbers) "计算平均值")
```

### 使用示例

**myapp/main.lisp**：
```lisp
;; 导入第三方模块（绝对导入）
(import ystyle::log)
(log.init "myapp")

;; 导入项目内包（相对导入）- 有前缀
(import "./helpers")
(helpers.validateEmail "test@example.com")

(import "./math.stats")
(stats.average [1 2 3])

;; 导入项目内文件（相对导入）- 无前缀
(import "./utils.lisp")
(processData "test")
```

---

## 与其他语言对比

| 特性 | Xisp | Go | Rust | Node.js |
|------|------|-----|------|---------|
| 元数据文件 | `module.lisp` | `go.mod` | `Cargo.toml` | `package.json` |
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
  - `src/core/package_parser.cj` - module.lisp 解析器
- **配置文件**：`module.lisp` - 模块元数据
