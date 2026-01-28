# Xisp 模块系统

Xisp 的代码组织分为两个层面：**模块系统**（以 `module.lisp` 为入口的命名空间隔离）和 **文件导入**（简单文件脚本）。

---

## 一、加载模式

Xisp 有两种加载模式，由 Loader 策略控制：

| 模式 | Loader | 使用场景 | 支持 `(import pkg)` | 支持 `(import "./xxx")` |
|------|--------|---------|-------------------|----------------------|
| **脚本模式** | ScriptLoader | REPL、CLI 脚本 | ✅ | ✅ |
| **模块模式** | ModuleLoader | 模块内部、嵌入式 | ✅ | ❌ 返回错误 |

两种模式的 `(import module-name)` 行为完全一致，都通过 `ModuleSource` 接口解析。

---

## 二、模块系统（(import pkg-name)）

### 基本概念

| 术语 | 定义 | 标志 |
|------|------|------|
| **模块** | 有 `module.lisp` 的目录 | 存在 `module.lisp` |
| **包** | 模块内的子目录组织 | 模块内的子目录 |
| **组织** | 模块命名空间前缀 | `module.lisp` 的 `organization` 字段 |

### module.lisp

每个模块必须有 `module.lisp` 文件：

```lisp
(module myapp
  (version "1.0.0")
  (description "My application")
  (author "Me")
  (dependencies
    (ystyle::log "0.2.0")))
```

第三方模块还需要 `organization` 字段：
```lisp
(module log
  (version "0.2.0")
  (organization "ystyle")
  (description "Logging library"))
```

### 目录结构

```
~/.xisp/modules/              ; 全局模块目录
└── ystyle/                   ; 组织
    └── log/                  ; 模块（有 module.lisp）
        ├── module.lisp
        ├── core.lisp
        └── zlog/             ; 包
            ├── core.lisp
            └── file.lisp

myapp/                        ; 项目模块
├── module.lisp              ; 模块声明
├── main.lisp
├── utils.lisp
├── helpers/                  ; 包
│   ├── consts.lisp
│   └── validate.lisp
└── math/
    ├── core.lisp
    └── stats/                ; 子包
        └── average.lisp
```

### 组织名

```lisp
(import ystyle::log)    ; 组织: ystyle, 模块: log
(import pkg1)           ; 无组织名（标准库预留）
```

- **空组织名**：为未来标准库预留，当前解析为普通模块
- **第三方库必须指定组织名**，不能省略 `org::` 前缀

### 搜索路径

模块按名称搜索：

- **默认路径**：`~/.xisp/modules/`（用户全局目录）
- **环境变量**：`XISP_PATH`，多个路径用 `:` 分隔
- **嵌入 API**：`withModulePath()` 或 `withModulePaths()` 选项
- **CLI 启动**：自动读取 `XISP_PATH` 环境变量

注意：当前工作目录 `.` 不自动加入搜索路径，需要通过 `XISP_PATH=.` 或 `withModulePath(".")` 显式添加。

### 导入语法

```lisp
;; 绝对导入 - 按模块名搜索
(import ystyle::log)           ; → ystyle/log/ 模块
(import ystyle::log.zlog)      ; → ystyle/log/zlog/ 包
(import pkg1)                  ; → 搜索路径/pkg1/ 模块
```

符号前缀 = 模块/包名的最后一级：

```lisp
(import ystyle::log)
(log.init "myapp")             ; ✅ log. 前缀

(import pkg1)
(pkg1.greet "test")            ; ✅ pkg1. 前缀
```

### 高级导入语法

```lisp
;; 别名
(import ystyle::log :as mylog)
(mylog.init "app")

;; 限定导入（只导入指定符号）
(import (only ystyle::log init write))
(log.init "app")
```

### export

```lisp
;; 模块/包文件内
(export publicFunc)

(define (publicFunc) ...)      ; 导出，外部可访问
(define (privateFunc) ...)     ; 不导出，模块私有
```

### 文件加载规则

模块/包目录下的 `.lisp` 文件自动加载：

- ✅ 加载所有 `.lisp` 文件
- ✅ 按文件名排序
- ❌ 忽略 `.` 开头的文件
- ❌ `module.lisp` 不执行

---

## 三、文件导入（(import "./xxx")）

### 使用场景

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

(import "./math.stats")        ; 目录包，有前缀
(stats.average [1 2 3])
```

**⚠️ 模块内部不支持文件导入**。如果模块文件需要引用其他文件，应使用模块名导入：

```lisp
;; myapp/main.lisp - 模块内部
(import myapp.helpers)         ; ✅ 模块名导入
(import ystyle::log)            ; ✅ 模块名导入

(import "./utils.lisp")        ; ❌ 模块内部不支持
```

---

## 四、版本管理

目录使用 `@版本号` 区分版本：

```
~/.xisp/modules/ystyle/log/         ; 默认版本
~/.xisp/modules/ystyle/log@0.1.0/   ; 指定版本
```

```lisp
(import ystyle::log.zlog)   ; 使用默认版本

;; 项目 module.lisp 固定版本
(module myapp
  (dependencies
    (ystyle::log "0.2.0")))
```

---

## 五、最佳实践

### 推荐的项目结构

```
myapp/
├── module.lisp              ; 项目模块
├── main.lisp
├── helpers/
│   ├── consts.lisp
│   └── validate.lisp
└── math/
    └── stats/
        └── average.lisp
```

### 推荐的做法

1. **模块内引用用模块名**：`(import myapp.helpers)`
2. **文件导入只在 REPL/脚本用**：`(import "./utils.lisp")`
3. **用 `export` 控制可见性**，只暴露公共 API
4. **模块目录自动加载**，不需要手动 import 内部文件

---

## 六、嵌入式场景

仓颉嵌入 Xisp 时，可通过选项配置 Loader 模式：

```cangjie
// 模块模式（默认，禁止文件导入）
let interpreter = LispInterpreter([
    withModuleMode()
])

// 脚本模式（允许文件导入）
let interpreter = LispInterpreter([
    withScriptMode()
])

// 运行时切换
interpreter.setScriptMode()
interpreter.setModuleMode()
```

---

## 相关资源

- **实现代码**：
  - `src/core/module.cj` - 模块系统核心
  - `src/core/loader.cj` - Loader 接口
  - `src/core/module_loader.cj` - 模块加载器
  - `src/core/script_loader.cj` - 脚本加载器
  - `src/core/module_source.cj` - ModuleSource 接口与实现
  - `src/core/eval_module.cj` - import/export 求值
- **配置文件**：`module.lisp`
