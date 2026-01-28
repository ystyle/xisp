# Loader 接口设计

## 1. 问题分析

当前 `import` 有两种路径：

```
Symbol("pkg1") → importModuleByName → ModuleLoader → ModuleSource
Str("./utils.lisp") → importRelativeModule → 文件系统路径
```

两者都依赖 `currentFilePath` 来解析相对路径，但 ModuleSource 加载的文件没有真实的 `currentFilePath`。模块上下文和脚本上下文的职责混在一起。

## 2. 分层设计

```
请求层 (import 语法解析)
  │
策略层 (Loader 接口)
  ├─ ModuleLoader  ← 模块上下文
  └─ ScriptLoader  ← 脚本/REPL 上下文
  │
数据层 (ModuleSource 接口)
  ├─ FileSystemSource ← 默认
  └─ MemorySource     ← 测试/动态注入
  │
执行层 (Evaluator)
  └─ eval / currentModule / export
```

## 3. Loader 接口

```cangjie
public interface Loader {
    func loadByName(moduleName: String): LispValue
    func loadRelative(relativePath: String): LispValue
    func getCurrentEnv(): Environment
}
```

## 4. 两分支语义

### ModuleLoader

| 方法 | 行为 |
|------|------|
| `loadByName` | 走 ModuleSource → 创建 Module → 加载文件 → 绑定符号 |
| `loadRelative` | 返回错误："模块上下文中不支持相对路径导入" |

### ScriptLoader

| 方法 | 行为 |
|------|------|
| `loadByName` | 同 ModuleLoader（走 ModuleSource） |
| `loadRelative` | 走文件系统路径解析，支持 `(import "./file.lisp")` 和 `(import "./dir")` |

## 5. currentFilePath

| 上下文 | 值 |
|--------|-----|
| **ModuleLoader** 加载的文件 | 虚拟路径（`pkg1/utils.lisp`） |
| **ScriptLoader** 加载的文件 | 真实路径（`/home/.../script/utils.lisp`） |
| REPL | `None`（基于 CWD） |
| CLI 脚本入口 | `./script.lisp` |

## 6. 初始化和切换

```cangjie
// Evaluator 默认 ModuleLoader
public class Evaluator {
    var currentLoader: Loader
}

// CLI 执行脚本时
interpreter.setScriptLoader()

// REPL
repl.loader = ScriptLoader(...)
```

## 7. evalImport 简化

```cangjie
func evalImport(expr: LispValue): LispValue {
    match (expr) {
        case Cons(cell) => match (cell.car) {
            case Symbol(name) => this.currentLoader.loadByName(name)
            case Str(path) => this.currentLoader.loadRelative(path)
            case Cons(specCell) => this.parseImportSpec(specCell)
        }
    }
}
```

## 8. 文件变更清单

| 文件 | 改动 |
|------|------|
| `src/core/loader.cj` | 新增：Loader 接口 |
| `src/core/script_loader.cj` | 新增：ScriptLoader |
| `src/core/module_loader.cj` | 改为实现 Loader 接口 |
| `src/core/evaluator.cj` | 加 currentLoader 字段 |
| `src/core/eval_module.cj` | evalImport 委托给 currentLoader，删 loadModuleFromFileSystem |
| `src/interpreter.cj` | 加 setScriptLoader / setModuleLoader |
| `src/cli/main.cj` | 脚本执行用 ScriptLoader |
| `src/repl/repl.cj` | REPL 用 ScriptLoader |
| `docs/modules.md` | 更新文档 |
