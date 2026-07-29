# Xisp 架构重构与字节码编译路线图

**版本**: 0.1 (设计草案)  
**日期**: 2026-07-29  
**状态**: 设计评审

---

## 1. 动机

Xisp 当前的核心求值器是一个大包 `ystyle::xisp.core`，所有求值逻辑（特殊形式、宏展开、高阶函数、模式匹配、模块系统、内置函数注册）共 25 个文件混在一起。这导致：

- 模块边界模糊，新人理解成本高
- 无法单独替换/测试求值器的某个部分
- 为字节码编译准备时，没有清晰的编译前端/运行时后端分界线
- 仓颉不允许包级循环依赖，混在大包里虽然避开了循环，但也阻止了合理的分层

这次重构的目标是：**在不改变运行时行为的前提下，把架构理清楚，并为字节码编译铺路。**

---

## 2. 目标架构

```text
┌─────────────────────────────────────────────────────┐
│                    应用层                           │
│  cli / examples / 用户仓颉代码                      │
├─────────────────────────────────────────────────────┤
│                  整合层（根包）                      │
│  interpreter.cj (编排初始化 + 公共 API)              │
├────────────────┬────────────────┬──────────────────┤
│  bridge        │  repl          │  options/sandbox │
│  (仓颉互操作)   │  (REPL 界面)    │  (配置选项)      │
├────────────────┴────────────────┴──────────────────┤
│                   核心层                            │
│  ┌──────────┐  ┌──────────┐  ┌──────────────────┐ │
│  │ core.eval│  │ core.builtin │  │ core.module   │ │
│  │ 求值器    │←─│ 内置函数注册 │  │ 模块系统       │ │
│  │ 特殊形式  │  │ 内置宏定义  │  │ 加载/解析/依赖  │ │
│  │ 宏展开    │  └──────────┘  └──────────────────┘ │
│  │ 模式匹配  │                                      │
│  └─────┬─────┘                                     │
├────────┼───────────────────────────────────────────┤
│        ↓                   基础层                   │
│  ┌──────────┐  ┌──────────┐                        │
│  │ parser   │  │ types    │                        │
│  │ 语法分析  │  │ LispValue│                        │
│  │ AST构建  │  │ Env      │                        │
│  └──────────┘  └──────────┘                        │
├────────────────────────────────────────────────────┤
│  terminal（独立，不依赖任何 xisp 包）                │
└────────────────────────────────────────────────────┘
```

### 依赖方向（严格单向）

```
types  ← 只定义数据结构，无 xisp 导入
parser ← 依赖 types（AST = LispValue 树）
  ↑
core.eval ← 依赖 types + parser（求值器核心）
  ↑
core.builtin ← 依赖 types + core.eval（注册函数）
core.module ← 依赖 types + parser + core.eval（模块系统）
  ↑
bridge ← 依赖 types + core.eval（桥接 API）
repl ← 依赖 types + core.eval + terminal（用户界面）
  ↑
根包 ystyle::xisp（interpreter.cj）→ 编排所有初始化
  ↑
cli / examples → 使用根包 API
```

---

## 3. 包拆分方案

| 当前 | 目标 | 包含文件 |
|------|------|---------|
| `ystyle::xisp.core` | `ystyle::xisp.core.eval` | evaluator.cj, eval_core.cj, eval_special_forms.cj, eval_macro.cj, eval_higher_order.cj, eval_pattern_match.cj, eval_helpers.cj |
| `ystyle::xisp.core` | `ystyle::xisp.core.builtin` | builtin.cj, builtin_*.cj (arithmetic, comparison, logic, list, print, predicates, hashmap, higher_order, macros, aliases) |
| `ystyle::xisp.core` | `ystyle::xisp.core.module` | module.cj, module_parser.cj, module_loader.cj, module_source.cj, loader.cj, script_loader.cj, dependency_resolver.cj |
| `ystyle::xisp.types` | `ystyle::xisp.types` (不变) | types.cj, environment_test.cj, lispvalue_test.cj |
| `ystyle::xisp.parser` | `ystyle::xisp.parser` (不变) | lexer.cj, parser.cj, token.cj, 测试文件 |
| `ystyle::xisp.bridge` | `ystyle::xisp.bridge` (不变) | bridge.cj, lisp_deserializable.cj, lisp_value_extension.cj |
| `ystyle::xisp.repl` | `ystyle::xisp.repl` (不变) | repl.cj, line_editor.cj, completer.cj, highlighter.cj, history.cj |
| `ystyle::xisp.terminal` | `ystyle::xisp.terminal` (不变) | terminal.cj, types.cj, posix_impl.cj, win_impl.cj |
| `ystyle::xisp.cli` | `ystyle::xisp.cli` (不变) | main.cj |
| 根包 `ystyle::xisp` | 根包 `ystyle::xisp` (不变) | interpreter.cj, options.cj, 测试文件 |

### 目录结构

```
src/
├── core/
│   ├── eval/       → core.eval
│   ├── builtin/    → core.builtin
│   └── module/     → core.module
├── types/          → types（不变）
├── parser/         → parser（不变）
├── bridge/         → bridge（不变）
├── repl/           → repl（不变）
├── terminal/       → terminal（不变）
├── cli/            → cli（不变）
├── interpreter.cj  → ystyle::xisp（根包）
├── options.cj      → ystyle::xisp（根包）
└── *_test.cj       → ystyle::xisp（根包，测试文件）
```

---

## 4. 循环依赖破解方案

### 已知的潜在循环

`core.builtin` 中的 `builtin_macros.cj` 需要 `Evaluator` 来定义宏（当前用 `(defmacro ...)` 字符串 + eval 的方式）。而 `core.eval` 不依赖 `core.builtin`。

```
core.builtin → core.eval  (builtin_macros 引用 Evaluator)
core.eval → core.builtin  (❌ 不存在)
```

### 验证

当前 `evaluator.cj` 的 import 列表：
```
import ystyle::xisp.types.*
import std.collection.ArrayList
```

不导入任何 builtin。`registerAll` 由 `interpreter.cj`（根包）编排调用，不在 `Evaluator` 内部触发。

**结论：拆包后不会出现循环。**

### 长期改进：直接构造 Macro 值

`builtin_macros.cj` 当前用字符串方式定义宏：

```cangjie
let code = "(defmacro when (test then) (list (quote if) test then (quote 0)))"
let exprs = Parser.parseFromString(code)
Evaluator(env).eval(exprs[0])  // ← 依赖 Evaluator
```

可以改为直接构造 `Macro` 值以消除对 `Evaluator` 的依赖：

```cangjie
env.define("when", Macro(
    params: ["test", "then"],
    body: consValue(Symbol("list"),
        consValue(Symbol("quote"),
        ...)),
    macroEnv: env
))
```

但这样代码会更长。**第一阶段保持现有方式**（`core.builtin → core.eval` 是安全的单向引用），后续阶段再优化。

---

## 5. 对单元测试的影响

### 当前状况

所有核心测试文件都在根包 `ystyle::xisp`（`src/*_test.cj`），通过 `import ystyle::xisp.core.*` 通配符导入。

### 拆包后的变更

仓颉的 `import pkg.*` 不导入子包符号，所以每个测试文件需要显式导入所需的子包：

```cangjie
// 拆包前
import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*

// 拆包后
import ystyle::xisp.core.eval.*      // Evaluator
import ystyle::xisp.core.builtin.*   // BuiltinFunctions
import ystyle::xisp.core.module.*   // ModuleRegistry, Module 等
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
```

### 可见性验证

对所有测试文件的访问模式进行了彻底检查：

- **所有测试直接调用的符号都是 `public` 的** — `Evaluator`、`BuiltinFunctions.registerAll`、`ModuleRegistry` 等
- **没有测试访问 package-private（无修饰符）的内部函数** — 例如 `evalDefine`、`matchPattern`、`expandMacro` 等只在求值器内部使用
- **`extend Evaluator` 块**（在 `eval_*.cj` 中）添加公共方法到 `Evaluator` 类。跨包 `extend` 在仓颉中可行，测试文件只需导入 `core.eval` 即可看到 `eval()` 等方法

### 涉及文件

约 15 个测试文件需要更新 import 语句：

| 文件 | 需要的子包 |
|------|-----------|
| evaluator_test.cj | core.eval, core.builtin |
| modern_test.cj | core.eval, core.builtin |
| module_test.cj | core.eval, core.builtin, core.module |
| module_source_test.cj | core.eval, core.module |
| builtin_edge_test.cj | core.eval, core.builtin |
| builtin_hashmap_test.cj | core.eval, core.builtin |
| sandbox_test.cj | core.eval, core.builtin |
| bridge_test.cj | core.eval, core.builtin |
| reverse_call_test.cj | core.eval, core.builtin |
| comprehensive_test.cj | core.eval, core.builtin |
| letstar_test.cj | core.eval, core.builtin |
| procedure_test.cj | core.eval, core.builtin |
| repl_test.cj | core.eval, core.builtin |
| evaluator_test.cj | core.eval, core.builtin |

**这是纯机械变更，不涉及任何逻辑调整。** 实施阶段一（包拆分）时一并处理。

---

## 6. 字节码编译路线（预留接口）

### 设计原则

1. **不破坏现有求值路径** — 未编译的表达式继续走 AST 解释
2. **编译是可选的** — 解释器选项决定是否启用编译
3. **分层替换** — 先从热点路径开始，逐步替换

### 编译器接口（预留）

```cangjie
/// 编译结果
public enum CompiledCode {
    | Bytecode(code: ArrayList<Instruction>, constants: ArrayList<LispValue>)
    | NativeFuncPointer(ptr: Pointer)  // JIT 编译到原生
}

/// 编译器接口
public open class Compiler {
    public func compile(expr: LispValue): Option<CompiledCode> {
        // 默认实现：返回 None（由 VM 解释执行 AST）
        None
    }

    public func compileAll(exprs: ArrayList<LispValue>): ArrayList<Option<CompiledCode>> {
        exprs.map(fn(e) => this.compile(e))
    }
}
```

### VM 接口（预留）

```cangjie
public open class VM {
    public func execute(code: CompiledCode, env: Environment): LispValue {
        // 默认实现：回退到 AST 解释
        LispValue.Error(...)
    }
}
```

### Evaluator 集成

```cangjie
// Evaluator 中增加编译缓存和 VM 使用
public class Evaluator {
    var compiler: Compiler = Compiler()
    var vm: VM = VM()

    func eval(expr: LispValue): LispValue {
        // 尝试编译
        if (let Some(compiled) <- this.compiler.compile(expr)) {
            return this.vm.execute(compiled, this.env)
        }
        // 回退到 AST 解释
        this.evalAST(expr)
    }
}
```

这个接口设计可以在不改变任何现有代码的情况下，逐步替换实现：
- `Compiler` 默认返回 `None`，走原求值路径
- 后续实现子类 `BytecodeCompiler` → 编译热点
- 后续实现 `BytecodeVM` → 执行字节码

---

## 7. 实施计划

### 阶段一：包拆分（不改变行为）

1. 创建 `core/eval/`、`core/builtin/`、`core/module/` 子目录
2. 移动文件并更新 `package` 声明
3. 更新所有测试文件的 import 语句
4. 更新根包 `interpreter.cj` 和 `options.cj` 的 import 语句
5. 编译通过，314 测试全部通过

### 阶段二：代码清理

1. 统一命名规范
2. 将公共工具函数合并到 `eval/EvalHelpers.cj`
3. 减少重复的 `import` 通配符为精确导入
4. 评估 `Environment` 是否应移出 `types/` 到 `core/env/`

### 阶段三：字节码编译接口

1. 引入 `CompiledCode` 和 `Compiler` 定义（`core.eval` 中）
2. 引入 `VM` 定义，默认回退 AST
3. `Evaluator.eval()` 集成编译缓存
4. 测试确保零行为变化

## 8. 风险与注意事项

1. **仓颉子包命名规则** — 确认 `ystyle::xisp.core.eval` 子包语法是否被仓颉支持（标准仓颉包名用 `.` 分隔，理论上支持多级）
2. **测试文件位置** — 测试文件放在根包 `ystyle::xisp` 中，保持现有模式
3. **`Environment` 的位置** — 当前在 `types/types.cj`，这是历史原因（避免循环）。第一阶段不动它，第二阶段评估
