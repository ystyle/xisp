# Xisp 任务清单

> 仓颉嵌入式 Lisp 脚本语言 - 开发进度管理

---

## 📊 总体进度

- ✅ M1: 核心 Lisp 解释器 (MVP)
- ✅ M2: 仓颉互操作桥接
- ✅ M3: 现代化语法扩展
- ✅ M3.5: 模块系统基础
- 🚧 M4: 高级特性和完善
- ✅ M4.9: ModuleSource 模块数据源抽象
- ✅ M5: 工作区化拆分（阶段 1）
- ✅ M6: 字节码后端拆分（阶段 2）
- ✅ M14: 桥接反向调用与命名空间统一
- ✅ M15: 设计文档拆分（工作区化 / 能力域 / 科学计算族）
- ✅ M16: 符号分层实测 + 两级装配模型
- ✅ M17: 分层目标澄清 —— 可替换
- ✅ M18: 能力域文档按「可替换」重构
- ✅ M19: R5RS 级符号分层重构（gensym / 编译前宏展开 / ①收敛 / 域装配 / 宏包）
- ✅ M20: 兼容性修复 + 接缝 0e / 3 / 4
- ✅ M21: 错误传播语义统一 + 三后端差分闸门
- ✅ M22: 收尾审计（5 个「看起来在跑、实际没跑」的东西）
- ✅ M23: 域装配 5 处缺陷订正 + 两档装配收敛
- ✅ M24: 装配档位 XispLevel（Expr / Pure / Script）

---

## M1: 核心 Lisp 解释器 (MVP) ✅

- [x] 解析和求值 S-表达式
- [x] 核心特殊形式（define, lambda, if, quote, let）
- [x] 闭包和词法作用域
- [x] REPL 交互
- [x] 核心测试套件（183个测试全部通过）

**完成时间**: 2026-01-21

---

## M2: 仓颉互操作桥接 ✅

- [x] Lisp → 仓颉函数调用
- [x] 仓颉 → Lisp 函数调用 ✅ 已完成 (2026-01-28)
- [x] std.io 桥接
- [x] std.fs 桥接
- [x] LispInterpreter API
- [x] 桥接层文档

**完成时间**: 2026-01-28

---

## M3: 现代化语法扩展 ✅

- [x] 解构绑定
- [x] 管道操作符 (->)
- [x] 向量字面量 []
- [x] 哈希映射字面量 {}
- [x] 哈希集合字面量 #{}
- [x] 字符串插值 #"{}"
- [x] 高阶函数 (map/filter/reduce)

**完成时间**: 2026-01-22

---

## M3.5: 模块系统 ✅

- [x] 模块命名空间 (组织名::包名)
- [x] 模块元数据 (module.lisp)
- [x] 模块加载和注册
- [x] 依赖解析
- [x] 相对路径导入 (./local.lisp)
- [x] 绝对路径导入 (org::module)
- [x] 符号导出 (export)
- [x] 模块系统测试（183个测试通过）

**完成时间**: 2026-01-24

---

## M4: 高级特性和完善 🚧

### 4.1 可变参数和宏系统 ✅
- [x] lambda 可变参数 (. rest 和 &rest rest)
- [x] define 函数可变参数
- [x] ,@ (comma-at) 拼接功能
- [x] eval 特殊形式
- [x] macroexpand 返回语法树
- [x] 宏的纯可变参数 bug（bindMacroParams 修复） ✅ 已修复 (2026-01-27)

### 4.2 文件和脚本支持 ✅
- [x] evalFile 实现（通过 runScript + evalMultiple）
- [x] CLI 脚本运行参数（-c 执行代码、直接运行 .lisp 文件）
- [x] shebang 支持（Lexer 忽略 #! 行）

### 4.3 HashMap 解构和模式匹配 ✅
- [x] HashMap 解构：`(let [{:key1 var1 :key2 var2} hashmap-value] ...)`
- [x] match HashMap 模式匹配：`(match value {:key1 var1 :key2 var2} result)`

### 4.4 模块系统（已完成）

> 当前模块系统设计已完善：
> ✅ 绝对导入 `(import org::module)`
> ✅ 相对导入 `(import "./file.lisp")` `(import "./package")`
> ✅ 模块元数据 `module.lisp`
> ✅ 符号导出 `(export ...)`
> ✅ 依赖管理 `(dependencies ...)`
> ✅ 搜索路径 `XISP_PATH`
> ✅ 版本管理（精确版本 + @版本号 目录）
> ✅ 目录版本管理 `org/module@0.1.0/`
>
> 版本管理说明：
> - 采用精确版本匹配，避免复杂依赖解析
> - 通过 @版本号 目录实现多版本共存
> - 使用 latest 标记获取默认版本
> - 不支持版本范围语法（^, ~, >=, <=），保持简单

### 4.5 异步支持
- [ ] spawn 宏实现（创建线程）
- [ ] Future<T> 操作（get, get(timeout), tryGet, cancel）
- [ ] 线程工具（Thread.currentThread, id, hasPendingCancellation）

> 设计说明：
> - 使用仓颉的 spawn + Future<T> 模型（非 async/await 语法）
> - spawn 创建线程并返回 Future<T>
> - Future<T> 提供结果获取和取消机制
> - 参考 std.sync 和 std.core.Future<T> API

### 4.6 宏系统增强
- [x] 宏展开调试工具 ✅ 已有（`macroexpand` / `macroexpand-all` 特殊形式）
- [ ] 宏卫生性检查
- [ ] compile-time 计算

### 4.7 错误处理和调试
- [x] 错误类型系统 ✅ 已完成 (2026-01-27)
- [x] 调用栈追踪 ✅ 已完成
- [ ] 调试器 (debugger)
- [ ] 性能分析

### 4.8 文档和工具
- [x] API 文档生成 ✅ 已完成 (2026-08-02)
  - `docs-site/api/` 下 8 个 VitePress 页面（概览、算术与比较、列表、哈希映射、谓词、逻辑、字符串、流程控制）
  - 覆盖 109 个内置符号，全部示例经 CLI 实测验证
- [ ] 包管理器 (cjlpm)
- [ ] 测试覆盖率报告
- [x] REPL 补全（Completer）✅ 已完成 (2026-07-18)
  - Completer 类：前缀匹配、大小写不敏感、环境符号 + 内置关键字
  - getCommonPrefix 最长公共前缀计算
  - cycleIndex Tab 循环索引支持
  - 8 个单元测试全部通过

### 4.9 ModuleSource 模块数据源抽象 ✅
- [x] `ModuleSource` 接口定义（getMetadata/listFiles/readFile）
- [x] `MemorySource` 内存数据源实现
- [x] `FileSystemSource` 文件系统数据源实现
- [x] `ModuleData` 批量注册数据结构
- [x] `ModuleLoader.setSource()` / `hasCustomSource()` / `loadModule()` 分发机制
- [x] `LispInterpreter.setModuleSource()` 集成
- [x] `withModuleSource()` 解释器选项
- [x] 单元测试（12 个测试用例，240 测试全部通过）

**完成时间**: 2026-07-07

---

## 📝 待修复问题

### 高优先级
- [x] ~~修复代码中反斜杠导致的内存溢出 bug~~ ✅ 已修复 (2026-01-27)
  - 问题：在代码中使用 `\n` 等转义字符导致内存溢出
  - 修复位置：`src/parser/lexer.cj`, `src/parser/token.cj`, `src/parser/parser.cj`
  - 影响：现在正确报告语法错误，不再崩溃

- [x] ~~修复宏的纯可变参数 bug（bindMacroParams）~~ ✅ 已修复 (2026-01-27)
  - 问题：`(. args)` 只绑定第一个参数
  - 修复位置：`src/parser/parser.cj`, `src/core/eval_core.cj`
  - 影响：现在可以正常使用宏的纯可变参数

### 中优先级
- [x] ~~实现命名参数和默认值~~ ✅ 已完成 (2026-01-27)
  - 功能：`&key` 参数和 `(x default)` 语法
  - 复杂度：中
  - 实现：
    - 扩展 `ParamInfo` 类支持 `isKey` 和 `defaultValue` 字段
    - 添加 `ProcedureFromParams` 和 `MacroFromParams` 类型
    - 修改 `extractParamsWithRest` 识别 `&key` 标记和 `(param default)` 语法
    - 修改 `applyProcedure` 实现三阶段参数绑定（位置→命名→可变）
    - 修改 `evalArguments` 不求值 `:keyword` 符号
    - 更新 `evalFunctionCall` 使用模式匹配支持新旧类型
  - 文档：添加 `docs/syntax/01-basics.md` 高级参数特性章节
  - 示例：创建 `examples/02-intermediate/06_keyword_args.lisp`
  - 测试：213 个单元测试全部通过（新增 `testKeywordParameters`）

### 低优先级
(无)

---

## 🔧 代码 TODO

(无)

---

## 📅 更新记录

- 2026-01-28: **仓颉反向调用 Lisp 函数** ✅
  - 实现 `LispDeserializable<T>` 接口（Lisp → 仓颉类型转换）
  - 为 Int64、Float64、String、Bool 实现 LispDeserializable
  - 实现 `ExtendLispValue` 接口定义类型转换契约
  - 在 bridge 包中通过接口扩展为 LispValue 添加转换方法：
    - `asInt(): ?Int64` - 支持从 Int 和 Float 转换
    - `asFloat(): ?Float64` - 支持从 Float 和 Int 转换
    - `asString(): ?String` - 支持从 Str 和 Symbol 转换
    - `asBool(): ?Bool` - 支持从 Boolean 转换
    - `asCjValue<T>(): ?T` - 泛型转换方法，使用 LispDeserializable 接口
  - 在 `LispInterpreter` 中实现 `call<T>(funcName, args)` 方法
    - 支持变长参数：`Array<T>` 其中 `T <: LispConvertible`
    - 自动将仓颉类型转换为 LispValue
    - 构造函数调用表达式并求值
  - 创建完整测试套件（12个测试用例）：
    - testCallWithInt - 整数类型转换
    - testCallWithFloat - 浮点类型转换
    - testCallWithString - 字符串类型转换
    - testCallWithBool - 布尔类型转换
    - testCallWithMultipleArgs - 多参数调用
    - testCallNoArgs - 无参数调用
    - testTypeMismatch - 类型不匹配处理
    - testDirectFromLisp - 直接使用 fromLisp 静态方法
    - testIntFromFloat - Float 转 Int
    - testFloatFromInt - Int 转 Float
    - testStringFromSymbol - Symbol 转 String
    - testComplexScenario - 复杂场景综合测试
  - 测试覆盖：所有 228 个测试通过（新增 12 个）
  - 代码文件：
    - `src/bridge/lisp_deserializable.cj` - 接口定义和实现
    - `src/interpreter.cj` - call 方法实现
    - `src/reverse_call_test.cj` - 测试套件
  - 设计要点：
    - 使用接口扩展而非直接扩展，确保跨包可见性
    - 接口定义在 bridge 包，扩展也在 bridge 包
    - 通过 `<: ExtendLispValue` 实现接口扩展
    - 类型转换支持自动类型提升（Int ↔ Float）

- 2026-01-27: **实现 evalFile 公共 API** ✅
  - 在 interpreter.cj 中实现 evalFile(filePath) 方法
  - main.cj 的 runScript() 改为调用 interpreter.evalFile()
  - 提供公共 API 供仓颉代码直接调用解释器执行文件
  - 添加单元测试 testEvalFile()
  - 测试覆盖：所有 216 个测试通过（新增 1 个）
  - 代码复用：删除 main.cj 中重复的文件读取逻辑

- 2026-01-27: **错误类型系统实现** ✅
  - 将错误从字符串改为专门的 `LispValue.Error` 类型
  - 修改 `src/types/types.cj`:
    - 添加 `ErrorType` 枚举（8 种错误类型）
    - 创建 `XispError` 类（实现 `ToString` 接口）
    - 在 `LispValue` 中添加 `Error(XispError)` 变体
    - 实现 `operator func ==` 支持 ErrorType 比较
  - 修改 `Environment`:
    - `lookup()` 对未定义变量返回 `Error` 而不是 `Nil`
    - 添加 `has()` 方法检查变量是否存在
  - 修改 `src/core/eval_core.cj`:
    - `eval()` 的 Symbol 处理传播错误
    - `evalFunctionCall()` 区分变量和函数错误
    - `evalCons()` 处理宏查找错误
  - 错误迁移（54 处）:
    - `eval_special_forms.cj`: 6 处
    - `eval_pattern_match.cj`: 2 处
    - `eval_macro.cj`: 11 处
    - `eval_module.cj`: 20 处
    - `eval_higher_order.cj`: 2 处
    - `builtin_arithmetic.cj`: 2 处
    - `bridge.cj`: 15 处
  - 测试更新：更新 214 个测试用例以支持新的错误类型
  - 测试覆盖：所有 214 个测试通过
  - 影响：错误成为一等公民，可编程处理，类型安全

- 2026-01-27: **修复代码中反斜杠导致的内存溢出 bug** ✅
  - 问题：在代码中使用 `\n` 等转义字符导致内存溢出
  - 根本原因：lexer 没有处理 `\` 字符，导致 parser 无限循环
  - 修改 `src/parser/lexer.cj`:
    - 在 `nextToken()` 中添加 `\` 字符处理
    - 返回 `Token.Error("Syntax error: escape character '\' outside string literal")`
  - 修改 `src/parser/token.cj`:
    - 添加 `Error(String)` token 类型
    - 更新 `operator func ==` 处理 Error 比较
    - 更新 `toString()` 返回错误信息
  - 修改 `src/parser/parser.cj`:
    - 在 `parseExpression()` 中处理 Error token
    - 转换为 `LispValue.Error(XispError(SyntaxError, message))`
  - 修改 `src/interpreter.cj`:
    - `evalMultiple()` 在每个表达式求值后检查并打印错误
  - 修改 `src/cli/main.cj`:
    - 简化 `runScript()` 错误处理（避免重复打印）
  - 添加单元测试：`src/parser/parser_test.cj:testEscapeCharOutsideString`
  - 测试覆盖：所有 215 个测试通过（新增 1 个）
  - 影响示例：`(f 1 2)\n(print x)` 现在正确报告 4 个错误，不再崩溃

- 2026-01-27: **命名参数和默认值实现** ✅
  - 实现 Common Lisp 风格的命名参数（&key）和默认值功能
  - 修改 `src/types/types.cj`:
    - 将 `ParamInfo` 类移到 types.cj（LispValue enum 之前）
    - 添加 `isKey: Bool` 和 `defaultValue: ?LispValue` 字段
    - 添加 `ProcedureFromParams` 和 `MacroFromParams` 枚举变体
    - 更新 `cloneValue`, `isProcedure`, `isMacro`, `toString` 方法
  - 修改 `src/core/eval_macro.cj`:
    - 移除 `ParamInfo` 定义（移到 types.cj）
    - 修改 `extractParamsWithRest` 支持：
      - 纯可变参数（Symbol 处理 `(func . args)`）
      - `&key` 标记进入命名参数区域
      - `(param default)` 语法解析默认值
    - 添加 `hasKeyOrDefaultParams` 辅助函数
    - 更新 `evalDefmacro` 支持命名参数
  - 修改 `src/core/eval_special_forms.cj`:
    - 添加 `hasKeyOrDefaultParams` 辅助函数
    - 修改 `evalLambda` 使用 `extractParamsWithRest` 并求值默认值
    - 修改 `evalDefine` 支持命名参数
  - 修改 `src/core/eval_higher_order.cj`:
    - 修改 `evalArguments` 不求值 `:keyword` 符号
    - 实现 `ProcedureFromParams` 的完整参数绑定逻辑（三阶段）
    - 移除调试 println 语句
  - 修改 `src/core/eval_core.cj`:
    - 更新 `evalFunctionCall` 使用模式匹配处理新旧类型
  - 添加单元测试：`src/modern_test.cj:testKeywordParameters`（6个测试用例）
  - 文档更新：`docs/syntax/01-basics.md` 添加"高级参数特性"章节
  - 示例文件：`examples/02-intermediate/06_keyword_args.lisp`
  - 测试覆盖：213 个单元测试全部通过（新增 1 个测试）
  - 向后兼容：保留旧的 `Procedure` 和 `Macro` 类型

- 2026-01-27: **match HashMap 模式匹配实现** ✅
  - 实现 match 表达式中的 HashMap 模式匹配功能
  - 修改 `src/core/eval_pattern_match.cj`:
    - `evalMatch()` - HashMap 模式路由（lines 100-107）
    - `matchPattern()` - HashMap 模式检测（lines 369-381）
    - `matchHashMapPattern()` - HashMap 模式匹配实现（lines 526-598）
  - 修改 `src/core/eval_helpers.cj`:
    - `extractHashMapBindings()` - 返回类型改为 `ArrayList<(String, LispValue)>` 以支持常量匹配
  - 添加单元测试：`src/modern_test.cj:testMatchHashMapPattern` (7个测试用例)
  - 集成测试：`lisp-tests/match_hashmap_test.lisp`
  - 关键修复：将 HashMap 模式的 `areAllPatterns` 从 `false` 改为 `true`
  - 测试覆盖：210 个单元测试全部通过（新增 1 个测试）
  - 更新 UNSUPPORTED_FEATURES.md：match HashMap 模式从不支持移到已实现
  - 不支持功能从 2 个减少到 1 个

- 2026-01-27: **HashMap 解构实现** ✅
  - 实现 let 表达式中的 HashMap 解构：`(let [{:key1 var1 :key2 var2} hashmap-value] ...)`
  - 修改 `src/core/eval_helpers.cj`:
    - `isHashMapDestructurePattern()` - 检测 `(hashmap (quote :key) var)` 格式
    - `extractHashMapBindings()` - 从解析后的 HashMap 模式中提取键值对
    - `processHashMapDestructure()` - 从 HashMap 值中获取并绑定变量
  - 添加单元测试：`src/modern_test.cj:testHashMapDestructuring` (8个测试用例)
  - 集成测试：`lisp-tests/hashmap_destruct_test.lisp`
  - 代码优化：减少 match 嵌套层级（从 11 层降到 6-8 层）
  - 测试覆盖：209 个单元测试全部通过（新增 1 个测试）
  - 更新 UNSUPPORTED_FEATURES.md：HashMap 解构从不支持移到已实现
  - 不支持功能从 3 个减少到 2 个

- 2026-01-27: **shebang 支持** ✅
  - 实现 Lexer.skipShebang() 方法，跳过 #! 开头的行
  - 支持可执行的 .lisp 脚本文件
  - 添加 4 个 shebang 单元测试（通过率 100%）
  - 测试脚本：lisp-tests/test_shebang.lisp
  - 更新 task.md：标记 shebang 支持已完成
  - 学习仓颉多行字符串字面量语法（cangjie-mem 记忆 ID: 175）

- 2026-01-27: **宏的纯可变参数 Bug 修复** ✅
  - 修复解析器：`src/parser/parser.cj:93` - 正确解析点号
  - 修复求值器：`src/core/eval_core.cj:169-172` - 正确处理宏调用
  - 添加单元测试：`src/modern_test.cj:testMacroRestParameters` (5个测试用例)
  - 优化测试：将不严谨的 `.contains()` 断言改为精确匹配
  - 测试覆盖：208 个单元测试全部通过
  - 更新 UNSUPPORTED_FEATURES.md：
    - 宏的纯可变参数从不支持列表移到已实现列表
    - 添加修复历史记录
    - 不支持功能从 4 个减少到 3 个

- 2026-01-27: **可变参数和宏系统** ✅
  - 实现 lambda 可变参数（. rest 和 &rest rest）
  - 实现 define 函数可变参数
  - 实现 ,@ (comma-at) 拼接功能
  - 实现 eval 特殊形式
  - 修复 macroexpand 返回语法树
  - 测试覆盖：207 个单元测试全部通过
  - 发现宏的纯可变参数 bug（待修复）
  - 更新 UNSUPPORTED_FEATURES.md（4 个不支持功能 + 1 个设计限制）
  - 记录命名参数和默认参数特性到项目记忆

- 2026-07-07: **Git 工作流改为 Feature Branch + Squash Merge** ✅
  - 新功能开发在 feat/ 分支进行
  - 合并到 master 使用 squash merge
  - 提交信息遵循 Conventional Commits
  - CLAUDE.md 转换为 AGENTS.md（opencode 格式）
  - 为函数添加 :Unit 返回类型注解以兼容新版仓颉编译器

- 2026-01-24: 更新异步支持为 spawn + Future<T> 模型
- 2026-01-24: 删除 std.collection 模块（Lisp Cons 已够用）
- 2026-01-24: 添加异步支持任务（await/await-all）
- 2026-01-24: 模块系统基础完成，采用精确版本管理
- 2026-01-24: 移除版本范围语法，保持设计简单
- 2026-01-23: 模块系统测试全部通过
- 2026-01-22: 现代化语法和桥接层完成
- 2026-01-21: 核心 MVP 完成

## M9: 字节码调用链修复 + 性能验证 ✅

- [x] 定位性能瓶颈：直接函数调用不经 VM 帧切换（Str(name)→env.lookup→Closure→AST 解释）
- [x] 帧切换接入 callFunction：Str(name) 预解析 + Closure 统一帧切换
- [x] 帧布局重构：局部槽区与栈区分离（CompiledFunction.localSlots/globalSlots），
      消除栈/槽重叠导致的寄存器污染
- [x] 调用结果槽修复：函数调用结果落在调用点函数槽，保证 (f a)(g b) 等操作数毗邻
- [x] savedFrames 保存/恢复 currentEnv + 调用点槽（嵌套返回环境正确）
- [x] 子求值器继承 compiler/vm（宏展开/回退 AST 的子树可再编译进 VM）
- [x] 嵌套 VM 执行安全化（executeFunc 状态保存/恢复）
- [x] 正确性修复：命名 let 不再静默编译（回退 AST）、set! 回退 AST（保持修改语义）、
      多参数算术左折叠、globalRefs 实时解析（set!/重新定义可见）、宏槽保留名字
- [x] 新增单元测试：src/bytecode_test.cj（11 用例，覆盖直接/间接调用/宏/嵌套/named let）
- [x] 重写 lisp-tests/benchmark.lisp（旧的 define-macro/命名 let/current-time-millis 全部不可用）
- [x] 全量 325 单元测试通过
- [x] 性能：AST 2038ms → 字节码 109ms（~18.7x）

**完成**: 2026-08-25
**分支**: feat/bytecode-compiler

## M7: AST 求值器性能优化（拆箱快速路径） ✅

- [x] 算术特殊形式化：`+ - * / < > =` 从函数调用提升为特殊形式
  - 跳过 env.lookup / funcChecker / pushFrame / NativeFunc 分派 / evalArguments
- [x] 二元 Int 快速路径：`(+ a b)` 两参均为 Int 时直接 Int64 计算，零中间分配
- [x] `isNativeArith` 守卫：用户重定义操作符时回退普通函数调用
- [x] 特殊形式分派重排：算术操作符移到 match 最前，减少字符串比较
- [x] evalCons 去冗余：宏检查移到特殊形式分派之后（特殊形式零 env.lookup）
- [x] eval 直接调 doEvalList（去掉中间层）
- [x] 消除重复函数 lookup（evalFunctionCallWithValue 复用已解析值）
- [x] evalBegin 单表达式快速路径
- [x] Environment 惰性初始化 keywordAliases/exportedSymbols（减少 createChild 分配）

**性能**: fib(30)+fact(20)+sum-to(500000) 22.3s → ~10.6s (~52%)
**测试**: 314 单元测试 + 全部 Lisp 集成测试通过
**分支**: feat/unboxed-stack（从 master 切出）
**待合并**: master

## M8: 求值器细节优化（match/别名/守卫） ✅

- [x] CLI 支持 `--stack-depth <N>` 参数 + `XISP_MAX_STACK_DEPTH` 环境变量
  - 栈深优先级: 命令行 > 环境变量 > 默认 1000
- [x] evalMatch 子句跳过去深克隆（getNextAndSkip）
  - 复用原链表只读遍历，避免 O(N²) 深克隆
- [x] evalListInternal 特殊形式免查别名链（trySpecialForm）
  - 先以原始符号匹配特殊形式，非特殊形式才 lookupKeyword
- [x] Environment 关键字别名链短路标志（hasKeywordAliasInChain）
- [x] evalMatch 变量绑定隔离到分支环境，消除泄漏 bug
  - 列表模式/HashMap 模式绑定不再污染外层作用域
- [x] 算术操作符守卫缓存（ArithVersionCell 共享版本号）
  - 对 + - * / < > = 的 define/set 递增版本，守卫 O(1) 缓存

**性能**:
- match 常量基准: 4.56s → ~3.0s (~34%)
- match symbol 专项: 6.57s → ~2.7s (~58%)
- fib(30): 1.98s → ~1.85s (~6%)
- benchmark 综合: ~1.92-1.96s → ~1.82-1.87s (~5%)

**修复 bug**: match 列表模式/HashMap 模式变量绑定泄漏到外层作用域
**测试**: 318 单元测试（+4 新增）+ 全部 Lisp 集成测试通过
**分支**: feat/perf-detail-opt（从 master 切出）
**待合并**: master

## M10: 字节码全语法编译 + examples 双模式验证 ✅

- [x] lambda 编译（子函数 + 变长 MAKE_CLOSURE + 单层闭包捕获）
  - 捕获环境：child of 定义环境 + 复制外层帧槽值（set! 穿透捕获环境，counter 语义正确）
  - 跨层捕获（祖父槽）编译回退 AST（运行时槽号不可达，语义保真）
- [x] set! 编译（OP_SET_GLOBAL 环境链修改 + 帧槽快照；局部直接写槽）
- [x] quasiquote 编译（OP_CONS/OP_CONCAT 段式拼接，unquote/unquote-splicing）
- [x] `->` 编译（编译期展开，map/filter/reduce/apply 特殊顺序与 AST 一致）
- [x] apply 编译（OP_APPLY 列表解包）
- [x] if-let / when-let* 编译（两者与 AST 语义对齐）
- [x] 真值语义统一：JUMP_IF_FALSE 改用 isTruthy（0 为假，与 AST 一致）
- [x] 编译纪律：只有顶层表达式尝试编译；编译失败/宏调用/回退树内禁用再编译
  （AST 局部变量不在编译帧槽中，解决 UnboundVariable 语义问题）
- [x] match 重构：Symbol 模式绑定帧槽、字面量回填修复、guard/列表模式回退 AST
- [x] 参数语义修复：&rest/&key/默认值编译回退 AST（原静默错误编译）；多参数算术左折叠
- [x] 宏调用参数 quote 化（编译路径已求值 → 恢复"值作为字面"宏语义）
- [x] 高阶回调支持 Closure（map/filter/reduce 回调编译函数）
- [x] 新增 17 个 BytecodeTest 用例；全量 347 单元测试通过
- [x] 子 agent 双模式跑全部 26 个 examples：22 个完全一致，
      4 个修复（closures/thread_macro/macro_basics/macro_advanced），
      剩余差异均为示例自身环境项（模块元数据/中文模式/桥接字符串插值示例）

**完成**: 2026-08-25
**分支**: feat/bytecode-compiler

## M11: 发版 v0.3.0 - 字节码编译器 ✅

- [x] 字节码编译器 + 寄存器栈 VM（--with-bytecode-compiler）
- [x] 全语法编译（lambda/闭包/set!/quasiquote/->/apply/if-let/when-let*/match）
- [x] 混合执行：语义边界（高阶回调/宏/&rest）回退 AST 保真
- [x] 性能：fib 14.0x vs AST（5 轮优化：match 分派/缓存/超指令/组合指令/家族化）
- [x] JIT 通路验证（fib 模板 2.5ms = 13x BC / 176x AST）
- [x] 347 单元测试 + 30 examples 双模式一致
- [x] 性能文档/基准套件/设计文档

## M12: 优化收尾 - 高阶回调进 VM ✅

- [x] 高阶回调直接进 VM：applyProcedure Closure 分支按 vm 分派 vm.callClosure
- [x] callClosure 帧切换式执行（bp=1000 布局、参数/全局引用入槽、嵌套安全）
- [x] 回调体 ~14x（fib 回调实测）；高阶循环 AST 侧为瓶颈 → 整管线 ~1.0x（性能边界已记录）
- [ ] 待办：&rest/&key 参数编译（C）| JIT 正式工程（大）

## M13: JIT 正式工程 - 设计中 ✅（J0 完成，J1 启动）

- [x] 设计草案 docs/plans/2026-08-25-jit-engineering.md（评审通过）
- [x] J0 风险验证（temp-match-bench 实测）：@C helper 取址/调用/全局访问/参数传递 ✅；异常穿越机器码帧 ✅ catch 成功；栈错位必崩（16B 对齐硬约束）；callee-saved 不得入 JIT 持久帧
- [x] 设计修正：JIT 内部 (tag,payload) 双字表示（LispValue 布局风险消除）；慢路径 = deopt 到解释器（单一路径）
- [x] J1 翻译器 + 直接调用（7367cb8）：fib(30) 27ms = 8.3x BC；6/6 JitTest + 355/355 + 26 examples 一致
- [x] J2 增量（c972978）：全局解析 helper（自调用 FLAG 复制 / 跨函数 helper 解析）+ 入口表基建
      - 跨函数单程 JIT 正确（503/4/16）；互递归 ping-pong bug → 暂 deopt 保正确
      - 357/357 测试（+2 JitTest）；26 examples 一致；fib 27ms 保持
  - [x] J4 整数特化（INT-spec）：fib 30：27→15ms（~1.8x，资格=纯自递归 INT 函数）；357/357 + 26 examples 一致
- [x] J3 闭包共享可变捕获（7413fb1）：env-scope 61/62 + 共享扫描器；三模式 500 ✓；359/359 + examples 8→7（改前已知差异）
- [x] J5 R 形式（寄存器级，9d3a624）：fib(30) **进程内 3ms**（≤4ms 达标；BC ~400ms ≈ 140x）；fib-direct 基准 63x；370/370；examples 22/22 三模式一致
      - R 形式根因：pushReg r8-r15 编码错（0x50+12=pop 编码域）/ BIN_LC kind4-6 丢 cmp / imul 缺 REX.RB / 自调用 arity 缺参（v1 同修）/ 固定值池映射替代动态 allocVal / 纯 rax ABI
- [x] J6 收尾（748774c + 本轮）：J2b 互递归交叉调用 + R 形式 k≥2
  - [x] J2b 互递归（入口表直连 + 冷启动 deopt）：f↔g 深度互递归全机器码（deopt 计数 ≤2）；376/376；examples 22/22；fib 60.7x 未回退
        - 根因归档：movR11R9 方向反（4D 89 D9=mov r9,r11，call r11 用陈旧 site id → pc=0x3）/ deopt-bail 握手（test rax,rax 漏检非零 id → 改 cmp rax,1024; jb bail + 独立 bail 桩）/ v1 桥缺 arg 类型守卫（Float 静默垃圾值）
  - [x] R 形式 k≥2（rbx 入池容量 5-k + phantom 函数值）：sum-to/gcd2 类 k=2 尾递归累加器 R 形式正确；k=3 池=2 < 需求 3 恒不可行（文档化）
  - [x] 深机器码递归宿主栈限制修复（栈守卫：per-invoke rsp 预算单元 + R bail 标志旁路 + rsp 探针桩）：
        - 机器码调用点前 cmp rsp,[预算单元]; jb deopt/bail；预算=桥实测 rsp-48KB（~125KB 可用留 ~77KB 安全）
        - R 返回裸 Int 不能用 <1024 级联（合法小值误判）→ mmap bail 标志旁路；generic/v1 走既有级联
        - 实测 sum-to(5000)/互递归 f(3000) 从 SIGSEGV/StackOverflow → 优雅 deopt 正确结果；fib(30) 3ms 无回退
        - 取舍：深递归 deopt 后 VM 顶层重解释 → O(n×预算) 重算（正确，浅递归 0 bail 不受影响）
  - [x] 语义审计（unbox 绑定模式坑 + 大常量 imm32 + R/v1 返回类型，8ff10be + 本轮）：380/380；examples 22/22；perf fib 61.7x 未回退
  - [x] 三模式性能对比文档（§0）+ JIT 条件编译限定 + terminal Windows 移植修复（本轮）：
        - 文档：AST/BC/JIT 对比（fib(30) 2.75s/0.40s/3ms；15 场景基准表）
        - JIT 用 @When[os=="Linux" && arch=="x86_64"] 限定：非 x86 编译 stub（mmap/@C/机器码执行排除），--with-jit 自动降级
        - 交叉编译验证：cjpm build --target x86_64-pc-windows-gnu 全项目成功（JIT stub 生效、mmap 缺席）
        - terminal 包 Windows 修复：CPointer(0) 构造 / foreign 调用缺 unsafe / 位运算优先级(a&b)!=0 / while(true) 尾部补 None
        - unbox 用 match{case JIT_TAG_* =>} 恒走第一分支（Cangjie 绑定模式）→ 非 INT 结果错解为 Int → 改 if/else；暴露 generic arity 守卫缺失（checkSupported 保守拒绝 + emitCall deopt 兜底）
        - mov r64,imm32 符号扩展：|常量|>2^31-1 静默错值（3000000000→-1294967296）→ 三形式资格拒绝超范围常量
        - R/v1 返回 Boolean/Nil 被错解为 Int（三模式不一致）→ returnsNonInt CFG 类型跟踪拒绝非 Int 返回，落 generic 保类型

---

## M5: 工作区化拆分（阶段 1）✅

把单模块包拆成 6 个 cjpm 工作区成员，让嵌入式宿主不再被 REPL / CLI / 示例拖累。

| 目录 | 模块 | 包 | 发布 |
|---|---|---|---|
| `core/` | `xisp` | `ystyle::xisp.*`（**不变**） | ✅ |
| `terminal/` | `xisp_terminal` | `ystyle::xisp_terminal` | ✅ |
| `repl/` | `xisp_repl` | `ystyle::xisp_repl` | ✅ |
| `cli/` | `xisp_cli` | `ystyle::xisp_cli` | ❌ 可执行包 |
| `xisp_examples/` | `xisp_examples` | `ystyle::xisp_examples.*` | ❌ |
| `xisp_tests/` | `xisp_tests` | `ystyle::xisp_tests` | ❌ |

**要点**

- [x] core 模块名保持 `xisp` → `ystyle::xisp.*` 包名不变，下游消费者的 import 几乎零改动
- [x] `runREPL` / `runSimpleREPL` 从 `LispInterpreter` 移出，改由 `xisp_repl` 提供 `runRepl` / `runSimpleRepl`
- [x] **core 变成零外部依赖**：`XISP_VERSION` 与 banner 一并移到 `xisp_repl`
      （`cjpminfo` 需 ≥1.2.1：1.2.0 在工作区下从 PWD 定位清单，命中根的 `[workspace]` → `@ModuleVer` 展开成空串）
- [x] 测试两层：包内白盒测试跟源码目录（可测该包 `internal`）；16 个 root 包测试迁入 `xisp_tests`（只用 public API）
- [x] cjpm-ws 接入：`workspace.toml`（产品版本 0.5.0，`xisp_cli` 排除发布），`cjpm ws check` 通过
- [x] 构建 CI：`.github/workflows/ci.yml`（cjpm-ws 闸门 + build + test）

**验证**：`cjpm build` 通过；`cjpm test` **427 / 427**；产物 `ystyle::xisp_cli` + 10 个 examples 可执行件；
冒烟 `(println (+ 1 2))` → `3`；CLI / REPL banner 版本号显示 0.5.0。

**设计**：`docs/plans/2026-09-27-workspace-modularization.md`（**已实施并随 0.5.0 发布，现冻结为记录**）
`xisp_bc` 的落地见该文档 §3.2；通用 kit（`bytecode_kit`）的演进路径见 §3.3 与 `2026-08-25-bytecode-fw-design.md`。

**待收口**：跨仓 `atelier/cli` 的 4 处 `ystyle::xisp.terminal.*` → `ystyle::xisp_terminal.*`（并升 0.5.0）；
`cordis_xisp` 仅升版本。

**完成时间**: 2026-09-28

---

## M6: 字节码后端拆分（阶段 2）✅

把字节码编译器 / 寄存器栈 VM / JIT 从核心拆成**可选后端模块**：宿主不依赖它，就不会引入这部分代码。

**形态**：新成员 `xisp_bc`（依赖核心）；**IR 留在核心**（`CompiledCode` / `CompiledFunction` / `OP_*`）
作为核心与后端之间的协议；核心定义 `ExecutionBackend` 接口，后端实现并注入。

**要点**

- [x] 接缝 `ExecutionBackend`（`compile` / `execute` / `callClosure` / `setInterruptChecker`）：核心侧仅 **6 处**引用点
- [x] 注入点 `LispInterpreter.setExecutionBackend`；`withBytecodeCompiler()` / `withJit()` 移入 `xisp_bc`
- [x] JIT 与 VM **同模块** → 不需要跨模块执行钩子（`JitRuntime.shared` / `functionTable` 均为模块内访问）
- [x] `OP_*` 常量加 `public`（IR 是协议）；`Evaluator.evalBegin` 加 `public`（VM 回退 AST 时按 begin 语义求值）
- [x] 破坏性变更：`setCompiler` / `setVM` → `setExecutionBackend`；两个选项的 import 来源变为 `ystyle::xisp_bc`
- [x] 通用 kit（`bytecode_kit`）留作演进：需 IR 泛型化 + VM 指令语义外置（139 条指令 / ~470 行值语义方法）

**验证**：`cjpm build` 通过；`cjpm test` **427/427**；`cjpm ws check` 通过（拓扑序 `xisp → xisp_bc → …`）；
只依赖 `ystyle::xisp` 的探针产物中 `Bytecode:` / JIT 字面量均为 **0**（CLI 对照 1 / 5）。

**完成时间**: 2026-09-28

---

## M14: 桥接反向调用与命名空间统一 ✅

修掉桥接层三处长期存在的问题：反向调用的集合实参、命名空间两套拼写、文档与代码不一致。

**1. 反向调用 `call` 的集合实参（真 bug）**

- 现象：`call("sum-list", [ArrayList<Int64>([1,2,3,4])])` → `[TypeError] Attempt to call a non-function value: 1`
- 根因：`call` 把 `toLisp()` 产出的实参直接拼进调用式再交给求值器；列表值（`Cons`）落在**代码位置**上被二次求值
  （`(sum-list (1 2 3 4))` 试图调用 `1`）。不是"实参被展开成多个"。
- 修法：实参**按值注入** —— 列表值包成 `(quote value)`，与 `thread-first` 既有做法一致；
  该做法在 VM / JIT 下同样成立（`compileQuote` 走 `OP_PUSH_CONSTANT`），已有字节码模式回归用例
- 配套：`extend LispValue <: LispConvertible`（恒等转换）。`call` 的实参是 `Array<T>`（同构），
  没有它就无法表达「一个列表 + 一个标量」的异构实参

**2. 命名空间分隔符统一为 `::`**

- 根因：**一次漏改**，不是两次决策。init 时 `Bridge.registerFuncWithNS` 与 `BridgeManager.registerWithNS`
  都是单冒号；`2ee656e`（统一命名空间为 `cangjie::`）只改了前者，后者当时嵌在 `interpreter.cj` 里被漏掉，
  文档却同步改成了 `::` —— 从此文档与代码分叉。`bf49919` 把 `BridgeManager` 原样搬出，单冒号一并带走。
  两处均**零测试覆盖**，所以当时"155 个测试通过"没能拦住。
- 修法：`BridgeManager.register` / `registerWithNS` 直接委托 `Bridge.registerFunc` / `registerFuncWithNS`
  —— 消除重复实现，顺带获得命名空间校验。`ns::name` 与标准库桥接（`cangjie::read-file`）同一约定
- 破坏性：已发布的 `ns:name` 行为改变（0.5.0 未发布，无外部影响）

**3. 文档与代码一致性**

- `ErrorType.TypeMismatch` 不存在（`ErrorType` 是 8 成员封闭枚举），文档 4 处 → `TypeError`
- 文档示例的 import 不完整（`LispValue` 在 `ystyle::xisp.types`，`asInt()` 在 `ystyle::xisp.bridge`
  的 `ExtendLispValue` 上）——**这些示例从未编译过**，正是错名能存活的原因
- 新增**可编译示例** `xisp_examples/src/bridge_demo/main.cj`，与文档「完整示例」逐行对应：
  文档里的 API 名字从此受编译校验保护

**测试**：新增 11 个用例（反向调用 7：ArrayList / Array / HashSet / 空列表 / 嵌套列表 / 异构实参 / 返回列表；
命名空间 3 + 字节码模式回归 1）。全量 `cjpm test` 通过。

**完成时间**: 2026-09-28

---

## M15: 设计文档拆分 —— 工作区化 / 能力域 / 科学计算族 📄

`2026-09-27-workspace-modularization.md` 原为一份 871 行的三合一文档（模块划分 + 运行期能力域 +
科学计算族）。模块化那部分已实施并随 **0.5.0 发布**，故按"已实施 / 未实施"拆开，各自独立演进。

| 文档 | 内容 | 状态 |
|---|---|---|
| `docs/plans/2026-09-27-workspace-modularization.md` | 仓颉侧模块划分（原 §1–§3、§5–§9） | **冻结**（0.5.0 已发布，仅作记录） |
| `docs/plans/2026-10-11-capability-domains-design.md` | 符号四层模型、可替换性、域装配、register 收敛、`External` 接缝 | **未实施** |
| `docs/plans/2026-10-11-sci-family-design.md` | 科学计算族的依赖分层、C 库、数组归属比选 | **规划中**（排很后面） |

- [x] 拆分两份新文档；原文档裁剪 871 → 438 行，§4 / §10 留为占位节
- [x] **保留原节号**：外部有 5 处引用 `§3.2` / `§3.3` / `§5.2`（CI、docs-site、两个 README、08-25 文档），改号会断链
- [x] 原文档内指向已迁出内容的引用（`§4.4` / `§4.5` / `Q4` / `Q5` / `R4` / `R5`）改为跨文档指针
- [x] 顺带纠正过期事实：0.5.0 **已发布**（原文多处仍写"建议并入 0.5.0"）、`xisp_cli` 已纳入发布（Q2）、core 去 `cjpminfo` 已完成（Q3）、消费者版本 0.4.1 → 0.5.0
- [x] 新补 `register 分层` 一节（能力域文档 §3）：三条注册路径的粒度与缺口、`registerAll` 与 `registerPure` 两份平行清单的实测差异、"能力 + 可逃逸性"主轴、`registerPure` 归宿待定
- [x] 更新 6 处仓库内引用（`AGENTS.md` / `CLAUDE.md` / `core/README.md` / `xisp_bc/README.md` / `task.md` / 08-25 文档）

**完成时间**: 2026-10-11

---

## M16: 符号分层实测 + 两级装配模型（能力域设计推进）🔍

评审"lisp 必需符号的划分是否正确"时，逐个符号实测默认环境行为，得出对能力域设计的**结构性修正**。

**实测结论**：现有三个真值来源互不一致（`docs/core.md` 清单 / `trySpecialForm` 33 条硬编码 / `registerXxx` 注册表）。
符号实为**三类**，而「不注册表达禁止」只对第三类成立：

| 类 | 符号 | 注册为值 | 一等值 | 「不注册」能否禁止 |
|---|---|---|---|---|
| ① 求值器原语 | `if` `quote` `define` `lambda` `let` `begin` `set!` `defmacro` `match` `import` `export`… | ❌ | ❌ | ❌ 从未注册，由其提供 |
| ② 控制流原语 | `and` `or` `apply` `eval` `map` `filter` `reduce` `for-each` `->`、`+ - * / < > =` | ✅/部分 | ❌/✅ 混合 | ❌ 注册了也关不掉（硬编码优先） |
| ③ 纯函数 | `cons` `car` `length` `not` `eq?` `str` `print`… | ✅ | ✅ | ✅ |

**暴露的实现问题**（已记入 `docs/bugs/symbol-layering-findings.md`，本次不改代码）：
- [ ] `funcChecker` 对①②类失效 → `withSandbox()` 的 `blockFunctions(["eval","apply"])` **实测拦不住**
- [ ] `and`/`or` 在 AST 求值器下**不短路**（注册成普通 `NativeFunc`）
- [ ] **两后端语义不一致**：BC 把 `and`/`or` 当真特殊形式（短路）；`(apply 42 …)` AST `nil` / BC 报错；
      **BC 下拉黑 `length` 仍返回 2**（VM 不查 `funcChecker` → 沙箱绕过）
- [ ] 静默吞错：`(apply 42 '(1 2))` → `nil`、`(map 非函数 …)` → `(nil nil)`

**设计推进**（已写入能力域文档）：
- [x] §3.5 修正 §3.4 的硬约束（"注册集合"只覆盖 1/3 符号）
- [x] §3.6 符号归类（①语言原语 / ②控制流原语 / ③纯函数），明确①②**不可移除但可携带宿主策略**
      —— 既有先例：`import` 是①类，但已接 `moduleLoadChecker`，`withNoModuleLoad()` 实测能拦
- [x] §3.7 **两级装配模型**：`LispCapability` 是 L1（core 内置域）与 L2（宿主自定义域）共用的同一抽象；
      `registerAll`/`registerPure` 重新表述为**预置组合** `PRESET_FULL`/`PRESET_PURE`；
      域新增 `escapable` / `hostPolicy` / `after` 属性，`PRESET_PURE` 由属性**推导**而非手写清单
- [x] §5.1 域模型从三要素扩为五要素；`install(interp)` → `install(ctx: CapabilityContext)`
- [x] §9 实施顺序新增**接缝 0：符号归类收敛**，排在最前（不做它，域装配只能管住纯函数）
- [x] 风险表新增 C11（②类收敛形态待定）、C12（`PRESET_PURE` 减法 vs 显式列表）

**完成时间**: 2026-10-11

---

## M17: 分层目标澄清 —— 可替换（不只是可裁剪）🔄

用户澄清分层要解决的真问题：**"除了最基本的，所有其它的都可以不要，而且同能力（或符号）core 带了，
别人可以不要，直接用宏自行做一个"** —— 即 **可替换**，对应"lisp 实现 lisp 所有"。

**可裁剪 vs 可替换**：判据不同。可裁剪只看"不注册是否生效"；**可替换还要看"core 的实现会不会被顶掉"**。

**实测（2026-10-11）**：可替换性极不均匀

| 符号 | 层 | 用户重定义后 | 可替换 |
|---|---|---|---|
| `car` `length` `not` `+` `<` | ③ | 用户实现生效 | ✅ |
| `when`（宏） | ④ | 用户宏生效 | ✅ |
| `and` | ②（伪） | 用户实现生效 —— 空环境里它 `[UndefinedFunction]`，非真特殊形式 | ✅ |
| `map` `filter` `reduce` | ② | `define` 成功但**调用仍走 core** | ❌ 顶不掉 |
| `let` `if` `quote` `begin` | ① | 完全无效 | ❌ |

**决定性事实**：`for-each` 是③类纯注册函数，却**只认 `NativeFunc` 回调**
（`builtin.cj:113`）→ 实测 `(for-each (lambda (x) x) '(1 2 3))` 返回 `nil`，**静默失败、无报错**。
而 `map`（求值器实现）能正常调闭包。**这是"用宏自行实现"的硬门槛**：
Lisp 层要复刻 `map`/`for-each`，必须能从注册函数调用任意 Lisp 函数值，
而 `applyProcedure` 是 `internal`（`higher_order.cj:323`）。
（注：纯 Lisp 自己实现 `my-map` **可行**，实测行为与 core 一致——门槛只卡在"仓颉注册的同能力函数"上。）

**设计推进（能力域文档 §3.8 新增）**：
- [x] §3.8 可替换性实测 + 四层 × 可替换性矩阵（①不可替换，②③④应可替换）
- [x] 新增两道接缝：**0a** Lisp 函数值调用入口（`ctx.invoke`，同时修 `for-each` 静默失败）、
      **0b** ②类符号"让位"机制（已由用户定义则不走硬编码快速路径）
- [x] §3.8.5 **宏包即域**：域的 `install` 两种形态 —— `register`（仓颉实现）/ `evalSource`（Lisp 源码）
- [x] `CapabilityContext` 补 `invoke` 与 `evalSource`
- [x] 风险表新增 C13（可替换的边界：静默 vs 显式声明）、C14（②类让位对热路径性能的影响）、
      C15（`evalSource` 与沙箱 eval 禁令的关系：安装期 vs 运行期）
- [x] `docs/bugs/symbol-layering-findings.md` 补 §4.5 可替换性实测与 `for-each` 问题

**完成时间**: 2026-10-11

---

## M18: 能力域文档按「可替换」首要目标重构 📐

按首要目标（**可替换**，而非可裁剪）重排 `2026-10-11-capability-domains-design.md`，765 → ~690 行。

**重构前的问题**：
- 首要目标埋在 §3.8（文档接近尾声），读者要从"register 分层"读起才知道要解决什么
- **三套重叠分类法并存**：§3.5 的 A/B/C、§3.6 的 ①②③、§3.8.4 的四层 —— 同一批符号三套叫法
- **存在自相矛盾**：§3.5 写"`funcChecker` 对 A/B 类失效"，但 §3.8.1 的复测已证明算术符号有效

**重构后结构**：
- **§1 首要目标：可替换** —— 目标阶梯（可不要 / 可替换）、可替换性实测、空环境判别法、两个缺口
- **§2 四层模型** —— **全文唯一的符号分类法**（①语言原语 / ②求值器拦截 / ③注册函数 / ④Lisp 层宏），
  含"①的边界就是 lisp 实现 lisp 的边界"、"①也可携带宿主策略"、目标态
- **§3 接缝设计** —— A（函数值调用入口）/ B（②类让位）/ C（CapabilityContext）
- **§4 域装配模型** —— 五要素、install 两形态、preset、属性推导、域清单、命名冲突
- **§5 register 收敛** —— 三路径现状、两份清单差异、**硬约束及其边界**（只对③类有效）、funcChecker 缺口
- **§6 外部依赖域 / §7 外部值接缝** —— 与首要目标正交，降为后半
- **§8 接缝清单与实施顺序** —— 接缝重新编号 0/0a/0b/1/2/3/4/5，实施顺序以可替换优先
- **§9 风险** —— 按主题分三组（首要目标 / 域装配 / 外部依赖）

**同步**：
- [x] 消除三套分类法，统一为四层；删除矛盾表述
- [x] 修正 09-27 冻结文档与 sci 文档里指向本文旧 §号的引用（§3–§5 → §2/§4/§5；§5→§4；§5.2→§4.6；§6→§6.1；§7、§8 → §7 + §2/§3.3/§4）
- [x] 修正 `docs/bugs/symbol-layering-findings.md` 的引用（§3.4 → §5.3；§3.6 → §2/§8；§9 → §8/§9）
- [x] 补回重写时遗漏的 §4.7 命名与符号冲突（`xisp-net` 连字符、`symbols` 冲突检测）
- [x] 内部 §引用 0 处失效；跨文档引用逐条核对通过；文档链接 0 坏链
- [x] 内容完整性核对 33 项关键结论/数据/代码片段，无丢失

**完成时间**: 2026-10-11

---

## M19: R5RS 级符号分层重构 ✅

**目标**：让"除了最基本的，所有其它都可以不要，且同能力 core 带了也能被用户用宏自行替换"
（即 **可替换**，对应"lisp 实现 lisp 所有"）。

**独立工作树**：`../xisp-r5rs`（分支 `feat/r5rs-symbol-layering`），主工作树保持可用可发布。
**不做**：不 bump 版本、不 publish（原"中间发布无意义"的判断）。

### 完成情况

| 步 | 内容 | 提交 | 验证 |
|---|---|---|---|
| 1 | **`gensym`** —— 非卫生宏的唯一符号来源 | `c70b15f` | 9 个用例；200 次无重复 |
| 2 | **宏改为编译前展开** —— 消除运行时反复展开与语义偏差 | `054aa9a` | 含宏递归 **10767us → 971us**；展开次数不再随调用增长；AST/BC 8/8 一致 |
| 3 | **①收敛** —— `and`/`or`/`condb`/`if-let`/`let*`/`when-let*` 降为宏 | `978521c` `93e4aa1` | ① 由 33 条 → **10 条**；`and`/`or` 的 4 处 AST/BC 分歧归零 |
| 4 | **接缝 0a/0b** —— 函数值调用入口 + 特殊形式让位 | `23452fd` | 7 个符号可被顶替且双后端一致；顺带修 `for-each` 静默失败 |
| 5 | **域抽象** —— `LispCapability` + preset + `withDomains` | `c094677` | 9 个 core 域；`presetPure` 按属性推导；`replace` 独占装配 |
| 6 | **宏包即域** —— 16 个内建宏合并为单一 Lisp 源码 | `c979390` | `macros.cj` 286 → 150 行；`swap` 卫生性修复 |

### 关键实测（都推翻了先前判断）

1. **"降级为宏会毁掉字节码"是错的**：那是修复**前**的现象（宏在运行时展开导致整树回退 AST）。
   编译前展开后，宏版 `let` 字节码 1043us vs 内建 1011us（几乎无差），AST 模式反而更快
   （24671 vs 36978us）。
2. **宏收语法不收值**：用"辅助宏"做值层变换会栈溢出 —— `(%helper x)` 传的是符号 `x` 本身。
   需值层运算时用普通函数（`reverse`/`car`/`map`）。
3. **点对参数深一层**：`(. cs)` 收到的是子句列表，判定子句头要 `(car (car cs))`。
4. **让位必须先于编译**：否则用户 `define apply` 后 AST 走用户实现而 BC 仍按 core 编译。
5. **`for-each` 旧实现静默失败**：只认 `NativeFunc` 回调，传 `lambda` 什么都不做也不报错。
6. **域装配的边界**：即便独占装配，①语言原语与②求值器拦截**仍然可用**
   —— 它们不在环境里。域装配只覆盖③注册函数。

### 最终符号分层

| 层 | 成员 | 可替换 |
|---|---|---|
| **① 核心原语**（6） | `quote` `lambda` `if` `set!` `define` `defmacro` | ❌（这就是"最基本的"） |
| **① 编译可见形式**（4） | `let`（含解构） `match` `import` `export` | ❌ |
| **② 求值器拦截**（7） | `map` `filter` `reduce` `apply` `eval` `->` `for-each` | ✅ 让位机制 |
| **③ 注册函数** | `+ - * / < > =` `cons` `car` … | ✅ |
| **④ Lisp 层宏**（16） | `when` `unless` `condb` `if-let` `let*` `when-let*` `and` `or` `incf` `decf` `swap` `negate` `push` `pop` `do` `defun` | ✅ |

### 遗留（均已收口，见 M20 / M21）

- ~~**0e**：②类一等值化~~ → M20 完成（双轨）。
- ~~**接缝 3**（`External` 变体）与 **接缝 4**（`funcChecker` 覆盖①②类）~~ → M20 完成。
- 合并回 master：破坏性变更，需 `cjpm ws bump` 推进版本（**待决定**，当前按要求不合）。

**验证**（当时）：`cjpm build` 通过；`cjpm test` 481/481；`cjpm ws check` 通过。

**完成时间**: 2026-10-11（阶段完成；收尾见 M20/M21）

---

## M20: 兼容性修复 + 接缝 0e / 3 / 4（M19 收尾）✅

用户指定的三件（0e 一等值化、接缝 3 `External` 变体、接缝 4 `funcChecker` 覆盖），
外加回答"字节码兼容做好了吗"时新增的兼容性工作。

### 1. 字节码兼容性：新增差分测试 + 修 7 处真不兼容

**先回答"做好了吗"**：此前**没有任何测试验证两个后端语义一致**，只是被假设。
新增 `BackendDiffTest`（91 个 `.lisp` 语料逐表达式比对），一上线暴露 **726 处差异**。
现 32 个可比对文件**全部一致**（其余跳过原因见测试注释）。

| # | 问题 | 性质 |
|---|---|---|
| 1 | `println` 吞掉实参错误（AST 短路、VM 不查） | **静默算错** |
| 2 | 关键字别名（`setq`/`progn`）BC 下失效 | **静默不生效** |
| 3 | `+ - * / mod round` 类型错误静默返 `nil` | 静默 |
| 4 | `(= 2 2.0)` AST true / BC false | 语义分歧 |
| 5 | 编译闭包不被认作过程（`procedure?`/`type-of`） | 语义分歧 |
| 6 | 裸关键字符号 `:ok` BC 下未绑定 | 语义分歧 |
| 7 | `(match 1 (1 "one") …)` 字面量模式被误判为列表模式 | 语义分歧 |

另修语料自身 bug：`combination_functions_test.lisp` 缺右括号。

### 2. 接缝 4：`funcChecker` 覆盖②类

`withSandbox()` 的 `blockFunctions(["eval","apply"])` 此前**形同虚设**（实测拦不住）；
现在②类（`map`/`filter`/`reduce`/`apply`/`eval`/`->`/`for-each`）均可拦，
①语言原语刻意不拦（语法结构，拦掉语言不可用）。

### 3. 接缝 0e：②类一等值化

三条实测约束决定不能简单改成普通函数：纯 Lisp 版 `map` **500 元素爆栈**、
`apply`/`eval` 纯 Lisp 无法表达、求值器有专门迭代实现。
故用**双轨**：调用走快速路径，值位置取另注册的等价 `NativeFunc`。

### 4. 接缝 3：`External` 变体 + 协议族

`LispValue.External(LispExternal)` + 五个协议接口；`type-of` 报告实现方 typeName；
`length` 改为能力接口分派；`external?`/`external-type`。
破坏面比预估小得多（编译器只指出 2 处，其余有 `case _` 兜底）。
端到端用假扩展 `CounterBox` 验证，含双后端一致。

**验证**：`cjpm build` 通过；`cjpm test` **501/501** 通过。

**完成时间**: 2026-10-11

---

## M21: 错误传播语义统一 + 三后端差分闸门 ✅

M20 的差分测试只覆盖 AST ↔ 字节码两模式、且**逐表达式**求值（不带文件上下文）。
本轮把它扩成**三模式**（含 JIT）、补上**逐文件**层，一并暴露并修掉了一批
"错误被当成正常值"的缺陷。

### 1. 根本问题：`LispValue.Error` 是值，不是异常

错误"要不要中止"完全由各处分发代码自己决定。历史实现AST 与字节码各走一半：

| 写法 | AST（旧） | 字节码（旧） |
|---|---|---|
| `(define (f) (assert 假) 42) (f)` | `42` —— **assert 形同虚设** | `RuntimeError` |
| `(begin (error "b") 42)` | `42` | `RuntimeError` |
| `(let ((a (error "b"))) 42)` | `42` | `RuntimeError` |
| `(if 未定义变量 1 2)` | `[UnboundVariable]` | `1`（错误当真值） |
| `(+ 1 未定义变量)` | `[UnboundVariable]` | `[TypeError]` |
| `(< 1 未定义变量)` / `(not 未定义变量)` | `[UnboundVariable]` | `false` |
| `(match 未定义变量 … (else "other"))` | `"other"` | `[UnboundVariable]` |
| `(foo 1)`（foo 未绑定） | `[UndefinedFunction]` | `[UnboundVariable]` |
| `(apply 42 '(1 2))` | `nil` 静默 | `RuntimeError` |

最有用户价值的是第一行：`assert` 是**给用户做参数校验的 API**，在 AST 下校验失败后
函数继续算完并返回正常结果——保护作用为零。

### 2. 统一语义：错误一旦产生就中止

- AST：`evalBegin` 逐步检查、遇错即返；`define` / `set!` / `let` 的绑定初值出错时
  **不建立绑定**直接传播；`match` 的被匹配表达式出错即中止；`apply` 同。
- VM：`OP_POP` / `OP_JUMP_IF_FALSE` / `OP_AND_JF` / `OP_OR_JF` / `OP_NORM_FALSE` /
  `OP_STORE_GLOBAL` / `OP_STORE_LOCAL` / `OP_BIN_LC` / `OP_CMP_LC_JF` 与七个栈式
  二元算子（`+ - * / < > =`）、`OP_NOT` 全部加错误中止；`apply` 非可调用值统一成
  `TypeError`。
- 顶层**逐表达式**执行仍继续（`evalMultiple` 的行为不变）：中止的是出错的那条表达式。

### 3. 顺带查出 3 个被静默吞错掩盖的真缺陷

| # | 缺陷 | 为何一直没暴露 |
|---|---|---|
| 1 | **点对绑定只推进一个元素**，值表达式被当成下一个绑定的模式再处理 (`(let ((x . y) '(1 2 3)) x)` 报类型错误) | 错误被 begin 吞掉 |
| 2 | **函数体内 `define` 一律发 `STORE_GLOBAL`**，而符号引用走帧槽（进帧时解析，变量尚未定义）→ `(define (f) (define r 5) r)` 字节码下 `UnboundVariable` | 单测没有"函数内 define"用例 |
| 3 | `(apply 非函数 …)` 静默返回 `nil` | 其安全属性被"返回 nil 也算安全"误当成正确行为（`pure_env_test` 就 pin 了这个错误预期） |

### 4. 三后端差分闸门（`BackendDiffTest`）

- **逐文件**层：走 `evalFile`，因此相对导入、模块搜索路径、输出副作用序列都在覆盖内。
- **逐表达式**层：把源码切成顶层表达式逐步比对，差异能定位到"第 N 步"。
- 归一化：错误只比**种类**（文案由不同代码路径产出），并剥掉调用栈
  （AST 有、字节码没有——诊断信息完整度差异，不是语义差异）。
- 结果：**63 个文件逐文件一致 / 39 个文件逐步一致**，跳过 28 个（perf 基准、
  REPL 演示、`module.lisp` 元数据清单——都是语料自身性质，不是覆盖缺口）。

### 5. 接缝 3 的协议族从"只有定义"变成"真有分派"

`docs-site` 同步时发现五个能力接口里四个只是定义、无人调用。本轮接通：

| 接口 | 消费点 |
|---|---|
| `LispExternalEquatable` | `=` / `!=` |
| `LispExternalComparable` | `<` `>` `<=` `>=`（未实现时恒 false，不能当"相等"） |
| `LispExternalIndexable` | `length` + 新增的 `nth` |
| `LispExternalCallable` | 调用形式 `((ext) args…)`（AST 与 VM 两侧） |

### 6. 其它

- `ErrorType` 新增 `name()`：此前只有 `XispError.toString()` 里有份内联 match，
  宿主按种类分发时无处可取；`toString` 改为复用它，避免两份口径漂移。
- 新增 `nth`（列表 + 外部值）；`length` 语义**保持**"非列表返回 0"（有意设计）。

### 7. 后续追加修复（同一轮）

补 `OP_CHECK_ERR` 让字节码在**实参求值前**先查函数值：AST 是「先算函数值、
出错立即返回」，实参根本不求值；字节码原先把实参里的副作用也执行了
（`(define x 1) (foo (set! x 2)) x` → AST 1 / 字节码 2）。JIT 两侧把该指令
视为空操作（v1/R 形式的资格判据本就要求全局引用均为自身，检查恒不成立），
资格表与 emit 分派同步更新，否则整段函数会失去 JIT。

同时修正差分测试的输出捕获：`setOutputFn` 只接管解释器自身日志，Lisp 的
`println` 是环境里的 `NativeFunc`、直接写 stdout —— 改为覆盖环境绑定后
文件层才真正比对到输出序列（此前"63 个文件全一致"里输出根本没参与比对）。

全量语料三后端**归一化后零语义差异**。

**验证**：`cjpm build` 通过；`cjpm test` **535/535** 通过；`bash lisp-tests/check-examples.sh`
22/22；`cjpm ws check` 通过。

**完成时间**: 2026-11-06（在 `feat/r5rs-symbol-layering` 分支，未合并 master）

---

## M22: 收尾审计 —— 5 个「看起来在跑、实际没跑」的东西 ✅

合入前的完整性审计。逐项都是"存在且看似正确，实际未生效"的同一类问题。

### 1. `lisp-tests/perf/run.sh` 二进制路径又写的是旧包名

`BIN=./target/release/bin/ystyle::xisp.cli`（工作区化前的名字），文件不存在 →
每次调用都是瞬时失败 → 整张表输出 **0~1 ms**。`set -u` 挡不住（不是未定义变量）。
**与 `check-examples.sh` 是同一个 bug 的第二次出现**：改包名时漏了一个脚本。
修法：路径改正 + **加二进制存在性闸门**（`exit 2`）。修好后真实数字
（01-fib-direct）：AST 21779ms / BC 3785ms / JIT 40ms。

### 2. 相对文件导入不尊重 `export`

`(import "./utils.lisp")` 把被导入文件的**所有** `define` 灌进调用方环境，
于是 `(export processData)` 里没列出的 `internalHelper` 照样能调用。
更微妙的是根因：`evalExport` 只往 `currentModule` 记，而相对导入路径**不设**
`currentModule`（它只对模块导入有意义），于是 `getExportedBindings()` 恒为空，
加载方退回"全都导出"分支。
修法：`evalExport` 同时记进 `env.exportedSymbols`；`loadRelativeFile` 改为在
独立 child env 加载后按白名单绑出（与目录包导入、模块导入一致）。
**兼容保留**：文件里一个 `export` 都没写时仍视为全公开（老脚本不受影响）。
新增 2 条用例（`testRelativeFileImportRespectsExport` / `...WithoutExportStillWorks`）。

### 3. 5 个语料用了语言不存在的 `try/catch`

`grep -rn '"try"' core/ xisp_bc/` 全仓零实现。于是那 5 个文件里
`(try … ) catch (e) …` 展开成 4 条错误，而 `"✓ 正确：…"` 是**无条件打印**的——
**无论被测行为对不对都会打勾**。这些是死测试代码。
修法：改成不捕获、直接执行，靠 stderr 里的错误种类作为证据
（`check-examples.sh` 收 stderr；`xisp_tests` 断言错误种类）。

### 4. 差分测试跳过 perf 的理由写错了

注释说"输出含耗时，本身不稳定"。实测 15 个 perf 语料**两两重跑哈希一致**，
脚本内根本不打印时间（耗时靠外部 `time`）。真实理由只是**太慢**
（01-fib-direct 单次 AST 15~20 秒）。已按事实改写注释，并把 `benchmark.lisp`
纳入差分（它跑 0.9 秒、输出确定）。

### 5. 文档状态与事实脱节

- 能力域设计文档状态写"**未实施**"，实际全部完成。
- `AGENTS.md` 写 `cli` "不发布"，而 `workspace.toml` 明确"cli 也可分发"、
  中心仓**已有** `ystyle::xisp_cli 0.5.0`。
- `docs/design.md` 7 处 `Number(...)` 是早已删除的变体。
- `first_class_values.cj` 两处注释指向不存在的 `first_class_owner.cj`。

### 6. 结果

`cjpm test` **537/537**、`check-examples.sh` 22/22、三后端差分
**64 个文件逐文件一致 / 40 个文件逐步一致**、`cjpm ws check` 通过。

**完成时间**: 2026-11-06（在 `feat/r5rs-symbol-layering` 分支，未合并 master）

---

## M23: 域装配 5 处缺陷 + 两档装配收敛 ✅

对着 `coreDomains()` 与 `registerAll` **逐符号比对**发现的。这些缺陷全都只在
`replace: true`（"只留所选域"，即"可裁剪"的唯一路径）下暴露 —— 默认追加装配会把它们掩盖。

| # | 缺陷 | 后果 |
|---|---|---|
| 1 | `domainPrint` 不在 `coreDomains()` | `presetFull` 独占装配下 `println` / `print` / `display` / `newline` / `princ` 全缺 |
| 2 | `registerStringOps` 与 `registerOutput` 同挤在 `hostPolicy` 的 `print` 域 | `presetPure` 连 `str` / `string-length` 都没有（`registerPure` 一直有） |
| 3 | `registerModernAliases` **不属任何域** | 16 个别名全缺（`first` / `rest` / `second` / `setq` / `progn` / `string-append`…），而 `domainList.symbols` 却声明了 `first` / `rest` |
| 4 | `macros.core` 标 `escapable: true` | 纯环境里 `(when #t 1)` 都不可用；宏是代码变换器、不提供逃逸手段，应为 `false` |
| 5 | `domainArithmetic.symbols` 声明了 `abs` | `abs` 从未实现（全仓无 `env.define("abs")`）——域声明是对外契约，多声明等于谎报 |

### 修法

- 拆 `domainString()`（纯）/ `domainPrint()`（hostPolicy）；新增 `domainAliases()`
  （`after: ["base","list","string"]`，把"别名最后注册"的隐式约束显式化 = C17）；
- `macros.core` 改 `escapable: false`，`after` 补 `symbols`（`swap`/`or` 展开用 `gensym`）；
- `domainArithmetic.symbols` 移除 `abs`、补 `round`；
- **`registerAll` / `registerPure` 改为 `installDomains(env, presetFull()/presetPure())` 的委托**，
  消除两份平行清单（C9 收口）。

### 新增 4 条"反向钉住"用例（`capability_domain_test.cj`）

| 用例 | 钉住什么 |
|---|---|
| `testPresetFullCoversRegisterAll` | 独占 `presetFull` 后，`registerAll` 的每个符号都必须存在 |
| `testPresetPureCoversRegisterPure` | 同上针对 pure；且宏包可用 |
| `testDeclaredSymbolsActuallyInstalled` | 每个域 `symbols` 声明都必须真的装上（防声明撒谎） |
| `testAliasesAvailableUnderExclusiveAssembly` | 12 条别名在两种独占装配下逐个可用 |

另纠正一处错误断言：原先断言 `presetPure` **排除** `macros.core`（注释称"保守判定"），
实际是语义错误。

### 接缝 5 同轮收口

外部模块的**中断义务**契约写进 `core/README.md`：`CapabilityContext.isInterrupted` 的
调用时机（元素数相关循环必须周期查）、中止形式（错误值 + `INTERRUPTED_MESSAGE`，
不是抛异常）、粒度建议，以及"不查中断宿主就杀不掉"的后果。**§8.1 的 12 条接缝至此全部完成。**

### §9 同轮收口的四条 + 一条不适用

| 项 | 结论 |
|---|---|
| **C9** `registerPure` 归宿 | 保留函数签名、内部改 `installDomains(env, presetPure())` 委托；调用方无需改动 |
| **C12** PRESET_PURE 推导 | `SimpleCapability.escapable!` 默认确为 `true`（`capability.cj:127`），新增域默认落在 pure 之外；同时发现推导把 3 个纯域误判（见上表 #2/#4） |
| **C15** `evalSource` 沙箱影响 | 属**宿主安装期**，不经过脚本侧权限检查；用例 `testEvalSourceInstallSurvivesSandbox` 钉住"沙箱下 `(eval …)` 被拦、`when`/`unless`/`incf` 正常" |
| **C17** 域 `after` 完整性 | 显式化：`domainAliases()` + `macros.core` 补 `symbols` 依赖 |
| **C7** stdx 环境变量 | **不适用**——全工作区无 stdx 依赖（核实 7 个 `cjpm.toml`） |

### C6 / C8 也落了（原为"建议项"）

**C6 —— `LispValue` 新增变体的编译期闸门**：不改 lint（静态判定"哪些 `match` 作用在
`LispValue` 上"不可靠），改用**编译期强制**：`xisp_tests/src/lispvalue_matrix_test.cj`
的 `variantTag` 对 16 个变体**穷尽匹配、无 `case _`**。将来加变体 → 该文件编译失败 →
作者必须先登记新变体并回答横切语义，才有机会去核对那些 `case _` 兜底处。

它**上线即查出**一个真缺陷：`type-of` 对 `Error` 变体返回 `"unknown"`（已修，报 `"error"`）。
另附三条横切契约：显示非空、`cloneValue()` 保形、`type-of` 不得为 `"unknown"`；
以及真值矩阵（只有 `nil`/`false`/`0`/`0.0` 为假，`External` 恒真）。

**C8 —— 成员配置闸门**：新增 `scripts/check-members.sh` 并接进 CI（`Member config gate`）。
只查 `cjpm ws check` **管不到**的部分：

- `version` 与 `workspace.toml` 一致、各成员 `cjc-version` 一致；
- 库成员（`static`）**禁用 `[replace]`**（工作区硬规则：只对本仓生效，下游照样冲突）；
- 平台相关配置（`bin-dependencies` / `standard-library` / `link-option`）必须在
  `NATIVE_ALLOWED` 白名单内（当前为空：xisp 零原生依赖）；
- 发布成员：`include` 显式且**锚定**（0.4.0 就栽在未锚定/漏配，制品把整个 docs-site 打进去）、
  `license`/`authors`/`repository`/`description` 齐全、目录内有 `README.md`。

已用两处**故意破坏**验证确实会拦（把 `include` 改成未锚定 → exit 1；给 `core` 加 `[replace]` → exit 1）。

### C1 域边界对齐（2026-11-08）

**结论：C1 之前不是"待宿主诉求"，而是根本还没开始。** 域边界与注册器边界不一致，
导致"声明 ≠ 安装"，粒度无从谈起。

实测（只装单个域，不是 `presetFull`）：

```
只装 arithmetic 域： (sum '(1 2)) => UndefinedFunction   ← 声明里有 sum
只装 list 域：       (sum '(1 2)) => 3                   ← sum 实际挂这里
```

| # | 错位 | 修法 |
|---|---|---|
| 1 | `sum`/`product`/`max`/`min` 声明在 arithmetic，随 `registerHigherOrder` 挂在 list | 拆出 `registerAggregates`，归 arithmetic |
| 2 | `reverse`/`range` 与聚合混在 `registerHigherOrder` | 拆出 `registerListExtras`，归 list |
| 3 | `not`/`eq?` 与 14 个类型谓词同挤 logic | 拆成 `logic` + 新域 `predicates` |
| 4 | `first`/`rest` 声明在 list 域，实际由 aliases 域注册 | 从 list 声明移除 |
| 5 | `hashmap?` 由 `registerHashMap` 注册却不属任何域声明 | 归 hashmap 域；同时补齐该域漏声明的 9 个 Redis 别名 |

域数 **12 → 13**。护栏：`testEachDomainDeclaresTruthfully` —— **每个域单独安装
（连同 `after` 依赖的传递闭包）后，其 `symbols` 必须全部可用**。它上来就抓出第 5 条；
此前只测"全装 `presetFull`"，A 域声明、B 域注册的错位在里面照样通过。

C1 现在能回答的部分：

- 粒度**只作用于③类注册函数**。①原语与②求值器拦截（`map`/`filter`/`reduce`/`apply`/
  `eval`/`->`/`for-each`）**不属任何域**，域装配动不了 → 原始例子"不要高阶函数"**做不到**，
  根因在②类（§5.3 已记录的边界），不是粒度。
- 能做的现在成立了：只装 `arithmetic` 确实没有 `sum/max`，只装 `list` 确实没有聚合，
  只装 `logic` 确实没有谓词。
- 真实诉求现状：下游 `atelier`（`ptc/runtime.cj:88`）与 `cordis_xisp`（`runtime.cj:184`）
  都只用 `registerPure(env, output:)` 一档，**没有宿主在挑域**。所以"要不要更细"仍无驱动；
  但两档已等价，将来换 `withPreset(presetPure()` 不会变行为。

### 范围内仍未做（均需外部输入）

- **C1 的更细拆分 / C2 / C10**：是否把 `arithmetic` 再拆 `math`、`predicates` 按族拆
  （需真实诉求，多一个域宿主就多记一个名字）；域装配与 `ExecutionBackend` 接缝合并
  "成熟后再说"；网络域待调研。
- **C3 / C5**：`External` 专用变体归属、符号命名策略 —— 属下游 sci 族决策，不阻塞本次重构。

**验证**：`cjpm build` 通过；`cjpm test` **547/547**；`check-examples.sh` 22/22；`scripts/check-members.sh` 通过；
三后端差分 64 文件逐文件一致 + 40 文件逐步一致；`cjpm ws check` 通过。

**完成时间**: 2026-11-08（在 `feat/r5rs-symbol-layering` 分支，未合并 master）

---

## M24: 装配档位 `XispLevel` ✅

宿主不想逐个挑 13 个域，于是再上一层：**命名档位**。

```cangjie
public enum XispLevel { | Expr | Pure | Script }
public func withLevel(level: XispLevel): InterpreterOption
```

| 档位 | 域集合 | 说明 |
|---|---|---|
| `Expr` | `exprDomains()` = `presetPure` − `macros.core` − `symbols` | 纯计算：**无宏、无 `gensym`、无输出**，只要"算出一个值" |
| `Pure` | `presetPure()` | `Expr` + ④层宏包 + `gensym`；仍无输出 |
| `Script` | `presetFull()` + `std.io`/`std.fs`/`std.collection` | 脚本 / CLI 全量 |

### 三条不变量（均有用例）

1. **严格包含** `Expr ⊂ Pure ⊂ Script` —— 实测符号数 **92 / 109 / 114**；
2. **独占装配**：先 `clearEnvironmentBindings()` + `markDomainsExclusive()`，
   结果与"解释器默认装了什么"无关；之后再 `withDomains(...)` 只追加；
3. **与策略正交**：档位管"装哪些能力"，`withSandbox`/`withTimeout`/`withMaxStackDepth`
   管"允许做什么"，可任意叠加。

档位不维护第四套清单：`Expr` 由 `presetPure` 推导，`Pure` = `presetPure`，
`Script` = `presetFull` + 桥接。新增域只需判断属性，档位自动跟上。

### 顺带修掉 `clearEnvironmentBindings()` 的两个后遗症

都是"换环境后没把**持有环境引用的组件**一起换掉"：

| # | 后遗症 | 现象 | 修法 |
|---|---|---|---|
| 1 | 不重建 ②类一等值 | `withDomains(…, replace: true)` 后 `(procedure? map)` → `UnboundVariable`，而 `(map f lst)` 照样能跑（快速路径）→ 看着"还能用"，实际不能作值传递 | 重置 `firstClassValuesInstalled` 并按开关立即重建 |
| 2 | 桥接仍指向旧环境 | `withLevel(Script)` 后 `cangjie::exists?` 不存在，默认构造却有 | 新增 `BridgeManager.retarget(env)`，清空时同步调用 |

`reset()` 因为重建了整个 `BridgeManager` 没踩到第 2 条；`clearEnvironmentBindings()`
没重建才暴露。

### 文档

- 用户文档 `docs-site/integration/options.md`：新增「装配档位（宿主首选）」与
  **「能力域装配」**两节（后者此前**完全没有**——13 个域、两个预置、自定义域、
  `CapabilityContext`、中断义务、以及"域装配动不了①②类"的边界都是首次成文）；
  速查表补 `withLevel` / `withDomains` / `withPreset` / `presetFull` / `presetPure` /
  `withNoFirstClassValues`。
- 设计文档：新增 §4.5.1（档位三条不变量）与 §12.6（档位落地 + 两个后遗症）。

### 用例

`xisp_tests/src/level_test.cj` 6 条：档位内容 / 严格包含 / 独占装配 / 与沙箱叠加 /
`replace` 后一等值仍在 / `withNoFirstClassValues` 仍生效。

**验证**：`cjpm build` 通过；`cjpm test` **554/554**；`check-examples.sh` 22/22；
`scripts/check-members.sh` 通过。

**完成时间**: 2026-11-08（分支 `feat/r5rs-symbol-layering`，未合 master）
