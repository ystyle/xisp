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
- [ ] API 文档生成
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
