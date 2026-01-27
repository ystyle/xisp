# Xisp 任务清单

> 仓颉嵌入式 Lisp 脚本语言 - 开发进度管理

---

## 📊 总体进度

- ✅ M1: 核心 Lisp 解释器 (MVP)
- ✅ M2: 仓颉互操作桥接
- ✅ M3: 现代化语法扩展
- ✅ M3.5: 模块系统基础
- 🚧 M4: 高级特性和完善

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
- [ ] 仓颉 → Lisp 函数调用
- [x] std.io 桥接
- [x] std.fs 桥接
- [x] LispInterpreter API
- [x] 桥接层文档

**完成时间**: 2026-01-22

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
- [ ] 宏的纯可变参数 bug（bindMacroParams 修复）

### 4.2 文件和脚本支持
- [x] evalFile 实现（通过 runScript + evalMultiple）
- [x] CLI 脚本运行参数（-c 执行代码、直接运行 .lisp 文件）
- [ ] shebang 支持（Lexer 忽略 #!、简化命令名）

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
- [ ] 宏展开调试工具
- [ ] 宏卫生性检查
- [ ] compile-time 计算

### 4.7 错误处理和调试
- [ ] 错误类型系统
- [ ] 调用栈追踪
- [ ] 调试器 (debugger)
- [ ] 性能分析

### 4.8 文档和工具
- [ ] API 文档生成
- [ ] 包管理器 (cjlpm)
- [ ] 测试覆盖率报告
- [ ] REPL 增强功能

---

## 📝 待修复问题

### 高优先级
- [ ] 修复宏的纯可变参数 bug（bindMacroParams）
  - 问题：`(. args)` 只绑定第一个参数
  - 位置：`src/core/eval_macro.cj`
  - 影响：无法实现优雅的可变参数宏

### 中优先级
- [ ] 实现命名参数和默认值
  - 功能：`&key` 参数和 `(x default)` 语法
  - 复杂度：低-中
  - 参考：已记录到项目记忆

### 低优先级
- [ ] 仓颉反向调用 Lisp 函数

---

## 🔧 代码 TODO

- [ ] src/interpreter.cj:607 - evalFile 文件读取

---

## 📅 更新记录

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

- 2026-01-24: 更新异步支持为 spawn + Future<T> 模型
- 2026-01-24: 删除 std.collection 模块（Lisp Cons 已够用）
- 2026-01-24: 添加异步支持任务（await/await-all）
- 2026-01-24: 模块系统基础完成，采用精确版本管理
- 2026-01-24: 移除版本范围语法，保持设计简单
- 2026-01-23: 模块系统测试全部通过
- 2026-01-22: 现代化语法和桥接层完成
- 2026-01-21: 核心 MVP 完成
