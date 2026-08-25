# Bytecode-FW：字节码框架拆分与语法驱动编译器生成设计

**版本**: 0.1（设计草案）
**日期**: 2026-08-25
**状态**: 待评审（评审通过前不实现）

---

## 1. 背景与目标

xisp 现有字节码实现（`feat/bytecode-compiler`，2026-07-29 起）已经验证：
- 字节码模式相对 AST 解释 **~18.7x** 加速（固定调用链缺陷后，2026-08-25）
- 全量 325 单元测试通过

本次设计回答两个问题：

1. **能否把 xisp 的字节码编译器/虚拟机拆出来**，做成可复用的库（后续姑且命名 `bytecode-fw`）？
2. **能否用"指定 EBNF 语法"自动生成字节码编译器**（新语言只写语法描述就得到编译器）？

**结论先行**：
- (1) 可以，三层边界清晰（IR / VM / 编译前端），是必经的第一步；
- (2) 可以，但形态必须是 **「EBNF + 语义模板 + 指令集描述」三输入生成器**——EBNF 只生成 parser，**代码生成（语义）无法从 EBNF 推导**，必须有显式语义模板。对 Lisp 类语言收益小（语法成本≈0，语义成本 100%），对命令式语言收益大。

---

## 2. xisp 字节码现状分析

### 2.1 三层结构

```
┌─ 编译前端（语言语义）──── src/core/eval/compiler.cj (~780 行)
│   compileExpr / compileList 分派 + compileIf/Let/Match/Condb/Define...
│   全部耦合：LispValue AST、bytecode 发射、局部槽分配（locals/localCount）
├─ IR/指令层 ────────────── src/core/eval/code.cj (~90 行)
│   OP_* 常量表、CompiledFunction（code/constants/functions/params/
│   globalRefs/globalSlots/localSlots）
└─ 执行引擎 ─────────────── src/core/eval/vm.cj (~950 行)
    BytecodeVM：寄存器栈、帧切换(switchFrame/restoreFrame)、调用分派
    (callFunction/callProcedure/callMacro)、嵌套执行安全化
```

### 2.2 耦合点清单（拆分时需解耦）

| 耦合点 | 位置 | 解耦方式 |
|---|---|---|
| 值类型 `LispValue` | vm.cj 全文件、code.cj 常量池 | VM 泛型化 `BytecodeVM<T>` 或值接口 `VirtualValue` |
| 环境查找 `Environment.lookup` | vm.cj（globalRefs 解析、Str(name) 调用） | 注入 `lookup(name): T` 回调/接口 |
| 调用约定（Closure/Macro/NativeFunc） | vm.cj callFunction | 注入 `resolveCallable(value): Option<Callable>` 或直接由宿主闭包处理 |
| AST 回退求值器 `Evaluator` | vm.cj makeSubEvaluator/callProcedure | 注入 `fallbackEvaluator` 工厂（宿主提供） |
| 语义编译 | compiler.cj | 不需要解耦——它就是"语言前端"，被替换/新写 |

**关键洞察**：VM 与求值器之间的"边境"是 **makeSubEvaluator（AST 回退）**。拆出后该回退点由宿主注入，框架本身不含任何语言语义。

---

## 3. 方案 A：bytecode-fw SDK 拆分（第一步，评审通过后实施）

### 3.1 目标

发布独立仓颉包 `bytecode-fw`：
- 不依赖 xisp 任何模块；
- 提供：指令流/常量池 IR、寄存器栈 VM（泛型值）、编译器基座（emit/跳转回填/槽分配）；
- xisp 成为第一个宿主（适配层 ~200 行）。

### 3.2 模块与接口草案（仓颉）

```cangjie
// ---- module: ir ----
public class FnUnit<T> {                    // 编译单元（函数）
    public let code: ArrayList<Int64>
    public let constants: ArrayList<T>
    public let nested: ArrayList<FnUnit<T>>
    public let localSlots: Int64
    public let globalRefs: ArrayList<GlobalRef>   // (slotIdx, name)
}
public enum CompiledCode<T> { | Unit(FnUnit<T>) }

// ---- module: vm ----
public interface ValueHost<T> {             // 宿主值语义
    func isCallable(v: T): Bool
    func call(f: T, args: ArrayList<T>, env: Object): T
    func callNamed(name: String, args: ArrayList<T>, env: Object): T
}
public class RegisterVM<T> {
    // 帧切换/寄存器栈/globalRefs 解析/嵌套执行安全（从 BytecodeVM 平移，全量保留）
    public func execute(code: CompiledCode<T>, env: Object, host: ValueHost<T>): T
}

// ---- module: compiler-kit ----
public open class Codegen {
    public func emit(op: Int64)                                  // 原 emit
    public func emitArg(op: Int64, arg: Int64)                   // 原 emitWithArg
    public func patch(opIdx: Int64, target: Int64)               // 原 code[patch+1] 回填
    public func allocSlot(): Int64                               // 原 localCount++
    public func allocTemp(): Int64                               // 原 allocReg（偏移可配置）
    public func addConst(v: T): Int64
}
```

### 3.3 xisp 适配层（留在 xisp 内）

- `LispValueHost <: ValueHost<LispValue>`：实现 `call`（NativeFunc/Procedure/Closure/Macro 分派，现 callFunction 逻辑平移）、`callNamed`（Str(name) 解析）；
- `LispEvaluatorHost`：注入 AST 回退求值器工厂；
- `compileDefine` 等前端语义**留在 xisp**（这是 xisp 的"语义模板"，下一步规则化时移到"语言描述"处）。

### 3.4 验收标准

1. `bytecode-fw` 可独立 cjpm 构建（零 xisp 依赖）；
2. xisp 字节码模式**行为与性能不回归**（全量测试 + 基准 ~18.7x）；
3. 提供最小示例宿主（~100 行的"计数器语言"）证明"新语言接入"路径通畅。

---

## 4. 方案 B：语义规则化（第二步）

把 xisp 的前端从"手写 match 分派"改造成**规则表 + 基座**：

```cangjie
public struct SemRule<T> {
    let head: String                       // 特殊形式名（如 "if"）
    let compile: (Codegen, Expr, Ctx) -> Bool   // 语义动作（返回 false 触发回退）
}
```

- 规则表 = 「语言描述」的一部分（与 AST 求值器的特殊形式表同源，一份表喂两端，消除语义二义性）；
- `ctx` 提供作用域/槽分配/跳转目标；回退策略由宿主声明（哪些规则回退 AST）；
- 产物：xisp 前端变成"一份规则表"，方便日后导出到生成器模板。

---

## 5. 方案 C：语法驱动生成器（第三步，本设计核心，**先不做**）

### 5.1 为什么 EBNF 单独不够

编译 = 语法分析 + 语义分析 + 代码生成。

| 环节 | EBNF 能推导？ | 说明 |
|---|---|---|
| Lexer/parser | ✅ | EBNF → LL(1) 递归下降（经典算法，可生成） |
| 语义（作用域/类型/回填） | ❌ | 需要显式规则：符号查找、label 引用、寄存器/槽分配 |
| 字节码发射 | ❌ | 需要指令模式模板 |

业界同构系统：yacc/bison 语义动作、ANTLR visitor/template、tree-sitter + codegen 模板。**共同结论：语法只给"结构"，语义必须显式给"模板"**。

### 5.2 生成器形态

```
输入A  language.ebnf      词法 + 语法规则
输入B  codegen.tmpl       语义模板（每 production 一条）
输入C  isa.toml           指令集描述（opcode 表 + 栈/寄存器语义说明）
   ↓ 生成器（仓颉 CLI 工具）
输出H  language_parser.cj      递归下降 parser（LL(1)，含错误恢复）
输出I  language_codegen.cj     编译前端（语义模板 → Codegen 基座 API）
输出J  isa_const.cj + vm.cj    （可选：由指令集描述生成指令常量与 VM 分支）
```

### 5.3 语义模板 DSL 草案

每一条绑定到一个语法 production（以类 C 片段为例）：

```toml
[[rule]]
head = "if_statement"          # ebnf production 名
emit = """
  compile(cond);               # 递归模板调用
  JUMP_IF_FALSE -> L_else
  compile(then)
  JUMP -> L_end
  L_else:
  compile(else)
  L_end:
"""
```

模板需要的原语（全部落在 Codegen 基座）：
- `compile(子规则)`（递归下降语义）
- `emit/emitArg`（指令发射）
- `label()` / `patch()`（控制流回填，处理"前向引用定义"）
- `symbol_lookup(name) -> SlotRef` / `define(name)`（作用域符号表）
- `allocSlot`（帧槽）、`allocTemp`（临时寄存器）
- `value_arity`（返回约定：结果槽=调用点）

### 5.4 关键设计决策

1. **生成源码 vs 解释模板**：第一版**生成仓颉源码**（编译期全量优化、无模板解释开销、可调试）；模板引擎本身是重心，解释模式留作互查。
2. **LL(1) 假设**：生成器仅支持 LL(1)/LL(k) 小 k 语法；左递归由用户改写（提供推导工具）。对类 C 语言足够。
3. **语义模板不可由 EBNF 推导**——明确该边界，避免产品预期错位。
4. **指令集描述可选**：最小可用版本只做「EBNF + 模板 → 编译前端」，跑在方案 A 的 `RegisterVM` 上；`isa.toml → VM` 作为阶段 2 增强。
5. **回退机制保留**：Lisp 类语言的"模板失败 → AST 回退"模式在生成器中缺失（生成器面对的是完整语法），因此生成器更适合**完整编译**模型，混合模型留给宿主手动接入。

### 5.5 适用性评估

| 场景 | EBNF+模板 收益 |
|---|---|
| 类 C 语言（语句块/表达式/循环） | 高：结构即编译模式 |
| Lisp 类（S 表达式） | 低：parser 成本≈0，语义全在特殊形式/宏 |
| 混合（如 xisp 未来 DSL/子语言） | 中：文法差异部分受益 |

---

## 6. 路线图与工作量

| 阶段 | 内容 | 工作量（人日估） | 产出 |
|---|---|---|---|
| A | bytecode-fw 拆分 + 泛型化 + xisp 适配 | 3–5 | cjpm 包 `bytecode-fw` v0.1 |
| B | 语义规则化（xisp 前端规则表） | 2–3 | 前端"语言描述"第一版 |
| C1 | 生成器（EBNF→parser + 模板→codegen） | 10–15 | `langforge` CLI（暂名） |
| C2 | isa.toml → 指令常量/VM 骨架 | 3–5 | 指令集描述化 |
| C3 | 演示语言（微 Pascal/类 C 子集） | 3–5 | 端到端验证 |

**风险**：
- 泛型化 VM 在仓颉中可能遇到 trait 约束/性能问题——方案 A 先做**特化版本**（`BytecodeVM<LispValue>` 直接内联 `ValueHost` 调用，避免每指令 trait 分派），实测性能后决定是否走泛型接口；
- LL(1) 改写对复杂语言（如 Python）不可行——生成器定位在**中等规模语言/DSL**。

---

## 7. 后续待办（依序执行）

1. 评审本设计（重点：A 的接口边界、C 的模板 DSL 原语充分性）；
2. 实施 A：拆分 `bytecode-fw` 并用 xisp 全量测试 + 基准回归验收；
3. 实施 B：规则表化 xisp 前端（同时服务 AST 求值器与字节码编译器）；
4. 实施 C1：定义 .ebnf/.tmpl/isa 格式规范，写生成器原型（先支持一个玩具语言验证闭环）。

---

## 附录 A：xisp 现成可平移清单

- 指令发射：`emit/emitWithArg/emitR/emitR3`（compiler.cj）
- 控制流回填模式：compileIf（jifPatch/jPatch）、compileAnd/Or（patches+endPos）、compileMatch（nextClause 回填）
- 槽分配：locals/localCount/globalToLocal/globalSlots（含"交错分配"语义）
- 帧语义：switchFrame（参数左移、globalRefs 实时解析、宏保留名）、restoreFrame（调用点结果槽）
- 嵌套安全：executeFunc 状态保存/恢复
- 回退机制：compile 返回 Option、eval 主入口 isMacroCallExpr 宏跳过编译
