# Bytecode-Kit：字节码基座拆分与语言生成器设计（原名 Bytecode-FW）

**版本**: 0.2（2026-09-27 更新）
**日期**: 2026-08-25
**状态**: **方案 A 已定，待实施**；方案 B / C **不做**（设计保留在 §4 / §5，谁要谁捡）
**模块名**: `ystyle::bytecode_kit`（中性名，不含 `xisp`，见 §3.1）
**关联**: 整体路线与排期见 [`2026-09-27-workspace-modularization.md`](2026-09-27-workspace-modularization.md) §3.2 / §3.3

---

## 1. 背景与目标

xisp 现有字节码实现（`feat/bytecode-compiler`，2026-07-29 起）已经验证：
- 字节码模式相对 AST 解释 **~18.7x** 加速（固定调用链缺陷后，2026-08-25）
- 全量 325 单元测试通过

本次设计回答两个问题：

1. **能否把 xisp 的字节码编译器/虚拟机拆出来**，做成可复用的库（命名定为 `bytecode_kit`，见 §3.1）？
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

## 3. 方案 A：拆出 `bytecode_kit`（**本次实施范围**）

### 3.1 目标

发布独立仓颉包 **`ystyle::bytecode_kit`**：

- **命名**：描述型 + 中性（不含 `xisp`）——将来若被第三方复用**不必改名**，改名会破坏所有消费者。
  原名 `bytecode-fw` 弃用（`fw` 易被读成 firmware，且 "framework" 没有信息量）；
- 不依赖 xisp 任何模块；
- 提供：指令流/常量池 IR、寄存器栈 VM、编译器基座（emit/跳转回填/槽分配）；
- xisp 成为第一个宿主（适配层 ~200 行）；
- **实现深度：特化优先**——先做 `ValueHost<LispValue>` / `BytecodeVM<LispValue>`（避免每指令 trait 分派），
  **泛型化留白**：若不出现 xisp 之外的宿主，就不必做（理由见 §6 风险）。

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
public interface ValueHost<T> {             // 宿主值语义（VM → 宿主方向）
    func isCallable(v: T): Bool
    func call(f: T, args: ArrayList<T>, env: Object): T
    func callNamed(name: String, args: ArrayList<T>, env: Object): T

    // 【2026-09-27 补】指令派发钩子：宿主在每指令分派处插快速路径
    //   （xisp 的 JIT 经此接入，见 vm.cj:599）。不装钩子时必须零开销；形态待定（拦截式 / 通知式）。
}
public class RegisterVM<T> {                // 宿主 → VM 方向
    // 帧切换/寄存器栈/globalRefs 解析/嵌套执行安全（从 BytecodeVM 平移，全量保留）
    public func execute(code: CompiledCode<T>, env: Object, host: ValueHost<T>): T

    // 【2026-09-27 补】宿主调用"编译闭包"（xisp higher_order.cj:510 需要）
    public func callClosure(id: Int64, args: ArrayList<T>, env: Object): T

    // 【2026-09-27 补】闭包 id → 参数/体（xisp higher_order.cj:513 现在直读静态表
    //   BytecodeVM.functionTable，跨包后不可见）。返回类型待定。
    public func closureProcedure(id: Int64): Option<ClosureRef<T>>
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

### 3.2.1 2026-09-27 补记：接口必须双向

原草案的 `ValueHost<T>` 只覆盖 **VM → 宿主**方向（求值过程中调宿主函数）。
但 xisp 作为宿主还需要 **宿主 → VM** 方向的三项，缺了拆不动——这三项是 §2.2 耦合点清单的直接落点：

| 需求 | xisp 现位置 | 落点 |
|---|---|---|
| 调编译闭包 | `higher_order.cj:510` `vm.callClosure(id, args, env)` | `RegisterVM.callClosure` |
| 闭包 id 还原 | `higher_order.cj:513` 直读 `BytecodeVM.functionTable`（静态表，跨包不可见） | `RegisterVM.closureProcedure` |
| JIT 快速路径 | `vm.cj:599` | `ValueHost` 的指令派发钩子 |

### 3.3 xisp 适配层（留在 xisp 内）

- `LispValueHost <: ValueHost<LispValue>`：实现 `call`（NativeFunc/Procedure/Closure/Macro 分派，现 callFunction 逻辑平移）、`callNamed`（Str(name) 解析）；
- `LispEvaluatorHost`：注入 AST 回退求值器工厂；
- `compileDefine` 等前端语义**留在 xisp**（这是 xisp 的"语义模板"，下一步规则化时移到"语言描述"处）。

### 3.4 验收标准

1. `bytecode_kit` 可独立 cjpm 构建（零 xisp 依赖）；
2. xisp 字节码模式**行为与性能不回归**（全量测试 + 基准 ~18.7x）；
3. 提供最小示例宿主（~100 行的"计数器语言"）证明"新语言接入"路径通畅
   —— **可选但推荐**：它同时验证了"特化形状没有写死成 xisp 专用"。

---

## 4. 方案 B：语义规则化（**不做**，设计保留）

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

## 5. 方案 C：语法驱动生成器（**不做**，设计保留）

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

| 阶段 | 内容 | 工作量（人日估） | 产出 | 状态 |
|---|---|---|---|---|
| A | `bytecode_kit` 拆分 + xisp 适配（**特化优先**，泛型化留白） | 3–5 | cjpm 包 `ystyle::bytecode_kit` v0.1 | **实施** |
| B | 语义规则化（xisp 前端规则表） | 2–3 | 前端"语言描述"第一版 | **不做** |
| C1 | 生成器（EBNF→parser + 模板→codegen） | 10–15 | `langforge` CLI（暂名） | **不做** |
| C2 | isa.toml → 指令常量/VM 骨架 | 3–5 | 指令集描述化 | **不做** |
| C3 | 演示语言（微 Pascal/类 C 子集） | 3–5 | 端到端验证 | **不做** |

**风险**：
- 泛型化 VM 在仓颉中可能遇到 trait 约束/性能问题——方案 A 先做**特化版本**（`BytecodeVM<LispValue>` 直接内联 `ValueHost` 调用，避免每指令 trait 分派），实测性能后决定是否走泛型接口；
- LL(1) 改写对复杂语言（如 Python）不可行——生成器定位在**中等规模语言/DSL**。

---

## 7. 后续待办（依序执行）

1. ~~评审本设计~~ —— **已完成（2026-09-27）**：只实施方案 A；方案 B / C 不做，设计保留在 §4 / §5；
2. 实施 A：拆分 `ystyle::bytecode_kit`（**特化优先**）并用 xisp 全量测试 + 基准回归验收；
   - 接口按 §3.2.1 补记的三项定（`callClosure` / `closureProcedure` / 指令派发钩子）；
3. （B / C 不在本计划内，谁要谁捡。）

---

## 附录 A：xisp 现成可平移清单

- 指令发射：`emit/emitWithArg/emitR/emitR3`（compiler.cj）
- 控制流回填模式：compileIf（jifPatch/jPatch）、compileAnd/Or（patches+endPos）、compileMatch（nextClause 回填）
- 槽分配：locals/localCount/globalToLocal/globalSlots（含"交错分配"语义）
- 帧语义：switchFrame（参数左移、globalRefs 实时解析、宏保留名）、restoreFrame（调用点结果槽）
- 嵌套安全：executeFunc 状态保存/恢复
- 回退机制：compile 返回 Option、eval 主入口 isMacroCallExpr 宏跳过编译
