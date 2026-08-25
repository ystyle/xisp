# Xisp 字节码编译器设计

**版本**: 0.1 (设计草案)  
**日期**: 2026-07-29  
**状态**: 设计评审

---

## 1. 设计原则

1. **渐进式实现** — 先跑通最小子集（常量、变量、函数调用、条件、闭包），逐步补全
2. **VM 架构一次到位** — 栈帧、寄存器数组、闭包捕获按最终设计，不重构
3. **编译器渐进增强** — 第一版编译器简单（按名查变量），后续升级为索引
4. **AST 回退** — 编译器不支持的表达式返回 `None`，走 AST 解释

### 升级路径

```
指令集:      栈机（PUSH/POP/ADD/CALL）
VM 底层:     寄存器数组（registers: ArrayList<LispValue>）
             阶段一：隐式寻址（sp 栈指针）
             阶段二：显式寻址（r1, r2, r3）— 指令格式变，VM 数组不变
编译器:      阶段一：按名查变量（LOAD_VAR_STR）
             阶段二：按索引（LOAD_LOCAL 2）— 更快，VM 兼容两种指令
```

---

## 2. 指令集（第一版）

每条指令是一个 `Int64`（opcode）+ 后续 Int64（操作数）。

```
// 常量操作
PUSH_CONSTANT  idx     // 从常量池压入值
PUSH_NIL               // 压入 nil
PUSH_TRUE              // 压入 true
PUSH_FALSE             // 压入 false

// 栈操作
POP                    // 丢弃栈顶
DUP                    // 复制栈顶

// 变量操作
LOAD_VAR_STR  strIdx   // 从常量池取变量名，env.lookup 加载
STORE_GLOBAL  strIdx   // env.define（顶层定义）
LOAD_GLOBAL   strIdx   // env.lookup（顶层读取）

// 函数操作
CALL           n       // 调用函数。栈上为 [func, arg1, ..., argn]
                        // 弹出 n+1 个，压入返回值
RETURN                 // 返回。弹出当前帧，压入返回值到调用方栈
MAKE_CLOSURE   funcIdx // 从函数表取编译后的函数，捕获当前 env，创建闭包

// 控制流
JUMP           offset  // 无条件跳转
JUMP_IF_FALSE  offset  // 弹出栈顶，false/nil 则跳转
JUMP_IF_TRUE   offset  // 弹出栈顶，true 则跳转

// 算术/比较（弹出两个，压入结果）
ADD, SUB, MUL, DIV
NEG                    // 弹出一个，压入 -v
NOT                    // 弹出一个，压入 (not v)
EQ, LT, GT

// 特殊
HALT                   // 停止执行
```

### 指令编码

```
指令流: ArrayList<Int64>

示例: (if (> x 0) x (- x))
→
  LOAD_VAR_STR "x"      [OP_LOAD_VAR_STR, 0]    (常量池[0]="x")
  PUSH_CONSTANT 0       [OP_PUSH_CONSTANT, 1]   (常量池[1]=0)
  GT                    [OP_GT]
  JUMP_IF_FALSE else    [OP_JUMP_IF_FALSE, 12]  (跳转到 else 分支)
  LOAD_VAR_STR "x"      [OP_LOAD_VAR_STR, 0]
  JUMP end              [OP_JUMP, 17]           (跳过 else)
  LOAD_VAR_STR "x"      [OP_LOAD_VAR_STR, 0]   (else:)
  NEG                   [OP_NEG]
                        (end:)
```

---

## 3. 常量池

```cangjie
public class CompiledFunction {
    let code: ArrayList<Int64>            // 指令序列
    let constants: ArrayList<LispValue>   // 常量池
    let functions: ArrayList<CompiledFunction>  // 内嵌函数（闭包体）
    let name: String                      // 调试用
}
```

常量池存所有字面量：数字、字符串、布尔、符号。

---

## 4. 编译器流程

```cangjie
public class BytecodeCompiler <: Compiler {
    var constants: ArrayList<LispValue>
    var functions: ArrayList<CompiledFunction>

    public func compile(expr: LispValue): Option<CompiledCode> {
        this.reset()
        let func_ = this.compileTopLevel(expr)
        Some(Bytecode(func_.code, func_.constants))
    }

    func compileTopLevel(expr: LispValue): CompiledFunction {
        match (expr) {
            case LispValue.Int(v)    => this.emitConst(LispValue.Int(v))
            case LispValue.Float(v)  => this.emitConst(LispValue.Float(v))
            case Str(v)              => this.emitConst(Str(v))
            case Boolean(true)       => this.emit(OP_PUSH_TRUE)
            case Boolean(false)      => this.emit(OP_PUSH_FALSE)
            case Nil                 => this.emit(OP_PUSH_NIL)
            case Symbol(s)           => this.compileSymbol(s)
            case Cons(cell)          => this.compileCall(cell)
            case _                   => None  // 不支持，回退 AST
        }
    }
}
```

### 编译器不支持的情况

- 宏（编译前宏已展开，无需处理）
- match 模式匹配
- 部分特殊形式（复杂 case 回退 AST）
- 关键字参数（后续支持）

---

## 5. VM 设计

### 数据结构

```cangjie
/// VM 帧
struct Frame {
    let func: CompiledFunction        // 当前执行的函数
    var ip: Int64                     // 指令指针
    var bp: Int64                     // 基址指针（寄存器数组中本帧起始）
    var parent: ?Environment          // 捕获的父环境（闭包时用）
}

/// 字节码 VM
public class BytecodeVM <: VM {
    var registers: ArrayList<LispValue>  // 虚拟寄存器（做栈用）
    var sp: Int64                        // 栈顶指针（寄存器索引）
    var frames: ArrayList<Frame>         // 调用栈
    var frameIndex: Int64                // 当前帧索引
}
```

### 执行流程

```
execute(code, env):
  1. 从 CompiledCode 中取出指令流 + 常量池
  2. 创建初始帧
  3. 循环：
     a. 取指令 (func.code[ip])
     b. 匹配 opcode 执行
     c. ip++
  4. HALT 或 RETURN 时结束
```

### 函数调用

```
CALL n:
  1. sp -= n+1  // 栈顶往下 n+1 个位置：函数 + 参数
  2. 检查函数类型：
     - NativeFunc → 直接调用
     - Closure(fun, env) → 压帧（新帧的寄存器从 sp 开始）
       栈上已有参数，新帧直接复用
     - Procedure → 回退 AST 解释
  3. 返回值压入 sp（调用方的栈）
```

### 闭包

```
MAKE_CLOSURE funcIdx:
  1. 编译时已确定 funcIdx 对应的 CompiledFunction
  2. 运行时捕获当前环境（this.env）
  3. 创建 Closure(compiledFunc, env) 压入栈
```

### 栈帧复用优化

调用函数时，参数已经在栈上。新帧的 `bp = sp + 1`（参数之后的第一个位置作为局部变量起始），无需复制参数。

```
调用前栈: [..., 参数1, 参数2, 函数, ...]
                     ↑sp-2   ↑sp-1   ↑sp

CALL 2:
  栈:     [..., 参数1, 参数2, ...]
                ↑bp             ↑sp （新帧的栈底）
  
  新帧: bp=原sp-2, 局部变量从 bp 开始
```

---

## 6. 渐进实现计划

### 阶段一：最小子集（常量 + 算术 + 打印）

- 支持：数字、字符串、布尔、nil
- 支持：`+ - * /` 等算术
- 支持：`println` 调用
- 编译 `(+ 1 2)` → `PUSH_CONSTANT 1, PUSH_CONSTANT 2, ADD, CALL "println"`
- 通过解释器选项启用编译：`withBytecodeCompiler()`

### 阶段二：变量和控制流

- 支持：`define`、`set!`、变量引用
- 支持：`if`、`cond`、`begin`
- 支持：`LOAD_VAR_STR` / `STORE_GLOBAL` / `LOAD_GLOBAL`

### 阶段三：函数和闭包

- 支持：`lambda`、`define` 函数
- 支持：`MAKE_CLOSURE` / `CALL` / `RETURN`
- 支持：递归

### 阶段四：增量补全

- 向量、哈希映射字面量
- let 解构
- 管道操作符
- match 模式匹配
- 关键字参数

---

## 7. 测试策略

- 每阶段都有**完全相同的 AST 和字节码测试**
- `(compile-and-eval '(+ 1 2))` 返回 3，与 `(eval '(+ 1 2))` 一致
- 关键测试：先通过 AST 解释器执行，再用字节码 VM 执行，结果对比
