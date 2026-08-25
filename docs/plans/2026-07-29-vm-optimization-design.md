# Xisp VM 架构优化设计

**版本**: 0.1 (设计草案)  
**日期**: 2026-07-29  
**状态**: 设计评审

---

## 1. 目标

- 变量访问从字符串查找改为按索引定位（O(1)）
- 指令格式从栈机改为寄存器机（减少指令数）
- 保留旧指令兼容，逐步迁移

---

## 2. A: 变量索引化

### Frame 结构

```cangjie
struct Frame {
    let func: CompiledFunction
    var ip: Int64
    var bp: Int64        // 基址指针（registers 数组中的起始位置）
    let parentEnv: ?Environment  // 闭包捕获的父环境
}
```

### 新指令

```
LOAD_LOCAL rd, idx    // rd = registers[bp + idx]
STORE_LOCAL idx, rs   // registers[bp + idx] = rs
LOAD_ENV rd, depth, idx // rd = env[depth][idx]（闭包捕获变量）
```

### 编译器变量追踪

编译器为每个函数维护一个变量表：

```cangjie
// 编译 (define (foo x y) (+ x y))
// 局部变量: x → idx 0, y → idx 1
// LOAD_LOCAL r1, 0 → x, LOAD_LOCAL r2, 1 → y
// ADD r3, r1, r2
```

### 闭包捕获

当内层函数引用外层变量时，编译器检测到 `x` 是捕获变量：
- 不生成 `LOAD_LOCAL`，而是生成 `LOAD_ENV rd, depth, idx`
- VM 通过 `parentEnv` 链查找

第一版不支持闭包编译（lambda 继续回退 AST），先做顶层函数的局部变量索引。

---

## 3. B: 寄存器指令

### 新指令集

```
// 常量/变量
LOAD_CONST rd, idx       // rd = constants[idx]
LOAD_LOCAL rd, idx       // rd = locals[idx]
STORE_LOCAL idx, rs      // locals[idx] = rs
LOAD_GLOBAL rd, strIdx   // rd = env.lookup(name)
STORE_GLOBAL strIdx, rs  // env.define(name, rs)

// 算术
ADD rd, rs1, rs2
SUB rd, rs1, rs2
MUL rd, rs1, rs2
DIV rd, rs1, rs2
NEG rd, rs
NOT rd, rs

// 控制流
JUMP target
JUMP_IF_FALSE rs, target
JUMP_IF_TRUE rs, target

// 函数
CALL rd, n              // rd = call func at rs with n args (rs+1..rs+n)
RETURN rs
MAKE_CLOSURE rd, funcIdx // rd = closure(compiledFunc, env)

// 数据
MOVE rd, rs             // rd = rs
```

### 编码格式

每条指令变长：
```
[opcode, reg1, reg2, reg3, ...]
```

如 `LOAD_CONST r1, 0` → `[OP_LOAD_CONST, 1, 0]`
如 `ADD r3, r1, r2` → `[OP_ADD_REG, 3, 1, 2]`

### 寄存器分配（简单版）

线性扫描，每个子表达式分配新寄存器：

```cangjie
var regCounter: Int64 = 0
func allocReg(): Int64 {
    let r = regCounter
    regCounter++
    r
}
// 函数入口时 regCounter = 局部变量数
// 临时表达式从 regCounter 开始
```

### 兼容策略

新指令用新的 opcode 编号（100+），VM 同时支持新旧指令。编译器逐步迁移：

| 阶段 | 生成 | 执行 |
|------|------|------|
| 当前 | PUSH/CALL 等旧指令 | VM 支持旧指令 |
| 过渡 | 混合（简单表达式用新指令） | VM 支持新旧指令 |
| 最终 | 全部新指令 | VM 移除旧指令支持 |

---

## 4. 实施步骤

### 步骤 1: Frame 结构 + LOAD_LOCAL/STORE_LOCAL

- 添加 Frame 结构到 VM
- 添加 LOAD_LOCAL/STORE_LOCAL 指令
- 编译器为函数参数生成索引 (param0→idx0, param1→idx1)
- 旧指令继续工作

### 步骤 2: 寄存器分配器

- 添加 allocReg/freeReg 到编译器
- 算术指令改为 ADD_REG rd, rs1, rs2
- 逐步迁移各指令

### 步骤 3: 清理旧指令

- 确认新指令覆盖所有场景
- 移除旧指令支持
- 更新 opcode 常量定义
