# 解构绑定功能文档

## 概述

Xisp 现在支持在 `let` 特殊形式中使用解构绑定语法，允许将列表元素直接绑定到变量。

## 语法

### 基础解构

```lisp
; 绑定第一个元素到 x，剩余部分到 y
(let ((x . y) '(1 2 3))
  x)  ; => 1

(let ((x . y) '(1 2 3))
  y)  ; => (2 3)
```

### 嵌套解构

```lisp
; 复杂的嵌套模式
(let (((x . y) . z) '((1 2) 3 4))
  x)  ; => 1
```

### 正常绑定（不受影响）

```lisp
; 普通的变量绑定仍然正常工作
(let ((a 1) (b 2))
  (+ a b))  ; => 3
```

## 实现细节

### 核心逻辑

解构绑定在 `evaluator.cj` 的以下函数中实现：

1. **`processBindings`** (src/core/evaluator.cj:390-417)
   - 遍历绑定列表
   - 检测 improper list binding（解构模式）
   - 将下一个元素作为值

2. **`isImproperList`** (src/core/evaluator.cj:396-401)
   - 检查列表是否为 improper list（非真列表）
   - 用于识别解构模式

3. **`processDestructuringBinding`** (src/core/evaluator.cj:384-390)
   - 处理解构绑定
   - 提取 quote 表达式的参数
   - 调用模式匹配

4. **`destructurePattern`** (src/core/evaluator.cj:426-456)
   - 递归模式匹配
   - 支持嵌套解构

5. **`extractQuoteArg`** (src/core/evaluator.cj:283-296)
   - 检测并提取 quote 表达式的参数
   - 避免对 quoted list 进行求值

### 检测机制

解构绑定的检测基于以下逻辑：

```cangjie
// 检查 binding 是否为 improper list
// 例如：(x . y) 其中 cdr 是 Symbol，不是 ConsCell
private func isImproperList(value: LispValue): Bool {
    match (value) {
        case Cons(cell) => this.isConsCellImproper(cell)
        case _ => false
    }
}

private func isConsCellImproper(cell: ConsCell): Bool {
    let cdrIsCons = match (cell.cdr) {
        case Cons(_) => true
        case _ => false
    }
    !cdrIsCons && !cell.cdr.isNil()
}
```

### 处理流程

```
(let ((x . y) '(1 2 3)) x)
  ↓
evalList 识别 "let"
  ↓
evalLet 处理
  ↓
processBindings 遍历绑定列表
  ↓
检测到 (x . y) 是 improper list
  ↓
将 '(1 2 3) 作为值
  ↓
processDestructuringBinding
  ↓
extractQuoteArg 提取 (1 2 3)（不求值）
  ↓
bindPattern 模式匹配
  ↓
env.define("x", 1)
env.define("y", (2 3))
```

## 测试

完整的测试用例位于：`src/examples/test_destruct/main.cj`

运行测试：
```bash
cjpm build
target/release/bin/ystyle::xisp.examples.test_destruct
```

测试用例包括：
- ✅ 简单解构：`(x . y) '(1 2 3)` => x=1, y=(2 3)
- ✅ 嵌套解构：`((x . y) . z) '((1 2) 3 4)` => x=1
- ✅ 普通绑定：`(z 10)` => z=10
- ✅ 多个绑定：`(a 1) (b 2)` => a+b=3

## 设计决策

### 语法选择

我们选择了以下语法：
```lisp
(let ((x . y) '(1 2 3)) ...)
```

而不是：
```lisp
(let ([(x . y) '(1 2 3)]) ...)  ; Racket 风格（使用向量）
```

原因：
1. 与现有的 let 语法一致
2. 不需要引入新的字面量语法（向量）
3. 解析器改动最小

### 检测算法

使用"相邻元素检测"：
- 检测当前 binding 是否为 improper list
- 如果是，下一个元素作为值
- 跳过下一个元素继续遍历

这种设计允许混合使用解构绑定和普通绑定：
```lisp
(let ((x . y) '(1 2 3)  ; 解构绑定
      (z 10))           ; 普通绑定
  ...)
```

## 限制和注意事项

1. **解构模式必须是 improper list**
   - ✅ `(x . y)` - 正确
   - ❌ `(x y)` - 被识别为普通绑定

2. **值必须是下一个元素**
   - ✅ `((x . y) '(1 2 3))` - 正确
   - ❌ `((x . y) '(1 2 3) ...)` - 值后面不能有其他绑定

3. **Quote 表达式特殊处理**
   - 使用 `extractQuoteArg` 直接提取参数，避免求值
   - 支持复杂列表结构

## 未来改进

可能的增强功能：

1. **支持向量解构**
   ```lisp
   (let ([x y] [1 2]) ...)  ; 向量解构
   ```

2. **支持哈希解构**
   ```lisp
   (let ({:a x :b y} {:a 1 :b 2}) ...)  ; 哈希解构
   ```

3. **更多模式**
   - As 模式：`(x & rest)` 或 `(x :as whole)`
   - 默认值：`(x y = 10)`

## 参考

- 实现：src/core/evaluator.cj
- 测试：src/examples/test_destruct/main.cj
- 任务：task.md 7.1
