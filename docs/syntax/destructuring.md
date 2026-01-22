# 解构绑定功能文档

## 概述

Xisp 支持现代化的解构绑定语法，使用向量 `[]` 和 `&` 符号，让代码更简洁、更易读。

同时也支持传统的点对语法（向后兼容）。

## 现代语法（推荐）

### 基础解构

```lisp
; 绑定前两个元素
(let [[x y] '(1 2 3 4)]
  (list x y))
; => (1 2)

; 使用 & 收集剩余元素
(let [[x y & rest] '(1 2 3 4 5)]
  (list x y rest))
; => (1 2 (3 4 5))
```

### 嵌套解构

```lisp
; 嵌套列表解构
(let [[[a b] c] '((1 2) 3)]
  (list a b c))
; => (1 2 3)

; 复杂嵌套
(let [[[a b] [c d & rest]] '((1 2) (3 4 5 6))]
  (list a b c d rest))
; => (1 2 3 4 (5 6))
```

### 多个绑定

```lisp
; 混合使用解构和普通绑定
(let [[x y] data
       z 10]
  (list x y z))
```

## 传统语法（向后兼容）

### 点对解构

```lisp
; 绑定第一个元素到 x，剩余部分到 y
(let ((x . y) '(1 2 3))
  x)  ; => 1

(let ((x . y) '(1 2 3))
  y)  ; => (2 3)
```

### 嵌套点对解构

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

解构绑定在 `eval_helpers.cj` 的以下函数中实现：

1. **`processBindingsNew`** (src/core/eval_helpers.cj:150-212)
   - 支持向量形式的 bindings
   - 自动检测向量解构模式
   - 支持嵌套向量解构

2. **`processVectorDestructure`** (src/core/eval_helpers.cj:68-148)
   - 处理向量解构 `[x y & rest]`
   - 查找 `&` 符号位置
   - 绑定元素和剩余部分
   - 支持嵌套向量解构

3. **`isVectorDestructurePattern`** (src/core/eval_helpers.cj:13-22)
   - 检查是否为向量解构模式
   - 识别 `(cangjie:vector ...)`

4. **`extractVectorElements`** (src/core/eval_helpers.cj:27-43)
   - 提取向量元素列表
   - 跳过 `cangjie:vector` 标记

### 检测机制

现代解构语法的检测基于以下逻辑：

```cangjie
// 检查是否为向量解构模式
func isVectorDestructurePattern(value: LispValue): Bool {
    match (value) {
        case Cons(cell) =>
            match (cell.car) {
                case Symbol("cangjie:vector") => true
                case _ => false
            }
        case _ => false
    }
}
```

### 处理流程

```
(let [[x y & rest] data] ...)
  ↓
解析器将 [x y & rest] 转换为 (cangjie:vector x y & rest)
  ↓
processBindingsNew 检测到向量形式的 bindings
  ↓
提取元素：[x, y, &, rest]
  ↓
查找 & 符号位置
  ↓
绑定 x 和 y 到对应元素
  ↓
绑定 rest 到剩余部分
```

## 测试

完整的测试用例位于：`src/examples/test_destruct/main.cj`

运行测试：
```bash
cjpm build
target/release/bin/ystyle::xisp.examples.test_destruct
```

测试用例包括：
- ✅ 现代向量解构：`[x y] '(1 2 3)` => x=1, y=2
- ✅ & rest 收集：`[x y & rest] '(1 2 3 4 5)` => rest=(3 4 5)
- ✅ 嵌套解构：`[[a b] c] '((1 2) 3)` => a=1, b=2, c=3
- ✅ 传统点对解构：`(x . y) '(1 2 3)` => x=1, y=(2 3)（向后兼容）
- ✅ 普通绑定：`(z 10)` => z=10

## 设计决策

### 为什么使用向量语法？

1. **可读性**：`[x y & rest]` 比 `(x . y)` 更清晰
2. **现代性**：与 Clojure、Racket 等现代 Lisp 方言保持一致
3. **明确性**：`&` 符号明确表示"收集剩余部分"
4. **简洁性**：`[[a b] c]` 比 `((a . b) . c)` 更易读

### 向后兼容

传统点对语法继续有效：
- `(let ((x . y) '(1 2 3)) ...)` 仍然工作
- 两种语法可以共存
- 向量语法是推荐方式，但不强制

### 转换策略

所有向量语法在 Reader（解析器）层被转换：
- `[x y]` → `(cangjie:vector x y)`
- `[x y & rest]` → `(cangjie:vector x y & rest)`
- `[[a b] c]` → `(cangjie:vector (cangjie:vector a b) c)`

这种设计：
- **向后兼容**：不改变核心求值器
- **易于扩展**：可以单独优化向量解构
- **保持一致**：符合 Lisp 的"代码即数据"哲学

## 限制和注意事项

1. **向量解构必须是向量**
   - ✅ `[x y]` - 正确
   - ❌ `(x y)` - 被识别为普通列表

2. **& 符号用于收集剩余**
   - ✅ `[x y & rest]` - 正确
   - ❌ `[x y rest]` - rest 也会尝试匹配，可能失败

3. **嵌套解构**
   - ✅ `[[a b] c]` - 支持嵌套向量
   - ✅ `[[a b] & rest]` - 支持嵌套 + 收集

4. **值必须是列表**
   - 解构的目标值必须是一个列表或点对
   - 如果值元素不足，未匹配的变量绑定到 `Nil`

## 代码示例

### 数据处理

```lisp
; 解析配置
(define config '(("localhost" 8080) ("example.com" 80)))

(let [[[host port] & rest] config]
  (println "Host: " host)
  (println "Port: " port))
```

### 列表操作

```lisp
; 分离头部和尾部
(let [[first & rest] numbers]
  (list "First is" first
        "Rest is" rest))
```

### 嵌套数据

```lisp
; 复杂数据结构
(define data '((name "Alice") (age 30) (city "Beijing")))

(let [[[_ name] [_ age] & rest] data]
  (list name age rest))
```

## 未来改进

可能的增强功能：

1. **支持向量解构**
   ```lisp
   (let [#["x" x] #["x" 1]] ...)  ; 向量解构
   ```

2. **支持哈希解构**
   ```lisp
   (let [{:a x :b y} {:a 1 :b 2}) ...)  ; 哈希解构
   ```

3. **更多模式**
   - As 模式：`[x :as whole]`
   - 默认值：`[x y = 10]`

## 参考

- 实现：src/core/eval_helpers.cj
- 测试：src/examples/test_destruct/main.cj
- 现代语法：docs/syntax/modern_syntax.md
- 设计文档：docs/syntax/design.md
