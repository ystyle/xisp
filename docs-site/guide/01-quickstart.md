# 快速开始

**阅读时间**: 10 分钟

本指南带你在 10 分钟内体验 Xisp 的核心能力。完整语法见后续章节。

---

## 安装

### 编译

```bash
cjpm build
```

编译产物：

```
./target/release/bin/ystyle::xisp.cli
```

也可以使用符号链接（已配置）：

```bash
./xisp
```

### 运行

```bash
# 启动 REPL
./target/release/bin/ystyle::xisp.cli

# 运行 Lisp 脚本
./target/release/bin/ystyle::xisp.cli script.lisp

# 直接执行代码
./target/release/bin/ystyle::xisp.cli -c "(+ 1 2 3)"
```

::: tip 栈深度配置
- `--stack-depth <N>`：设置最大调用栈深度（命令行优先）
- `XISP_MAX_STACK_DEPTH=<N>`：环境变量方式设置（默认 1000）

```bash
./target/release/bin/ystyle::xisp.cli --stack-depth 5000 script.lisp
```
:::

---

## 5 分钟快速体验

### 1. 变量与函数

```lisp
; 定义变量
(define name "Xisp")

; 定义函数
(define (square x) (* x x))

(square 5)
; => 25
```

### 2. 数据结构

```lisp
; 向量（数组）
[1 2 3 4 5]

; 哈希映射（字典）
{:name "张三" :age 25}

; 列表
'(1 2 3)
```

### 3. 字符串插值

```lisp
(define name "Xisp")
#"Hello, #{name}!"
; => "Hello, Xisp!"
```

### 4. 管道操作符

数据流从左到右处理，告别嵌套括号：

```lisp
(-> [1 2 3 4 5]
    (map square)
    (filter even?)
    length)
; => 2 (4 和 16 是偶数)
```

### 5. 解构绑定

从数据结构中提取值：

```lisp
(let [[x y & rest] '(1 2 3 4 5)]
  (list x y rest))
; => (1 2 (3 4 5))
```

### 6. 模式匹配

```lisp
(match 42
  (x when (> x 40)) "big"
  _ "small")
; => "big"
```

---

## 仓颉嵌入式

在仓颉代码中嵌入 Xisp：

```cangjie
import ystyle::xisp.*

// 创建解释器
let interpreter = LispInterpreter([withStdLib()])

// 求值 Lisp 表达式
let result = interpreter.eval("(+ 1 2 3)")
// => 6
```

完整集成指南见 [嵌入解释器](../integration/embedding)。

---

## 下一步

- [基础语法](02-basics) - 数据类型、特殊形式、内置函数
- [现代语法](03-modern) - 字面量、解构、管道、模式匹配
- [宏系统](04-macros) - 元编程和代码生成
- [API 参考](../api/) - 全部内置函数
