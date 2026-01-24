# 仓颉嵌入式 Lisp 脚本语言设计文档

**版本**: 1.0.0  
**设计目标**: 为仓颉应用提供轻量级、现代化、安全可控的嵌入式脚本能力，实现仓颉与Lisp的双向无缝互操作。

---

## 一、核心架构设计

### 1.1 分层架构
```
┌─────────────────────────────────────────────────┐
│          用户层 (Lisp脚本/应用代码)              │
├──────────┬──────────────────────┬──────────────┤
│  Lisp标准库 │  三方库(cjlpm管理)   │  原生仓颉接口 │
├──────────┴──────────────────────┴──────────────┤
│          桥接层 (类型转换 & 函数注册)            │
├─────────────────────────────────────────────────┤
│          解释器核心 (求值器 & REPL)              │
│  ┌──────────────┐  ┌──────────────┐          │
│  │  Parser      │  │  Macro系统   │          │
│  │  (递归下降)  │  │  (编译期展开)│          │
│  └──────────────┘  └──────────────┘          │
├─────────────────────────────────────────────────┤
│          仓颉运行时 (std::*, async, FFI)         │
└─────────────────────────────────────────────────┘
```

**关键设计**: 解释器不重复实现IO/网络等能力，直接桥接仓颉标准库，保持零性能损耗。

---

## 二、语法规范（新旧对照）

### 2.1 核心语法（兼容层）

| 传统语法 | 现代别名 | 示例 | 说明 |
|----------|----------|------|------|
| `car` | `first` | `(first '(1 2 3))` → `1` | 获取首元素 |
| `cdr` | `rest` / `more` | `(rest '(1 2 3))` → `(2 3)` | 获取剩余部分 |
| `cadr` | `second` | `(second '(a b c))` → `b` | 第2个元素 |
| `caddr` | `third` | `(third '(a b c))` → `c` | 第3个元素 |
| `cons` | `prepend` | `(prepend 1 '(2 3))` → `(1 2 3)` | 前缀添加 |
| `cond` | `condb`/`match` | `(condb test1 expr1 test2 expr2)` | 条件表达式/模式匹配 |
| `progn` | `begin` | `(begin (println 1) 2)` | 顺序执行 |


**初始化策略**: 解释器启动时同时注册新旧名称，指向同一内置函数实现。

### 2.2 现代化语法扩展

#### **1. 解构绑定 (`let`)**
```lisp
;; 传统写法
(let ((x (first data))
      (y (second data)))
  (println x y))

;; 现代解构（自动展开为car/cdr调用）
(let [[x y & rest] data]  ; & 收集剩余
  (println x y rest))

;; 嵌套解构
(let [[[a b] c] nested]
  (println a b c))
```

**实现**: 宏展开阶段将解构语法转换为`first`/`rest`调用。

#### **2. 管道操作符 (`->`)**
```lisp
;; 传统嵌套
(println (string-upcase (trim input)))

;; 现代管道（从左到右执行）
(-> input trim string-upcase println)

;; 带参数
(-> data
    (filter even?)
    (map (fn (x) (* x 2)))
    sum)
```

**实现**: `->`宏展开为嵌套调用：
```lisp
;; 展开规则
(-> a b c)    → (c (b a))
(-> a (b x))  → (b a x)
```

#### **3. 向量/哈希表字面量**
```lisp
;; 传统：只能用list
'(1 2 3)  ; 链表

;; 现代：仓颉原生类型
[1 2 3]   ; 对应仓颉 ArrayList（随机访问O(1)）
#{1 2 3}  ; 对应仓颉 HashSet（自动去重）
{:a 1 :b 2} ; 对应仓颉 HashMap（关键字语法糖）

;; Reader层转换规则
[1 2 3] → (cangjie:vector 1 2 3)
{:a 1} → (cangjie:hashmap :a 1)
```

#### **4. 字符串插值**
```lisp
;; 传统：繁琐拼接
(string-append "Hello " name "!")
;; 现代：插值语法
#"Hello {name}, you are {age} years old."
```

**实现**: Reader层将插值展开为`string-append`调用。

#### **5. 异步支持 (spawn + Future<T>)**
>尚未实现
```lisp
;; 创建异步任务
(let [future (spawn (http-get url))]
  ;; 在主线程做其他事情
  (println "请求已发送...")
  ;; 获取结果（阻塞等待）
  (let [data (future.get)]
    (process data)))

;; 带超时的获取
(let [future (spawn (long-computation))]
  (match (future.get(timeout: 5000))
    (Some result) (println "结果:" result)
    (None) (println "超时")))

;; 并行执行多个任务
(let [f1 (spawn (task1))
      f2 (spawn (task2))
      f3 (spawn (task3))]
  ;; 获取所有结果
  (let [r1 (f1.get)
        r2 (f2.get)
        r3 (f3.get)]
    (combine r1 r2 r3)))

;; 取消任务
(let [future (spawn (long-task))]
  (if (should-cancel?)
    (future.cancel)))
```

**说明**：
- `spawn` 创建新线程并返回 `Future<T>` 对象
- `Future.get()` 阻塞等待结果
- `Future.get(timeout)` 带超时等待，返回 `Option<T>`
- `Future.tryGet()` 非阻塞检查是否完成
- `Future.cancel()` 取消任务
- `Thread.currentThread` 获取当前线程信息

---

## 三、模块系统

Xisp 提供了完整的模块系统，支持代码组织、命名空间隔离和依赖管理。

### 3.1 核心概念

**术语对照**：
- **模块**：有 `module.lisp` 的目录（类似仓颉的包）
- **包**：模块内的子目录组织
- **组织**：模块的命名空间前缀（类似仓颉的包名）

**导入语法**：
```lisp
;; 绝对导入 - 导入外部模块
(import ystyle::log)           ; 导入 ystyle 组织的 log 模块
(import pkg1)                  ; 从搜索路径导入 pkg1 模块

;; 相对导入 - 导入项目内文件/包
(import "./utils.lisp")        ; 导入文件（无前缀）
(import "./helpers")           ; 导入目录包（有前缀）
```

**分隔符规则**：
- `.`：用于模块名/相对路径的层级分隔
- `::`：用于绝对导入时的组织和模块名分隔

### 3.2 基本示例

```lisp
;; 导入第三方模块（绝对导入）
(import ystyle::log)
(log.init "myapp")             ; 使用 log. 前缀

;; 导入项目内包（相对导入）- 有前缀
(import "./helpers")
(helpers.validateEmail "test@example.com")  ; 使用 helpers. 前缀

;; 导入项目内文件（相对导入）- 无前缀
(import "./utils.lisp")
(processData "test")           ; 直接使用，无前缀
```

### 3.3 module.lisp 格式

每个模块目录下必须有 `module.lisp` 文件：

```lisp
(module myapp
  (version "1.0.0")
  (description "My application")
  (author "Me")

  (dependencies
    (ystyle::log "0.2.0")))    ; 依赖声明
```

**详细文档**：完整的模块系统说明请参见 **[docs/modules.md](modules.md)**，包括：
- 目录结构设计
- 符号导出与导入
- 版本管理
- 错误处理
- 最佳实践
- 完整示例项目

---

## 四、仓颉桥接与互操作

Xisp 提供了完整的桥接层 API，实现 Lisp 与仓颉的双向互操作。

### 4.1 创建解释器

```cangjie
import ystyle::xisp.*

let interpreter = LispInterpreter()
```

创建解释器时会自动：
- 初始化 Lisp 环境
- 注册所有内置函数
- 注册标准桥接函数（std.io, std.fs）

### 4.2 仓颉调用 Lisp

在仓颉代码中求值 Lisp 表达式：

```cangjie
// 求值单个表达式
let result = interpreter.eval("(+ 1 2 3)")  // => Number(6.0)

// 求值多个表达式
let code = "
    (define x 10)
    (define y 20)
    (+ x y)
"
let result2 = interpreter.evalMultiple(code)  // => Number(30.0)
```

### 4.3 注册自定义函数

在仓颉中注册函数到 Lisp 环境：

```cangjie
// 注册不带命名空间的函数
interpreter.registerBridgeFunction("square", { args =>
    if (args.size > 0 && let Number(n) <- args[0]) {
        Number(n * n)
    } else {
        Str("Error: argument must be a number")
    }
})

// 注册带命名空间的函数
interpreter.registerBridgeFunctionWithNS("mycalc", "add", { args =>
    if (args.size >= 2 && let Number(a) <- args[0] && let Number(b) <- args[1]) {
        Number(a + b)
    } else {
        Str("Error: requires 2 numbers")
    }
})
```

然后在 Lisp 中使用：

```lisp
;; 调用无命名空间函数
(square 5)  ; => 25.0

;; 调用带命名空间的函数
(mycalc::add 3 4)  ; => 7.0
```

### 4.4 类型转换

所有仓颉基本类型都实现了 `LispConvertible` 接口：

```cangjie
// 数值类型
let num: Int64 = 42
let lispNum = num.toLisp()  // Number(42.0)

// 字符串
let str: String = "Hello"
let lispStr = str.toLisp()  // Str("Hello")

// ArrayList
let numbers = ArrayList<Int64>()
numbers.append(1)
numbers.append(2)
let lispList = numbers.toLisp()  // (1.0 2.0)

// HashMap
let map = HashMap<String, Int64>()
map["a"] = 1
map["b"] = 2
let lispMap = map.toLisp()  // (("a" 1.0) ("b" 2.0))
```

### 4.5 标准库桥接

Xisp 已实现 std.io 和 std.fs 的桥接函数：

```lisp
;; 文件操作
(cangjie::write-file "config.txt" "name=Xisp\nversion=0.1.0")
(define config (cangjie::read-file "config.txt"))
(cangjie::append-file "log.txt" "\nNew entry")

;; 文件系统操作
(cangjie::exists? "config.txt")      ; => true
(cangjie::file? "config.txt")        ; => true
(cangjie::directory? "/tmp")         ; => true
(cangjie::list-dir ".")              ; => ("file1.txt" "file2.cj" ...)
```

**详细文档**：完整的桥接 API 文档请参见 **[docs/integration/bridge.md](integration/bridge.md)**，包括：
- LispInterpreter 完整 API
- LispConvertible 接口
- TypeConverter 工具类
- 错误处理最佳实践
- 完整使用示例

---

## 五、核心功能实现

### 5.1 沙箱系统

Xisp 提供了完整的安全沙箱系统，可以安全地执行不受信任的 Lisp 代码。

#### **核心功能**

```cangjie
import ystyle::xisp.*

// 严格沙箱模式（推荐用于不受信任的代码）
let interpreter = LispInterpreter([
    withSandbox(),        // 栈深度 500，超时 30s
    withStdIO(),          // 受限的文件 I/O
    withQuietMode()       // 不显示 Banner
])

// 危险操作将被阻止
interpreter.eval("(cangjie::write-file \"/etc/passwd\" \"hack\")")
// 返回: "Error: File write denied: /etc/passwd"
```

#### **沙箱选项**

```cangjie
// 自定义配置
let interpreter = LispInterpreter([
    // 栈深度限制
    withMaxStackDepth(200),

    // 执行超时
    withTimeout(Some(Duration.second * 60)),

    // 函数黑名单
    withBlockedFunctions(["eval", "apply", "load"]),

    // 路径白名单
    withAllowedPaths(["/tmp/sandbox/"]),
    withStdIO()
])
```

**详细文档**：完整的沙箱系统文档请参见 **[docs/integration/sandbox.md](integration/sandbox.md)**，包括：
- 完整的选项列表
- 栈深度限制
- 执行超时控制
- 函数访问控制
- 文件访问控制
- 使用示例和最佳实践

---

## 六、解析器实现策略

### 6.1 手写递归下降（推荐）
```cangjie
// 不依赖正则，手动扫描
class LispLexer {
    func nextToken(): Token { /* 字符级扫描 */ }
}

class LispParser {
    func parse(): ASTNode {
        match(peek()) {
            '(' => parseList()
            '"' => parseString()
            _ => parseAtom()
        }
    }
}
```

**优势**: 错误信息精准、易调试、与仓颉生态一致。

### 6.2 性能优化
1. **Token复用**: `()`等单字符Token用单例
2. **字符串切片**: Token存`start/end`索引而非拷贝
3. **字节码缓存**: 重复`eval-string`缓存编译结果
4. **AST常量折叠**: 编译期计算纯常量表达式

---

## 七、安全与调试

### 7.1 安全机制
- **符号白名单**: 限制沙箱环境可调用的函数
- **资源配额**: CPU时间、内存使用量限制
- **调用栈深度**: 防止无限递归
- **FFI权限**: 禁止Lisp直接调用危险仓颉函数

### 7.2 调试支持
```lisp
;; 断点调试
(debug: break)
(debug: inspect variable)

;; 打印AST
(quote (debug-ast (some-form)))

;; 宏展开查看
(macroexpand '(when x y))
```

---

## 八、实现路线图

### **阶段1：MVP（2周）**
- [ ] 手写Lexer/Parser（递归下降）
- [ ] 实现核心求值器（eval）
- [ ] 完成`first`/`rest`/`prepend`别名
- [ ] 对接`std.io`/`std.fs`

### **阶段2：功能完备（3周）**
- [x] `let`解构绑定
- [x] `->`管道操作符
- [x] `eval-string`与沙箱
- [x] 模块系统（import/export）

### **阶段3：现代化（2周）**
- [x] 向量/哈希字面量`[]`/`{}`
- [x] 字符串插值`#"..."`
- [x] 模式匹配`match`
- [ ] `spawn`异步语法（Future<T>）

### **阶段4：生产级（持续）**
- [ ] 性能优化（字节码缓存）
- [ ] 完整调试工具
- [ ] 安全加固
- [ ] 示例项目（HTTP Server）

---

## 九、设计原则

1. **不重复造轮子**: 所有IO/网络/并发复用仓颉标准库
2. **渐进现代化**: 保留`car`/`cdr`兼容，主推`first`/`rest`
3. **零成本抽象**: 语法糖在宏层展开，无运行时开销
4. **安全第一**: 默认沙箱，显式授权高危操作

---
5. **仓颉风格**: 代码结构、命名、错误处理遵循仓颉惯例


**文档状态**: 设计评审版  
**下一步**: 根据反馈完善后提交仓颉社区讨论
