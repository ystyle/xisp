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
| `cond` | `match` | `(match lst [[a b] -> a])` | 模式匹配（扩展） |
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

## 三、标准库与仓颉对接

### 3.1 命名空间映射
```lisp
;; 仓颉模块 → Lisp命名空间
std.io          → cangjie:io
std.fs          → cangjie:fs
std.net         → cangjie:net
std.sync        → cangjie:sync   (spawn, Future<T>, Thread)
std.collection  → cangjie:collection (内部使用，Lisp用户用Cons)
```

### 3.2 常用函数示例
```lisp
;; IO模块
(cangjie:io:println "Hello")      ; 打印
(cangjie:io:read-line)            ; 读取输入

;; 文件系统
(cangjie:fs:read-to-string "config.json")  ; 读文件
(cangjie:fs:exists? "/tmp")      ; 检查存在

;; HTTP客户端（生产级）
(let [resp (cangjie:net:http-get "https://api.github.com/users")]
  (println (cangjie:io:read-to-end resp)))
```

**实现机制**: 解释器启动时自动扫描仓颉`std`模块，通过FFI动态绑定函数指针。

---

## 四、三方库管理（cjlpm）- 待定

### 4.1 项目结构
```
my-app/
├── cjlpm.toml          ; 依赖配置
├── cjlpm.lock          ; 版本锁定
├── main.cj             ; 仓颉主程序
└── lisp-src/
    ├── app.lisp        ; 入口脚本
    └── libs/
        ├── utils.lisp  ; 本地库
        └── http.lisp   ; 三方库
```

### 4.2 配置文件示例
```toml
[package]
name = "my-lisp-app"
version = "0.1.0"

[lisp-dependencies]
json = { git = "https://github.com/cj-lisp/json", tag = "v1.0" }
http = { path = "../local/http" }

[cangjie-dependencies]
std = "0.53.4"
```

### 4.3 加载机制
```lisp
;; 在Lisp中
(import "json")            ; 从cjlpm缓存加载
(import "http")            ; 加载HTTP库
(load-file "src/utils.lisp")  ; 加载本地文件
(eval-string "(+ 1 2)")    ; 动态求值
```

---

## 五、核心功能实现

### 5.1 双向互调用

#### **仓颉 → Lisp**
```cangjie
let lisp = LispInterpreter()
lisp.evalString("(define (add a b) (+ a b))")
let result = lisp.call("add", 10, 20)  // Int64: 30
```

#### **Lisp → 仓颉**
```lisp
// 仓颉端：注册函数到Lisp环境
lisp.register("http-get", { url: String =>
    let client = HttpClient()
    client.get(url).body
})

// Lisp端调用
(http-get "https://api.example.com/data")
```

### 5.2 `eval`系统与沙箱

#### **基础eval**
```lisp
(eval '(+ 1 2 3))          ; 表达式求值
(eval-string "(def! x 100)") ; 字符串求值
```

#### **安全沙箱**
```lisp
;; 受限环境执行
(safe-eval code :allow '(+ - * /) :deny '(file-delete system-call))

;; 超时控制
(eval-with-timeout code "5s")
```

**实现**: 通过自定义`Env`类限制符号查找，结合仓颉的 `Future<T>.get(timeout)` 超时机制。

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
- [ ] `let`解构绑定
- [ ] `->`管道操作符
- [ ] `eval-string`与沙箱
- [ ] 基础`cjlpm`包管理

### **阶段3：现代化（2周）**
- [ ] 向量/哈希字面量`[]`/`{}`
- [ ] 字符串插值`#"..."`
- [ ] 模式匹配`match`
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
