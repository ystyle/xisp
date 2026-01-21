# Xisp - 星枢

> 仓颉嵌入式 Lisp 脚本语言

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Version](https://img.shields.io/badge/version-0.1.0--MVP-green.svg)](https://github.com/ystyle/xisp)
[![Cangjie](https://img.shields.io/badge/Cangjie-1.1.0-orange.svg)](https://cangjie-lang.cn/)

## 简介

Xisp（星枢）是一个用仓颉（Cangjie）语言编写的嵌入式 Lisp 解释器，具有以下特点：

- ✨ **纯仓颉实现** - 无需外部依赖
- 🎯 **图灵完备** - 支持函数式编程
- 📦 **嵌入式友好** - 可轻松集成到仓颉项目
- 🚀 **高性能** - 基于仓颉原生性能
- 🔧 **REPL 交互** - 支持交互式开发
- 🌏 **完整中文支持** - 支持中文变量名和编程

## 快速开始

### 前置要求

- [仓颉 SDK](https://cangjie-lang.cn/) 1.1.0+

### 编译

```bash
cjpm build
```

### 运行

```bash
# 启动 REPL
./xisp

# 或运行示例脚本
./xisp < examples/tutorial.lisp
```

## 使用示例

### Lisp 语法

```lisp
; 算术运算
xisp> (+ 1 2 3)
6.000000

; 变量定义
xisp> (define x 42)
42.000000

; 函数定义
xisp> (define (square x) (* x x))
#<procedure>

xisp> (square 7)
49.000000

; 列表操作
xisp> (map square (list 1 2 3))
(1.000000 4.000000 9.000000)

; 高阶函数
xisp> (filter (lambda (n) (> n 2)) (list 1 2 3 4 5))
(3.000000 4.000000 5.000000)
```

### 中文和 Unicode 支持

Xisp 完全支持中文变量名和多语言编程。

#### 默认模式（英文关键字 + 中文变量）

```lisp
xisp> (define 年龄 25)
25.000000

xisp> (define 姓名 "张三")

xisp> (define 计算面积 (lambda (宽 高) (* 宽 高)))

xisp> (println (计算面积 5 3))
15.000000
```

#### 启用中文关键字

在 REPL 中输入 `,lang zh`：

```lisp
xisp> ,lang zh
启用中文关键字支持...
现在可以使用:
  定义 (define)  过程 (lambda)  如果 (if)  让 (let)
  打印 (println)  显示 (print)

xisp> (定义 年龄 25)
25.000000

xisp> (定义 计算面积 (lambda (宽 高) (* 宽 高)))

xisp> (打印 (计算面积 5 3))
15.000000
```

切换回英文：
```lisp
xisp> ,lang en
切换回英文关键字模式
```

**详细文档**：
- [Unicode 支持文档](UNICODE_SUPPORT.md) - Unicode 和多语言支持
- [中文支持详细文档](docs/chinese_support.md) - 完整的中文关键字说明
- [选项系统文档](docs/options_system.md) - 选项配置系统

### 打印输出

```lisp
; 打印并换行
xisp> (println "Hello" "Xisp")
"Hello" "Xisp"

nil

; 不换行打印
xisp> (print "No ") (print "newline")
"No ""newline"

; 换行
xisp> (newline)
```

### 文件 I/O

```lisp
; 写入文件
xisp> (cangjie:io:write-file "hello.txt" "Hello, World!")
"Success: written to hello.txt"

; 读取文件
xisp> (cangjie:io:read-file "hello.txt")
"Hello, World!"

; 追加内容
xisp> (cangjie:io:append-file "hello.txt" "\nAppended line")
"Success: appended to hello.txt"

; 检查文件存在
xisp> (cangjie:fs:exists? "hello.txt")
true

; 判断文件类型
xisp> (cangjie:fs:file? "hello.txt")
true

xisp> (cangjie:fs:directory? "hello.txt")
false

; 列出目录
xisp> (cangjie:fs:list-dir ".")
("src" "docs" "examples" "README.md" ...)
```

## 内置函数

### 算术
- `+` `-` `*` `/` `mod`

### 比较
- `=` `<` `>` `<=` `>=`

### 逻辑
- `and` `or` `not`

### 列表
- `list` `cons` `prepend` `first` `rest` `second` `third`
- `length` `map` `filter` `reduce` `sum` `product`
- `reverse` `range`

### 谓词
- `number?` `string?` `symbol?` `list?` `null?` `procedure?`

### 打印
- `print` `println` `princ` `display` `newline`

### 仓颉桥接（文件 I/O）

#### std.io 模块
- `cangjie:io:read-file` - 读取文件内容
- `cangjie:io:write-file` - 写入文件（覆盖）
- `cangjie:io:append-file` - 追加内容到文件

#### std.fs 模块
- `cangjie:fs:exists?` - 检查文件/目录是否存在
- `cangjie:fs:file?` - 判断是否为文件
- `cangjie:fs:directory?` - 判断是否为目录
- `cangjie:fs:list-dir` - 列出目录内容

## 特殊形式

- `define` - 定义变量和函数
- `lambda` - 匿名函数
- `if` - 条件判断
- `quote` / `'` - 引用
- `let` - 局部绑定
- `begin` - 顺序执行
- `set!` - 变量赋值

## 示例代码

项目包含丰富的示例代码：

| 示例 | 说明 | 运行 |
|------|------|------|
| [基础教程](examples/tutorial.lisp) | 12 个基础主题 | `./xisp < examples/tutorial.lisp` |
| [打印测试](examples/print_test.lisp) | 打印功能完整测试 | `./xisp < examples/print_test.lisp` |
| [高级特性](examples/advanced.lisp) | 闭包、高阶函数等 | `./xisp < examples/advanced.lisp` |
| [快速验证](examples/quick_test.lisp) | 核心功能验证 | `./xisp < examples/quick_test.lisp` |
| [文件 I/O 测试](examples/file_io_test.lisp) | 文件读写和目录操作 | `./xisp < examples/file_io_test.lisp` |

更多示例请查看 [examples/](examples/) 目录。

## 测试

```bash
# 运行所有测试
cjpm test

# 或使用测试脚本
./test.sh
```

当前测试覆盖：
- ✅ 46 个单元测试全部通过
- ✅ 核心数据类型
- ✅ 词法和语法分析
- ✅ 求值器
- ✅ 内置函数
- ✅ 特殊形式

## REPL 命令

| 命令 | 功能 |
|------|------|
| `,help` | 显示帮助信息 |
| `,env` | 查看环境变量 |
| `,lang zh` | 启用中文关键字 |
| `,lang en` | 切换回英文关键字 |
| `,exit` | 退出 REPL |
| `,quit` | 退出 REPL |

## 项目结构

```
xisp/
├── src/                  # 源代码
│   ├── core/            # 核心功能（类型、求值器、内置函数）
│   ├── parser/          # 词法和语法分析
│   ├── bridge/          # 仓颉互操作桥接层
│   └── repl/            # REPL 交互
├── examples/            # 示例代码
├── docs/                # 文档
├── tests/               # 测试
├── cjpm.toml            # 项目配置
├── task.md              # 任务追踪
└── README.md            # 本文件
```

## 开发路线图

- [x] **M1: 核心 Lisp 解释器 (MVP)** ✅ 已完成
  - [x] S-表达式解析和求值
  - [x] 特殊形式（define, lambda, if, quote, let）
  - [x] 闭包和词法作用域
  - [x] REPL 交互
  - [x] 46 个测试通过

- [ ] **M2: 仓颉互操作桥接** 🚧 进行中
  - [x] 桥接层基础架构
  - [x] LispConvertible 接口和扩展系统
  - [x] std.io 和 std.fs 桥接函数
  - [x] 实际文件 I/O 实现
  - [ ] 仓颉调用 Lisp 函数
  - [ ] 更多标准库对接

- [ ] **M3: 现代化语法扩展**
  - [ ] 解构绑定、管道操作符
  - [ ] 向量/哈希字面量
  - [ ] 字符串插值

- [ ] **M4: 生产级特性**
  - [ ] 安全沙箱机制
  - [ ] 性能优化（字节码缓存）
  - [ ] 完整的调试工具

详细任务列表请查看 [task.md](task.md)。

## 贡献

欢迎贡献代码、报告问题或提出建议！

## 许可证

[MIT License](LICENSE)

## 链接

- [仓颉语言官网](https://cangjie-lang.cn/)
- [项目文档](docs/)
  - [桥接层 API 文档](docs/bridge.md)
  - [Lisp 核心功能](docs/core.md)
  - [设计文档](docs/design.md)
- [示例代码](examples/)
- [任务追踪](task.md)

---

**版本**: 0.1.0 MVP
**最后更新**: 2026-01-21
