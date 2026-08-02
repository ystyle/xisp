# Xisp - 星枢

> 仓颉嵌入式 Lisp 脚本语言

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Version](https://img.shields.io/badge/version-0.2.0-green.svg)](https://github.com/ystyle/xisp)
[![Cangjie](https://img.shields.io/badge/Cangjie-1.1.0-orange.svg)](https://cangjie-lang.cn/)
![star](https://atomgit.com/ystyle/xisp/star/badge.svg)

## 简介

Xisp（星枢）是一个用仓颉（Cangjie）语言编写的嵌入式 Lisp 解释器，具有以下特点：

- ✨ **纯仓颉实现** - 无需外部依赖
- 🎯 **图灵完备** - 支持函数式编程
- 📦 **嵌入式友好** - 可轻松集成到仓颉项目
- 🚀 **高性能** - 基于仓颉原生性能
- 🔐 **安全沙箱** - 限制文件访问和函数调用权限
- 🎨 **强大的宏系统** - 元编程能力，扩展语法
- 📚 **模块化支持** - 代码组织和复用
- 🔧 **REPL 交互** - 支持交互式开发
- 🌏 **完整 Unicode 支持** - 支持中文、日文、韩文等多语言编程

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
./target/release/bin/ystyle::xisp.cli

# 运行 Lisp 脚本
./target/release/bin/ystyle::xisp.cli examples/tutorial.lisp
```

## 示例代码

```shell
❯ ./target/release/bin/ystyle::xisp.cli
╔═══════════════════════════════════════════════════════════╗
║           星枢 - 仓颉嵌入式 Lisp 脚本语言                   ║
║                   版本 0.1.0 - MVP                        ║
╚═══════════════════════════════════════════════════════════╝
xisp> (print "你好，仓颉")
"你好，仓颉"
nil
xisp> (print "你好，星枢")
"你好，星枢"
nil
xisp> (print "Hello, Xisp")
"Hello, Xisp"
nil
xisp>
```

### REPL 脚本示例（Lisp）

Lisp 脚本示例位于 `examples/` 目录，按难度和学习路径组织：

| 目录 | 说明 | 学习时间 | 示例文件 |
|------|------|----------|----------|
| [01-basics](examples/01-basics/) | 基础教程 | 35分钟 | 快速开始、基础语法、文件I/O |
| [02-intermediate](examples/02-intermediate/) | 中级特性 | 40分钟 | 高阶函数、作用域、闭包 |
| [03-advanced](examples/03-advanced/) | 高级特性 | 30分钟 | 模式匹配、守卫条件、解构 |
| [04-macros](examples/04-macros/) | 宏系统 | 15分钟 | 宏定义、宏展开、卫生宏 |
| [05-modules](examples/05-modules/) | 模块系统 | 20分钟 | 模块导入、导出、管理 |
| [06-interop](examples/06-interop/) | 互操作 | 20分钟 | 仓颉桥接、双向调用 |
| [legacy](examples/legacy/) | 特色功能演示 | - | 中文编程、Unicode 支持 |

**快速开始**:
```bash
# 5分钟快速体验
./target/release/bin/ystyle::xisp.cli examples/01-basics/01_quick_start.lisp

# 完整基础教程
./target/release/bin/ystyle::xisp.cli examples/01-basics/02_tutorial.lisp
```

**完整学习路径**: 查看 [examples/README.md](examples/README.md)

---

### 仓颉嵌入式示例（Cangjie）

仓颉代码示例位于 `src/examples/` 目录，展示如何在仓颉项目中嵌入和使用 Xisp：

| 示例 | 说明 | 运行方式 |
|------|------|----------|
| [选项系统](src/examples/options_usage/) | 解释器选项配置 | `cjpm run --name options_usage` |
| [沙箱系统](src/examples/sandbox_demo/) | 安全沙箱使用 | `cjpm run --name sandbox_demo` |
| [扩展功能演示](src/examples/extension_demo/) | LispConvertible/LispDeserializable 接口和双向转换 | `cjpm run --name extension_demo` |
| [模式匹配](src/examples/match_demo/) | 模式匹配测试 | `cjpm run --name match_demo` |
| [现代化语法](src/examples/modern_syntax/) | 现代语法测试 | `cjpm run --name modern_syntax` |
| [解构绑定](src/examples/test_destruct/) | 解构绑定测试 | `cjpm run --name test_destruct` |
| [管道操作符](src/examples/test_pipeline/) | 管道操作符测试 | `cjpm run --name test_pipeline` |
| [守卫条件](src/examples/guard_test/) | 守卫条件测试 | `cjpm run --name guard_test` |

**更多详情**: 查看 [docs-site/integration/bridge.md](docs-site/integration/bridge.md) - 桥接层 API 文档

---

## 核心语法

### 特殊形式

- [`define`](docs-site/guide/02-basics.md) - 定义变量和函数
- [`lambda`](docs-site/guide/02-basics.md) - 匿名函数
- [`if`](docs-site/guide/02-basics.md) - 条件判断
- [`quote`](docs-site/guide/02-basics.md) / `'` - 引用
- [`let`](docs-site/guide/03-modern.md) - 局部绑定（支持解构）
- [`begin`](docs-site/guide/02-basics.md) - 顺序执行
- [`set!`](docs-site/guide/02-basics.md) - 变量赋值
- [`match`](docs-site/guide/03-modern.md) - 模式匹配
- [`->`](docs-site/guide/03-modern.md) - 管道操作符（线程宏）

### 内置函数

**算术**: `+` `-` `*` `/` `mod` `round` `sum` `product`
**比较**: `=` `!=` `<` `>` `<=` `>=` `eq?`
**逻辑**: `and` `or` `not`
**列表**: `list` `prepend` `first` `rest` `map` `filter` `reduce` `length` `range`
**谓词**: `number?` `string?` `symbol?` `list?` `null?` `procedure?`
**打印**: `print` `println` `princ` `display` `newline`

**完整语法参考**: [docs-site/guide/02-basics.md](docs-site/guide/02-basics.md)

**标准符号参考**: [docs-site/api/index.md](docs-site/api/index.md) - 109 个标准库函数快速索引

### 现代化语法

- **向量字面量**: `[1 2 3]` → 列表
- **哈希映射**: `{:key value}` → 哈希映射
- **字符串插值**: `#"Value: #{x}"` → 动态字符串
- **解构绑定**: `(let [[x y & rest] list] ...)`
- **管道操作**: `(-> x (f) (g))`
- **模式匹配**: `(match x (pattern) result ...)`

**现代化语法文档**: [docs-site/guide/03-modern.md](docs-site/guide/03-modern.md)

## Unicode 和多语言支持

### REPL 中的 Unicode 支持

在 REPL 中可以直接使用中文变量名和关键字：

```lisp
; 中文变量名
xisp> (define 年龄 25)
25.000000

xisp> (define 姓名 "张三")
"张三"

xisp> (define 计算面积 (lambda (宽 高) (* 宽 高)))
#<procedure>

xisp> (计算面积 5 3)
15.000000

; 启用中文关键字
xisp> ,lang zh
启用中文关键字支持...
现在可以使用:  定义 (define)  过程 (lambda)  如果 (if)  让 (let)

xisp> (定义 年龄 25)
25.000000
```

**示例文件**: [examples/03-advanced/03_unicode.lisp](examples/03-advanced/)
**详细文档**: [docs-site/guide/06-unicode.md](docs-site/guide/06-unicode.md)

---

### 仓颉代码中的 Unicode 支持

在仓颉代码中可以使用选项系统配置中文关键字：

```cangjie
import ystyle::xisp.*

main() {
    // 启用中文关键字
    let interpreter = LispInterpreter([
        withChineseKeywords()    // 使用中文关键字（定义、过程、如果等）
    ])
    interpreter.runREPL()
}
```

**示例代码**: [src/examples/options_usage/main.cj](src/examples/options_usage/)
**详细文档**: [docs-site/integration/options.md](docs-site/integration/options.md)

---

## 仓颉桥接

Xisp 提供了完整的双向桥接能力，支持 Lisp 和仓颉之间的互操作。

### LispInterpreter 基本用法

在仓颉代码中嵌入 Lisp 解释器：

```cangjie
import ystyle::xisp.*

main() {
    // 创建解释器
    let interpreter = LispInterpreter()

    // 求值表达式
    let result = interpreter.eval("(+ 1 2 3)")
    println(result)  // 输出: 6.000000

    // 定义并调用函数
    interpreter.eval("(define (add x y) (+ x y))")
    let sum = interpreter.eval("(add 10 20)")
    println(sum)  // 输出: 30.000000
}
```

### Lisp 调用仓颉函数

在仓颉中注册自定义函数，供 Lisp 调用：

```cangjie
import ystyle::xisp.*

main() {
    let interpreter = LispInterpreter()

    // 注册自定义函数
    interpreter.registerBridgeFunction("square", { args =>
        if (args.size > 0 && let Int(n) <- args[0]) {
            Int(n * n)
        } else {
            Str("Error: argument must be a number")
        }
    })

    // 在 Lisp 中调用自定义函数
    let result = interpreter.eval("(square 5)")
    println(result)  // 输出: 25

    // 也可以在 Lisp 代码中直接使用
    interpreter.eval("(println (square 10))")  // 输出: 100
}
```

### 仓颉调用 Lisp 函数（反向调用）

使用 `Interpreter.call()` 方法从仓颉调用已定义的 Lisp 函数：

```cangjie
import ystyle::xisp.*

main() {
    let interpreter = LispInterpreter()

    // 定义 Lisp 函数
    interpreter.eval("(define (add x y) (+ x y))")
    interpreter.eval("(define (square x) (* x x))")

    // 从仓颉调用 Lisp 函数
    let sum = interpreter.call("add", [10, 20])
    if (let Some(i) <- sum.asInt()) {
        println("10 + 20 = ${i}")  // 输出: 10 + 20 = 30
    }

    let squared = interpreter.call("square", [5])
    if (let Some(i) <- squared.asInt()) {
        println("5² = ${i}")  // 输出: 5² = 25
    }
}
```

### 类型转换接口

**LispConvertible**（仓颉 → Lisp）：让自定义类型可转换为 Lisp 值

```cangjie
class Point <: LispConvertible {
    let x: Float64
    let y: Float64
    public func toLisp(): LispValue { ... }
}
```

**LispDeserializable**（Lisp → 仓颉）：从 Lisp 值创建仓颉对象

```cangjie
class Point <: LispDeserializable<Point> {
    public static func fromLisp(value: LispValue): ?Point { ... }
}
```

**完整桥接 API 文档**: [docs-site/integration/bridge.md](docs-site/integration/bridge.md)

## 文档

**文档索引**: [docs/README.md](docs/README.md) - 完整文档导航

### 语法文档
- [基础语法](docs-site/guide/02-basics.md) - 数据类型、特殊形式、内置函数
- [现代语法特性](docs-site/guide/03-modern.md) - 向量、哈希、插值、解构、管道、模式匹配
- [宏系统](docs-site/guide/04-macros.md) - 宏定义、宏展开、卫生宏
- [设计文档](docs/design.md) - 架构设计和技术选型
- [模块系统](docs-site/guide/05-modules.md) - 模块导入、导出、管理

### 集成文档
- [桥接层 API](docs-site/integration/bridge.md) - Lisp 与仓颉互操作（双向调用、类型转换）
- [选项系统](docs-site/integration/options.md) - 解释器配置选项
- [沙箱系统](docs-site/integration/sandbox.md) - 安全执行环境和权限控制

### 参考文档
- [标准符号附录](docs-site/api/index.md) - 84 个标准库符号分类参考
- [模块来源设计](docs/module-source-design.md) - 模块系统扩展性设计

### Unicode 支持
- [Unicode 支持](docs-site/guide/06-unicode.md) - Unicode 和多语言支持
- [中文支持详细文档](docs-site/guide/06-unicode.md) - 完整的中文关键字说明
- [中文快速开始](docs-site/guide/06-unicode.md) - 中文编程入门

## 贡献

欢迎贡献代码、报告问题或提出建议！

## 许可证

[MIT License](LICENSE)

## 链接

- [仓颉语言官网](https://cangjie-lang.cn/)
- [示例代码](examples/) - Lisp 脚本示例
- [仓颉示例](src/examples/) - 仓颉代码示例
- [任务追踪](task.md) - 开发进度

---

**版本**: 0.2.0
**最后更新**: 2026-08-02
