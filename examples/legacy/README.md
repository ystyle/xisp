# Legacy - 特色功能演示

本目录展示 Xisp 的特殊功能。

## 📁 文件列表

| 文件 | 说明 | 功能 |
|------|------|------|
| [chinese_demo.lisp](./chinese_demo.lisp) | 中文编程演示 | 完整的中文关键字支持 |

## 🌟 中文编程支持

Xisp 支持**完整的中文编程**，包括中文关键字和 Unicode 变量名。

**📖 详细文档**: [中文支持完整文档](../../docs/unicode/chinese_support.md)

### 快速体验

```bash
# 1. 启动 REPL
./target/release/bin/ystyle::xisp.cli

# 2. 启用中文关键字
xisp> ,lang zh

# 3. 使用中文编程
xisp> (定义 年龄 25)
xisp> (打印 年龄)
```

### 中文关键字对照

| 英文 | 中文 | 说明 |
|------|------|------|
| `define` | `定义` | 定义变量和函数 |
| `lambda` | `过程` | 匿名函数 |
| `if` | `如果` | 条件判断 |
| `let` | `让` | 局部绑定 |
| `println` | `打印` | 输出 |
| `print` | `显示` | 输出 |

### Unicode 变量名

```lisp
; 中文
(定义 年龄 25)

; 日文
(定義 日本語 "東京")

; 韩文
(정의 한국어 "서울")

; 混合
(define test値 "mixed")
```

## 📖 相关文档

- **[中文支持完整文档](../../docs/unicode/chinese_support.md)** - 中文关键字和 Unicode 支持
- **[中文快速开始](../../docs/unicode/chinese_quickstart.md)** - 快速上手指南
- **[Unicode 支持说明](../../UNICODE_SUPPORT.md)** - Unicode 支持概述
- **[选项系统文档](../../docs/integration/options_system.md)** - 选项配置系统

---

**特色功能**：Xisp 是少数几个完整支持中文编程的现代编程语言之一！
