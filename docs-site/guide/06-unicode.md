# Unicode 支持

**阅读时间**: 10 分钟

Xisp 完全支持 Unicode 字符，包括中文、日文、韩文等。你可以在符号名、字符串、注释中使用任何 Unicode 字符。

---

## 中文关键字

**默认情况下**：Xisp 的内置函数使用英文名称（如 `define`、`print` 等）。

**启用中文关键字后**：可以使用中文关键字（`定义`、`打印`、`过程` 等）。

### 方式 1：REPL 动态启用（推荐）

```lisp
xisp> ,lang zh
启用中文关键字支持...
  定义 (define)  过程 (lambda)  如果 (if)  让 (let)
  打印 (println)  显示 (print)

xisp> (定义 年龄 25)
xisp> (打印 年龄)
25
```

### 方式 2：代码中启用（脚本）

```cangjie
import ystyle::xisp.*

let interpreter = LispInterpreter([
    withChineseKeywords()  // 启用中文关键字
])
```

### 方式 3：自定义别名

```cangjie
let interpreter = LispInterpreter([
    withKeywordAlias("定义", "define"),
    withKeywordAlias("打印", "println"),
])
```

## 多语言关键字

除中文外，还支持日文和韩文关键字：

```cangjie
// 日文关键字
let interpreter = LispInterpreter([
    withJapaneseKeywords()
])
// 定義 -> define, もし -> if, 表示 -> println, λ -> lambda

// 韩文关键字
let interpreter = LispInterpreter([
    withKoreanKeywords()
])
// 정의 -> define, 만약 -> if, 출력 -> println
```

## 默认支持（无需启用）

- ✅ 中文变量名（英文关键字 + 中文变量）
- ✅ 中文函数名
- ✅ 中文参数名
- ✅ 中文符号名
- ✅ 中文字符串和注释

## 示例

### 英文关键字 + 中文变量名

```lisp
(define 计算面积 (lambda (宽 高)
  (* 宽 高)))

(计算面积 3 4)
; => 12
```

### 中文关键字（启用后）

```lisp
(定义 年龄 25)
(定义 (双倍 x) (* x 2))
(双倍 21)
; => 42
```

---

## 下一步

- [API 参考](../api/) - 全部内置函数
