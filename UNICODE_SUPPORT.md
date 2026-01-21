# Unicode 支持

Xisp 完全支持 Unicode 字符，包括中文、日文、韩文等。你可以在变量名、函数名、字符串、注释中使用任何 Unicode 字符。

## ✅ 默认支持（无需配置）

### 变量和函数名

可以使用任何 Unicode 字符作为变量名和函数名：

```lisp
; 中文
xisp> (define 年龄 25)
25.000000

xisp> (define 计算面积 (lambda (宽 高) (* 宽 高)))

; 日文
xisp> (define 日本語 "東京")
"東京"

xisp> (define 関数 (lambda (x) (* x x)))

; 韩文
xisp> (define 한국어 "서울")
"서울"

; 混合
xisp> (define test値 "mixed")
"mixed"
```

### 支持的字符

- **中文**：`中文变量` `函数名` `列表`
- **日文**：`日本語` `変数` `テスト`
- **韩文**：`한국어` `변수` `함수`
- **混合**：`test测试` `my变量` `変数foo`

### 字符串和注释

```lisp
; Unicode 字符串
(define 问候 "你好，世界！")
(define greeting "Hello 世界！")

; Unicode 注释
; 这是一个中文注释
(define π 3.14159)
```

## ⚙️ Unicode 关键字（可选启用）

### 启用中文关键字

在 REPL 中输入 `,lang zh`：

```lisp
xisp> ,lang zh
启用中文关键字支持...
现在可以使用:
  定义 (define)  过程 (lambda)  如果 (if)  让 (let)
  打印 (println)  显示 (print)

xisp> (定义 年龄 25)      ; 使用"定义"代替"define"
25.000000

xisp> (定义 平方 (过程 (x) (* x x)))  ; 使用"过程"代替"lambda"

xisp> (打印 年龄)          ; 使用"打印"代替"println"
25.000000
```

**说明**：使用"过程"而不是"函数"或"λ"，避免与各种语言中的 `func` 关键字混淆。

### 启用日文关键字

修改 `src/main.cj`：

```cangjie
main(): Int64 {
    let interpreter = LispInterpreter([
        withJapaneseKeywords()  // 启用日文关键字
    ])
    interpreter.runREPL()
}
```

### 自定义别名

```cangjie
main(): Int64 {
    let interpreter = LispInterpreter([
        // 企业简化版
        withKeywordAlias("def", "define"),
        withKeywordAlias("fn", "lambda")
    ])
    interpreter.runREPL()
}
```

## 📖 详细文档

- [中文支持详细文档](docs/chinese_support.md) - 完整的中文关键字说明
- [中文快速开始](docs/chinese_quickstart.md) - 快速上手指南
- [选项系统文档](docs/options_system.md) - 选项配置系统

## 🌍 国际化

Xisp 支持 Unicode 标准，可以用于任何语言：

- **亚洲语言**：中文、日文、韩文、越南语等
- **欧洲语言**：法语、德语、西班牙语、俄语等
- **中东语言**：阿拉伯语、希伯来语等
- **其他**：可以使用任何 Unicode 字符

## 💡 最佳实践

### 推荐做法

```lisp
; 默认模式：英文关键字 + Unicode 变量名
(define 圆周率 3.14159)
(define 计算圆面积 (lambda (半径)
  (* 圆周率 半径 半径)))
```

### 生产环境

建议使用英文关键字，保持代码兼容性：

```lisp
; 生产推荐
(define calculate-circle-area (lambda (radius)
  (* 3.14159 radius radius)))
```

### 教学场景

可以启用中文关键字，降低学习门槛：

```lisp
; 教学场景：启用中文关键字
xisp> ,lang zh
xisp> (定义 圆面积 (lambda (r) (* 3.14159 r r)))
```

---

**更多信息**：查看 [docs/](docs/) 目录下的详细文档
