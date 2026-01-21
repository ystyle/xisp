# Xisp 选项系统实现总结

## ✅ 实现完成

Xisp 现在支持完整的选项配置系统，允许用户灵活地定制解释器功能。

## 📋 新增功能

### 1. 选项类型系统

**文件**: `src/options.cj`

定义了 `InterpreterOption` 类型，所有配置选项都使用这个类型：

```cangjie
public type InterpreterOption = (LispInterpreter) -> Unit
```

### 2. 关键字别名选项

#### 预设语言包

- `withChineseKeywords()` - 中文关键字（定义、过程、如果、让、打印等）
- `withJapaneseKeywords()` - 日语关键字（定義、もし、表示等）
- `withKoreanKeywords()` - 韩语关键字（정의、만약、출력等）

#### 自定义别名

- `withKeywordAlias(alias, original)` - 注册单个别名
- `withKeywordAliases(aliases)` - 批量注册别名

**示例**:
```cangjie
// 企业场景：简化关键字
let interpreter = LispInterpreter([
    withKeywordAliases([
        ("def", "define"),
        ("fn", "lambda"),
        ("->>", "thread-last")
    ])
])
```

### 3. 标准库模块选项

- `withStdIO()` - 启用 std.io 模块（文件读写）
- `withStdFS()` - 启用 std.fs 模块（文件系统操作）
- `withStdCollection()` - 启用 std.collection 模块（预留）
- `withStdLib()` - 启用所有标准库

**示例**:
```cangjie
// 仅启用需要的模块
let interpreter = LispInterpreter([
    withStdIO()  // 仅启用文件读写
])
```

### 4. 配置选项

- `withQuietMode()` - 静默模式（不显示 Banner）
- `withVerboseMode()` - 详细模式（显示更多信息）
- `withDebugMode()` - 调试模式
- `withOutputFn(fn)` - 自定义输出函数

**示例**:
```cangjie
// 记录日志到文件
let interpreter = LispInterpreter([
    withOutputFn({ s => logger.log(s) }),
    withVerboseMode()
])
```

### 5. REPL 动态切换

在 REPL 中使用 `,lang` 命令动态切换语言：

```
xisp> ,lang zh
启用中文关键字支持...
现在可以使用:
  定义 (define)  过程 (lambda)  如果 (if)  让 (let)
  打印 (println)  显示 (print)

xisp> (定义 年龄 25)
25.000000

xisp> (打印 年龄)
25.000000

xisp> ,lang en
切换回英文关键字模式

xisp> (define age 25)
25.000000
```

## 🔧 修改的文件

### 新增文件

1. **src/options.cj** - 选项类型和所有 with 函数
2. **examples/options_usage.cj** - 选项使用示例

### 修改文件

1. **src/core/types.cj**
   - 在 `Environment` 类中添加 `keywordAliases` HashMap
   - 添加 `registerKeywordAlias()` 方法
   - 添加 `lookupKeyword()` 方法

2. **src/core/evaluator.cj**
   - 在 `evalList()` 方法中先查找关键字别名

3. **src/interpreter.cj**
   - 添加配置字段（showBanner, verbose, debug, outputFn）
   - 添加带选项的构造函数
   - 添加关键字别名注册方法
   - 添加标准库模块注册方法
   - 添加配置方法

4. **src/repl/repl.cj**
   - 添加 `showBanner` 字段和 `setShowBanner()` 方法
   - 添加 `,lang` 命令支持
   - 添加 `enableChineseKeywords()` 和 `disableChineseKeywords()` 方法
   - 更新帮助信息

5. **src/main.cj**
   - 使用新的选项系统配置解释器

## 📊 使用示例

### 示例 1：默认配置（推荐）

```cangjie
main(): Int64 {
    let interpreter = LispInterpreter([
        // 默认：英文关键字，std.io 和 std.fs 已启用
    ])
    interpreter.runREPL()
}
```

### 示例 2：启用中文关键字

```cangjie
main(): Int64 {
    let interpreter = LispInterpreter([
        withChineseKeywords()
    ])
    interpreter.runREPL()
}
```

### 示例 3：企业自定义

```cangjie
main(): Int64 {
    let interpreter = LispInterpreter([
        // 简化关键字
        withKeywordAliases([
            ("def", "define"),
            ("fn", "lambda")
        ]),
        // 仅启用需要的模块
        withStdIO()
    ])
    interpreter.runREPL()
}
```

### 示例 4：多语言混合

```cangjie
main(): Int64 {
    let interpreter = LispInterpreter([
        withChineseKeywords(),
        withJapaneseKeywords(),  // 可以同时启用！
        withKeywordAlias("정의", "define"),  // 额外添加韩语
        withVerboseMode()
    ])
    interpreter.runREPL()
}
```

## 🎯 关键特性

1. **完全灵活** - 用户可以定义任意关键字别名
2. **多语言支持** - 预设中、日、韩语言包
3. **可组合** - 多个选项可以组合使用
4. **可扩展** - 未来添加新功能只需添加新的 `withXxx` 函数
5. **动态切换** - REPL 中支持 `,lang` 命令动态切换
6. **向后兼容** - 默认配置保持原有行为

## 🧪 测试验证

### 测试 1：中文关键字

```lisp
xisp> ,lang zh
xisp> (定义 年龄 25)
25.000000

xisp> (定义 平方 (过程 (x) (* x x)))
#<procedure>

xisp> (打印 (平方 5))
25.000000
```

### 测试 2：中英文混合

```lisp
xisp> (define age 25)      ; 英文
25.000000

xisp> (定义 姓名 "张三")    ; 中文
"张三"

xisp> (println age 姓名)   ; 混合使用
25.000000
"张三"
```

### 测试 3：企业别名

```lisp
xisp> (def x 10)           ; def -> define
10.000000

xisp> (fn (x) (* x x))     ; fn -> lambda
#<procedure>
```

## 📖 相关文档

- [选项使用示例](../examples/options_usage.cj)
- [中文支持文档](chinese_support.md)
- [中文快速开始](chinese_quickstart.md)

## 🚀 未来扩展

可以继续添加的选项：

1. **更多语言包**
   - `withSpanishKeywords()` - 西班牙语
   - `withFrenchKeywords()` - 法语
   - `withGermanKeywords()` - 德语

2. **更多标准库模块**
   - `withStdMath()` - 数学函数
   - `withStdString()` - 字符串处理
   - `withStdDateTime()` - 日期时间

3. **性能选项**
   - `withOptimization(level)` - 优化级别
   - `withBytecodeCache()` - 字节码缓存

4. **安全选项**
   - `withSandbox()` - 沙箱模式
   - `withMemoryLimit(limit)` - 内存限制
   - `withTimeLimit(seconds)` - 执行时间限制

---

**实现日期**: 2026-01-22
**版本**: 0.1.0 MVP
