# 中文支持快速开始

## ⚠️ 重要说明

**默认情况下**：只支持英文关键字（`define`、`lambda` 等），但可以使用中文变量名

**启用中文关键字后**：可以使用中文关键字（`定义`、`过程` 等）

## 🎯 快速开始

### 方式 1：在 REPL 中启用（推荐）

```lisp
xisp> ,lang zh
启用中文关键字支持...
现在可以使用:
  定义 (define)  过程 (lambda)  如果 (if)  让 (let)
  打印 (println)  显示 (print)

xisp> (定义 年龄 25)
25.000000

xisp> (定义 平方 (过程 (x) (* x x)))

xisp> (打印 年龄)
25.000000
```

**说明**：使用"过程"代替 lambda，避免与 `func` 混淆。

**优点**：
- ✅ 无需修改代码
- ✅ 随时切换
- ✅ 方便测试

### 方式 2：在代码中启用

修改 `src/main.cj`：

```cangjie
main(): Int64 {
    let interpreter = LispInterpreter([
        withChineseKeywords()  // 启用中文关键字
    ])
    interpreter.runREPL()
}
```

重新编译：
```bash
cjpm build
./xisp
```

**优点**：
- ✅ 启动即用
- ✅ 适合脚本

### 方式 3：使用自定义别名

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

## 📝 使用示例

### 默认模式（英文关键字）

```lisp
xisp> (define 年龄 25)
25.000000

xisp> (define 计算面积 (lambda (宽 高)
  (* 宽 高)))

xisp> (print (计算面积 5 3))
15.000000
```

### 中文模式（启用后）

```lisp
xisp> ,lang zh

xisp> (定义 年龄 25)
25.000000

xisp> (定义 计算面积 (lambda (宽 高)
  (* 宽 高)))

xisp> (打印 (计算面积 5 3))
15.000000
```

## 🔄 切换语言

```lisp
xisp> (define x 10)        ; 英文
10.000000

xisp> ,lang zh              ; 切换到中文
xisp> (定义 y 20)          ; 中文
20.000000

xisp> ,lang en              ; 切换回英文
xisp> (define z 30)         ; 英文
30.000000
```

## 💡 常用命令

### REPL 命令

```
,lang zh    - 启用中文关键字
,lang en    - 切换回英文
,help       - 显示帮助
,env        - 查看环境变量
,exit       - 退出
```

### 中文关键字对照表

| 中文 | 英文 | 说明 |
|------|------|------|
| 定义 | define | 定义变量/函数 |
| 过程 | lambda | 匿名过程/函数 |
| 如果 | if | 条件判断 |
| 让 | let | 局部绑定 |
| 打印 | println | 打印并换行 |
| 显示 | print | 打印不换行 |

**注**：使用"过程"（procedure）而不是"函数"或"λ"，避免与其他语言的 `func` 关键字混淆。

## ✨ 最佳实践

### 初学者

1. **第1步**：使用默认模式（英文关键字 + 中文变量）
   ```lisp
   (define 年龄 25)
   (print 年龄)
   ```

2. **第2步**：在 REPL 中尝试中文关键字
   ```lisp
   ,lang zh
   (定义 年龄 25)
   (打印 年龄)
   ```

3. **第3步**：选择喜欢的方式

### 教学场景

1. **基础教学**：启用中文关键字（完整中文）
2. **算法教学**：默认模式（英文关键字）
3. **项目开发**：默认模式 + 中文注释

### 生产环境

推荐使用**默认模式**，保持代码兼容性

## 🔗 相关资源

- [完整中文支持文档](chinese_support.md)
- [选项系统文档](options_system.md)
- [中文编程示例](../examples/chinese_demo.lisp)
- [README](../README.md)

### 选项 A：直接运行中文示例

```bash
# 运行中文演示
target/release/bin/ystyle::xisp < examples/chinese_demo.lisp

# 或在 REPL 中加载
target/release/bin/ystyle::xisp
xisp> (load "examples/chinese_demo.lisp")
```

### 选项 B：创建自己的中文程序

创建 `my_program.lisp`：

```lisp
; 我的第一个中文 Lisp 程序

(define 姓名 "小明")
(define 年龄 18)
(define 爱好 "编程")

(print "姓名：" 姓名)
(print "年龄：" 年龄)
(print "爱好：" 爱好)

; 定义函数
(define 自我介绍 (lambda (n)
  (print "大家好，我是" 姓名)
  (print "今年" 岁)
  (print "爱好是" 爱好)))

(自我介绍 姓名)
```

运行：

```bash
target/release/bin/ystyle::xisp < my_program.lisp
```

---

## 📝 常用绑定快速参考

如果要用完整中文关键字，复制这个到文件开头：

```lisp
; ===== 中文关键字绑定 =====

; 变量定义
(define 定义 define)

; 条件
(define 如果 if)

; Lambda
(define 过程 lambda)

; Let
(define 让 let)

; 打印
(define 打印 println)
(define 显示 print)

; 运算符
(define 加 +)
(define 减 -)
(define 乘 *)
(define 除 /)

; 逻辑
(define 且 and)
(define 或 or)
(define 非 not)

; 比较
(define 等于 =)
(define 大于 >)
(define 小于 <)
(define 大于等于 >=)
(define 小于等于 <=)

; 列表
(define 列表 list)
(define 取首 car)
(define 取尾 cdr)
(define 添加 cons)
(define 追加 append)

; 高阶函数
(define 映射 map)
(define 过滤 filter)
(define 折叠 reduce)

; ================================
```

---

## 💡 最佳实践

### 推荐做法

```lisp
; ✅ 英文关键字 + 中文变量名
(define 用户列表 '(张三 李四 王五))
(define 过滤成年人 (lambda (列表)
  (filter (lambda (人) (>= (cdr 人) 18)) 列表))))
```

### 避免的做法

```lisp
; ❌ 直接使用中文关键字（未绑定）
(定义 年龄 25)  ; 错误！'定义' 未定义

; ✅ 正确方式
(define 年龄 25)  ; 使用 define
; 或先绑定
(define 定义 define)
(定义 年龄 25)
```

---

## 🎓 学习建议

### 初学者
1. **第 1 周**：使用英文关键字 + 中文变量名
2. **第 2 周**：理解关键字绑定机制
3. **第 3 周**：尝试完全中文编程

### 教学场景
1. **基础教学**：完整中文关键字
2. **算法教学**：英文关键字 + 中文变量
3. **项目开发**：英文关键字为主，中文注释

---

## 🔗 相关资源

- [完整中文支持文档](docs/chinese_support.md)
- [中文编程示例](examples/chinese_demo.lisp)
- [README](README.md)

---

## ✨ 总结

Xisp 的中文支持是**完整的、灵活的**：

- ✅ **开箱即用**：英文关键字 + 中文变量名
- ✅ **完全中文**：绑定后可用中文关键字
- ✅ **自由切换**：按需混合使用
- ✅ **生产就绪**：无性能损失

选择最适合你的方式，享受中文编程的乐趣！🎉
