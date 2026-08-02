---
# https://vitepress.dev/reference/default-theme-home-page
layout: home

hero:
  name: "星枢 Xisp"
  text: "仓颉嵌入式 Lisp 脚本语言"
  tagline: 轻量级 · 现代化 · 安全可控 · 与仓颉双向互操作
  actions:
    - theme: brand
      text: 快速开始
      link: /guide/01-quickstart
    - theme: alt
      text: API 参考
      link: /api/
    - theme: alt
      text: 集成指南
      link: /integration/embedding

features:
  - title: 轻量级
    details: 手写 Lexer/Parser + AST 求值器，自包含核心，不重复实现 IO/网络，直接桥接仓颉标准库。
    icon: ⚡
  - title: 现代化
    details: 向量/哈希字面量、字符串插值、解构绑定、管道操作符、模式匹配，现代 Lisp 语法开箱即用。
    icon: 🚀
  - title: 安全可控
    details: 沙箱模式控制栈深度、执行超时、函数白名单/黑名单、文件访问权限。
    icon: 🛡️
  - title: 双向互操作
    details: LispInterpreter 嵌入仓颉，LispConvertible 类型转换，支持反向调用仓颉函数。
    icon: 🔗
  - title: 宏系统
    details: defmacro + 反引号语法，支持自定义控制结构、DSL、代码生成。
    icon: 📦
  - title: Unicode 支持
    details: 内置中文关键字别名（定义/如果/当），面向中文开发者友好。
    icon: 🌏
---
