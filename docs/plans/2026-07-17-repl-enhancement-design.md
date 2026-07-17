# REPL 增强设计

## 背景

当前 REPL 基于 `readln()` 逐行读取，终端在 cooked mode 下运行，无法实现：
- 行内光标移动和历史导航
- Tab 补全
- 语法着色输出
- Ctrl+C 安全中断

引入 `crossterm 0.29.1`（纯仓颉跨平台终端库）后，可在不依赖 C 编译器的前提下获得 raw mode、逐键事件、ANSI 着色能力。

## 架构

```
┌─────────────────────────────────────────────┐
│              EnhancedRepl                    │
│  ┌─────────┐ ┌──────────┐ ┌──────────────┐ │
│  │LineEditor│ │Completer │ │HistoryManager│ │
│  │ 缓冲区   │ │ 符号补全 │ │ 内存+文件    │ │
│  │ 多行管理 │ │ 模块补全 │ │ 去重+限容   │ │
│  └────┬────┘ └────┬─────┘ └──────┬───────┘ │
│       │            │              │          │
│  ┌────┴────────────┴──────────────┴───────┐ │
│  │        crossterm (raw mode + 事件)     │ │
│  └────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────┐ │
│  │        XispEvaluator (不变)            │ │
│  └────────────────────────────────────────┘ │
└─────────────────────────────────────────────┘
```

## 组件设计

### 1. LineEditor

核心状态：
- `buffer: ArrayList<String>` — 多行缓冲区，每行一个元素
- `cursorRow: Int` / `cursorCol: Int` — 光标位置
- `scrollOffset: Int` — 垂直滚动偏移

按键映射：
| 按键 | 行为 |
|------|------|
| 字符 | 插入到光标处 |
| Enter | 换行（插入新行，移到行首） |
| Ctrl+Enter / Ctrl+J | 提交缓冲区到求值器 |
| Ctrl+D (空缓冲区) | 退出 REPL |
| 退格 | 删除光标前字符 |
| Delete | 删除光标处字符 |
| ← → | 行内左右移动 |
| ↑ ↓ | 行内上下移动（非多行时翻历史） |
| Home / End | 行首/行尾 |
| Ctrl+A | 行首 |
| Ctrl+E | 行尾 |
| Ctrl+K | 删除到行尾 |
| Ctrl+U | 删除整行 |
| Ctrl+L | 清屏重绘 |
| Ctrl+C | 中断当前输入，回到新行 |
| Tab | 补全 |
| Ctrl+P / Ctrl+N | 翻历史（替代上下箭头） |
| Alt+Enter | 强制提交（即使括号不平衡） |

渲染：
- 每帧完整重绘当前行区域
- 使用 crossterm `MoveTo` / `Clear` 系列命令
- 多行时显示行号指示（`1| ...` / `2| ...`）
- 提示符 `xisp> `（根级）或 `...> `（续行）

括号匹配：
- 实时扫描 buffer 中 `()` `[]` `{}` 的对齐情况
- 不平衡时提示符变色或在状态行显示
- Enter 时自动检测：平衡则提示 Alt+Enter 提交，不平衡则继续换行

### 2. HistoryManager

- 内存：`ArrayList<String>` 环形缓冲，最大 1000 条
- 文件：`~/.xisp_history`，每条一行
- 启动时加载，退出时追加（去重，保留最后 1000 条）
- 支持 `,history` 命令查看
- Ctrl+R 反向搜索（v2 功能，暂不实现）

### 3. Completer

- 补全源：当前环境的符号、内置函数、模块名
- 触发：Tab 键
- 行为：
  - 有唯一匹配 → 自动补全
  - 多匹配 → 显示候选列表
  - 已是最长公共前缀 → 再次 Tab 循环候选
- 匹配逻辑：前缀匹配，大小写不敏感

### 4. SyntaxHighlighter

- 错误输出着色（红色 `[Type]` + 消息）
- 结果值着色（数字黄色、字符串绿色、关键字蓝色）
- 只在 REPL 输出时生效，`-c` 模式无着色

### 5. 信号处理

- Ctrl+C：中断求值，回到 REPL 顶层，不清除 buffer
- 异常退出：注册 `finally` 块确保 `disable_raw_mode()` 被调用
- 注册 crossterm `SetPanicHandler` 或类似机制

## 文件结构

```
src/repl/
├── repl.cj              # 入口，替换旧 Repl 类
├── line_editor.cj       # LineEditor 核心
├── history.cj           # HistoryManager
├── completer.cj         # Completer
├── highlighter.cj       # SyntaxHighlighter
└── repl_test.cj         # 单元测试
```

## 与现有系统的集成

- `LispInterpreter` 新增 `runReplInteractive()` 方法（可选）
- `main.cj` 的 `runRepl()` 调用新 REPL
- 保留旧 REPL 作为 `--simple-repl` 回退（非 TTY 环境自动降级）
- 非 TTY 环境（管道重定向）自动降级到旧 `readln()` 模式
- `isatty` 检测用 crossterm 的 `is_tty()` 或 `std.posix.isatty()`

## 错误处理

- raw mode 开始时保存终端状态，结束时恢复
- REPL 内部所有异常在顶层 `try/catch` 捕获后恢复终端
- 求值错误不退出 REPL，只打印错误信息
- Ctrl+C 中断时重新创建 Evaluator（清除中间状态）
