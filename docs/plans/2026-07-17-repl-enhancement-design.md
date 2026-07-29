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

---

## 实施计划

### Task 1: HistoryManager

**Files:**
- Create: `src/repl/history.cj`

实现内存环形缓冲 + `~/.xisp_history` 文件持久化。纯数据层，不依赖 crossterm，可独立测试。

API:
```
HistoryManager(maxEntries: Int64 = 1000)
  .load(path: String): Unit        — 从文件加载
  .save(path: String): Unit        — 追加写入
  .add(entry: String): Unit        — 添加条目（去重）
  .navigate(offset: Int): ?String  — 历史导航（-1 上一条，+1 下一条）
  .resetNavigation(): Unit         — 重置导航到最新位置
  .getAll(): ArrayList<String>
  .getCurrentIndex(): Int64
```

测试重点：去重、环形缓冲边界、文件读写、空历史导航。

### Task 2: Completer

**Files:**
- Create: `src/repl/completer.cj`
- Read: `src/core/evaluator.cj`（env 接口）
- Read: `src/types/types.cj`（LispValue 结构）

从当前环境提取符号列表，前缀匹配。不依赖 crossterm，可独立测试。

API:
```
Completer(evaluator: Evaluator)
  .complete(prefix: String): ArrayList<String>  — 返回匹配列表
  .getCommonPrefix(candidates: ArrayList<String>): String  — 最长公共前缀
  .setCycleIndex(idx: Int): Unit  — Tab 循环索引
```

补全源：
- `evaluator.env.getKeys()` 符号
- Evaluator 中的内置函数注册表
- 模块名（从 ModuleLoader 获取）

### Task 3: LineEditor

**Files:**
- Create: `src/repl/line_editor.cj`
- Read: `src/repl/history.cj`
- Read: `src/repl/completer.cj`
- Read: crossterm 文档（event API）

核心编辑引擎，在 crossterm raw mode 下逐键处理。

状态类：
```
LineEditorState
  buffer: ArrayList<String>      — 多行缓冲区
  cursorRow: Int                 — 当前行索引
  cursorCol: Int                 — 当前列索引
  history: HistoryManager
  completer: Completer
```

按键处理（`processKey(key: KeyEvent): EditorAction`）：
```
EditorAction enum:
  Submit(String)     — 提交缓冲区内容
  Exit              — 退出 REPL
  Continue          — 继续编辑
  Cancel            — Ctrl+C，清空当前行
  Complete(ArrayList<String>)  — 补全候选
```

每个按键动作实现：
- 普通字符：插入到 cursorRow/cursorCol
- Enter：在光标处插入新行
- Backspace：删除光标前字符
- Delete：删除光标处字符
- Left/Right：移动 cursorCol
- Up/Down：cursorRow > 0 时行内上移；cursorRow == 0 时翻历史
- Ctrl+A/E：行首/行尾
- Ctrl+K：删到行尾
- Ctrl+U：删整行
- Ctrl+L：清屏信号
- Ctrl+C：取消信号
- Tab：触发补全
- Ctrl+Enter/Ctrl+J：提交
- Alt+Enter：强制提交

Render 方法不在此 task 中（移至 Task 5 集成）。

测试重点：
- 单行编辑（插入/删除/移动）
- 多行管理（enter 换行、行间导航）
- 括号平衡检测
- Tab 补全触发逻辑
- 历史导航触发逻辑

### Task 4: SyntaxHighlighter

**Files:**
- Create: `src/repl/highlighter.cj`

ANSI 着色工具函数。不依赖 crossterm（直接用 ANSI 转义序列）。

```
Highlighter
  .highlight(value: LispValue): String    — 结果值着色
  .highlightError(err: XispError): String — 错误信息着色
  .highlightLine(line: String): String    — 行内关键字着色（v2）
```

着色规则：
- 数值（Int/Float）：黄色 `\x1b[33m`
- 字符串：绿色 `\x1b[32m`
- 关键字（define/let/if/lambda/import）：蓝色 `\x1b[34m`
- 错误类型：红色 `\x1b[31m`
- 重置：`\x1b[0m`

测试重点：
- 每种类型的着色输出
- 组合值（列表/Cons 递归着色）
- 错误格式
- ANSI 序列正确性

### Task 5: EnhancedRepl 集成

**Files:**
- Modify: `src/repl/repl.cj` — 替换为 EnhancedRepl
- Modify: `src/cli/main.cj` — 集成新 REPL，添加 `--simple-repl` 回落
- Modify: `src/interpreter.cj` — 可选暴露 runReplInteractive

EnhancedRepl 主循环：
```
run():
  1. isatty 检测 → 非 TTY 降级
  2. enable_raw_mode()
  3. 加载历史文件
  4. 主循环：poll(50ms) → read() → processKey → render → 按 action 执行
  5. finally: disable_raw_mode() + saveHistory()
```

渲染方法 `render(state: LineEditorState)`：
- 使用 crossterm `MoveTo`/`CursorSave`/`CursorRestore`
- 从行 0 开始输出 prompt + 每行内容
- 光标定位到 cursorRow/cursorCol

集成关键点：
- `main.cj`: `runRepl()` 检测是否为 TTY。TTY → EnhancedRepl，否则 → 旧 Repl
- `--simple-repl` 标志强制使用旧 REPL
- REPL 命令（`,exit`, `,help`, `,env`, `,history` 等）保留

测试重点：
- TTY 检测逻辑
- `--simple-repl` 标志
- 主循环启动/退出
- raw mode 错误恢复

### Task 6: 信号处理和错误恢复

**Files:**
- Modify: `src/repl/repl.cj`

- Ctrl+C 处理：crossterm 捕获 KeyEvent(Ctrl+c) → 调用 evaluator 中断
- 异常恢复：所有 crossterm 操作在 `try` 块内，`finally` 保证 `disable_raw_mode()`
- 进程退出信号：crossterm 不支持 SIGINT handler，可以 `std.posix.signal()` 或用仓颉的 Runtime.addShutdownHook

实现方式：
```
try {
    enable_raw_mode()
    mainLoop()
} finally {
    disable_raw_mode()
    history.save()
}
```

以及单独的 Ctrl+C 信号处理（通过 crossterm 事件循环检测）。

### Task 7: 单元测试集成

**Files:**
- Create: `src/repl/repl_test.cj`

单元测试覆盖（crossterm 不可用时可 mock 事件）：

- HistoryManager 全部功能（12+ 测试）
- Completer 全部功能（8+ 测试）
- LineEditor 核心状态转换（15+ 测试，构造 KeyEvent 对象直接调用 processKey）
- Highlighter 着色（6+ 测试，验证 ANSI 序列正确性）
- REPL 命令处理（,exit ,help ,env ,history）
- TTY 回退逻辑
- 括号平衡检测（5+ 测试）
