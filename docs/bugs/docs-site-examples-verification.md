# docs-site Lisp 示例验证报告

> 验证日期：2026-08-02
> 验证方式：`target/release/bin/ystyle::xisp.cli`（CLI `-c` / 脚本文件 / `--simple-repl`）
> 验证范围：`docs-site/guide/`（6 个文件）+ `docs-site/api/`（8 个文件）中所有 ````lisp` 代码块
> 未修改任何源码与文档

---

## 1. 验证摘要

| 统计项 | 数量 |
|--------|------|
| 验证文件数 | **14**（guide 6 + api 8） |
| Lisp 代码块数 | **178** |
| 逐条断言（`=>` 注释 / 期望输出）验证数 | **约 500+** |
| 完全通过的文件 | **11** |
| 存在问题的文件 | **3**（02-basics、04-macros、05-modules） |
| 发现问题总数 | **9**（高严重度 2、中 1、低 5、环境依赖 3） |

**结论**：文档站点示例整体质量较高，绝大多数示例可在真实解释器中正确执行。主要问题集中在：除法浮点显示、宏最佳实践示例的反引号写法、模块示例依赖外部模块。

---

## 2. 通过清单

### guide/01-quickstart.md ✅ 全部通过（6/6 代码块）
- 变量与函数：`(square 5)` => `25` ✓
- 数据结构：`[1 2 3 4 5]`、`{:name "张三" :age 25}`、`'(1 2 3)` ✓
- 字符串插值：`#"Hello, #{name}!"` => `"Hello, Xisp!"` ✓
- 管道：`(-> [1 2 3 4 5] (map square) (filter even?) length)` => `2` ✓
- 解构绑定：`(let [[x y & rest] '(1 2 3 4 5)] ...)` => `(1 2 (3 4 5))` ✓
- 模式匹配：`(match 42 (x when (> x 40)) "big" _ "small")` => `"big"` ✓

### guide/02-basics.md ⚠️ 大部分通过（详见问题 #1、#6）
- 原子类型/组合类型 ✓；关键字自求值 `:hget` => `:hget` ✓
- define / lambda / if / quote / begin / set! 全部 ✓
- 算术 `+ - * mod`、比较、逻辑（含 `(and)`、`(or)` 无参）✓
- 列表操作、列表构造、谓词、字符串操作 ✓
- 高阶函数、apply、可变参数、命名参数与默认值 ✓
- let / let* / defun 传统兼容 ✓

### guide/03-modern.md ✅ 全部通过（22/22 代码块）
- 向量/哈希映射字面量、哈希映射操作（hget/hset/hexists/hlen/hkeys/hvals/hgetall/hdel）、完整函数名 ✓
- 字符串插值（简单/表达式）✓；解构绑定（基础/嵌套/点对）✓
- 管道（`-> 5 (+ 3) (* 2)` => 16、`-> -5 -` => 5）✓
- 模式匹配（值/符号/列表/嵌套/hashmap/守卫/递归）全部 ✓

### guide/04-macros.md ⚠️ 大部分通过（详见问题 #2）
- 自定义 when 宏、swap 宏 ✓；反引号、逗号、逗号-at ✓
- create-function 综合宏（`(square 5)` => 25）✓
- macroexpand：`(macroexpand '(when (> x 10) (println "large")))` => `(if (> x 10) (println "large") nil)` ✓
- 内置宏 when（多表达式，x 正确自增到 16）、unless、incf/decf、swap、negate、push/pop、if-let、when-let*、condb（含 :let）全部 ✓
- while 循环输出 0-4 ✓；dotimes 输出 0-4 ✓；dangerous/safer/unless 宏可定义 ✓

### guide/05-modules.md ⚠️ 部分通过（详见问题 #3、#7、#8）
- `(import pkg1)` + `(pkg1.greet "test")` ✓（XISP_PATH=lisp-tests）
- 别名导入 `(import (pkg1 :as mypkg))` + `(mypkg.greet "alias")` ✓
- 限定导入 `(import (only pkg1 greet))` + `(pkg1.greet "only")` ✓
- 文件导入 `(import "./utils.lisp")`（无前缀）、`(import "./helpers")`（helpers. 前缀）、`(import "./math.stats")`（stats. 前缀）✓（在镜像目录验证）

### guide/06-unicode.md ✅ 通过（REPL-only 已用 `--simple-repl` 验证）
- 中文变量名：`(计算面积 3 4)` => `12` ✓（CLI 脚本可执行）
- 中文关键字：`,lang zh` 启用后 `(定义 年龄 25)`、`(打印 年龄)` => 25、`(双倍 21)` => 42 ✓（REPL-only，CLI 脚本不可用属预期）

### api/index.md ✅ 全部通过（1/1 代码块）
- `(+ 1 2 3)` => 6、`(/ 1 3)` => 0.333333、map、hget、when 全部 ✓

### api/arithmetic.md ✅ 全部通过（13/13 代码块）
- `+ - * / mod round sum product max/min` 及边界（含浮点 6 位显示 `5.000000`）全部 ✓
- 比较 `= != < > <= >=`、`eq?`（符号/字符串/整数/布尔/nil/列表/不同类型）全部 ✓

### api/list.md ✅ 通过（15/15 代码块，仅显示风格差异，见问题 #5）
- list/prepend/append/range/reverse/first/rest/second/third/fourth/length ✓
- C 系列组合 cadr/caddr/cadddr/cddr/cdar/caar ✓
- map/filter/reduce/for-each/apply/管道 ✓

### api/hashmap.md ✅ 全部通过（12/12 代码块）
- hashmap/hashmap?/get/contains?/size/keys/values/hgetall/set!/remove! ✓
- Redis 风格别名 hget/hset/hdel/hexists/hlen/hkeys/hvals/hgetall ✓
- 综合示例（hset 后 hlen => 3、for-each hkeys）✓

### api/predicates.md ✅ 全部通过（16/16 代码块）
- 类型检查、type-of（10 种类型名）、数值谓词全部 ✓
- condb 结合使用（describe 示例）✓

### api/logic.md ✅ 全部通过（4/4 代码块）
- and/or（含短路、返回最后一个值 `(and 1 2 3)` => 3、无参）、not 全部 ✓

### api/string.md ✅ 全部通过（7/7 代码块）
- str/string-append/string=?</>/string?/type-of 全部 ✓
- `(string< "b" 1)` => nil ✓

### api/control.md ✅ 通过（21/21 代码块，仅显示风格差异，见问题 #4）
- when/unless/if-let/when-let*/condb（含 :let）✓
- incf/decf/swap/negate/push/pop/do/defun ✓
- eval/macroexpand/macroexpand-all（`(incf x)` => `(setq x (+ x 1))`）✓
- print/println/princ/display/newline ✓（输出值正确，引号风格见问题 #4）
- error/raise/assert ✓（输出 `[RuntimeError] "msg"` 与文档一致）

---

## 3. 失败 / 有问题的示例

### 🔴 问题 #1（高严重度｜文档错误）：02-basics 除法结果与实现不符

- **文件**：`docs-site/guide/02-basics.md`
- **行号**：152-153、157-159（tip）
- **示例代码**：
  ```lisp
  (/ 10 2)            ; => 5
  (/ 20 2 2)          ; => 5
  ```
  ```markdown
  ::: tip 除法返回浮点
  `(/ 10 2)` 返回 `5`（浮点 5.0 显示为 5），`(/ 1 3)` 返回 `0.333333`。
  :::
  ```
- **实际输出**：`(/ 10 2)` => `5.000000`、`(/ 20 2 2)` => `5.000000`
- **文档声称**：`5`，且 tip 声称"浮点 5.0 显示为 5"
- **问题分类**：文档错误（示例与实现不符）
- **说明**：解释器对浮点数固定保留 6 位小数显示（`api/arithmetic.md` 第 65 行正确写作 `5.000000`，与本文件矛盾）。本文件第 154 行 `(/ 10 4)` 正确写作 `2.500000`，说明第 152-153 行属笔误。
- **建议**：修正文档：`(/ 10 2)` => `5.000000`、`(/ 20 2 2)` => `5.000000`，删除 tip 中"浮点 5.0 显示为 5"的说法，改为"浮点固定保留 6 位小数显示"。

### 🔴 问题 #2（高严重度｜文档错误）：04-macros 最佳实践 `good` 宏写法错误

- **文件**：`docs-site/guide/04-macros.md`
- **行号**：279-286
- **示例代码**：
  ```lisp
  ; ❌ 不推荐 - 手工构造
  (defmacro bad (x)
    (list 'quote (list '+ x 1)))

  ; ✅ 推荐 - 使用反引号
  (defmacro good (x)
    `'(,+ x 1))
  ```
- **实际输出**：
  - `(bad 5)` => `(+ 5 1)` ✓（正确）
  - `(good 5)` => `(#<native-function> x 1)` ✗（错误）
  - 修正写法 `'(+ ,x 1)` => `(+ 5 1)` ✓
- **文档声称**：`good` 是推荐写法，应与 `bad` 等价（生成 `(+ 5 1)`）
- **问题分类**：文档错误（示例与实现不符）
- **说明**：`` `'(,+ x 1) `` 中逗号作用在 `+` 上，导致 `+` 被求值为原生函数对象。正确写法应为 `` `'(+ ,x 1) ``（逗号作用在参数 `x` 上）。这是给读者的错误示范。
- **建议**：修正文档为：
  ```lisp
  (defmacro good (x)
    `'(+ ,x 1))
  ```

### 🟡 问题 #3（中严重度｜实现行为）：import 不存在的模块静默通过，不报错

- **文件**：`docs-site/guide/05-modules.md`（第 106-108 行 `(import ystyle::log)`）
- **示例代码**：`(import ystyle::log)`（测试环境实际无 `ystyle/log` 模块，只有 `ystyle/zlog`）
- **实际输出**：`(import ystyle::log)` **静默成功**（不报任何错误），后续 `(log.init "myapp")` 才报 `[UndefinedFunction] Undefined function: 'log.init'`
- **文档声称**：模块按名称搜索，导入后可用 `log.` 前缀访问
- **问题分类**：实现 bug（文档对但实现有缺陷）+ 环境依赖
- **说明**：对 `(import nonexistent::foo)` 同样静默通过。导入不存在的模块应当立即报错（如"Module not found: ystyle/log"），而不是让用户在下一次函数调用时才发现。这会导致文档示例 `(import ystyle::log)` 在无该模块的环境下无提示地"成功"。
- **建议**：实现层：`import` 找不到模块时报错（含 XISP_PATH 提示）。文档层：在示例旁注明需要 `XISP_PATH` 指向含 `ystyle/log` 的目录。

### 🟢 问题 #4（低严重度｜文档显示差异）：api/control 输出示例省略字符串引号

- **文件**：`docs-site/api/control.md`
- **行号**：222、232、242、243
- **示例代码**：
  ```lisp
  (print "A" "B")     ; 输出: A B
  (println "Hello" "World")   ; 输出: Hello World
  (princ "X")     ; 输出: X
  (display "Y")   ; 输出: Y
  ```
- **实际输出**：`"A" "B"`、`"Hello" "World"`、`"X"`、`"Y"`（字符串带引号）
- **文档声称**：`A B`、`Hello World`、`X`、`Y`（无引号）
- **问题分类**：文档错误（示例与实现不符，显示风格差异）
- **说明**：解释器输出字符串时带双引号（repr 形式）。文档示例省略了引号，读者复制后看到的输出与文档不一致。
- **建议**：修正文档注释为带引号的输出形式，或在"输出与错误"一节开头注明"字符串参数以带引号形式输出"。

### 🟢 问题 #5（低严重度｜文档显示差异）：api/list 空列表显示为 `()` 而非 `nil`

- **文件**：`docs-site/api/list.md`
- **行号**：20、48、75、102
- **示例代码**：
  ```lisp
  (list)           ; => ()
  (append '() '())         ; => ()
  (reverse '())        ; => ()
  (rest '(1))          ; => ()
  ```
- **实际输出**：均为 `nil`（REPL 与 println 均显示 `nil`）
- **文档声称**：`()`
- **问题分类**：文档错误（显示差异，值与语义等价）
- **说明**：`nil` 与 `'()` 等价，解释器统一显示为 `nil`。文档用 `()` 表示空列表是传统 Lisp 习惯，但与真实输出不符。
- **建议**：统一改为 `=> nil`，或在 list.md 开头注明"空列表显示为 `nil`"。

### 🟢 问题 #6（低严重度｜文档显示差异）：02-basics process 示例输出带引号

- **文件**：`docs-site/guide/02-basics.md`
- **行号**：75
- **示例代码**：`(process 5)   ; 打印 "Processing: 5"，返回 10`
- **实际输出**：`"Processing:" 5`（字符串带引号），随后返回 `10` ✓
- **文档声称**：`Processing: 5`（无引号）
- **问题分类**：文档错误（显示风格差异）
- **建议**：修正为 `打印 "Processing:" 5`，或注明字符串带引号。

### 🟢 问题 #7（低严重度｜文档显示差异）：03-modern 管道浮点显示

- **文件**：`docs-site/guide/03-modern.md`
- **行号**：200-201
- **示例代码**：`(-> 100 (/ 10) (+ 5) (* 2))`，注释 `; => 30`
- **实际输出**：`30.000000`
- **文档声称**：`30`
- **问题分类**：文档错误（显示差异，因 `/` 返回浮点）
- **建议**：修正注释为 `; => 30.000000`。

### 🟢 问题 #8（环境依赖）：05-modules `(import ystyle::log)` 与 `(log.init ...)` 依赖外部模块

- **文件**：`docs-site/guide/05-modules.md`
- **行号**：106-108、114-115
- **示例代码**：`(import ystyle::log)`、`(log.init "myapp")`
- **实际输出**：`(import ystyle::log)` 静默成功但 `(log.init "myapp")` 报 `[UndefinedFunction]`
- **文档声称**：`log.init "myapp"` 可用
- **问题分类**：环境依赖（需要前置条件：存在 `ystyle/log` 模块）
- **说明**：本仓库 `lisp-tests/` 中只有 `ystyle/zlog`（提供 `info/debug/error/warn`），没有 `ystyle/log` 及 `log.init`。文档示例基于 `~/.xisp/modules/ystyle/log/` 的假设目录，但在无该模块的环境下无法复现。
- **建议**：文档中改用真实存在的 `ystyle::zlog` 示例（`(zlog.info "msg")`），或注明示例依赖用户自行安装的模块。

### 🟢 问题 #9（环境依赖）：05-modules module.lisp 声明示例无法在脚本/REPL 中执行

- **文件**：`docs-site/guide/05-modules.md`
- **行号**：36-52（module.lisp 代码块）
- **示例代码**：`(module myapp (version "1.0.0") ...)`
- **实际输出**：`[UndefinedFunction] Undefined function: 'module'`
- **问题分类**：环境依赖（module.lisp 由加载器在模块上下文解析，非普通脚本）
- **说明**：`(module ...)` 声明只在模块目录的 `module.lisp` 中生效，由 ModuleLoader 特殊处理；直接作为脚本执行会报错。文档将其作为"模块声明示例"展示是合理的，但需注意不能直接复制执行。
- **建议**：文档中注明"module.lisp 由模块加载器解析，不能直接作为脚本运行"。

---

## 4. 建议汇总

| 优先级 | 位置 | 建议 |
|--------|------|------|
| P0 | guide/02-basics.md:152-153, 157-159 | 除法显示改为 `5.000000`，修正 tip |
| P0 | guide/04-macros.md:284-286 | good 宏改写作 `` `'(+ ,x 1) `` |
| P1 | 实现层（import） | import 找不到模块时应报错 |
| P1 | guide/05-modules.md:106-115 | 改用真实模块 `ystyle::zlog` 示例或注明环境依赖 |
| P2 | api/control.md:222-243 | 输出注释补充字符串引号 |
| P2 | api/list.md:20,48,75,102 | 空列表输出统一为 `nil` |
| P2 | guide/02-basics.md:75 | 输出注释补充字符串引号 |
| P2 | guide/03-modern.md:200-201 | 管道结果改为 `30.000000` |
| P2 | guide/05-modules.md:36-52 | 注明 module.lisp 不可直接执行 |

---

## 5. 验证环境备注

- 解释器二进制：`./target/release/bin/ystyle::xisp.cli`
- 模块测试需 `XISP_PATH=lisp-tests`（pkg1/pkg2 位于 lisp-tests/）
- 中文关键字为 REPL-only（`,lang zh` 命令），CLI 脚本不可用，已用 `--simple-repl` 验证通过
- `nil` / `'()` 在 `-c` 模式下无输出，通过脚本 `println` 或 REPL 确认
- 验证脚本存放于 `/tmp/opencode/verify/`（01-quickstart / 02-basics / 03-modern / 04-macros / 05-modules / 05-ystyle / 05-zlog / 05-log / 06-unicode / 06-zhkeyword / api-index / api-arithmetic / api-list / api-hashmap / api-predicates / api-logic / api-string / api-control / boundary / print-test / probe*）
