# 求值器细节优化设计

日期: 2026-08-02
分支: feat/perf-detail-opt
基线: master (e04fcb8)

## 背景

AST 求值器已完成架构级优化（算术特殊形式化 + Int 快速路径，22.3s → 2.0s，~91%）。
本设计聚焦求值器内部的细节浪费，在不改变语义的前提下消除冗余计算与分配。

## 优化清单

### 1. `evalMatch` 子句跳过去克隆

**位置**: `src/core/eval/pattern_match.cj:58` `getNextAndSkip`

**问题**: 每次子句不匹配时执行 `Cons(nextClauses.clone())` 深克隆整个剩余子句链表。
`evalMatch` 对每个失败的子句都调用它，N 个子句时最坏 O(N²) 克隆分配。

**改法**: 匹配失败时直接返回 `(nextClauses.cdr, Some(nextClauses.car))`，
复用原链表。遍历过程只读，无修改，克隆非必要。

**语义**: 结果表达式求值路径不变，仅减少分配。

**验证**: 所有 match 相关测试（match_hashmap_test、destruct_test、evaluator_test 中的
testMatchListPattern、comprehensive_test）+ 新增失败子句场景测试。

### 2. `lookupKeyword` 环境链短路

**位置**: `src/core/eval/core.cj:68`（调用点）、`src/types/types.cj:256`（实现）

**问题**: `evalListInternal` 对每个列表求值先调 `this.env.lookupKeyword(sym)`，
递归遍历完整环境链查询关键字别名。英文代码默认无别名，但每次 `(+ a b)`、`(< n 2)`、
`(if ...)` 都做一遍完整链遍历。

**改法**: Environment 增加一个懒缓存标志 `hasKeywordAliasInChain: Bool`（默认 false）。
- `registerKeywordAlias` 置位当前环境标志为 true
- `createChild` 继承父环境标志
- `lookupKeyword` 入口检查：若链上无任何别名，直接返回原符号（O(1) 短路）

**正确性**: 别名注册后需传播到已有子环境。REPL 场景在启动时注册中文别名，
后续子环境通过 createChild 继承。需处理「已创建子环境后再注册别名」的边界
（现有代码中别名都在求值前注册，标志继承足够）。

**验证**: 中文别名 REPL 测试、`(定义 x 5)` 类用例 + 新增别名链路测试。

### 3. `matchPattern` 变量绑定不建子环境

**位置**: `src/core/eval/pattern_match.cj:349`

**问题**: 符号模式 `Symbol(sym)` 匹配时 `env.createChild()` + define 创建子环境。
列表模式 `(x y z)` 对每个元素重复此操作，嵌套多层环境，链式 lookup 变慢。

**改法**: 普通符号变量直接在当前 env 上 define（与 `matchListPattern` 的做法一致）。
匹配成功返回的仍是传入的 env。

**注意**: `matchListPattern` 已采用直接 define 模式，此处统一。需确认变量遮蔽语义
（同名变量在后续模式引用时应取最新绑定——直接 define 天然满足）。

**验证**: 现有 match 测试全部通过 + 新增「列表模式变量先后引用」测试。

### 4. `isNativeArith` 守卫缓存

**位置**: `src/core/eval/arithmetic_fast.cj:272`

**问题**: 每次算术特殊形式都 `this.env.lookup(opName)` 守卫（已实测 ~18% 成本）。
之前的 HashMap 版本缓存更慢（HashMap 查询开销 ≈ env.lookup 链式查找）。

**改法**: Evaluator 增加:
- `arithGuardVersion: Int64`（环境变更计数）
- 7 个操作符的 `Array<Bool>` 缓存 + 缓存版本号
- `isNativeArith` 入口：若缓存版本 == arithGuardVersion 直接返回缓存；
  否则一次性查 7 个操作符并更新缓存版本

**失效时机**: 任何 `env.define` / `env.set` / `env.registerKeywordAlias` 使环境版本号 +1。
需要 Environment 增加 `mutationVersion`（在 define/set 时递增）。

**风险**: 上次版本缓存方案（HashMap 缓存 + 全局 mutationVersion）被实测更慢。
本次改进：
- 用 `Array<Bool>` + 单个 Int 版本号替代 HashMap（访问 O(1) 无哈希）
- 环境自身持有版本号，避免全局计数器同步开销

**验证**: 重定义 `+` 回退用例、基准 benchmark.lisp、fib。

## 性能验证

- 每项优化独立 commit，各自跑: `cjpm build` + `cjpm test` + lisp-tests + benchmark
- match 类优化用 `/tmp/opencode/match_bench.lisp`（递归控制栈深）
- 综合 benchmark: `lisp-tests/benchmark.lisp`
- 关键回归点: 314 单元测试全过、所有 lisp-tests 全过

## 分支策略

按 AGENTS.md 流程: feat/perf-detail-opt 从 master 切出，开发完成后 squash 合并回 master。
