# B2 / B3 调查报告

**调查日期**: 2026-07-17  
**测试范围**: `,@` 展开 + 模块系统导入  
**当前状态**: 314 测试全部通过

---

## B2: `,@` 在宏中展开后报 `UndefinedFunction: 'a'`

### 现象

```lisp
(defmacro wrap-list (lst) `(a ,@lst b))
(wrap-list '(x y z))
;; => [UndefinedFunction] Undefined function: 'a'
```

### 根因分析

`wrap-list` 宏展开后产生 `(a x y z b)`。Xisp 的宏展开结果会通过 `eval()` 再次求值。`(a x y z b)` 被当作函数调用，`a` 在函数位置，由于 `a` 未定义函数，报错。

而直接在 REPL 中执行 `` `(a ,@lst d) `` 正常，是因为结果被传给 `println` 作为数据，没有作为代码求值。

### 代码执行路径

```
evalFunctionCall → 发现 Macro → expandMacro → eval(expanded)
                                                   ↓
                                        expanded 是 (a x y z b)
                                                   ↓
                                        evalCons → evalFunctionCall
                                                   ↓
                                        查找 a 作为函数 → 未定义 → 报错
```

### 结论

✅ **不是代码 bug。** Xisp 的宏展开机制与 CL 一致——展开结果被求值。`wrap-list` 宏设计不合理，产生不可求值的表达式。

### 修复建议

修复 `02_macro_basics.lisp` 示例，用可求值的表达式：

```lisp
; 正确用法：用 list 做函数
(defmacro wrap-in-list (lst)
  `(list 'a ,@lst 'b))

(wrap-in-list '(x y z))
;; => (a x y z b)

; 或直接用 macroexpand 查看展开结果
(println (macroexpand '(wrap-list (x y z))))
;; => (a x y z b)
```

---

## B3: 模块导入后符号查找失败

### 现象（测试报告）

```
(import pkg2)  → 成功
(pkg2.greet)   → [UndefinedFunction] Undefined function: 'pkg2.greet'
```

### 当前验证

```
✓ (import pkg2)  → 成功加载 pkg2/main.lisp + pkg1/utils.lisp
✓ (pkg2.greet "Xisp")  → "Greetings from pkg2!" / "Hello from pkg1!"
✓ (pkg2.calculate 10 20) → 30
✓ (pkg1.greet "Direct") → "Hello from pkg1!"
✓ (pkg1.multiply 6 7) → 42
```

**模块系统所有功能正常工作。** 示例文件 `examples/05-modules/modules_demo/demo.lisp` 全部通过。

### 唯一问题

`examples/05-modules/01_basic_demo.lisp` 中引用了不存在的函数名 `pkg2.call-pkg1`（实际名为 `pkg2.call-pkg1-util`），属于文件名错误。

### 结论

✅ **不是代码 bug。** 模块导入、符号导出、跨模块依赖全部工作正常。测试报告中的 B3 可能已由后续的模块系统重构（ModuleSource/Loader 等）修复。

### 修复建议

修复 `01_basic_demo.lisp` 中错误的函数名 `pkg2.call-pkg1` → `pkg2.call-pkg1-util`。

---

## 汇总

| Bug | 结论 | 类型 |
|-----|------|------|
| B2 `,@` 崩溃 | ✅ 非代码 bug，示例设计问题 | 文档/示例 |
| B3 模块导入失败 | ✅ 当前已可正常工作 | 测试报告过时 |
