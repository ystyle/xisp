# 模块系统全面测试计划

## 问题分析

### 当前测试的问题

1. **过度简化的测试** - 很多测试只验证字符串处理，不测试实际导入
   ```cangjie
   // ❌ 当前测试 - 只检查字符串以 "./" 开头
   @Assert(relativePath1.startsWith("./"))

   // ✅ 应该测试 - 真正执行导入并验证符号
   evaluator.eval("(import \"./utils.lisp\")")
   let result = evaluator.eval("(utilsFunc)")
   ```

2. **缺少实际导入测试**
   - `testRelativeImportCurrentDir` - 只测试路径包含 "zlog"
   - `testRelativeImportParentDir` - 只测试路径包含 "ystyle"
   - `testRelativeImportSyntax` - 只测试字符串以 "./" 或 "../" 开头

3. **没有测试符号绑定**
   - 导入后符号是否可用？
   - 前缀规则是否正确？
   - 未导出符号是否不可访问？

4. **没有测试错误处理**
   - 模块不存在
   - 相对导入遇到模块（有 module.lisp）
   - 文件/目录不存在

## 需要补充的测试（按优先级）

### 优先级 P0 - 核心功能（必须实现）

#### P0-1. 相对导入 - 文件导入（无前缀）

```lisp
;; 测试场景：导入同级文件
;; main.lisp
(import "./utils.lisp")
(processData "test")  ; 无前缀，直接调用

;; utils.lisp
(define (processData data) "处理数据")
(export processData)  ; 导出符号
```

**测试验证：**
- [ ] `evaluator.eval("(import \"./utils.lisp\")")` 成功
- [ ] `evaluator.eval("(processData \"test\")")` 返回正确结果
- [ ] 未导出的符号不可访问

#### P0-2. 相对导入 - 目录包导入（有前缀）

```lisp
;; 测试场景：导入同级目录
;; main.lisp
(import "./helpers")
(helpers.validateEmail "test@example.com")  ; 有前缀

;; helpers/validate.lisp
(define (validateEmail email) "验证邮箱")
(export validateEmail)

;; helpers/consts.lisp
(define MAX_SIZE 1000)
(export MAX_SIZE)
```

**测试验证：**
- [ ] `evaluator.eval("(import \"./helpers\")")` 成功
- [ ] `evaluator.eval("(helpers.validateEmail \"test\")")` 可用
- [ ] `evaluator.eval("(helpers.MAX_SIZE)")` 可用
- [ ] 多个文件的符号都加载到同一前缀下

#### P0-3. 相对导入检测模块错误

```lisp
;; 测试场景：相对导入不应该导入模块
;; 假设 subdir/ 有 module.lisp
(import "./subdir")  ; 应该报错
```

**测试验证：**
- [ ] 返回错误："Cannot import module 'subdir' using relative import"
- [ ] 提示使用 `(import subdir)` 或 `(import org::subdir)`

#### P0-4. 绝对导入 - 模块导入

```lisp
;; 测试场景：导入第三方模块
(import ystyle::log)
(log.init "myapp")
(log.write "Hello")
```

**测试验证：**
- [ ] 成功导入 `examples/ystyle/log` 模块
- [ ] 符号使用 `log.` 前缀
- [ ] 调用函数成功

### 优先级 P1 - 重要功能（应该实现）

#### P1-1. Export 功能测试

```lisp
;; 测试场景：验证导出功能
;; helpers.lisp
(define (publicFunc) "公开函数")
(define (privateFunc) "私有函数")
(export publicFunc)  ; 只导出 publicFunc
```

**测试验证：**
- [ ] 导入后可访问 `publicFunc`
- [ ] 导入后不可访问 `privateFunc`

#### P1-2. 嵌套目录包导入

```lisp
;; 测试场景：导入嵌套目录
(import "./math.stats")
(stats.average [1 2 3])  ; 前缀是最后一级

;; math/stats/average.lisp
(define (average numbers) "计算平均值")
(export average)
```

**测试验证：**
- [ ] 成功导入嵌套目录
- [ ] 前缀是 `stats.` 而不是 `math.stats.`
- [ ] 函数调用成功

#### P1-3. 文件不存在错误

```lisp
;; 测试场景：导入不存在的文件
(import "./notexist.lisp")
```

**测试验证：**
- [ ] 返回错误："file not found at './notexist.lisp'"

#### P1-4. 目录不存在错误

```lisp
;; 测试场景：导入不存在的目录
(import "./notexist-dir")
```

**测试验证：**
- [ ] 返回错误："directory not found at './notexist-dir'"

### 优先级 P2 - 边界情况（有时间再实现）

#### P2-1. 绝对导入 - 模块不存在

```lisp
(import nonexist::module)
```

**测试验证：**
- [ ] 返回错误："module 'nonexist::module' not found"

#### P2-2. 绝对导入 - 缺少 module.lisp

```lisp
;; 假设 somepath/ 存在但没有 module.lisp
(import somepath)
```

**测试验证：**
- [ ] 返回错误："Missing module.lisp in somepath/"

#### P2-3. 组织名不匹配

```lisp
;; ystyle/log/module.lisp 中 organization 是 "ystyle"
;; 但导入时使用了错误的组织名
(import wrongorg::log)
```

**测试验证：**
- [ ] 返回错误："Module 'log' belongs to organization 'ystyle', not 'wrongorg'"

#### P2-4. 混合导入测试

```lisp
;; 测试场景：同时使用相对导入和绝对导入
(import "./utils.lisp")        ; 无前缀
(import "./helpers")           ; helpers. 前缀
(import ystyle::log)          ; log. 前缀

(processData "test")          ; 无前缀
(helpers.validateEmail "...")  ; helpers. 前缀
(log.init "app")              ; log. 前缀
```

**测试验证：**
- [ ] 三种导入方式都成功
- [ ] 命名空间正确隔离
- [ ] 前缀规则正确应用

## 测试实现建议

### 1. 创建测试模块结构

```
lisp-tests/
├── module-tests/              # 新增：模块系统测试
│   ├── simple-import.lisp     # 简单导入测试
│   ├── relative-file/         # 文件导入测试
│   │   ├── main.lisp
│   │   └── utils.lisp
│   ├── relative-package/       # 目录包导入测试
│   │   ├── main.lisp
│   │   └── helpers/
│   │       ├── validate.lisp
│   │       └── consts.lisp
│   ├── nested-package/         # 嵌套目录测试
│   │   ├── main.lisp
│   │   └── math/
│   │       └── stats/
│   │           └── average.lisp
│   └── export-test/           # 导出功能测试
│       ├── main.lisp
│       └── funcs.lisp
```

### 2. 仓颉测试代码示例

```cangjie
/**
 * 测试相对导入 - 文件导入（无前缀）
 */
@TestCase
func testRelativeFileImport() {
    let env = Environment()
    BuiltinFunctions.registerAll(env)
    let evaluator = Evaluator(env)
    evaluator.initModuleSystem()

    // 设置当前文件路径
    evaluator.currentFilePath = Some("./lisp-tests/relative-file/main.lisp")

    // 执行导入
    let importResult = evaluator.eval("(import \"./utils.lisp\")")

    // 验证导入成功
    match (importResult) {
        case Nil => ()  // 成功
        case Str(msg) => @Fail("Import failed: ${msg}")
        case _ => @Fail("Unexpected result")
    }

    // 验证符号可用（无前缀）
    let result = evaluator.eval("(processData \"test\")")

    match (result) {
        case Str(msg) => @Assert(msg, "处理数据")
        case _ => @Fail("Function call failed")
    }
}

/**
 * 测试相对导入 - 目录包导入（有前缀）
 */
@TestCase
func testRelativePackageImport() {
    let env = Environment()
    BuiltinFunctions.registerAll(env)
    let evaluator = Evaluator(env)
    evaluator.initModuleSystem()

    evaluator.currentFilePath = Some("./lisp-tests/relative-package/main.lisp")

    // 执行导入
    let importResult = evaluator.eval("(import \"./helpers\")")

    match (importResult) {
        case Nil => ()
        case Str(msg) => @Fail("Import failed: ${msg}")
        case _ => @Fail("Unexpected result")
    }

    // 验证符号带前缀
    let result1 = evaluator.eval("(helpers.validateEmail \"test@example.com\")")
    let result2 = evaluator.eval("helpers.MAX_SIZE")

    match (result1) {
        case Str(msg) => @Assert(msg, "验证邮箱")
        case _ => @Fail("Function call failed")
    }

    match (result2) {
        case Int64(val) => @Assert(val, 1000)
        case _ => @Fail("Constant access failed")
    }
}

/**
 * 测试相对导入遇到模块时应该报错
 */
@TestCase
func testRelativeImportModuleError() {
    let env = Environment()
    BuiltinFunctions.registerAll(env)
    let evaluator = Evaluator(env)
    evaluator.initModuleSystem()

    evaluator.currentFilePath = Some("./lisp-tests/some/main.lisp")

    // 假设 subdir/ 有 module.lisp
    let result = evaluator.eval("(import \"./subdir\")")

    // 应该返回错误
    match (result) {
        case Str(msg) =>
            @Assert(msg.contains("Cannot import module"))
            @Assert(msg.contains("using relative import"))
        case _ => @Fail("Should return error")
    }
}
```

## 实施步骤

1. **创建测试模块结构** - 在 lisp-tests 下创建测试目录和文件
2. **实现 P0 测试** - 核心功能测试（文件导入、目录包导入、错误检测）
3. **实现 P1 测试** - 重要功能测试（export、嵌套目录、错误处理）
4. **实现 P2 测试** - 边界情况测试（可选）
5. **运行所有测试** - 确保测试通过

## 测试目标

- [ ] 所有相对导入测试都能真正执行导入并验证符号
- [ ] 覆盖文件导入（无前缀）和目录包导入（有前缀）
- [ ] 覆盖正常场景和错误场景
- [ ] 测试通过率达到 100%
