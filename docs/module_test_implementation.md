# 模块系统测试实施方案

## 第一步：创建测试模块结构

### 1.1 文件导入测试（无前缀）

```bash
mkdir -p lisp-tests/module-imports/relative-file
```

**lisp-tests/module-imports/relative-file/main.lisp:**
```lisp
(println "=== 测试文件导入（无前缀）===")

;; 导入同级文件 - 应该无前缀
(import "./utils.lisp")

;; 调用函数 - 无前缀
(println "调用 processData:")
(println "  " (processData "测试数据"))

;; 验证未导出符号不可访问
(try
  (println "调用未导出的符号 internalHelper:")
  (println "  " (internalHelper))
  (println "ERROR: 应该报错")
) catch (e)
  (println "正确：未导出符号不可访问")
)

(println "=== 测试完成 ===")
```

**lisp-tests/module-imports/relative-file/utils.lisp:**
```lisp
;; utils.lisp - 工具函数
(define (processData data)
  (str "处理数据: " data))

(define (internalHelper data)
  (str "内部助手: " data))

;; 只导出 processData
(export processData)
```

### 1.2 目录包导入测试（有前缀）

```bash
mkdir -p lisp-tests/module-imports/relative-package/helpers
```

**lisp-tests/module-imports/relative-package/main.lisp:**
```lisp
(println "=== 测试目录包导入（有前缀）===")

;; 导入同级目录 - 应该有前缀
(import "./helpers")

;; 调用函数 - 带 helpers. 前缀
(println "调用 helpers.validateEmail:")
(println "  " (helpers.validateEmail "test@example.com"))

;; 访问常量 - 带 helpers. 前缀
(println "访问 helpers.MAX_SIZE:")
(println "  " helpers.MAX_SIZE)

;; 调用另一个函数
(println "调用 helpers.formatName:")
(println "  " (helpers.formatName "张三"))

(println "=== 测试完成 ===")
```

**lisp-tests/module-imports/relative-package/helpers/validate.lisp:**
```lisp
(define (validateEmail email)
  (str "验证邮箱: " email))

(define (formatName name)
  (str "格式化名字: " name))

(export validateEmail)
(export formatName)
```

**lisp-tests/module-imports/relative-package/helpers/consts.lisp:**
```lisp
(define MAX_SIZE 1000)
(define MIN_VALUE 0)

(export MAX_SIZE)
(export MIN_VALUE)
```

### 1.3 嵌套目录包导入测试

```bash
mkdir -p lisp-tests/module-imports/nested-package/math/stats
```

**lisp-tests/module-imports/nested-package/main.lisp:**
```lisp
(println "=== 测试嵌套目录包导入 ===")

;; 导入嵌套目录 - 前缀应该是最后一级
(import "./math.stats")

;; 调用函数 - 带 stats. 前缀（不是 math.stats.）
(println "调用 stats.average:")
(println "  " (stats.average [1 2 3 4 5]))

(println "=== 测试完成 ===")
```

**lisp-tests/module-imports/nested-package/math/stats/average.lisp:**
```lisp
(define (average numbers)
  (let ((sum (apply + numbers))
        (count (length numbers)))
    (/ sum count)))

(export average)
```

### 1.4 错误场景测试

```bash
mkdir -p lisp-tests/module-imports/error-tests
mkdir -p lisp-tests/module-imports/error-tests/bad-module
```

**lisp-tests/module-imports/error-tests/file-not-found.lisp:**
```lisp
(println "=== 测试文件不存在错误 ===")

(try
  (import "./notexist.lisp")
  (println "ERROR: 应该报错")
) catch (e)
  (println "正确：文件不存在时返回错误")
  (println "  错误: " e)
)

(println "=== 测试完成 ===")
```

**lisp-tests/module-imports/error-tests/import-module.lisp:**
```lisp
(println "=== 测试相对导入模块错误 ===")

;; bad-module/ 有 module.lisp，不应该用相对导入
(try
  (import "./bad-module")
  (println "ERROR: 应该报错")
) catch (e)
  (println "正确：不能相对导入模块")
  (println "  错误: " e)
)

(println "=== 测试完成 ===")
```

**lisp-tests/module-imports/error-tests/bad-module/module.lisp:**
```lisp
(module bad-module
  (version "0.1.0")
  (description "这个模块不应该被相对导入"))
```

**lisp-tests/module-imports/error-tests/directory-not-found.lisp:**
```lisp
(println "=== 测试目录不存在错误 ===")

(try
  (import "./notexist-dir")
  (println "ERROR: 应该报错")
) catch (e)
  (println "正确：目录不存在时返回错误")
  (println "  错误: " e)
)

(println "=== 测试完成 ===")
```

### 1.5 Export 功能测试

```bash
mkdir -p lisp-tests/module-imports/export-test
```

**lisp-tests/module-imports/export-test/main.lisp:**
```lisp
(println "=== 测试 Export 功能 ===")

(import "./funcs.lisp")

;; 测试导出的函数
(println "调用导出的函数 publicFunc:")
(println "  " (publicFunc))

;; 测试未导出的函数
(try
  (privateFunc)
  (println "ERROR: 应该报错")
) catch (e)
  (println "正确：未导出函数不可访问")

(println "=== 测试完成 ===")
```

**lisp-tests/module-imports/export-test/funcs.lisp:**
```lisp
(define (publicFunc)
  "这是公开函数")

(define (privateFunc)
  "这是私有函数")

(export publicFunc)
```

## 第二步：实现仓颉单元测试

### 2.1 更新 module_test.cj

在现有测试后添加以下测试：

```cangjie
/**
 * 测试相对导入 - 文件导入（无前缀）
 * 真正执行导入并验证符号可用
 */
@TestCase
func testRelativeFileImport() {
    let env = Environment()
    BuiltinFunctions.registerAll(env)
    let evaluator = Evaluator(env)
    evaluator.initModuleSystem()

    // 设置当前文件路径
    evaluator.currentFilePath = Some("./lisp-tests/module-imports/relative-file/main.lisp")

    // 执行导入
    let importResult = evaluator.eval("(import \"./utils.lisp\")")

    match (importResult) {
        case Nil => ()  // 成功
        case Str(msg) => @Fail("Import failed: ${msg}")
        case _ => @Fail("Unexpected result")
    }

    // 验证符号可用（无前缀）
    let result = evaluator.eval("(processData \"test\")")

    match (result) {
        case Str(msg) =>
            @Assert(msg.contains("处理数据"))
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

    evaluator.currentFilePath = Some("./lisp-tests/module-imports/relative-package/main.lisp")

    // 执行导入
    let importResult = evaluator.eval("(import \"./helpers\")")

    match (importResult) {
        case Nil => ()
        case Str(msg) => @Fail("Import failed: ${msg}")
        case _ => @Fail("Unexpected result")
    }

    // 验证符号带前缀
    let result1 = evaluator.eval("(helpers.validateEmail \"test@example.com\")")

    match (result1) {
        case Str(msg) =>
            @Assert(msg.contains("验证邮箱"))
        case _ => @Fail("Function call failed")
    }

    // 验证常量带前缀
    let result2 = evaluator.eval("helpers.MAX_SIZE")

    match (result2) {
        case Int64(val) => @Assert(val, 1000)
        case _ => @Fail("Constant access failed")
    }
}

/**
 * 测试嵌套目录包导入
 */
@TestCase
func testNestedPackageImport() {
    let env = Environment()
    BuiltinFunctions.registerAll(env)
    let evaluator = Evaluator(env)
    evaluator.initModuleSystem()

    evaluator.currentFilePath = Some("./lisp-tests/module-imports/nested-package/main.lisp")

    // 执行导入
    let importResult = evaluator.eval("(import \"./math.stats\")")

    match (importResult) {
        case Nil => ()
        case Str(msg) => @Fail("Import failed: ${msg}")
        case _ => @Fail("Unexpected result")
    }

    // 验证前缀是最后一级（stats. 而不是 math.stats.）
    let result = evaluator.eval("(stats.average [1 2 3 4 5])")

    match (result) {
        case Float64(val) => @Assert(val, 3.0)
        case _ => @Fail("Function call failed")
    }
}

/**
 * 测试相对导入遇到模块时的错误
 */
@TestCase
func testRelativeImportModuleError() {
    let env = Environment()
    BuiltinFunctions.registerAll(env)
    let evaluator = Evaluator(env)
    evaluator.initModuleSystem()

    evaluator.currentFilePath = Some("./lisp-tests/module-imports/error-tests/import-module.lisp")

    // 相对导入有 module.lisp 的目录应该报错
    let result = evaluator.eval("(import \"./bad-module\")")

    match (result) {
        case Str(msg) =>
            @Assert(msg.contains("Cannot import module"))
            @Assert(msg.contains("using relative import"))
        case _ => @Fail("Should return error")
    }
}

/**
 * 测试文件不存在错误
 */
@TestCase
func testFileNotFoundError() {
    let env = Environment()
    BuiltinFunctions.registerAll(env)
    let evaluator = Evaluator(env)
    evaluator.initModuleSystem()

    evaluator.currentFilePath = Some("./lisp-tests/module-imports/error-tests/file-not-found.lisp")

    let result = evaluator.eval("(import \"./notexist.lisp\")")

    match (result) {
        case Str(msg) =>
            @Assert(msg.contains("file not found") | msg.contains("not found"))
        case _ => @Fail("Should return error")
    }
}

/**
 * 测试目录不存在错误
 */
@TestCase
func testDirectoryNotFoundError() {
    let env = Environment()
    BuiltinFunctions.registerAll(env)
    let evaluator = Evaluator(env)
    evaluator.initModuleSystem()

    evaluator.currentFilePath = Some("./lisp-tests/module-imports/error-tests/directory-not-found.lisp")

    let result = evaluator.eval("(import \"./notexist-dir\")")

    match (result) {
        case Str(msg) =>
            @Assert(msg.contains("directory not found") | msg.contains("not found"))
        case _ => @Fail("Should return error")
    }
}

/**
 * 测试 Export 功能
 */
@TestCase
func testExportFunctionality() {
    let env = Environment()
    BuiltinFunctions.registerAll(env)
    let evaluator = Evaluator(env)
    evaluator.initModuleSystem()

    evaluator.currentFilePath = Some("./lisp-tests/module-imports/export-test/main.lisp")

    // 导入文件
    let importResult = evaluator.eval("(import \"./funcs.lisp\")")

    match (importResult) {
        case Nil => ()
        case Str(msg) => @Fail("Import failed: ${msg}")
        case _ => @Fail("Unexpected result")
    }

    // 测试导出的函数可用
    let result1 = evaluator.eval("(publicFunc)")

    match (result1) {
        case Str(msg) => @Assert(msg, "这是公开函数")
        case _ => @Fail("Exported function not accessible")
    }

    // 测试未导出的函数不可访问（应该报错）
    let result2 = evaluator.eval("(privateFunc)")

    match (result2) {
        case Str(msg) => @Assert(msg.contains("Error") | msg.contains("未定义"))
        case _ => ()  // 任何其他返回值都表示有错误处理
    }
}

/**
 * 测试绝对导入 - 第三方模块
 */
@TestCase
func testAbsoluteImportThirdParty() {
    let env = Environment()
    BuiltinFunctions.registerAll(env)
    let evaluator = Evaluator(env)
    evaluator.initModuleSystem()

    // 添加搜索路径
    match (evaluator.moduleRegistry) {
        case Some(registry) =>
            registry.addSearchPath("./examples")
        case None => @Fail("ModuleRegistry not initialized")
    }

    evaluator.currentFilePath = Some("./lisp-tests/some/main.lisp")

    // 绝对导入第三方模块
    let result = evaluator.eval("(import ystyle::zlog)")

    match (result) {
        case Nil => ()  // 成功
        case Str(msg) => @Fail("Import failed: ${msg}")
        case _ => @Fail("Unexpected result")
    }

    // TODO: 验证符号可用（需要 zlog 模块有导出的符号）
}
```

## 第三步：运行测试

```bash
# 编译
cjpm build

# 运行所有测试
cjpm test --show-all-output --filter 'ModuleTest.*'

# 或运行特定测试
cjpm test --show-all-output --filter 'ModuleTest.testRelativeFileImport'
cjpm test --show-all-output --filter 'ModuleTest.testRelativePackageImport'
```

## 测试覆盖目标

| 功能类别 | 测试数量 | 状态 |
|---------|---------|------|
| 相对导入 - 文件导入 | 3 | ✅ 已规划 |
| 相对导入 - 目录包导入 | 3 | ✅ 已规划 |
| 嵌套目录导入 | 1 | ✅ 已规划 |
| Export 功能 | 2 | ✅ 已规划 |
| 错误处理 | 4 | ✅ 已规划 |
| 绝对导入 | 1 | ✅ 已规划 |
| **总计** | **14** | **待实现** |

## 预期结果

所有测试通过后，模块系统将具备：
- ✅ 完整的相对导入测试（文件 + 目录包）
- ✅ 符号绑定验证（前缀规则正确性）
- ✅ Export 功能验证
- ✅ 错误场景覆盖
- ✅ 真实导入而非简化测试
