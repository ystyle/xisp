# 模块系统互操作设计文档

**版本**: 0.1.0
**日期**: 2026-01-28
**状态**: 设计阶段

---

## 目录

- [1. 概述](#1-概述)
- [2. 设计目标](#2-设计目标)
- [3. 核心接口](#3-核心接口)
- [4. 实现方案](#4-实现方案)
- [5. 使用示例](#5-使用示例)
- [6. API 参考](#6-api-参考)
- [7. 迁移指南](#7-迁移指南)
- [8. 附录](#8-附录)

---

## 1. 概述

### 1.1 背景

Xisp 模块系统当前只支持从文件系统加载模块。随着应用场景的扩展，需要支持从其他数据源（如数据库、内存、网络）加载模块，以提高互操作性和灵活性。

### 1.2 当前限制

**现状**：
- ✅ 从文件系统加载模块
- ❌ 无法从内存/数据库加载
- ❌ 无法动态注册模块
- ❌ 无法在测试中模拟模块

**需求**：
- ✅ 从内存/字节数组加载模块
- ✅ 从数据库加载模块
- ✅ 支持自定义数据源
- ✅ 保持向后兼容

---

## 2. 设计目标

### 2.1 核心目标

1. **灵活性**：支持任意数据源
2. **向后兼容**：不破坏现有文件系统模块
3. **易用性**：提供便捷的 MemorySource
4. **可扩展**：企业用户可以自定义数据源
5. **类型安全**：编译期检查，避免运行时错误

### 2.2 非目标

- ❌ 模块打包格式（如 JSON、二进制包）
- ❌ 远程模块加载（HTTP、gRPC）
- ❌ 模块热更新/动态重载

这些功能可以在 ModuleSource 基础上扩展。

---

## 3. 核心接口

### 3.1 ModuleSource 接口

定义模块数据源的抽象接口。

```cangjie
package ystyle::xisp.core

import std.collection.ArrayList

/**
 * 模块数据源接口
 *
 * 用于支持从任意数据源加载模块（文件系统、内存、数据库等）
 */
public interface ModuleSource {
    /**
     * 获取模块的 module.lisp 内容（元数据）
     *
     * @param moduleName 模块名（如："ystyle::log" 或 "ystyle::log.zlog"）
     * @return module.lisp 的内容，如果模块不存在返回 None
     */
    func getMetadata(moduleName: String): ?String

    /**
     * 列出模块的所有文件（相对路径）
     *
     * @param moduleName 模块名
     * @return 文件路径列表（如：["core.lisp", "utils/helper.lisp"]）
     *         如果模块不存在返回空列表
     */
    func listFiles(moduleName: String): ArrayList<String>

    /**
     * 读取单个文件内容
     *
     * @param moduleName 模块名
     * @param filePath 文件相对路径（如："core.lisp" 或 "utils/helper.lisp"）
     * @return 文件内容，如果文件不存在返回 None
     */
    func readFile(moduleName: String, filePath: String): ?String
}
```

### 3.2 设计说明

**接口职责**：
- `getMetadata()`: 获取 `module.lisp` 的内容
- `listFiles()`: 获取模块包含的文件列表
- `readFile()`: 读取单个文件的内容

**为什么选择三方法设计**：
1. ✅ **职责分离**：元数据、文件列表、文件内容各自独立
2. ✅ **灵活性**：可以实现缓存、延迟加载、分页加载等
3. ✅ **可测试性**：容易 mock 和测试
4. ✅ **类型安全**：返回 `?String` 和 `ArrayList<String>`，符合仓颉习惯

---

## 4. 实现方案

### 4.1 架构图

```
┌────────────────────────────────────────────────────────┐
│                    LispInterpreter                      │
│  ┌──────────────────────────────────────────────────┐  │
│  │         ModuleRegistry                             │  │
│  │  - modules: HashMap<String, Module>               │  │
│  │  - searchPaths: ArrayList<String>                 │  │
│  └──────────────────────────────────────────────────┘  │
│  ┌──────────────────────────────────────────────────┐  │
│  │         ModuleLoader                              │  │
│  │  - source: Option<ModuleSource>                  │  │
│  │  - loadModule() → 优先用 source，否则用文件系统   │  │
│  └──────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────┘
           │                            │
    source = Some(MemorySource)   source = None (默认)
           │                            │
           ▼                            ▼
    从内存/数据库加载              从文件系统加载
```

### 4.2 MemorySource 实现

**完整实现**：

```cangjie
package ystyle::xisp.core

import std.collection.ArrayList
import std.collection.HashMap

/**
 * 内存模块数据源
 *
 * 适用场景：
 * - 测试/单元测试：避免文件系统依赖
 * - 动态生成模块：从数据库、网络等加载
 * - 嵌入式系统：模块代码编译进程序
 *
 * 使用示例：
 * ```
 * let source = MemorySource()
 * source.registerModule(
 *     "ystyle::log",
 *     "(module log (version \"0.2.0\")...",
 *     HashMap<String, String> {
 *         ["core.lisp"] => "(export info...)",
 *         ["utils/helper.lisp"] => "(export helper...)"
 *     }
 * )
 *
 * let interpreter = LispInterpreter([
 *     withStdLib(),
 *     withModuleSource(source)
 * ])
 * ```
 */
public class MemorySource <: ModuleSource {
    /**
     * 模块元数据（module.lisp 内容）
     * key: 模块名（如："ystyle::log"）
     * value: module.lisp 内容
     */
    private var metadata: HashMap<String, String>

    /**
     * 模块文件内容
     * key: 模块名（如："ystyle::log"）
     * value: HashMap<文件路径, 文件内容>
     */
    private var files: HashMap<String, HashMap<String, String>>

    /**
     * 构造函数
     */
    public init() {
        this.metadata = HashMap<String, String>()
        this.files = HashMap<String, HashMap<String, String>>()
    }

    /**
     * 注册模块（推荐方式）
     *
     * @param moduleName 模块名（如："ystyle::log"）
     * @param moduleMetadata module.lisp 的内容
     * @param moduleFiles 文件映射（文件路径 -> 内容）
     *
     * @example
     * ```
     * source.registerModule(
     *     "ystyle::log",
     *     "(module log ...)",
     *     HashMap<String, String> {
     *         ["core.lisp"] => "(export info...)",
     *         ["file.lisp"] => "(export error...)"
     *     }
     * )
     * ```
     */
    public func registerModule(
        moduleName: String,
        moduleMetadata: String,
        moduleFiles: HashMap<String, String>
    ) {
        this.metadata[moduleName] = moduleMetadata
        this.files[moduleName] = moduleFiles
    }

    /**
     * 注册模块的元数据（单独调用）
     *
     * @param moduleName 模块名
     * @param metadata module.lisp 内容
     */
    public func registerMetadata(moduleName: String, metadata: String) {
        this.metadata[moduleName] = metadata
    }

    /**
     * 添加模块文件
     *
     * @param moduleName 模块名
     * @param filePath 文件路径（如："core.lisp"）
     * @param content 文件内容
     */
    public func addFile(moduleName: String, filePath: String, content: String) {
        if (!this.files.contains(moduleName)) {
            this.files[moduleName] = HashMap<String, String>()
        }
        match (this.files.get(moduleName)) {
            case Some(fileMap) => fileMap[filePath] = content
            case None => ()
        }
    }

    /**
     * 批量注册模块
     *
     * @param modules 模块映射（模块名 -> ModuleData）
     *
     * @example
     * ```
     * let modules = HashMap<String, ModuleData>()
     * modules["ystyle::log"] = ModuleData(
     *     "ystyle::log",
     *     "0.2.0",
     *     "ystyle",
     *     HashMap<String, String> { ... }
     * )
     * source.registerModules(modules)
     * ```
     */
    public func registerModules(modules: HashMap<String, ModuleData>) {
        for ((moduleName, data) in modules) {
            this.registerModule(moduleName, data.metadata, data.files)
        }
    }

    // ==================== ModuleSource 接口实现 ====================

    /**
     * 获取模块元数据
     */
    public func getMetadata(moduleName: String): ?String {
        if (this.metadata.contains(moduleName)) {
            this.metadata.get(moduleName)
        } else {
            None
        }
    }

    /**
     * 列出模块的所有文件
     */
    public func listFiles(moduleName: String): ArrayList<String> {
        if (this.files.contains(moduleName)) {
            match (this.files.get(moduleName)) {
                case Some(fileMap) => {
                    let keys = ArrayList<String>()
                    for ((path, _) in fileMap) {
                        keys.add(path)
                    }
                    keys
                }
                case None => ArrayList<String>()
            }
        } else {
            ArrayList<String>()
        }
    }

    /**
     * 读取文件内容
     */
    public func readFile(moduleName: String, filePath: String): ?String {
        if (this.files.contains(moduleName)) {
            match (this.files.get(moduleName)) {
                case Some(fileMap) => fileMap.get(filePath)
                case None => None
            }
        } else {
            None
        }
    }
}

/**
 * 模块数据结构
 * 用于批量注册模块
 */
public class ModuleData {
    public let name: String
    public let version: String
    public let organization: String
    public let files: HashMap<String, String>

    public init(
        name: String,
        version: String,
        organization: String,
        files: HashMap<String, String>
    ) {
        this.name = name
        this.version = version
        this.organization = organization
        this.files = files
    }

    /**
     * 从 metadata 字符串和文件映射创建 ModuleData
     */
    public static func fromContent(
        name: String,
        version: String,
        organization: String,
        metadata: String,
        files: HashMap<String, String>
    ): ModuleData {
        ModuleData(name, version, organization, files)
    }
}
```

### 4.3 FileSystemSource 实现

```cangjie
package ystyle::xisp.core

import std.collection.ArrayList
import std.fs.*

/**
 * 文件系统模块数据源（默认实现）
 *
 * 封装现有的文件系统加载逻辑，使其符合 ModuleSource 接口
 */
public class FileSystemSource <: ModuleSource {
    private var basePath: String  // 模块搜索路径

    /**
     * 构造函数
     *
     * @param basePath 模块搜索路径（如："~/.xisp/modules"）
     */
    public init(basePath: String) {
        this.basePath = basePath
    }

    /**
     * 获取模块元数据
     */
    public func getMetadata(moduleName: String): ?String {
        let (org, packageName) = parseModuleName(moduleName)
        let modulePath = resolveModulePath(basePath, org, packageName)

        let metadataPath = Path(modulePath).join("module.lisp").toString()

        if (!exists(metadataPath)) {
            return None
        }

        try {
            let bytes = File.readFrom(metadataPath)
            String.fromUtf8(bytes)
        } catch (_: FSException) {
            None
        } catch (_: Exception) {
            None
        }
    }

    /**
     * 列出模块的所有文件
     *
     * 扫描模块目录，返回所有 .lisp 文件（排除 module.lisp 和 . 开头的文件）
     */
    public func listFiles(moduleName: String): ArrayList<String> {
        let (org, packageName) = parseModuleName(moduleName)
        let modulePath = resolveModulePath(basePath, org, packageName)

        if (!exists(modulePath)) {
            return ArrayList<String>()
        }

        let files = ArrayList<String>()
        let pathObj = Path(modulePath)

        try {
            Directory.walk(pathObj, { fileInfo =>
                let fileName = fileInfo.name

                // 忽略以 . 开头的文件
                if (fileName.startsWith(".")) {
                    return true
                }

                // 忽略目录
                if (fileInfo.isDirectory()) {
                    return true
                }

                // 只处理 .lisp 文件，排除 module.lisp
                if (fileName.endsWith(".lisp") && fileName != "module.lisp") {
                    // 获取相对路径
                    let filePath = fileInfo.path.toString()
                    let relativePath = getRelativePath(modulePath, filePath)
                    files.add(relativePath)
                }

                true
            })
        } catch (_: FSException) {
            // 目录不存在
        } catch (_: Exception) {
            // 其他异常
        }

        // 按文件名排序
        sort(files)
        files
    }

    /**
     * 读取文件内容
     */
    public func readFile(moduleName: String, filePath: String): ?String {
        let (org, packageName) = parseModuleName(moduleName)
        let modulePath = resolveModulePath(basePath, org, packageName)
        let fullPath = Path(modulePath).join(filePath).toString()

        if (!exists(fullPath)) {
            return None
        }

        try {
            let bytes = File.readFrom(fullPath)
            String.fromUtf8(bytes)
        } catch (_: FSException) {
            None
        } catch (_: Exception) {
            None
        }
    }

    // ==================== 辅助方法 ====================

    /**
     * 解析模块名
     * "ystyle::log" → ("ystyle", "log")
     * "io" → ("", "io")
     */
    private func parseModuleName(moduleName: String): (String, String) {
        let parts = moduleName.split("::")
        if (parts.size == 2) {
            (parts[0], parts[1])
        } else {
            ("", moduleName)
        }
    }

    /**
     * 解析模块路径
     */
    private func resolveModulePath(basePath: String, org: String, packageName: String): String {
        if (org.isEmpty()) {
            Path(basePath).join(packageName).toString()
        } else {
            Path(basePath).join(org).join(packageName).toString()
        }
    }

    /**
     * 获取相对路径
     */
    private func getRelativePath(basePath: String, fullPath: String): String {
        if (fullPath.startsWith(basePath)) {
            let relative = fullPath[basePath.size..]
            if (relative.startsWith("/")) {
                relative[1..]
            } else {
                relative
            }
        } else {
            fullPath
        }
    }
}
```

### 4.4 Interpreter 配置

```cangjie
package ystyle::xisp

import ystyle::xisp.core.*
import std.collection.ArrayList

/**
 * 解释器配置选项
 */

/**
 * 设置自定义模块数据源
 *
 * @param source ModuleSource 实例（MemorySource、FileSystemSource 或自定义）
 *
 * @example
 * ```
 * let source = MemorySource()
 * source.registerModule(...)
 *
 * let interpreter = LispInterpreter([
 *     withStdLib(),
 *     withModuleSource(source)
 * ])
 * ```
 */
public func withModuleSource(source: ModuleSource): (LispInterpreter) -> Unit {
    { interpreter => interpreter.setModuleSource(source) }
}

// 扩展 LispInterpreter
public class LispInterpreter {
    // ... 现有字段 ...

    /**
     * 模块数据源（None = 默认文件系统）
     */
    private var moduleSource: Option<ModuleSource> = None

    /**
     * 设置模块数据源
     *
     * @param source ModuleSource 实例
     *
     * @example
     * ```
     * let source = MemorySource()
     * interpreter.setModuleSource(source)
     * ```
     */
    public func setModuleSource(source: ModuleSource) {
        this.moduleSource = Some(source)
        this.evaluator.moduleLoader.setSource(source)
    }
}
```

### 4.5 ModuleLoader 改进

```cangjie
package ystyle::xisp.core

public class ModuleLoader {
    // ... 现有字段 ...

    /**
     * 自定义数据源（None = 使用默认文件系统逻辑）
     */
    private var source: Option<ModuleSource> = None

    /**
     * 设置自定义数据源
     */
    public func setSource(source: ModuleSource) {
        this.source = Some(source)
    }

    /**
     * 从数据源加载模块
     *
     * @param moduleName 模块名
     * @param env 环境
     * @return 是否成功
     */
    public func loadModule(moduleName: String, env: Environment): Bool {
        match (this.source) {
            case Some(src) => this.loadFromSource(src, moduleName, env)
            case None => this.loadFromFileSystem(moduleName, env)
        }
    }

    /**
     * 从自定义数据源加载
     */
    private func loadFromSource(source: ModuleSource, moduleName: String, env: Environment): Bool {
        // 1. 加载元数据
        match (source.getMetadata(moduleName)) {
            case Some(metadata) =>
                if (!this.evalContent(metadata, "${moduleName}/module.lisp", env)) {
                    return false
                }
            case None =>
                // 没有元数据也算成功（可选）
                ()
        }

        // 2. 加载文件
        let files = source.listFiles(moduleName)
        if (files.isEmpty()) {
            return true  // 空模块也算成功
        }

        // 3. 按顺序加载文件
        for (filePath in files) {
            match (source.readFile(moduleName, filePath)) {
                case Some(content) =>
                    if (!this.evalContent(content, "${moduleName}/${filePath}", env)) {
                        return false
                    }
                case None =>
                    return false  // 文件不存在
            }
        }

        true
    }

    /**
     * 从文件系统加载（原有逻辑，保持兼容）
     */
    private func loadFromFileSystem(moduleName: String, env: Environment): Bool {
        // 使用现有的 ModuleLoader 逻辑
        // ... 保持不变 ...
    }
}
```

---

## 5. 使用示例

### 5.1 基础用法：从内存加载模块

```cangjie
package main

import ystyle::xisp.*
import ystyle::xisp.core.*
import std.collection.HashMap

main() {
    // 1. 创建内存数据源
    let source = MemorySource()

    // 2. 注册模块
    source.registerModule(
        "myapp::utils",
        """
        (module utils
          (version "1.0.0")
          (organization "myapp")
          (description "Utility functions"))
        """,
        HashMap<String, String> {
            ["core.lisp"] => """
                (export greet)
                (define (greet name)
                    (str "Hello, " name "!"))
            """,
            ["math.lisp"] => """
                (export add)
                (define (add a b)
                    (+ a b))
            """
        }
    )

    // 3. 创建解释器
    let interpreter = LispInterpreter([
        withStdLib(),
        withModuleSource(source)  // 设置数据源
    ])

    // 4. 加载并使用模块
    interpreter.eval("(import myapp::utils)")
    interpreter.eval("(utils.greet \"World\")")  // 输出: Hello, World!
}
```

### 5.2 从数据库加载模块

```cangjie
package main

import ystyle::xisp.*
import ystyle::xisp.core.*

/**
 * 数据库模块数据源
 */
class DatabaseSource <: ModuleSource {
    private let db: DatabaseConnection

    public init(db: DatabaseConnection) {
        this.db = db
    }

    public func getMetadata(moduleName: String): ?String {
        db.queryMetadata(moduleName)
    }

    public func listFiles(moduleName: String): ArrayList<String> {
        db.queryFiles(moduleName)
    }

    public func readFile(moduleName: String, filePath: String): ?String {
        db.queryFileContent(moduleName, filePath)
    }
}

main() {
    let db = DatabaseConnection("jdbc:postgresql://localhost/modules")
    let source = DatabaseSource(db)

    let interpreter = LispInterpreter([
        withStdLib(),
        withModuleSource(source)
    ])

    // 从数据库加载的模块可以正常使用
    interpreter.eval("(import company::internal)")
}
```

### 5.3 单元测试

```cangjie
package ystyle::xisp.tests

import std.unittest.*
import std.unittest.testmacro.*
import ystyle::xisp.*
import ystyle::xisp.core.*

@Test
class ModuleSourceTest {
    @TestCase
    func testMemorySource() {
        // 创建内存数据源
        let source = MemorySource()

        // 注册模块
        source.registerModule(
            "test::mod",
            "(module mod (version \"1.0\"))",
            HashMap<String, String> {
                ["core.lisp"] => "(export foo) (define foo 42)"
            }
        )

        // 创建解释器
        let interpreter = LispInterpreter([
            withModuleSource(source)
        ])

        // 加载模块
        interpreter.eval("(import test::mod)")

        // 验证
        let result = interpreter.eval("test.mod.foo")
        if (let Some(i) <- result.asInt()) {
            @Assert(i == 42)
        }
    }
}
```

### 5.4 动态注册模块（运行时）

```cangjie
package main

import ystyle::xisp.*
import ystyle::xisp.core.*

main() {
    let interpreter = LispInterpreter([withStdLib()])
    let source = MemorySource()

    // 初始状态：没有模块
    let result1 = interpreter.eval("(import mylib)")
    // result1 应该是错误（模块不存在）

    // 运行时注册模块
    source.registerModule(
        "mylib",
        "(module mylib (version \"1.0\"))",
        HashMap<String, String> {
            ["core.lisp"] => "(export hello) (define (hello) \"Hi!\")"
        }
    )

    // 设置数据源
    interpreter.setModuleSource(source)

    // 现在可以加载了
    let result2 = interpreter.eval("(import mylib)")
    // result2 应该成功

    let result3 = interpreter.eval("mylib.hello")
    // result3 应该返回 "Hi!"
}
```

---

## 6. API 参考

### 6.1 ModuleSource 接口

| 方法 | 签名 | 说明 |
|------|------|------|
| getMetadata | `func getMetadata(moduleName: String): ?String` | 获取 module.lisp 内容 |
| listFiles | `func listFiles(moduleName: String): ArrayList<String>` | 列出模块文件 |
| readFile | `func readFile(moduleName: String, filePath: String): ?String` | 读取文件内容 |

### 6.2 MemorySource 类

| 方法 | 签名 | 说明 |
|------|------|------|
| registerModule | `func registerModule(moduleName, metadata, files)` | 注册模块（推荐） |
| registerMetadata | `func registerMetadata(moduleName, metadata)` | 注册元数据 |
| addFile | `func addFile(moduleName, filePath, content)` | 添加单个文件 |
| registerModules | `func registerModules(modules: HashMap<String, ModuleData>)` | 批量注册 |

### 6.3 FileSystemSource 类

| 方法 | 签名 | 说明 |
|------|------|------|
| init | `public init(basePath: String)` | 构造函数 |
| getMetadata | 继承自 ModuleSource | 从文件系统读取 |
| listFiles | 继承自 ModuleSource | 扫描目录 |
| readFile | 继承自 ModuleSource | 读取文件 |

### 6.4 选项函数

| 函数 | 说明 |
|------|------|
| `withModuleSource(source: ModuleSource)` | 设置自定义模块数据源 |

---

## 7. 迁移指南

### 7.1 从文件系统迁移到自定义数据源

**之前**（只能用文件系统）：
```cangjie
let interpreter = LispInterpreter([withStdLib()])
interpreter.addModuleSearchPath("lisp-tests")
interpreter.eval("(import ystyle::log)")  // 从文件系统加载
```

**之后**（可选自定义数据源）：
```cangjie
// 方式 1：仍使用文件系统（默认，完全兼容）
let interpreter1 = LispInterpreter([withStdLib()])
interpreter1.addModuleSearchPath("lisp-tests")
interpreter1.eval("(import ystyle::log)")

// 方式 2：使用自定义数据源
let source = MemorySource()
source.registerModule(...)
let interpreter2 = LispInterpreter([withStdLib(), withModuleSource(source)])
interpreter2.eval("(import ystyle::log)")  // 从内存加载
```

### 7.2 企业级自定义数据源

```cangjie
/**
 * 从公司内部数据库加载模块
 */
class EnterpriseModuleSource <: ModuleSource {
    private let configServer: ConfigServerClient
    private let cache: ModuleCache

    public init(configServer: ConfigServerClient) {
        this.configServer = configServer
        this.cache = ModuleCache()
    }

    public func getMetadata(moduleName: String): ?String {
        // 1. 尝试从缓存获取
        match (this.cache.getMetadata(moduleName)) {
            case Some(cached) => Some(cached)
            case None => {
                // 2. 从配置服务器获取
                match (this.configServer.fetchMetadata(moduleName)) {
                    case Some(metadata) => {
                        this.cache.putMetadata(moduleName, metadata)
                        Some(metadata)
                    }
                    case None => None
                }
            }
        }
    }

    public func listFiles(moduleName: String): ArrayList<String> {
        // 类似的缓存逻辑...
    }

    public func readFile(moduleName: String, filePath: String): ?String {
        // 类似的缓存逻辑...
    }
}
```

---

## 8. 附录

### 8.1 设计决策记录

#### A1. 为什么选择三方法接口而非单一方法？

**决策**：选择方案 B（三方法接口）

**理由**：
1. ✅ **职责分离**：元数据、文件列表、文件内容各自独立
2. ✅ **灵活性**：可以实现缓存、延迟加载、分页等
3. ✅ **可测试性**：每个方法都可以独立 mock

**替代方案**：
- 方案 A（单一方法）：`readFile(moduleAndPath: String): ?String`
  - ❌ 需要解析字符串，容易出错
  - ❌ 无法单独获取文件列表
  - ❌ 测试困难

#### A2. 为什么选择注册方法而非构造函数？

**决策**：选择选项 B（注册方法）

**理由**：
1. ✅ **灵活性**：可以分步注册，不需要一次性准备好所有数据
2. ✅ **动态性**：运行时可以添加新模块
3. ✅ **清晰性**：`registerModule()` 比 构造函数参数更清晰

**替代方案**：
- 选项 A（构造函数参数）：`MemorySource(modules: HashMap<...>)`
  - ❌ 一次性传入所有数据，不够灵活
  - ❌ 运行时无法添加新模块

#### A3. 为什么选择选项模式（withModuleSource）而非 setter？

**决策**：选择选项 B（选项模式）

**理由**：
1. ✅ **风格一致**：与 `withStdLib()` 保持一致
2. ✅ **不可变性**：构造后不可修改，线程安全
3. ✅ **声明式**：在构造时明确配置

**替代方案**：
- 选项 C（setter）：`interpreter.setModuleSource(source)`
  - ❌ 可变对象，不是线程安全
  - ❌ 可能被覆盖，不够明确

### 8.2 未解决的挑战

#### U1. 模块依赖传递

**问题**：如果模块 A 依赖模块 B，如何保证加载顺序？

**可能的方案**：
1. 在 module.lisp 中声明依赖（当前已支持）
2. 在加载时自动解析依赖（当前已支持）
3. ModuleSource 实现时需要考虑依赖关系

**建议**：保持现有依赖加载逻辑不变，ModuleSource 只负责提供文件。

#### U2. 循环依赖检测

**问题**：如果 A 依赖 B，B 又依赖 A，如何检测？

**建议**：依赖检测在 ModuleRegistry 层面处理，与 ModuleSource 无关。

#### U3. 模块版本冲突

**问题**：同一模块不同版本如何处理？

**建议**：
- 文件系统：使用目录版本（`log@0.1.0/`）
- MemorySource：在 `registerModule()` 时使用不同的模块名（如 `log@0.1.0`、`log@0.2.0`）

---

## 9. 实现检查清单

### 阶段 1：核心接口和基础实现
- [ ] 定义 `ModuleSource` 接口
- [ ] 实现 `MemorySource`
- [ ] 实现 `FileSystemSource`
- [ ] 单元测试

### 阶段 2：集成到 Interpreter
- [ ] `LispInterpreter.setModuleSource()`
- [ ] `withModuleSource()` 选项函数
- [ ] `ModuleLoader.setSource()` 和 `loadModule()`
- [ ] 向后兼容测试

### 阶段 3：文档和示例
- [ ] API 文档
- [ ] 使用示例
- [ ] 迁移指南
- [ ] 企业级示例（DatabaseSource）

### 阶段 4：高级特性（可选）
- [ ] 模块缓存机制
- [ ] 热更新/重载
- [ ] 远程模块加载

---

## 10. 参考资料

- [模块系统文档](./modules.md)
- [模块系统实现](../src/core/module.cj)
- [模块加载器实现](../src/core/module_loader.cj)
- [LispInterpreter API](./integration/bridge.md)

---

**文档版本**: 0.1.0
**最后更新**: 2026-01-28
**作者**: Xisp Team
