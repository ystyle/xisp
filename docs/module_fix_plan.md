# Xisp 模块系统修改方案

## 核心问题

当前实现将包名（如 `log.zlog`）当作完整目录路径使用，导致目录结构错误。

### 错误示例
```
(import ystyle::log.zlog)
→ ~/.xisp/modules/ystyle/log.zlog/package.lisp  ❌
```

### 正确规范
```
(import ystyle::log.zlog)
→ ~/.xisp/modules/ystyle/log/package.lisp  ✅
```

---

## 核心规则

### 规则 1：目录名提取

从包名提取目录名时，**去掉最后一级**：

| 包名 | 目录名 | 说明 |
|------|-------|------|
| `io` | `io` | 单级，不变 |
| `log.zlog` | `log` | 去掉 `.zlog` |
| `io.file` | `io` | 去掉 `.file` |
| `utils.string.parser` | `utils.string` | 去掉 `.parser` |

### 规则 2：使用时的包名

导入后使用时，使用**包名的最后一级**：

```lisp
(import ystyle::log.zlog)
(zlog.init "app")  ; 使用 zlog，不是 log

(import std.io.file)
(file.read path)   ; 使用 file，不是 io
```

### 规则 3：目录结构

```
~/.xisp/modules/
├── ystyle/
│   └── log/              ← log.zlog 的目录
│       ├── package.lisp  ← (package log.zlog ...)
│       └── zlog/         ← 子目录（可选）
│           └── app.lisp
├── std/
│   └── io/               ← io.file 的目录
│       ├── package.lisp  ← (package io.file ...)
│       └── file/         ← 子目录（可选）
│           └── core.lisp
└── utils.string/         ← utils.string.parser 的目录
    ├── package.lisp      ← (package utils.string.parser ...)
    └── parser/
        └── core.lisp
```

---

## 修改清单

### 修改 1：src/core/module.cj

**位置**：ModuleRegistry 类，第 327-372 行

**新增函数**：
```cj
/**
 * 从包名提取目录名
 * 规则：去掉包名的最后一级
 *
 * 示例：
 * - io → io
 * - log.zlog → log
 * - io.file → io
 * - utils.string.parser → utils.string
 */
public func extractPackageDir(packageName: String): String {
    let parts = packageName.split('.')

    if (parts.size <= 1) {
        packageName
    } else {
        // 去掉最后一级
        let dirParts = ArrayList<String>()
        for (i in 0..(parts.size - 1)) {
            dirParts.add(parts[i])
        }
        dirParts.join(".")
    }
}
```

**修改函数**：resolveModulePath（第 335-350 行）

```cj
/**
 * 解析模块名到目录路径
 * 模块名格式：org::package.subpackage
 * 路径格式：searchPath/org/packageDir/
 *
 * @param moduleName 模块名（如："ystyle::log.zlog"）
 * @return 找到的包目录路径，如果找不到返回 None
 */
public func resolveModulePath(moduleName: String): Option<String> {
    let (org, packageName) = this.parseModuleName(moduleName)
    let packageDir = this.extractPackageDir(packageName)  // ← 新增

    // 在所有搜索路径中查找
    for (searchPath in this.searchPaths) {
        let packagePath = Path(searchPath).join(org).join(packageDir).toString()
        if (this.isPackageDirectory(packagePath)) {
            return Some(packagePath)
        }
    }

    None
}
```

---

### 修改 2：src/core/module_loader.cj

**位置**：ModulePathUtils 类，第 232-287 行

**新增静态函数**：
```cj
/**
 * 从包名提取目录名
 * 规则：去掉包名的最后一级
 *
 * @param packageName 包名（如："log.zlog"）
 * @return 目录名（如："log"）
 */
private static func extractPackageDir(packageName: String): String {
    let parts = packageName.split('.')

    if (parts.size <= 1) {
        packageName
    } else {
        // 去掉最后一级
        let dirParts = ArrayList<String>()
        for (i in 0..(parts.size - 1)) {
            dirParts.add(parts[i])
        }
        dirParts.join(".")
    }
}
```

**修改函数**：moduleToPath（第 240-261 行）

```cj
/**
 * 解析模块路径
 * 从模块名生成文件系统路径
 *
 * @param moduleName 模块名（如："ystyle::log.zlog"）
 * @param searchPath 搜索路径（如："~/.xisp/modules/"）
 * @return 文件系统路径（如："~/.xisp/modules/ystyle/log/"）
 */
public static func moduleToPath(moduleName: String, searchPath: String): String {
    // 解析模块名：org::package.subpkg
    let parts = moduleName.split("::")
    let (org, packageName) = if (parts.size == 2) {
        // 有组织前缀：ystyle::log.zlog
        (parts[0], parts[1])
    } else if (parts.size == 1) {
        // 无组织前缀（标准库）：io
        ("", parts[0])
    } else {
        // 格式错误
        ("", moduleName)
    }

    // 只取包名去掉最后一级作为目录
    let packageDir = extractPackageDir(packageName)

    if (org.isEmpty()) {
        // 标准库：searchPath/packageDir/
        Path(searchPath).join(packageDir).toString()
    } else {
        // 带组织前缀：searchPath/org/packageDir/
        Path(searchPath).join(org).join(packageDir).toString()
    }
}
```

---

### 修改 3：src/core/package_parser.cj

**是否需要修改**：否 ✅

当前实现已符合文档规范，不需要修改。

**确认点**：
- ✅ 正确解析 `(package log.zlog ...)` 格式
- ✅ 正确处理 `organization` 字段
- ✅ 包名不含组织前缀
- ✅ 支持在 package.lisp 中声明 export

---

## 测试用例

### 测试 1：两级包名
```lisp
(import ystyle::log.zlog)
```

**解析过程**：
1. `parseModuleName("ystyle::log.zlog")`
   - `org = "ystyle"`
   - `packageName = "log.zlog"`
2. `extractPackageDir("log.zlog")`
   - `parts = ["log", "zlog"]`
   - `dirParts = ["log"]`
   - `return "log"`
3. 构建路径
   - `~/.xisp/modules/ystyle/log/package.lisp` ✅

**使用**：
```lisp
(zlog.init "myapp")  ; 使用 zlog，不是 log
```

---

### 测试 2：单级包名
```lisp
(import io)
```

**解析过程**：
1. `parseModuleName("io")`
   - `org = ""`
   - `packageName = "io"`
2. `extractPackageDir("io")`
   - `parts = ["io"]`
   - `parts.size = 1`
   - `return "io"`
3. 构建路径
   - `~/.xisp/modules/io/package.lisp` ✅

**使用**：
```lisp
(io.read path)  ; 使用 io
```

---

### 测试 3：三级包名
```lisp
(import utils.string.parser)
```

**解析过程**：
1. `parseModuleName("utils.string.parser")`
   - `org = ""`
   - `packageName = "utils.string.parser"`
2. `extractPackageDir("utils.string.parser")`
   - `parts = ["utils", "string", "parser"]`
   - `dirParts = ["utils", "string"]`
   - `return "utils.string"`
3. 构建路径
   - `~/.xisp/modules/utils.string/package.lisp` ✅

**使用**：
```lisp
(parser.parse code)  ; 使用 parser，不是 string
```

---

### 测试 4：带组织的三级包名
```lisp
(import std.io.file)
```

**解析过程**：
1. `parseModuleName("std::io.file")`
   - `org = "std"`
   - `packageName = "io.file"`
2. `extractPackageDir("io.file")`
   - `parts = ["io", "file"]`
   - `dirParts = ["io"]`
   - `return "io"`
3. 构建路径
   - `~/.xisp/modules/std/io/package.lisp` ✅

**使用**：
```lisp
(file.read path)  ; 使用 file，不是 io
```

---

## 单元测试

建议添加以下单元测试：

### 测试 1：extractPackageDir 函数

```cj
@Test
func testExtractPackageDir() {
    let registry = ModuleRegistry()

    // 单级
    assert(registry.extractPackageDir("io") == "io")

    // 两级
    assert(registry.extractPackageDir("log.zlog") == "log")
    assert(registry.extractPackageDir("io.file") == "io")

    // 三级
    assert(registry.extractPackageDir("utils.string.parser") == "utils.string")

    // 空字符串
    assert(registry.extractPackageDir("") == "")
}
```

### 测试 2：resolveModulePath 函数

需要 mock 文件系统，测试路径解析逻辑。

### 测试 3：moduleToPath 函数

```cj
@Test
func testModuleToPath() {
    // 两级包名
    let path1 = ModulePathUtils.moduleToPath("ystyle::log.zlog", "~/.xisp/modules/")
    assert(path1.endsWith("/ystyle/log"))

    // 单级包名
    let path2 = ModulePathUtils.moduleToPath("io", "~/.xisp/modules/")
    assert(path2.endsWith("/io"))

    // 三级包名
    let path3 = ModulePathUtils.moduleToPath("utils.string.parser", "~/.xisp/modules/")
    assert(path3.endsWith("/utils.string"))
}
```

---

## 迁移指南

### 对于现有用户

如果已有使用旧目录结构的模块，需要重新组织：

#### 旧结构（错误）
```
~/.xisp/modules/ystyle/log.zlog/
├── package.lisp
└── core.lisp
```

#### 新结构（正确）
```
~/.xisp/modules/ystyle/log/
├── package.lisp
├── core.lisp
└── zlog/  ← 可选的子目录
    └── app.lisp
```

### package.lisp 无需修改

```lisp
(package log.zlog
  (version "0.2.0")
  (organization "ystyle"))
```

包名仍然是 `log.zlog`，只是目录名改为 `log`。

---

## 实现步骤

1. **修改 src/core/module.cj**
   - 添加 `extractPackageDir` 函数
   - 修改 `resolveModulePath` 函数

2. **修改 src/core/module_loader.cj**
   - 添加 `extractPackageDir` 静态函数
   - 修改 `moduleToPath` 函数

3. **添加单元测试**
   - 测试 `extractPackageDir` 函数
   - 测试路径解析逻辑

4. **更新文档**
   - 确认 docs/modules.md 与实现一致
   - 添加迁移指南（如果需要）

5. **手动测试**
   - 使用 examples/modules_demo 测试
   - 测试各种导入场景

---

## 风险评估

### 低风险
- ✅ 修改集中（只有 2 个文件）
- ✅ 逻辑清晰（去掉包名最后一级）
- ✅ 向后兼容（包名本身不变，只改目录映射）

### 注意事项
- ⚠️ 需要用户重新组织目录结构
- ⚠️ 需要更新文档说明目录规范
- ⚠️ 建议添加版本检查或兼容模式

---

## 总结

### 核心修改
添加 `extractPackageDir` 函数，从包名提取目录名时**去掉最后一级**。

### 影响范围
- `src/core/module.cj`：ModuleRegistry 类
- `src/core/module_loader.cj`：ModulePathUtils 类

### 修改难度
- ⭐⭐☆☆☆ 中等（逻辑清晰，但需要仔细测试）

### 优先级
- 🔴 高优先级（核心功能，影响所有模块导入）
