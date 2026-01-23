# Xisp 模块系统实现与文档差异分析

## 问题概述

当前实现将包名（如 `log.zlog`）当作完整路径使用，导致目录结构不正确。

### 文档规范
```
(import ystyle::log.zlog)  →  ~/.xisp/modules/ystyle/log/package.lisp
```

### 当前实现问题
```
(import ystyle::log.zlog)  →  ~/.xisp/modules/ystyle/log.zlog/package.lisp  ❌
```

---

## 核心差异分析

### 1. 目录映射规则

#### 文档规范
- **包名**：`log.zlog`（包含层级）
- **目录路径**：只取第一级 `log/`
- **完整路径**：`ystyle/log/package.lisp`

#### 当前实现（module.cj:335-350）
```cj
public func resolveModulePath(moduleName: String): Option<String> {
    let (org, packageName) = this.parseModuleName(moduleName)

    for (searchPath in this.searchPaths) {
        // ❌ 问题：将整个 packageName 当作目录名
        let packagePath = Path(searchPath).join(org).join(packageName).toString()
        // 对于 log.zlog，会生成：searchPath/ystyle/log.zlog/

        if (this.isPackageDirectory(packagePath)) {
            return Some(packagePath)
        }
    }
    None
}
```

#### 应该如何修改
```cj
public func resolveModulePath(moduleName: String): Option<String> {
    let (org, packageName) = this.parseModuleName(moduleName)

    // ✅ 正确：只取包名的第一级作为目录名
    let packageDir = this.extractPackageDir(packageName)  // log.zlog -> log

    for (searchPath in this.searchPaths) {
        let packagePath = Path(searchPath).join(org).join(packageDir).toString()
        // 对于 log.zlog，生成：searchPath/ystyle/log/

        if (this.isPackageDirectory(packagePath)) {
            return Some(packagePath)
        }
    }
    None
}
```

---

### 2. moduleToPath 函数（module_loader.cj:240-261）

#### 当前实现
```cj
public static func moduleToPath(moduleName: String, searchPath: String): String {
    let parts = moduleName.split("::")
    let (org, packageName) = if (parts.size == 2) {
        (parts[0], parts[1])  // ystyle::log.zlog -> ("ystyle", "log.zlog")
    } else {
        ("", parts[0])
    }

    if (org.isEmpty()) {
        // ❌ 问题：完整包名作为目录
        Path(searchPath).join(packageName).toString()
    } else {
        // ❌ 问题：完整包名作为目录
        Path(searchPath).join(org).join(packageName).toString()
    }
}
```

#### 应该如何修改
```cj
public static func moduleToPath(moduleName: String, searchPath: String): String {
    let parts = moduleName.split("::")
    let (org, packageName) = if (parts.size == 2) {
        (parts[0], parts[1])  // ystyle::log.zlog -> ("ystyle", "log.zlog")
    } else {
        ("", parts[0])
    }

    // ✅ 只取第一级包名作为目录
    let packageDir = extractPackageDir(packageName)  // log.zlog -> log

    if (org.isEmpty()) {
        Path(searchPath).join(packageDir).toString()
    } else {
        Path(searchPath).join(org).join(packageDir).toString()
    }
}
```

---

### 3. package.lisp 解析（package_parser.cj）

#### 当前实现（第 119-154 行）
```cj
private func parsePackageDeclaration(expr: ConsCell, packagePath: String): Option<PackageInfo> {
    match (packageName) {
        case Symbol(name) =>
            // 解析包名：org.name 或 name
            let parts = name.split('.')
            let (org, pkg) = if (parts.size >= 2) {
                (parts[0], name)  // ❌ 问题：完整包名
            } else {
                ("", name)
            }

            let packageInfo = PackageInfo(
                name,              // 完整包名
                org,               // 组织
                pkg,               // 也是完整包名
                packagePath
            )
    }
}
```

#### 文档规范
package.lisp 中的包名应该：
- **不含组织前缀**：`(package log.zlog ...)` 而不是 `(package ystyle.log.zlog ...)`
- **organization 字段单独指定**：`(organization "ystyle")`

#### 当前示例（std/math/package.lisp）
```lisp
(package std.math
  (version "1.0.0")
  (description "标准数学库"))
```
这个示例遵循了规范，但实现代码需要调整。

---

## 需要修改的地方

### 1. **module.cj - resolveModulePath 函数（第 335-350 行）**

**修改内容**：
- 添加 `extractPackageDir` 辅助函数
- 修改路径构建逻辑，只使用包名的第一级

**新增函数**：
```cj
/**
 * 从包名提取目录名（只取第一级）
 * 例如：log.zlog -> log, io -> io, utils.string.parser -> utils
 */
public func extractPackageDir(packageName: String): String {
    let parts = packageName.split('.')
    if (parts.size > 0) {
        parts[0]
    } else {
        packageName
    }
}
```

**修改后的 resolveModulePath**：
```cj
public func resolveModulePath(moduleName: String): Option<String> {
    let (org, packageName) = this.parseModuleName(moduleName)
    let packageDir = this.extractPackageDir(packageName)

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

### 2. **module_loader.cj - moduleToPath 函数（第 240-261 行）**

**修改内容**：
- 添加包名提取逻辑
- 修改路径构建

**修改后的代码**：
```cj
public static func moduleToPath(moduleName: String, searchPath: String): String {
    let parts = moduleName.split("::")
    let (org, packageName) = if (parts.size == 2) {
        (parts[0], parts[1])
    } else if (parts.size == 1) {
        ("", parts[0])
    } else {
        ("", moduleName)
    }

    // 只取第一级包名作为目录
    let packageDir = extractPackageDir(packageName)

    if (org.isEmpty()) {
        Path(searchPath).join(packageDir).toString()
    } else {
        Path(searchPath).join(org).join(packageDir).toString()
    }
}

private static func extractPackageDir(packageName: String): String {
    let parts = packageName.split('.')
    if (parts.size > 0) {
        parts[0]
    } else {
        packageName
    }
}
```

---

### 3. **package_parser.cj - 包信息解析（第 119-154 行）**

**需要调整**：
- 确认 package.lisp 中包名的语义
- 当前实现将完整包名（如 `log.zlog`）作为 packageName
- 这部分可能不需要改，但需要与文档对齐

**文档规范检查**：
- ✅ package.lisp 中的包名不含组织前缀（通过 `organization` 字段单独指定）
- ✅ 实现代码正确处理了这种情况

---

### 4. **PackageInfo 类的字段语义（module.cj:39-124）**

**当前字段**：
```cj
public class PackageInfo {
    public let name: String         // 完整包名
    public let org: String          // 组织名
    public let packageName: String  // 包名（不含组织）
    // ...
}
```

**文档规范**：
- `name`：完整包名（如 `log.zlog`）
- `org`：组织名（如 `ystyle`）
- `packageName`：与 `name` 相同

**当前实现**：符合规范 ✅

---

### 5. **getShortName 函数（module.cj:116-123）**

**当前实现**：
```cj
public func getShortName(): String {
    let parts = this.packageName.split('.')
    if (parts.size > 0) {
        parts[parts.size - 1]  // 返回最后一级
    } else {
        this.packageName
    }
}
```

**文档规范**：
- 包名 = 导入路径的最后一个组件
- `log.zlog` → `zlog` ✅
- `io` → `io` ✅

**当前实现**：符合规范 ✅

---

## 目录结构示例对比

### 文档规范
```
~/.xisp/modules/
└── ystyle/
    └── log/                      ← 目录名只取第一级
        ├── package.lisp          ← (package log.zlog ...)
        ├── core.lisp
        └── zlog/                ← 子目录，不是包路径
            ├── core.lisp
            └── file.lisp
```

### 导入映射
```lisp
(import ystyle::log.zlog)  →  ~/.xisp/modules/ystyle/log/package.lisp
```

### 当前实现（错误）
```
~/.xisp/modules/
└── ystyle/
    └── log.zlog/                ← ❌ 完整包名作为目录
        ├── package.lisp
        └── ...
```

---

## 修改优先级

### 高优先级（必须修改）
1. ✅ **module.cj:335-350** - `resolveModulePath` 函数
2. ✅ **module_loader.cj:240-261** - `moduleToPath` 函数

### 中优先级（建议调整）
3. **package_parser.cj** - 确认包名解析逻辑与文档一致
4. **PackageInfo 字段文档** - 添加注释说明字段语义

### 低优先级（可选）
5. **添加验证逻辑** - 确保 package.lisp 中的 organization 与导入路径的组织名匹配
6. **完善错误提示** - 当目录结构不符合规范时给出清晰的错误信息

---

## package.lisp 解析是否需要调整？

### 当前状态
- ✅ 正确解析 `(package log.zlog ...)` 格式
- ✅ 正确处理 `organization` 字段
- ✅ 包名不含组织前缀（符合文档）

### 是否需要修改？
**不需要**。当前解析逻辑已经符合文档规范。

但需要注意：
1. package.lisp 中应该**不包含 export 声明**（文档第 445-459 行示例中没有 export）
2. export 声明应该在各个 `.lisp` 文件中，而不是 package.lisp

**检查当前 package_parser.cj**：
- 第 100-105 行：处理 `export` 声明
- 文档示例（std/math/package.lisp）第 8 行：有 export

**结论**：
- package_parser.cj 支持在 package.lisp 中声明 export
- 文档没有明确禁止这种做法
- **建议**：保持当前实现，允许在 package.lisp 中声明 export

---

## 完整修改清单

### 文件 1：src/core/module.cj

**位置**：第 327-350 行

**修改**：
```cj
// 在 ModuleRegistry 类中添加新方法
/**
 * 从包名提取目录名（只取第一级）
 * 例如：log.zlog -> log, io -> io
 */
public func extractPackageDir(packageName: String): String {
    let parts = packageName.split('.')
    if (parts.size > 0) {
        parts[0]
    } else {
        packageName
    }
}

/**
 * 解析模块名到目录路径（修改版）
 */
public func resolveModulePath(moduleName: String): Option<String> {
    let (org, packageName) = this.parseModuleName(moduleName)
    let packageDir = this.extractPackageDir(packageName)  // ← 新增

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

### 文件 2：src/core/module_loader.cj

**位置**：第 232-261 行

**修改**：
```cj
// 在 ModulePathUtils 类中修改
/**
 * 解析模块路径（修改版）
 */
public static func moduleToPath(moduleName: String, searchPath: String): String {
    let parts = moduleName.split("::")
    let (org, packageName) = if (parts.size == 2) {
        (parts[0], parts[1])
    } else if (parts.size == 1) {
        ("", parts[0])
    } else {
        ("", moduleName)
    }

    // 只取第一级包名作为目录
    let packageDir = extractPackageDir(packageName)  // ← 新增

    if (org.isEmpty()) {
        Path(searchPath).join(packageDir).toString()
    } else {
        Path(searchPath).join(org).join(packageDir).toString()
    }
}

/**
 * 从包名提取目录名（新增）
 */
private static func extractPackageDir(packageName: String): String {
    let parts = packageName.split('.')
    if (parts.size > 0) {
        parts[0]
    } else {
        packageName
    }
}
```

---

### 文件 3：src/core/package_parser.cj

**是否需要修改**：不需要 ✅

当前实现已符合文档规范。

---

## 测试用例建议

修改完成后，需要测试以下场景：

### 测试 1：基本导入
```lisp
(import ystyle::log.zlog)
```
期望路径：`~/.xisp/modules/ystyle/log/package.lisp`

### 测试 2：标准库导入
```lisp
(import std.math)
```
期望路径：`~/.xisp/modules/std/math/package.lisp`

### 测试 3：多级包名
```lisp
(import org::utils.string.parser)
```
期望路径：`~/.xisp/modules/org/utils/package.lisp`

### 测试 4：无组织前缀
```lisp
(import io)
```
期望路径：`~/.xisp/modules/io/package.lisp`

---

## 总结

### 核心问题
当前实现将**完整包名**（如 `log.zlog`）当作目录名，而文档规范要求只使用**第一级包名**（如 `log`）作为目录。

### 解决方案
在路径解析时添加 `extractPackageDir` 函数，提取包名的第一级作为目录名。

### 影响范围
- ✅ **必须修改**：`module.cj` 和 `module_loader.cj` 中的路径解析逻辑
- ✅ **无需修改**：`package_parser.cj` 已符合规范
- ✅ **语义清晰**：`PackageInfo` 类字段设计合理

### 向后兼容性
此修改会**改变目录结构**，需要用户重新组织模块目录。

建议：
1. 在文档中明确说明目录结构规范
2. 提供迁移指南
3. 考虑添加兼容模式（可选）
