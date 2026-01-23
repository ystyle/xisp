# 模块系统演示

本目录演示了 Xisp 模块系统的功能，包括相对路径导入。

## 目录结构

```
modules_demo/
├── demo.lisp          # 顶层演示文件
├── pkg1/              # 包 1
│   ├── package.lisp   # 包元数据
│   └── utils.lisp     # 工具函数
└── pkg2/              # 包 2
    ├── package.lisp   # 包元数据
    └── main.lisp      # 主文件（导入 pkg1）
```

## 相对路径导入语法

### 1. 当前目录导入

```lisp
(import "./local.module")
```

从当前文件所在目录导入 `local.module` 模块。

### 2. 上级目录导入

```lisp
(import "../parent.module")
```

从当前文件所在目录的上级目录导入 `parent.module` 模块。

### 3. 完整模块名导入

```lisp
(import demo::pkg1)
(import demo::pkg2)
```

使用完整的模块名（组织::包名）导入模块。

## 工作原理

### 路径解析

1. **当前目录 (`./`)**: 从当前文件所在目录开始查找
2. **上级目录 (`../`)**: 从当前文件所在目录的上级目录开始查找
3. **模块名解析**: 相对路径会被转换为绝对路径，然后查找 `package.lisp`

### 示例

假设当前文件是 `modules_demo/pkg2/main.lisp`:

```lisp
;; 导入同级目录的 pkg1
(import "../pkg1")

;; 等同于完整模块名导入
(import demo::pkg1)
```

## 限制

1. 相对路径导入需要设置 `currentFilePath`（由 ModuleLoader 自动设置）
2. 在 REPL 环境中，相对路径导入可能不可用（因为没有文件上下文）
3. 相对路径必须指向包含 `package.lisp` 的目录

## 运行演示

```bash
# 运行顶层演示文件
./target/release/bin/ystyle::xisp.cli examples/modules_demo/demo.lisp

# 或使用 cjpm
cjpm run --name ystyle::xisp.cli --run-args examples/modules_demo/demo.lisp
```

## 实现细节

### 添加的字段

- `Evaluator.currentFilePath: Option<String>` - 跟踪当前执行的文件路径

### 新增方法

- `importRelativeModule(relativePath: String)` - 处理相对路径导入
- `resolveRelativePath(baseDir: String, relativePath: String)` - 解析相对路径为绝对路径
- `pathToModuleName(path: String)` - 将文件路径转换为模块名
- `findPackageDirectory(path: String)` - 向上查找包含 package.lisp 的目录
- `packagePathToModuleName(packagePath: String)` - 将包路径转换为模块名
- `findPackageByPath(path: String)` - 根据路径查找对应的包

## 测试

相对路径导入功能有 5 个单元测试覆盖：

1. `testRelativeImportCurrentDir` - 测试当前目录导入
2. `testRelativeImportParentDir` - 测试上级目录导入
3. `testFindPackageDirectory` - 测试 package.lisp 查找
4. `testPackagePathToModuleName` - 测试路径到模块名转换
5. `testRelativeImportSyntax` - 测试语法解析

运行测试：

```bash
cjpm test --filter 'ModuleTest.testRelativeImport'
```

## 相关文档

- [模块系统文档](../../docs/modules.md)
- [包结构说明](../../docs/modules.md#包结构)
- [导入语法](../../docs/modules.md#导入语法)
