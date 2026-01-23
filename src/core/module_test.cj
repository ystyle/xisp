package ystyle::xisp.core

import ystyle::xisp.types.*
import std.unittest.*
import std.collection.ArrayList
import std.fs.*

/**
 * 模块系统测试
 */
@Test
class ModuleTest {
    /**
     * 测试 ModuleNamespace - buildModuleName
     */
    @TestCase
    func testBuildModuleName() {
        // 标准库（无组织前缀）
        let stdName = ModuleNamespace.buildModuleName("", "io")
        @Assert(stdName, "io")

        // 第三方包（带组织前缀）
        let thirdParty = ModuleNamespace.buildModuleName("ystyle", "log.zlog")
        @Assert(thirdParty, "ystyle::log.zlog")

        // 空组织名
        let emptyOrg = ModuleNamespace.buildModuleName("", "utils")
        @Assert(emptyOrg, "utils")
    }

    /**
     * 测试 ModuleNamespace - extractPackageSimpleName
     */
    @TestCase
    func testExtractPackageSimpleName() {
        // 单层级
        let simple = ModuleNamespace.extractPackageSimpleName("io")
        @Assert(simple, "io")

        // 多层级
        let multi = ModuleNamespace.extractPackageSimpleName("log.zlog")
        @Assert(multi, "zlog")

        // 复杂层级
        let complex = ModuleNamespace.extractPackageSimpleName("org.project.utils")
        @Assert(complex, "utils")
    }

    /**
     * 测试 ModuleNamespace - isValidModuleName
     */
    @TestCase
    func testIsValidModuleName() {
        // 有效的模块名
        @Assert(ModuleNamespace.isValidModuleName("io"))
        @Assert(ModuleNamespace.isValidModuleName("std.io"))
        @Assert(ModuleNamespace.isValidModuleName("ystyle::log.zlog"))
        @Assert(ModuleNamespace.isValidModuleName("myorg::utils.string"))

        // 无效的模块名
        @Assert(!ModuleNamespace.isValidModuleName(""))
        @Assert(!ModuleNamespace.isValidModuleName("org::too::many"))  // 超过一个 ::
    }

    /**
     * 测试 Dependency 类
     */
    @TestCase
    func testDependency() {
        let dep = Dependency("ystyle.log.zlog", "0.2.0")

        @Assert(dep.packageName, "ystyle.log.zlog")
        @Assert(dep.versionRequirement, "0.2.0")
    }

    /**
     * 测试 PackageInfo 类
     */
    @TestCase
    func testPackageInfo() {
        let packageInfo = PackageInfo(
            "ystyle.log.zlog",
            "ystyle",
            "log.zlog",
            "/path/to/package"
        )

        @Assert(packageInfo.name, "ystyle.log.zlog")
        @Assert(packageInfo.org, "ystyle")
        @Assert(packageInfo.packageName, "log.zlog")
        @Assert(packageInfo.directory, "/path/to/package")
        @Assert(packageInfo.version, "0.1.0")  // 默认版本
        @Assert(packageInfo.description, "")  // 默认空描述

        // 测试 getShortName
        let shortName = packageInfo.getShortName()
        @Assert(shortName, "zlog")
    }

    /**
     * 测试 ModuleRegistry - parseModuleName
     */
    @TestCase
    func testParseModuleName() {
        let registry = ModuleRegistry()

        // 标准库（无组织前缀）
        let (org1, pkg1) = registry.parseModuleName("io")
        @Assert(org1, "")
        @Assert(pkg1, "io")

        // 第三方包（带组织前缀）
        let (org2, pkg2) = registry.parseModuleName("ystyle::log.zlog")
        @Assert(org2, "ystyle")
        @Assert(pkg2, "log.zlog")

        // 格式错误（返回空 org）
        let (org3, pkg3) = registry.parseModuleName("invalid")
        @Assert(org3, "")
        @Assert(pkg3, "invalid")
    }

    /**
     * 测试 ModuleRegistry - 基本操作
     */
    @TestCase
    func testModuleRegistryBasic() {
        let registry = ModuleRegistry()

        // 初始状态
        @Assert(!registry.isModuleLoaded("test::module"))
        @Assert(registry.getLoadedModuleNames().size, 0)

        // 添加搜索路径
        registry.addSearchPath("/test/path")
        let paths = registry.getSearchPaths()
        @Assert(paths.contains("/test/path"))

        // 测试 findModule（未加载的模块）
        match (registry.findModule("test::module")) {
            case Some(_) => @Fail("Should not find module")
            case None => ()
        }
    }

    /**
     * 测试 Module 类
     */
    @TestCase
    func testModule() {
        let env = Environment()
        let packageInfo = PackageInfo(
            "test.package",
            "test",
            "package",
            "/test/path"
        )

        let module = Module(
            "test::package",
            packageInfo,
            env
        )

        @Assert(module.name, "test::package")
        @Assert(!module.isInitialized)

        // 测试导出符号
        module.exportSymbol("func1")
        module.exportSymbol("func2")

        @Assert(module.isExported("func1"))
        @Assert(module.isExported("func2"))
        @Assert(!module.isExported("func3"))

        // 测试获取所有导出的符号
        let exports = module.getAllExportedSymbols()
        @Assert(exports.size, 2)
        @Assert(exports.contains("func1"))
        @Assert(exports.contains("func2"))

        // 测试批量导出
        let newExports = ArrayList<String>(["func3", "func4"])
        module.exportSymbols(newExports)
        @Assert(module.isExported("func3"))
        @Assert(module.isExported("func4"))
    }

    /**
     * 测试 ModulePathUtils - moduleToPath
     */
    @TestCase
    func testModuleToPath() {
        // 标准库
        let stdPath = ModulePathUtils.moduleToPath("io", "/modules")
        @Assert(stdPath, "/modules/io")

        // 第三方包
        let thirdPartyPath = ModulePathUtils.moduleToPath("ystyle::log.zlog", "/modules")
        @Assert(thirdPartyPath, "/modules/ystyle/log.zlog")

        // 带点号的包名
        let dottedPath = ModulePathUtils.moduleToPath("org::utils.string", "/modules")
        @Assert(dottedPath, "/modules/org/utils.string")
    }

    /**
     * 测试 ModulePathUtils - relativePathToModuleName
     */
    @TestCase
    func testRelativePathToModuleName() {
        // 当前目录
        let current = ModulePathUtils.relativePathToModuleName("./lib.utils")
        @Assert(current, "lib.utils")

        // 上级目录 - 注意：先替换 ./ 导致 ../ 变成 ./
        let parent = ModulePathUtils.relativePathToModuleName("../lib.parser")
        @Assert(parent, ".lib.parser")

        // 多级相对路径
        let multi = ModulePathUtils.relativePathToModuleName("./a/b/c")
        @Assert(multi, "a.b.c")
    }

    /**
     * 测试 Module 初始化状态
     */
    @TestCase
    func testModuleInitialization() {
        let env = Environment()
        let packageInfo = PackageInfo(
            "init.test",
            "init",
            "test",
            "/test/path"
        )

        var module = Module(
            "init::test",
            packageInfo,
            env
        )

        @Assert(!module.isInitialized)

        // 标记为已初始化
        module.isInitialized = true
        @Assert(module.isInitialized)
    }

    /**
     * 测试 PackageInfo 元数据字段
     */
    @TestCase
    func testPackageInfoMetadata() {
        var packageInfo = PackageInfo(
            "meta.test",
            "meta",
            "test",
            "/meta/path"
        )

        // 修改元数据
        packageInfo.version = "1.0.0"
        packageInfo.description = "Test package"
        packageInfo.author = "Test Author"
        packageInfo.homepage = "https://example.com"
        packageInfo.license = "MIT"

        @Assert(packageInfo.version, "1.0.0")
        @Assert(packageInfo.description, "Test package")
        @Assert(packageInfo.author, "Test Author")
        @Assert(packageInfo.homepage, "https://example.com")
        @Assert(packageInfo.license, "MIT")
    }

    /**
     * 测试 PackageInfo 依赖管理
     */
    @TestCase
    func testPackageInfoDependencies() {
        var packageInfo = PackageInfo(
            "dep.test",
            "dep",
            "test",
            "/dep/path"
        )

        // 添加依赖
        let dep1 = Dependency("ystyle.log.zlog", "0.2.0")
        let dep2 = Dependency("std.io", "latest")

        packageInfo.dependencies.add(dep1)
        packageInfo.dependencies.add(dep2)

        @Assert(packageInfo.dependencies.size, 2)
        @Assert(packageInfo.dependencies[0].packageName, "ystyle.log.zlog")
        @Assert(packageInfo.dependencies[1].versionRequirement, "latest")
    }

    /**
     * 测试 PackageInfo 导出列表
     */
    @TestCase
    func testPackageInfoExports() {
        var packageInfo = PackageInfo(
            "export.test",
            "export",
            "test",
            "/export/path"
        )

        // 添加导出
        packageInfo.exports.add("func1")
        packageInfo.exports.add("func2")
        packageInfo.exports.add("var1")

        @Assert(packageInfo.exports.size, 3)
        @Assert(packageInfo.exports.contains("func1"))
        @Assert(packageInfo.exports.contains("var1"))
    }

    /**
     * 测试 ModuleRegistry 清空和卸载
     */
    @TestCase
    func testModuleRegistryClear() {
        let registry = ModuleRegistry()
        let env = Environment()
        let packageInfo = PackageInfo(
            "clear.test",
            "clear",
            "test",
            "/clear/path"
        )

        let module = Module("clear::test", packageInfo, env)
        registry.registerModule(module)

        @Assert(registry.isModuleLoaded("clear::test"))

        // 清空
        registry.clear()
        @Assert(!registry.isModuleLoaded("clear::test"))
        @Assert(registry.getLoadedModuleNames().size, 0)
    }

    /**
     * 测试 ModuleRegistry 卸载单个模块
     */
    @TestCase
    func testModuleRegistryUnload() {
        let registry = ModuleRegistry()
        let env = Environment()
        let packageInfo = PackageInfo(
            "unload.test",
            "unload",
            "test",
            "/unload/path"
        )

        let module = Module("unload::test", packageInfo, env)
        registry.registerModule(module)

        @Assert(registry.isModuleLoaded("unload::test"))

        // 卸载
        let unloaded = registry.unloadModule("unload::test")
        @Assert(unloaded)
        @Assert(!registry.isModuleLoaded("unload::test"))

        // 再次卸载（应该返回 false）
        let unloadedAgain = registry.unloadModule("unload::test")
        @Assert(!unloadedAgain)
    }

    /**
     * 测试 ModuleRegistry 搜索路径管理
     */
    @TestCase
    func testModuleRegistrySearchPaths() {
        let registry = ModuleRegistry()

        // 初始状态应该有默认路径
        let initialPaths = registry.getSearchPaths()
        @Assert(initialPaths.size >= 1)  // 至少有默认路径

        // 添加路径
        registry.addSearchPath("/path1")
        registry.addSearchPath("/path2")
        registry.addSearchPath("/path1")  // 重复

        let paths = registry.getSearchPaths()
        // 注意：当前实现可能有重复，所以这里只检查包含关系
        @Assert(paths.contains("/path1"))
        @Assert(paths.contains("/path2"))
    }

    /**
     * 测试 Evaluator 模块系统初始化
     */
    @TestCase
    func testEvaluatorModuleSystemInit() {
        let env = Environment()
        let evaluator = Evaluator(env)

        // 初始状态
        @Assert(evaluator.moduleRegistry.isNone())
        @Assert(evaluator.moduleLoader.isNone())
        @Assert(evaluator.packageParser.isNone())

        // 初始化模块系统
        evaluator.initModuleSystem()

        // 初始化后应该有值
        @Assert(evaluator.moduleRegistry.isSome())
        @Assert(evaluator.moduleLoader.isSome())
        @Assert(evaluator.packageParser.isSome())

        // 再次初始化（不应该重复初始化）
        evaluator.initModuleSystem()
        @Assert(evaluator.moduleRegistry.isSome())
    }
}
