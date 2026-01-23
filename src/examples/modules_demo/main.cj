package ystyle::xisp.examples.modules_demo

import ystyle::xisp.core.*
import ystyle::xisp.types.*
import std.collection.ArrayList
import std.io.*

/**
 * 模块系统演示
 *
 * 展示模块系统的核心功能：
 * 1. 模块命名空间管理
 * 2. 包信息管理
 * 3. 模块注册和查找
 * 4. 路径解析
 */
main() {
    println("=== Xisp 模块系统演示 ===\n")

    // 1. 模块命名空间演示
    demonstrateModuleNamespace()

    // 2. 包信息管理演示
    demonstratePackageInfo()

    // 3. 模块注册表演示
    demonstrateModuleRegistry()

    // 4. 路径工具演示
    demonstratePathUtils()

    // 5. 完整工作流演示
    demonstrateCompleteWorkflow()

    println("\n=== 演示完成 ===")
    0
}

/**
 * 演示模块命名空间功能
 */
func demonstrateModuleNamespace() {
    println("--- 1. 模块命名空间 ---")

    // 构建模块名
    let stdName = ModuleNamespace.buildModuleName("", "io")
    println("标准库模块名: ${stdName}")

    let thirdPartyName = ModuleNamespace.buildModuleName("ystyle", "log.zlog")
    println("第三方包模块名: ${thirdPartyName}")

    // 提取包简名
    let simpleName = ModuleNamespace.extractPackageSimpleName("log.zlog")
    println("包简名: ${simpleName}")

    // 验证模块名
    let valid1 = ModuleNamespace.isValidModuleName("ystyle::log.zlog")
    let valid2 = ModuleNamespace.isValidModuleName("io")
    let invalid = ModuleNamespace.isValidModuleName("org::too::many")
    println("'ystyle::log.zlog' 有效: ${valid1}")
    println("'io' 有效: ${valid2}")
    println("'org::too::many' 有效: ${invalid}")

    println()
}

/**
 * 演示包信息管理
 */
func demonstratePackageInfo() {
    println("--- 2. 包信息管理 ---")

    // 创建包信息
    var packageInfo = PackageInfo(
        "ystyle.log.zlog",
        "ystyle",
        "log.zlog",
        "/path/to/log.zlog"
    )

    println("包名: ${packageInfo.name}")
    println("组织: ${packageInfo.org}")
    println("包名（不含组织）: ${packageInfo.packageName}")
    println("目录: ${packageInfo.directory}")

    // 设置元数据
    packageInfo.version = "0.2.0"
    packageInfo.description = "Logging library for Xisp"
    packageInfo.author = "ystyle"
    packageInfo.license = "MIT"

    println("版本: ${packageInfo.version}")
    println("描述: ${packageInfo.description}")
    println("作者: ${packageInfo.author}")
    println("许可证: ${packageInfo.license}")

    // 添加依赖
    let dep1 = Dependency("std.io", "latest")
    let dep2 = Dependency("std.time", "1.0.0")
    packageInfo.dependencies.add(dep1)
    packageInfo.dependencies.add(dep2)

    println("依赖数量: ${packageInfo.dependencies.size}")
    for (dep in packageInfo.dependencies) {
        println("  - ${dep.packageName} (${dep.versionRequirement})")
    }

    // 导出符号
    packageInfo.exports.add("init")
    packageInfo.exports.add("write")
    packageInfo.exports.add("close")

    println("导出符号: ${packageInfo.exports.size} 个")

    println()
}

/**
 * 演示模块注册表
 */
func demonstrateModuleRegistry() {
    println("--- 3. 模块注册表 ---")

    let registry = ModuleRegistry()

    // 解析模块名
    let (org, pkg) = registry.parseModuleName("ystyle::log.zlog")
    println("解析 'ystyle::log.zlog':")
    println("  组织: ${org}")
    println("  包名: ${pkg}")

    // 创建并注册模块
    let env = Environment()
    var packageInfo = PackageInfo(
        "demo.math",
        "demo",
        "math",
        "/demo/math"
    )
    packageInfo.version = "1.0.0"
    packageInfo.exports.add("add")
    packageInfo.exports.add("subtract")

    let module = Module("demo::math", packageInfo, env)
    module.isInitialized = true

    registry.registerModule(module)

    println("\n已注册模块:")
    let names = registry.getLoadedModuleNames()
    for (name in names) {
        println("  - ${name}")
    }

    // 查找模块
    match (registry.findModule("demo::math")) {
        case Some(foundModule) =>
            println("\n找到模块: ${foundModule.name}")
            println("  版本: ${foundModule.packageInfo.version}")
            println("  导出: ${foundModule.getAllExportedSymbols().size} 个符号")
        case None => println("未找到模块")
    }

    println()
}

/**
 * 演示路径工具
 */
func demonstratePathUtils() {
    println("--- 4. 路径工具 ---")

    // 模块名转路径
    let path1 = ModulePathUtils.moduleToPath("io", "/modules")
    println("标准库路径: ${path1}")

    let path2 = ModulePathUtils.moduleToPath("ystyle::log.zlog", "/modules")
    println("第三方包路径: ${path2}")

    // 相对路径转模块名
    let module1 = ModulePathUtils.relativePathToModuleName("./lib.utils")
    println("相对路径 './lib.utils' -> 模块名 '${module1}'")

    let module2 = ModulePathUtils.relativePathToModuleName("../lib.parser")
    println("相对路径 '../lib.parser' -> 模块名 '${module2}'")

    println()
}

/**
 * 演示完整工作流
 */
func demonstrateCompleteWorkflow() {
    println("--- 5. 完整工作流演示 ---")

    // 创建求值器
    let env = Environment()
    let evaluator = Evaluator(env)

    // 初始化模块系统
    evaluator.initModuleSystem()
    println("求值器模块系统已初始化")

    // 获取模块注册表
    match (evaluator.moduleRegistry) {
        case Some(registry) =>
            println("模块注册表可用")

            // 添加搜索路径
            registry.addSearchPath("./modules")
            registry.addSearchPath("~/.xisp/modules")
            println("已添加 ${registry.getSearchPaths().size} 个搜索路径")
        case None => println("模块注册表不可用")
    }

    // 创建一个示例模块
    match (evaluator.moduleRegistry) {
        case Some(registry) =>
            var pkgInfo = PackageInfo(
                "example.utils",
                "example",
                "utils",
                "./example/utils"
            )
            pkgInfo.version = "1.0.0"
            pkgInfo.description = "Example utility functions"

            let moduleEnv = Environment()
            var module = Module("example::utils", pkgInfo, moduleEnv)

            // 导出一些符号
            module.exportSymbol("function1")
            module.exportSymbol("function2")
            module.isInitialized = true

            // 注册模块
            registry.registerModule(module)

            println("\n示例模块已创建:")
            println("  名称: ${module.name}")
            println("  版本: ${module.packageInfo.version}")
            println("  导出符号: ${module.getAllExportedSymbols().size} 个")
            println("  已初始化: ${module.isInitialized}")
        case None => ()
    }

    println()
}
