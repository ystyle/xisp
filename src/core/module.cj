package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList
import std.collection.HashMap
import std.fs.*

/**
 * 模块系统核心
 *
 * 实现包管理、模块加载、命名空间隔离等功能
 */

/**
 * 依赖项
 * 表示一个包对另一个包的依赖关系
 */
public class Dependency {
    /**
     * 包名（完整命名空间，如：ystyle.log.zlog）
     */
    public let packageName: String

    /**
     * 版本要求（如："0.2.0"、"latest"、"^1.0.0"）
     */
    public let versionRequirement: String

    public init(packageName: String, versionRequirement: String) {
        this.packageName = packageName
        this.versionRequirement = versionRequirement
    }
}

/**
 * 包元数据
 * 从 package.lisp 文件解析而来
 */
public class PackageInfo {
    /**
     * 完整包名（包含组织名，如：ystyle.log.zlog）
     */
    public let name: String

    /**
     * 组织名（如：ystyle）
     */
    public let org: String

    /**
     * 包名（不含组织名，如：log.zlog）
     */
    public let packageName: String

    /**
     * 版本号
     */
    public var version: String

    /**
     * 描述
     */
    public var description: String

    /**
     * 作者
     */
    public var author: String

    /**
     * 主页
     */
    public var homepage: String

    /**
     * 许可证
     */
    public var license: String

    /**
     * 依赖列表
     */
    public var dependencies: ArrayList<Dependency>

    /**
     * 导出的符号列表
     */
    public var exports: ArrayList<String>

    /**
     * 包目录路径
     */
    public var directory: String

    /**
     * 构造函数
     */
    public init(name: String, org: String, packageName: String, directory: String) {
        this.name = name
        this.org = org
        this.packageName = packageName
        this.version = "0.1.0"
        this.description = ""
        this.author = ""
        this.homepage = ""
        this.license = ""
        this.dependencies = ArrayList<Dependency>()
        this.exports = ArrayList<String>()
        this.directory = directory
    }

    /**
     * 获取包的简短名称（路径的最后一个组件）
     * 例如：log.zlog -> zlog, io -> io
     */
    public func getShortName(): String {
        let parts = this.packageName.split('.')
        if (parts.size > 0) {
            parts[parts.size - 1]
        } else {
            this.packageName
        }
    }
}

/**
 * 模块
 * 表示一个已加载到内存中的模块
 */
public class Module {
    /**
     * 模块名（完整命名空间，如：ystyle::log.zlog）
     */
    public let name: String

    /**
     * 包信息
     */
    public let packageInfo: PackageInfo

    /**
     * 模块环境（包含所有绑定的符号）
     */
    public var environment: Environment

    /**
     * 是否已初始化
     */
    public var isInitialized: Bool

    /**
     * 构造函数
     */
    public init(name: String, packageInfo: PackageInfo, environment: Environment) {
        this.name = name
        this.packageInfo = packageInfo
        this.environment = environment
        this.isInitialized = false
    }

    /**
     * 导出一个符号
     * 将符号添加到导出列表
     */
    public func exportSymbol(symbolName: String) {
        if (!this.packageInfo.exports.contains(symbolName)) {
            this.packageInfo.exports.add(symbolName)
        }
    }

    /**
     * 批量导出符号
     */
    public func exportSymbols(symbolNames: ArrayList<String>) {
        for (name in symbolNames) {
            this.exportSymbol(name)
        }
    }

    /**
     * 检查符号是否已导出
     */
    public func isExported(symbolName: String): Bool {
        this.packageInfo.exports.contains(symbolName)
    }

    /**
     * 获取导出的符号
     */
    public func getExportedSymbol(symbolName: String): Option<LispValue> {
        if (this.isExported(symbolName)) {
            let value = this.environment.lookup(symbolName)
            match (value) {
                case Nil => None
                case _ => Some(value)
            }
        } else {
            None
        }
    }

    /**
     * 获取所有导出的符号
     */
    public func getAllExportedSymbols(): ArrayList<String> {
        this.packageInfo.exports
    }
}

/**
 * 模块注册表
 * 管理所有已加载的模块和模块搜索路径
 */
public class ModuleRegistry {
    /**
     * 已加载的模块表
     * key: 完整模块名（如："ystyle::log.zlog"）
     * value: Module 实例
     */
    private var modules: HashMap<String, Module>

    /**
     * 模块搜索路径
     */
    private var searchPaths: ArrayList<String>

    /**
     * 当前项目的 package.lisp 路径（如果有）
     * 用于解析项目中的依赖版本
     */
    private var projectPackagePath: Option<String>

    /**
     * 全局模块目录
     */
    public static let GLOBAL_MODULES_DIR = "~/.xisp/modules/"

    /**
     * 构造函数
     */
    public init() {
        this.modules = HashMap<String, Module>()
        this.searchPaths = ArrayList<String>()
        this.projectPackagePath = None

        // 添加默认搜索路径
        this.addDefaultSearchPaths()
    }

    /**
     * 添加默认搜索路径
     */
    private func addDefaultSearchPaths() {
        // 添加全局模块目录
        // TODO: 获取用户主目录需要环境变量支持
        let globalModulesPath = Path("./.xisp/modules").toString()
        this.searchPaths.add(globalModulesPath)

        // 添加当前工作目录
        let currentDir = "."
        this.searchPaths.add(currentDir)
    }

    /**
     * 添加模块搜索路径
     */
    public func addSearchPath(path: String) {
        if (!this.searchPaths.contains(path)) {
            this.searchPaths.add(path)
        }
    }

    /**
     * 设置项目 package.lisp 路径
     * 用于解析项目依赖的版本
     */
    public func setProjectPackagePath(path: String) {
        this.projectPackagePath = Some(path)
    }

    /**
     * 注册模块
     */
    public func registerModule(module: Module) {
        this.modules[module.name] = module
    }

    /**
     * 查找模块
     */
    public func findModule(moduleName: String): Option<Module> {
        if (this.modules.contains(moduleName)) {
            match (this.modules.get(moduleName)) {
                case Some(m) => Some(m)
                case None => None
            }
        } else {
            None
        }
    }

    /**
     * 检查模块是否已加载
     */
    public func isModuleLoaded(moduleName: String): Bool {
        this.modules.contains(moduleName)
    }

    /**
     * 获取所有已加载的模块名
     */
    public func getLoadedModuleNames(): ArrayList<String> {
        let names = ArrayList<String>()
        for ((name, _) in this.modules) {
            names.add(name)
        }
        names
    }

    /**
     * 获取所有搜索路径
     */
    public func getSearchPaths(): ArrayList<String> {
        this.searchPaths
    }

    /**
     * 解析模块名到目录路径
     * 模块名格式：org::package.subpackage
     * 路径格式：searchPath/org/package.subpackage/
     *
     * @param moduleName 模块名（如："ystyle::log.zlog"）
     * @return 找到的包目录路径，如果找不到返回 None
     */
    public func resolveModulePath(moduleName: String): Option<String> {
        let (org, packageName) = this.parseModuleName(moduleName)

        // 在所有搜索路径中查找
        for (searchPath in this.searchPaths) {
            let packagePath = Path(searchPath).join(org).join(packageName).toString()
            if (this.isPackageDirectory(packagePath)) {
                return Some(packagePath)
            }

            // 尝试查找版本化的目录（如：log.zlog@0.2.0）
            // 这需要额外的版本管理逻辑，暂时跳过
        }

        None
    }

    /**
     * 解析模块名
     * 输入：org::package.subpkg
     * 输出：(org, package.subpkg)
     *
     * 如果没有 org 部分（标准库），org 返回空字符串
     */
    public func parseModuleName(moduleName: String): (String, String) {
        let parts = moduleName.split("::")

        if (parts.size == 2) {
            // 有组织前缀：ystyle::log.zlog
            (parts[0], parts[1])
        } else if (parts.size == 1) {
            // 无组织前缀（标准库）：io
            ("", parts[0])
        } else {
            // 格式错误
            ("", moduleName)
        }
    }

    /**
     * 检查路径是否是一个有效的包目录
     * 有效包目录必须包含 package.lisp 文件
     */
    private func isPackageDirectory(path: String): Bool {
        let packageFile = Path(path).join("package.lisp").toString()
        exists(packageFile)
    }

    /**
     * 列出指定目录下的所有包
     */
    public func listPackagesInDirectory(directory: String): ArrayList<String> {
        let packages = ArrayList<String>()

        if (exists(directory)) {
            let pathObj = Path(directory)
            let fileInfo = FileInfo(pathObj)

            if (fileInfo.isDirectory()) {
                // 列出目录内容
                // 注意：仓颉的文件系统 API 可能需要调整
                // 这里暂时返回空列表，实际实现需要遍历目录
            }
        }

        packages
    }

    /**
     * 清空所有已加载的模块
     */
    public func clear() {
        this.modules.clear()
    }

    /**
     * 卸载模块
     */
    public func unloadModule(moduleName: String): Bool {
        if (this.modules.contains(moduleName)) {
            this.modules.remove(moduleName)
            true
        } else {
            false
        }
    }
}

/**
 * 模块命名空间工具类
 * 用于处理模块名、导入等命名空间相关操作
 */
public class ModuleNamespace {
    /**
     * 构造完整模块名
     * @param org 组织名（可为空）
     * @param packageName 包名
     * @return 完整模块名（如："ystyle::log.zlog" 或 "io"）
     */
    public static func buildModuleName(org: String, packageName: String): String {
        if (org.isEmpty()) {
            packageName
        } else {
            "${org}::${packageName}"
        }
    }

    /**
     * 从模块名提取包名（最后一个组件）
     * 例如：log.zlog -> zlog, io -> io
     */
    public static func extractPackageSimpleName(packageName: String): String {
        let parts = packageName.split('.')
        if (parts.size > 0) {
            parts[parts.size - 1]
        } else {
            packageName
        }
    }

    /**
     * 验证模块名格式
     * 有效格式：
     * - io
     * - std.io
     * - org::package
     * - org::package.subpackage
     */
    public static func isValidModuleName(moduleName: String): Bool {
        if (moduleName.isEmpty()) {
            return false
        }

        // 检查是否包含 ::
        let parts = moduleName.split("::")

        if (parts.size > 2) {
            return false  // 最多一个 ::
        }

        if (parts.size == 2) {
            // 有组织前缀：检查两部分都不为空
            if (parts[0].isEmpty() || parts[1].isEmpty()) {
                return false
            }
            // 检查包名部分格式（允许 . 分隔）
            return isValidPackageName(parts[1])
        } else {
            // 无组织前缀
            return isValidPackageName(moduleName)
        }
    }

    /**
     * 验证包名格式
     * 允许字母、数字、下划线、点号
     */
    private static func isValidPackageName(packageName: String): Bool {
        if (packageName.isEmpty()) {
            return false
        }

        // 简单验证：不允许空字符串，不允许包含特殊字符
        // 实际实现可能需要更严格的正则表达式
        let invalidChars = ArrayList<String>(["::", ":", "'", "\"", " ", "\t", "\n"])
        for (ch in invalidChars) {
            if (packageName.contains(ch)) {
                return false
            }
        }

        true
    }

    /**
     * 规范化模块路径
     * 将 ./ 或 ../ 转换为绝对路径
     */
    public static func normalizeModulePath(relativePath: String, basePath: String): String {
        if (relativePath.startsWith("./") || relativePath.startsWith("../")) {
            let path = Path(basePath).join(relativePath).toString()
            canonicalize(path).toString()
        } else {
            relativePath
        }
    }
}
