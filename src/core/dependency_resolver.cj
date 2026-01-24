package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList
import std.collection.HashMap
import std.convert.*
import std.fs.*

/**
 * 依赖解析器
 * 负责解析模块依赖、管理版本、处理依赖传递
 */
public class DependencyResolver {
    private var moduleRegistry: ModuleRegistry
    private var moduleParser: ModuleParser

    /**
     * 版本缓存
     * key: 模块名（如：ystyle::log）
     * value: 可用版本列表
     */
    private var versionCache: HashMap<String, ArrayList<String>>

    /**
     * 依赖解析缓存
     * key: 项目根路径
     * value: 项目依赖的版本映射
     */
    private var dependencyCache: HashMap<String, HashMap<String, String>>

    public init(moduleRegistry: ModuleRegistry, moduleParser: ModuleParser) {
        this.moduleRegistry = moduleRegistry
        this.moduleParser = moduleParser
        this.versionCache = HashMap<String, ArrayList<String>>()
        this.dependencyCache = HashMap<String, HashMap<String, String>>()
    }

    /**
     * 解析模块导入，确定使用的版本
     * @param moduleName 模块名（如："ystyle::log"）
     * @param projectPath 项目根路径（用于查找 module.lisp）
     * @return (完整模块名, 版本) 或 None
     */
    public func resolveImport(moduleName: String, projectPath: Option<String>): Option<(String, String)> {
        let (org, packageName) = this.moduleRegistry.parseModuleName(moduleName)

        // 查找项目配置中的版本要求
        let requiredVersion = this.findVersionRequirement(packageName, projectPath)

        // 解析版本要求，找到合适的包目录
        match (this.resolveVersion(packageName, requiredVersion, org)) {
            case Some((actualOrg, actualPackage, version)) =>
                Some((ModuleNamespace.buildModuleName(actualOrg, actualPackage), version))
            case None =>
                None
        }
    }

    /**
     * 查找项目配置中对特定包的版本要求
     */
    private func findVersionRequirement(packageName: String, projectPath: Option<String>): String {
        match (projectPath) {
            case Some(path) =>
                // 检查缓存
                if (this.dependencyCache.contains(path)) {
                    match (this.dependencyCache.get(path)) {
                        case Some(deps) =>
                            if (deps.contains(packageName)) {
                                match (deps.get(packageName)) {
                                    case Some(version) => return version
                                    case None => ()
                                }
                            }
                        case None => ()
                    }
                }

                // 解析项目的 module.lisp
                match (this.parseProjectDependencies(path)) {
                    case Some(deps) =>
                        this.dependencyCache[path] = deps
                        if (deps.contains(packageName)) {
                            match (deps.get(packageName)) {
                                case Some(version) => return version
                                case None => ()
                            }
                        }
                    case None => ()
                }

                // 未找到版本要求，使用默认版本
                "latest"
            case None =>
                // 没有项目路径，使用默认版本
                "latest"
        }
    }

    /**
     * 解析版本要求，找到合适的模块
     * @param packageName 模块名（如：log 或 log.zlog）
     * @param versionRequirement 版本要求（如："0.2.0"、"latest"）
     * @param org 组织名（可选）
     * @return (组织名, 模块名, 版本) 或 None
     */
    private func resolveVersion(packageName: String, versionRequirement: String, org: String): Option<(String, String, String)> {
        // 如果指定了组织，直接在该组织下查找
        if (!org.isEmpty()) {
            match (this.findPackageInOrg(packageName, versionRequirement, org)) {
                case Some(version) => return Some((org, packageName, version))
                case None => ()
            }
        }

        // 遍历所有搜索路径，查找模块
        for (searchPath in this.moduleRegistry.getSearchPaths()) {
            // 列出搜索路径下的所有组织目录
            let orgs = this.listOrganizations(searchPath)

            for (actualOrg in orgs) {
                match (this.findPackageInOrg(packageName, versionRequirement, actualOrg)) {
                    case Some(version) => return Some((actualOrg, packageName, version))
                    case None => ()
                }
            }
        }

        None
    }

    /**
     * 在指定组织下查找模块
     */
    private func findPackageInOrg(packageName: String, versionRequirement: String, org: String): Option<String> {
        let basePath = Path(ModuleRegistry.GLOBAL_MODULES_DIR).join(org).toString()
        let packagePath = Path(basePath).join(packageName).toString()

        if (!exists(packagePath)) {
            return None
        }

        let pathObj = Path(packagePath)
        let fileInfo = FileInfo(pathObj)

        if (!fileInfo.isDirectory()) {
            return None
        }

        if (versionRequirement == "latest") {
            // 使用默认版本（无版本号后缀的目录）
            let defaultPath = Path(basePath).join(packageName).toString()
            if (exists(defaultPath)) {
                return Some("default")
            }
        } else {
            // 查找特定版本
            let versionedPath = Path(basePath).join("${packageName}@${versionRequirement}").toString()
            if (exists(versionedPath)) {
                return Some(versionRequirement)
            }
        }

        None
    }

    /**
     * 解析项目的 module.lisp，提取依赖信息
     */
    private func parseProjectDependencies(projectPath: String): Option<HashMap<String, String>> {
        let moduleFile = Path(projectPath).join("module.lisp").toString()

        if (!exists(moduleFile)) {
            return None
        }

        // 使用 ModuleParser 解析
        match (this.moduleParser.parse(projectPath)) {
            case Some(packageInfo) =>
                // 构建 HashMap<模块名, 版本要求>
                let deps = HashMap<String, String>()
                for (dependency in packageInfo.dependencies) {
                    deps[dependency.packageName] = dependency.versionRequirement
                }
                Some(deps)
            case None =>
                None
        }
    }

    /**
     * 列出指定路径下的所有组织目录
     */
    private func listOrganizations(searchPath: String): ArrayList<String> {
        let orgs = ArrayList<String>()

        try {
            // 使用 Directory.readFrom 遍历目录
            let fileInfos = Directory.readFrom(searchPath)

            for (fileInfo in fileInfos) {
                let dirName = fileInfo.name

                // 忽略以 . 开头的目录
                if (dirName.startsWith(".")) {
                    continue
                }

                // 只添加目录
                if (fileInfo.isDirectory()) {
                    orgs.add(dirName)
                }
            }
        } catch (e: FSException) {
            // 目录不存在或无法访问，返回空列表
        } catch (e: Exception) {
            // 其他异常，返回空列表
        }

        orgs
    }

    /**
     * 解析并加载模块的所有依赖（递归）
     * @param packageInfo 模块信息
     * @param loaded 已加载的模块（防止循环依赖）
     * @return 加载的模块列表
     */
    public func loadDependencies(packageInfo: PackageInfo, loaded: HashMap<String, Bool>): ArrayList<Module> {
        let modules = ArrayList<Module>()

        for (dependency in packageInfo.dependencies) {
            let depName = dependency.packageName

            // 检查是否已加载
            if (loaded.contains(depName)) {
                continue
            }

            // 解析依赖
            match (this.resolveImport(depName, Some(packageInfo.directory))) {
                case Some((moduleName, version)) =>
                    // 加载依赖模块
                    match (this.loadModule(moduleName)) {
                        case Some(module) =>
                            loaded[depName] = true
                            modules.add(module)

                            // 递归加载依赖的依赖
                            let depModules = this.loadDependencies(module.packageInfo, loaded)
                            for (depModule in depModules) {
                                modules.add(depModule)
                            }
                        case None => ()
                    }
                case None => ()
            }
        }

        modules
    }

    /**
     * 加载模块
     */
    private func loadModule(moduleName: String): Option<Module> {
        // 委托给模块注册表
        match (this.moduleRegistry.findModule(moduleName)) {
            case Some(module) => Some(module)
            case None => None
        }
    }

    /**
     * 获取模块的所有可用版本
     */
    public func getAvailableVersions(packageName: String, org: String): ArrayList<String> {
        let cacheKey = "${org}::${packageName}"

        if (this.versionCache.contains(cacheKey)) {
            match (this.versionCache.get(cacheKey)) {
                case Some(versions) => return versions
                case None => ArrayList<String>()
            }
        }

        let versions = ArrayList<String>()

        // 扫描目录，查找所有版本化的模块目录
        for (searchPath in this.moduleRegistry.getSearchPaths()) {
            let orgPath = Path(searchPath).join(org).toString()

            if (!exists(orgPath)) {
                continue
            }

            try {
                // 列出组织目录下的所有模块目录
                let fileInfos = Directory.readFrom(orgPath)

                for (fileInfo in fileInfos) {
                    let dirName = fileInfo.name

                    if (!fileInfo.isDirectory()) {
                        continue
                    }

                    // 检查是否是版本化目录（格式：moduleName@version）
                    if (dirName.contains("@")) {
                        let parts = dirName.split('@')
                        if (parts.size == 2) {
                            let baseName = parts[0]
                            let version = parts[1]

                            // 匹配目标模块名
                            if (baseName == packageName) {
                                versions.add(version)
                            }
                        }
                    } else if (dirName == packageName) {
                        // 默认版本（无版本号）
                        versions.add("default")
                    }
                }
            } catch (e: FSException) {
                // 目录不存在或无法访问，继续下一个搜索路径
            } catch (e: Exception) {
                // 其他异常，继续下一个搜索路径
            }
        }

        this.versionCache[cacheKey] = versions
        versions
    }

    /**
     * 清空缓存
     */
    public func clearCache() {
        this.versionCache.clear()
        this.dependencyCache.clear()
    }
}

/**
 * 版本比较工具
 */
public class VersionUtils {
    /**
     * 比较两个版本号
     * @return -1 如果 v1 < v2，0 如果 v1 == v2，1 如果 v1 > v2
     */
    public static func compareVersions(v1: String, v2: String): Int64 {
        let parts1 = v1.split('.')
        let parts2 = v2.split('.')

        let maxLen = if (parts1.size > parts2.size) {
            parts1.size
        } else {
            parts2.size
        }

        for (i in 0..maxLen) {
            let n1 = if (i < parts1.size) {
                match (Int64.tryParse(parts1[i])) {
                    case Some(n) => n
                    case None => 0
                }
            } else {
                0
            }

            let n2 = if (i < parts2.size) {
                match (Int64.tryParse(parts2[i])) {
                    case Some(n) => n
                    case None => 0
                }
            } else {
                0
            }

            if (n1 < n2) {
                return -1
            } else if (n1 > n2) {
                return 1
            }
        }

        0
    }

    /**
     * 检查版本是否满足要求
     * 支持的格式：
     * - "0.2.0" - 精确版本
     * - "latest" - 最新版本（默认版本）
     *
     * 说明：采用精确版本管理，不支持版本范围语法（^, ~, >=, <=）
     * 多版本通过 @版本号 目录管理，如 org/module@0.1.0/
     */
    public static func satisfiesVersion(version: String, requirement: String): Bool {
        if (requirement == "latest") {
            return true
        }

        if (requirement == version) {
            return true
        }

        false
    }
}
