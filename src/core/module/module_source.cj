package ystyle::xisp.core.module

import ystyle::xisp.types.*
import std.collection.ArrayList
import std.collection.HashMap
import std.fs.*
import std.sort.*

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
     * @param moduleName 模块名（如："ystyle::log"）
     * @param filePath 文件相对路径（如："core.lisp" 或 "utils/helper.lisp"）
     * @return 文件内容，如果文件不存在返回 None
     */
    func readFile(moduleName: String, filePath: String): ?String
}

/**
 * 模块数据结构
 * 用于批量注册模块
 */
public class ModuleData {
    public let name: String
    public let version: String
    public let organization: String
    public let metadata: String
    public let files: HashMap<String, String>

    public init(
        name: String,
        version: String,
        organization: String,
        metadata: String,
        files: HashMap<String, String>
    ) {
        this.name = name
        this.version = version
        this.organization = organization
        this.metadata = metadata
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
        ModuleData(name, version, organization, metadata, files)
    }
}

/**
 * 内存模块数据源
 *
 * 适用场景：
 * - 测试/单元测试：避免文件系统依赖
 * - 动态生成模块：从数据库、网络等加载
 * - 嵌入式系统：模块代码编译进程序
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
     */
    public func registerModule(
        moduleName: String,
        moduleMetadata: String,
        moduleFiles: HashMap<String, String>
    ): Unit {
        this.metadata[moduleName] = moduleMetadata
        this.files[moduleName] = moduleFiles
    }

    /**
     * 注册模块的元数据（单独调用）
     */
    public func registerMetadata(moduleName: String, content: String): Unit {
        this.metadata[moduleName] = content
    }

    /**
     * 添加模块文件
     *
     * @param moduleName 模块名
     * @param filePath 文件路径（如："core.lisp"）
     * @param content 文件内容
     */
    public func addFile(moduleName: String, filePath: String, content: String): Unit {
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
     */
    public func registerModules(modules: HashMap<String, ModuleData>): Unit {
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
                case Some(fileMap) =>
                    let keys = ArrayList<String>()
                    for ((path, _) in fileMap) {
                        keys.add(path)
                    }
                    keys
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
 * 文件系统模块数据源（默认实现）
 *
 * 封装现有的文件系统加载逻辑，使其符合 ModuleSource 接口
 */
public class FileSystemSource <: ModuleSource {
    private var searchPaths: ArrayList<String>

    /**
     * 构造函数
     *
     * @param searchPaths 模块搜索路径列表
     */
    public init(searchPaths: ArrayList<String>) {
        this.searchPaths = searchPaths
    }

    /**
     * 从单一搜索路径创建
     */
    public static func fromPath(basePath: String): FileSystemSource {
        let paths = ArrayList<String>()
        paths.add(basePath)
        FileSystemSource(paths)
    }

    /**
     * 获取模块元数据
     */
    public func getMetadata(moduleName: String): ?String {
        match (this.resolveModulePath(moduleName)) {
            case Some(modulePath) =>
                let metadataPath = Path(modulePath).join("module.lisp").normalize().toString()
                if (!exists(metadataPath)) {
                    return None
                }
                try {
                    let bytes = File.readFrom(metadataPath)
                    Some(String.fromUtf8(bytes))
                } catch (_: FSException) {
                    None
                } catch (_: Exception) {
                    None
                }
            case None => None
        }
    }

    /**
     * 列出模块的所有文件
     *
     * 扫描模块目录，返回所有 .lisp 文件（排除 module.lisp 和 . 开头的文件）
     */
    public func listFiles(moduleName: String): ArrayList<String> {
        match (this.resolveModulePath(moduleName)) {
            case Some(modulePath) =>
                if (!exists(modulePath)) {
                    return ArrayList<String>()
                }

                let files = ArrayList<String>()
                let basePathObj = Path(modulePath)

                try {
                    Directory.walk(basePathObj, { fileInfo =>
                        let fileName = fileInfo.name

                        if (fileName.startsWith(".")) {
                            return true
                        }

                        if (fileInfo.isDirectory()) {
                            return true
                        }

                        if (fileName.endsWith(".lisp") && fileName != "module.lisp") {
                            let filePath = fileInfo.path.toString()
                            let relativePath = getRelativePath(modulePath, filePath)
                            files.add(relativePath)
                        }

                        true
                    })
                } catch (_: FSException) {
                } catch (_: Exception) {
                }

                sort(files)
                files
            case None => ArrayList<String>()
        }
    }

    /**
     * 读取文件内容
     */
    public func readFile(moduleName: String, filePath: String): ?String {
        match (this.resolveModulePath(moduleName)) {
            case Some(modulePath) =>
                // 如果 filePath 是绝对路径则直接使用，否则用 modulePath 拼接
                let fullPath = if (Path(filePath).isAbsolute()) {
                    filePath
                } else {
                    Path(modulePath).join(filePath).toString()
                }
                let normalizedPath = Path(fullPath).normalize().toString()
                if (!exists(normalizedPath)) {
                    return None
                }
                try {
                    let bytes = File.readFrom(normalizedPath)
                    Some(String.fromUtf8(bytes))
                } catch (_: FSException) {
                    None
                } catch (_: Exception) {
                    None
                }
            case None => None
        }
    }

    // ==================== 辅助方法 ====================

    /**
     * 解析模块路径
     *
     * "ystyle::log.zlog" → 搜索 searchPaths 下 ystyle/log/ 目录
     */
    private func resolveModulePath(moduleName: String): Option<String> {
        let (org, packageName) = parseModuleName(moduleName)
        let packageDir = extractPackageDirFromName(packageName)

        for (searchPath in this.searchPaths) {
            let packagePath = if (org.isEmpty()) {
                Path(searchPath).join(packageDir).toString()
            } else {
                Path(searchPath).join(org).join(packageDir).toString()
            }

            if (isPackageDirectory(packagePath)) {
                return Some(Path(packagePath).normalize().toString())
            }
        }

        None
    }

    /**
     * 解析模块名
     * "ystyle::log.zlog" → ("ystyle", "log.zlog")
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
     * 从包名提取目录名
     * log.zlog → log, io → io
     */
    private func extractPackageDirFromName(packageName: String): String {
        let parts = packageName.split('.')
        if (parts.size > 1) {
            let dirParts = ArrayList<String>()
            for (i in 0..(parts.size - 1)) {
                dirParts.add(parts[i])
            }
            var result = ""
            for (i in 0..dirParts.size) {
                if (i > 0) {
                    result = "${result}.${dirParts[i]}"
                } else {
                    result = dirParts[i]
                }
            }
            result
        } else {
            packageName
        }
    }

    /**
     * 检查路径是否是一个有效的模块目录
     */
    private func isPackageDirectory(path: String): Bool {
        let packageFile = Path(path).join("module.lisp").toString()
        exists(packageFile)
    }

    /**
     * 获取相对路径
     * 将 fullPath（绝对路径）转换为相对于 basePath 的路径
     */
    private func getRelativePath(basePath: String, fullPath: String): String {
        let base = Path(basePath).normalize().toString()
        if (fullPath.startsWith(base)) {
            var relative = fullPath[base.size..]
            if (relative.startsWith("/")) {
                relative = relative[1..]
            }
            relative
        } else {
            // 尝试基于当前工作目录解析相对路径
            let cwd = Path(".").normalize().toString()
            let absBase = Path(cwd).join(basePath).normalize().toString()
            if (fullPath.startsWith(absBase)) {
                var relative = fullPath[absBase.size..]
                if (relative.startsWith("/")) {
                    relative = relative[1..]
                }
                relative
            } else {
                fullPath
            }
        }
    }
}
