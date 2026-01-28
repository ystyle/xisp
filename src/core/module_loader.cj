package ystyle::xisp.core

import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import std.collection.ArrayList
import std.collection.HashMap
import std.fs.*
import std.sort.*

/**
 * 模块文件加载器
 *
 * 负责加载模块文件，支持从自定义 ModuleSource 或文件系统加载
 */
public class ModuleLoader <: Loader {
    private var evaluator: Evaluator
    private var loadedFiles: HashMap<String, Bool>  // 已加载的文件，防止重复加载
    private var source: Option<ModuleSource> = None  // 自定义数据源
    private var registry: Option<ModuleRegistry> = None
    private var parser: Option<ModuleParser> = None

    public init(evaluator: Evaluator) {
        this.evaluator = evaluator
        this.loadedFiles = HashMap<String, Bool>()
    }

    /**
     * 设置 ModuleRegistry 和 ModuleParser 引用
     */
    public func setModuleSystem(registry: ModuleRegistry, parser: ModuleParser): Unit {
        this.registry = Some(registry)
        this.parser = Some(parser)
    }

    /**
     * 设置自定义模块数据源
     * @param source ModuleSource 实例
     */
    public func setSource(source: ModuleSource): Unit {
        this.source = Some(source)
    }

    /**
     * 检查是否设置了自定义数据源
     */
    public func hasCustomSource(): Bool {
        this.source.isSome()
    }

    /**
     * Loader 接口：按模块名导入
     *
     * 加载模块并注册到 ModuleRegistry，但不绑定符号。
     * 符号绑定由 evalImport/importModuleByName 处理（支持 :as/only）。
     */
    public func loadByName(moduleName: String): LispValue {
        match (this.registry) {
            case Some(reg) => match (this.parser) {
                case Some(par) =>
                    match (this.loadModule(moduleName, reg, par)) {
                        case Some(_) => Nil
                        case None =>
                            LispValue.Error(XispError(ErrorType.ModuleError,
                                "module '${moduleName}' not found"))
                    }
                case None =>
                    LispValue.Error(XispError(ErrorType.ModuleError,
                        "module system not initialized"))
            }
            case None =>
                LispValue.Error(XispError(ErrorType.ModuleError,
                    "module system not initialized"))
        }
    }

    /**
     * Loader 接口：按相对路径导入（ModuleLoader 不支持）
     */
    public func loadRelative(relativePath: String): LispValue {
        LispValue.Error(XispError(ErrorType.ModuleError,
            "模块上下文中不支持相对路径导入"))
    }

    /**
     * 获取当前 ModuleSource（不存在时创建默认 FileSystemSource）
     */
    private func getOrCreateSource(): ModuleSource {
        match (this.source) {
            case Some(src) => src
            case None =>
                let defaultSource = FileSystemSource.fromPath(".")
                this.source = Some(defaultSource)
                defaultSource
        }
    }

    /**
     * 加载模块
     *
     * 统一入口：通过 ModuleSource 加载模块，返回完整的 Module 对象。
     * 如果未设置自定义源，默认使用 FileSystemSource 搜索文件系统。
     *
     * @param moduleName 模块名（如："ystyle::log"）
     * @param registry 模块注册表
     * @param parser 模块解析器
     * @return 加载的模块，失败返回 None
     */
    public func loadModule(moduleName: String, registry: ModuleRegistry, parser: ModuleParser): Option<Module> {
        let src = this.getOrCreateSource()

        // 1. 获取 module.lisp 元数据
        let metadata = src.getMetadata(moduleName)

        // 2. 解析模块信息
        let packageInfo: Option<PackageInfo> = match (metadata) {
            case Some(content) =>
                parser.parseContent(content, moduleName, moduleName)
            case None => None
        }

        // 3. 创建 PackageInfo（有元数据则使用解析结果，否则创建默认的）
        let info = match (packageInfo) {
            case Some(pi) => pi
            case None =>
                PackageInfo(moduleName, "", moduleName, "")
        }

        // 4. 创建模块环境
        let moduleEnv = this.evaluator.env.createChild()

        // 5. 创建模块对象（先创建，用于 export）
        let module = Module(moduleName, info, moduleEnv)

        // 6. 设置当前模块，以便 export 可以工作
        let oldModule = this.evaluator.currentModule
        this.evaluator.currentModule = Some(module)

        // 7. 获取文件列表并加载
        let files = src.listFiles(moduleName)
        var hasError = false

        if (!files.isEmpty()) {
            sort(files)
            for (filePath in files) {
                match (src.readFile(moduleName, filePath)) {
                    case Some(content) =>
                        if (!this.evalContent(content, "${moduleName}/${filePath}", moduleEnv)) {
                            hasError = true
                            break
                        }
                    case None =>
                        hasError = true
                        break
                }
            }
        }

        // 8. 恢复之前的模块
        this.evaluator.currentModule = oldModule

        // 9. 注册或返回失败
        if (!hasError) {
            module.isInitialized = true
            registry.registerModule(module)
            Some(module)
        } else {
            None
        }
    }

    /**
     * 加载包目录下的所有 .lisp 文件
     * @param packagePath 包目录路径
     * @param env 模块环境
     * @return 是否成功加载
     */
    public func loadPackage(packagePath: String, env: Environment): Bool {
        if (!exists(packagePath)) {
            return false
        }

        let pathObj = Path(packagePath)
        let fileInfo = FileInfo(pathObj)

        if (!fileInfo.isDirectory()) {
            return false
        }

        // 列出目录下所有文件
        let files = this.listLispFiles(packagePath)

        if (files.isEmpty()) {
            return true  // 空包也算加载成功
        }

        // 按文件名排序加载
        let sortedFiles = this.sortFiles(files)

        // 依次加载每个文件
        var success = true
        for (fileName in sortedFiles) {
            let filePath = Path(packagePath).join(fileName).toString()

            if (!this.loadFile(filePath, env)) {
                success = false
            }
        }

        success
    }

    /**
     * 列出目录下的所有 .lisp 文件（递归，忽略以 . 开头的文件）
     */
    private func listLispFiles(directory: String): ArrayList<String> {
        let files = ArrayList<String>()
        let basePath = Path(directory)

        try {
            // 使用 Directory.walk 递归遍历目录
            Directory.walk(basePath, { fileInfo =>
                let fileName = fileInfo.name

                // 忽略以 . 开头的文件和目录
                if (fileName.startsWith(".")) {
                    return true  // 继续遍历
                }

                // 如果是目录，继续遍历
                if (fileInfo.isDirectory()) {
                    return true
                }

                // 只处理 .lisp 文件，但排除 module.lisp（它只用于元数据）
                if (fileName.endsWith(".lisp") && fileName != "module.lisp") {
                    // 使用 Path 获取相对路径
                    let filePath = fileInfo.path
                    // 检查是否是当前目录下的文件
                    let parent = filePath.parent
                    if (parent.toString() == basePath.toString()) {
                        // 直接在当前目录下
                        files.add(fileName)
                    } else {
                        // 在子目录下，构建相对路径
                        let parentStr = parent.toString()
                        let basePathStr = basePath.toString()
                        if (parentStr.startsWith(basePathStr)) {
                            // 获取相对部分
                            let relativePart = parentStr[basePathStr.size..]
                            // 去掉开头的 /
                            let cleanRelative = if (relativePart.startsWith("/")) {
                                relativePart[1..]
                            } else {
                                relativePart
                            }
                            files.add("${cleanRelative}/${fileName}")
                        } else {
                            // 不在目录下，使用文件名
                            files.add(fileName)
                        }
                    }
                }

                true  // 继续遍历
            })
        } catch (e: FSException) {
            // 目录不存在或无法访问，返回空列表
        } catch (e: Exception) {
            // 其他异常，返回空列表
        }

        files
    }

    /**
     * 按文件名排序
     */
    private func sortFiles(files: ArrayList<String>): ArrayList<String> {
        // 使用仓颉的 sort 函数排序
        sort(files)
        files
    }

    /**
     * 加载单个文件
     * @param filePath 文件路径
     * @param env 环境
     * @return 是否成功
     */
    public func loadFile(filePath: String, env: Environment): Bool {
        // 检查是否已加载
        if (this.isLoaded(filePath)) {
            return true  // 已加载，跳过
        }

        // 检查文件是否存在
        if (!exists(filePath)) {
            return false
        }

        // 检查是否应该忽略
        if (this.shouldIgnore(filePath)) {
            return true  // 忽略的文件不算失败
        }

        // 检查沙箱权限（读取操作）
        match (this.evaluator.pathChecker) {
            case Some(checker) =>
                if (!checker(filePath, false)) {  // false = 读取操作
                    return false  // 权限拒绝
                }
            case None => ()  // 没有沙箱检查，允许
        }

        // 读取文件内容
        try {
            let bytes = File.readFrom(filePath)
            let content = String.fromUtf8(bytes)
            this.evalContent(content, filePath, env)
        } catch (_: FSException) {
            false
        } catch (_: Exception) {
            false
        }
    }

    /**
     * 检查文件是否已加载
     */
    private func isLoaded(filePath: String): Bool {
        if (this.loadedFiles.contains(filePath)) {
            return true
        } else {
            false
        }
    }

    /**
     * 标记文件为已加载
     */
    private func markLoaded(filePath: String) {
        this.loadedFiles[filePath] = true
    }

    /**
     * 检查文件是否应该被忽略
     * 忽略规则：
     * - 以 . 开头的文件（如：.backup.lisp）
     */
    public func shouldIgnore(filePath: String): Bool {
        let pathObj = Path(filePath)
        let fullPath = pathObj.toString()
        // 获取路径的最后部分（文件名）
        let parts = fullPath.split("/")
        let fileName = if (parts.size > 0) {
            parts[parts.size - 1]
        } else {
            fullPath
        }
        fileName.startsWith(".")
    }

    /**
     * 执行文件内容
     */
    private func evalContent(content: String, filePath: String, env: Environment): Bool {
        // 使用词法分析器和语法分析器
        let lexer = Lexer(content)
        let tokens = lexer.tokenize()
        let parser = Parser(tokens)

        var hasError = false

        // 解析所有表达式
        let exprs = parser.parse()

        // 逐个求值表达式
        for (expr in exprs) {
            // 保存当前环境和文件路径
            let oldEnv = this.evaluator.env
            let oldFilePath = this.evaluator.currentFilePath

            // 设置当前环境和文件路径
            this.evaluator.env = env
            this.evaluator.currentFilePath = Some(filePath)

            // 求值
            let result = this.evaluator.eval(expr)

            // 恢复环境和文件路径
            this.evaluator.env = oldEnv
            this.evaluator.currentFilePath = oldFilePath

            // 检查是否有错误
            match (result) {
                case Str(msg) =>
                    if (msg.startsWith("Error:")) {
                        hasError = true
                    }
                case _ => ()
            }
        }

        if (!hasError) {
            this.markLoaded(filePath)
            true
        } else {
            false
        }
    }

    /**
     * 清空已加载文件缓存
     */
    public func clearCache(): Unit {
        this.loadedFiles.clear()
    }

    /**
     * 获取已加载文件列表
     */
    public func getLoadedFiles(): ArrayList<String> {
        let files = ArrayList<String>()
        for ((path, _) in this.loadedFiles) {
            files.add(path)
        }
        files
    }
}

/**
 * 文件路径工具类
 */
public class ModulePathUtils {
    /**
     * 解析模块路径
     * 从模块名生成文件系统路径
     * 注意：目录名是模块名去掉最后一级（log.zlog → log）
     */
    public static func moduleToPath(moduleName: String, searchPath: String): String {
        // 解析模块名：org::module.subpkg
        let parts = moduleName.split("::")
        let (org, packageName) = if (parts.size == 2) {
            // 有组织前缀：ystyle::log.zlog
            (parts[0], parts[1])
        } else if (parts.size == 1) {
            // 无组织前缀：io
            ("", parts[0])
        } else {
            // 格式错误
            ("", moduleName)
        }

        // 提取目录名（去掉模块名的最后一级）
        let packageDir = extractPackageDir(packageName)

        if (org.isEmpty()) {
            // 标准库：searchPath/packageDir/
            Path(searchPath).join(packageDir).toString()
        } else {
            // 带组织前缀：searchPath/org/packageDir/
            Path(searchPath).join(org).join(packageDir).toString()
        }
    }

    /**
     * 从模块名提取目录名
     * 规则：去掉模块名的最后一级
     * 例如：
     * - io → io
     * - log.zlog → log
     * - io.file → io
     * - utils.string.parser → utils.string
     *
     * @param packageName 模块名（如："log.zlog"）
     * @return 目录名（如："log"）
     */
    public static func extractPackageDir(packageName: String): String {
        let parts = packageName.split('.')
        if (parts.size > 1) {
            // 去掉最后一级，重新组合
            let dirParts = ArrayList<String>()
            for (i in 0..(parts.size - 1)) {
                dirParts.add(parts[i])
            }
            // 用 . 连接
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
            // 单级包名，直接返回
            packageName
        }
    }

    /**
     * 查找模块的实际路径
     * 在多个搜索路径中查找模块
     */
    public static func findModulePath(moduleName: String, searchPaths: ArrayList<String>): Option<String> {
        for (searchPath in searchPaths) {
            let modulePath = moduleToPath(moduleName, searchPath)

            if (exists(modulePath)) {
                let pathObj = Path(modulePath)
                let fileInfo = FileInfo(pathObj)

                if (fileInfo.isDirectory()) {
                    // 检查是否有 module.lisp
                    let packageFile = Path(modulePath).join("module.lisp").toString()
                    if (exists(packageFile)) {
                        return Some(modulePath)
                    }
                }
            }
        }

        None
    }

    /**
     * 解析相对路径导入
     * 例如：./lib.utils -> /current/dir/lib/utils/
     */
    public static func resolveRelativePath(relativePath: String, basePath: String): String {
        if (relativePath.startsWith("./") || relativePath.startsWith("../")) {
            let path = Path(basePath).parent.toString()
            Path(path).join(relativePath).toString()
        } else {
            relativePath
        }
    }

    /**
     * 将相对路径转换为模块名
     * 例如：./lib.utils -> lib.utils
     */
    public static func relativePathToModuleName(relativePath: String): String {
        // 移除 ./ 或 ../ 前缀
        let cleaned = relativePath.replace("./", "").replace("../", "")

        // 将路径分隔符转换为点号
        cleaned.replace("/", ".").replace("\\", ".")
    }
}
