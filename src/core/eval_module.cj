package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList
import std.fs.*

/**
 * 模块系统特殊形式
 * 处理 import、export 等模块相关的特殊形式
 */
extend Evaluator {
    /**
     * 获取模块注册表（如果未初始化则返回错误）
     */
    private func getModuleRegistry(): ModuleRegistry {
        if (let Some(registry) <- this.moduleRegistry) {
            registry
        } else {
            ModuleRegistry()  // 不应该发生，但提供默认值
        }
    }

    /**
     * 获取模块加载器（如果未初始化则返回错误）
     */
    private func getModuleLoader(): ModuleLoader {
        if (let Some(loader) <- this.moduleLoader) {
            loader
        } else {
            ModuleLoader(this)  // 不应该发生，但提供默认值
        }
    }

    /**
     * 获取模块解析器（如果未初始化则返回错误）
     */
    private func getModuleParser(): ModuleParser {
        if (let Some(parser) <- this.moduleParser) {
            parser
        } else {
            ModuleParser(this)  // 不应该发生，但提供默认值
        }
    }

    /**
     * 特殊形式：import
     * 导入一个模块，使其符号可用
     *
     * 语法格式：
     * (import module::name)
     * (import module::name :as alias)
     * (import (only module::name sym1 sym2))
     * (import ./local.module)
     * (import ../parent.module)
     */
    func evalImport(expr: LispValue): LispValue {
        // 确保模块系统已初始化
        if (this.moduleRegistry.isNone()) {
            this.initModuleSystem()
        }

        // 检查模块加载权限
        match (this.moduleLoadChecker) {
            case Some(checker) =>
                if (!checker()) {
                    return LispValue.Error(XispError(ErrorType.ModuleError, "Module loading is not allowed in this sandbox"))
                }
            case None => ()  // 没有沙箱检查，允许
        }

        match (expr) {
            case Cons(cell) =>
                if (cell.length() == 0) {
                    return LispValue.Error(XispError(ErrorType.ArityMismatch, "import requires at least one argument"))
                }

                let importSpec = cell.car

                // 处理不同的导入格式
                match (importSpec) {
                    // 格式1: (import module::name)
                    case Symbol(moduleName) =>
                        this.importModuleByName(moduleName, None, ArrayList<String>())

                    // 格式2: (import (module::name :as alias))
                    case Cons(specCell) =>
                        this.parseImportSpec(specCell)

                    // 格式3: (import "./local") - 相对路径导入
                    case Str(relativePath) =>
                        this.importRelativeModule(relativePath)

                    case _ =>
                        LispValue.Error(XispError(ErrorType.SyntaxError, "invalid import syntax"))
                }

            case _ => LispValue.Error(XispError(ErrorType.SyntaxError, "import requires a list"))
        }
    }

    /**
     * 解析导入规范
     * 处理以下格式：
     * - (module::name :as alias)
     * - (only module::name sym1 sym2)
     * - ("./local.module")
     */
    private func parseImportSpec(specCell: ConsCell): LispValue {
        let first = specCell.car

        match (first) {
            // 格式：:as 别名导入
            case Symbol(":as") =>
                this.parseAliasImport(specCell)

            // 格式：only 限定导入
            case Symbol("only") =>
                this.parseOnlyImport(specCell)

            // 格式：相对路径导入
            case Str(relativePath) =>
                this.importRelativeModule(relativePath)

            // 默认：模块名
            case Symbol(moduleName) =>
                // 检查是否有 :as 或其他修饰符
                this.parseSimpleImport(specCell)

            case _ => LispValue.Error(XispError(ErrorType.SyntaxError, "invalid import specification"))
        }
    }

    /**
     * 解析简单导入：(module::name)
     * 或带别名的导入：(module::name :as alias)
     */
    private func parseSimpleImport(specCell: ConsCell): LispValue {
        let first = specCell.car
        let rest = specCell.cdr

        if (let Symbol(moduleName) <- first) {
            if (let Cons(restCell) <- rest &&
                let Symbol(":as") <- restCell.car &&
                let Cons(aliasCell) <- restCell.cdr &&
                let Symbol(alias) <- aliasCell.car) {
                // (module::name :as alias)
                this.importModuleByName(moduleName, Some(alias), ArrayList<String>())
            } else if (let Cons(restCell) <- rest && let Symbol(":as") <- restCell.car) {
                // (module::name :as ?) - 缺少别名
                LispValue.Error(XispError(ErrorType.SyntaxError, ":as requires a symbol alias"))
            } else {
                // 只有模块名，普通导入
                this.importModuleByName(moduleName, None, ArrayList<String>())
            }
        } else {
            LispValue.Error(XispError(ErrorType.SyntaxError, "module name must be a symbol"))
        }
    }

    /**
     * 解析别名导入：(module::name :as alias)
     */
    private func parseAliasImport(specCell: ConsCell): LispValue {
        // 这个方法实际上不会独立调用，因为在 parseSimpleImport 中已经处理
        LispValue.Error(XispError(ErrorType.SyntaxError, ":as must follow module name"))
    }

    /**
     * 解析限定导入：(only module::name sym1 sym2)
     */
    private func parseOnlyImport(specCell: ConsCell): LispValue {
        let rest = specCell.cdr

        match (rest) {
            case Cons(restCell) =>
                let moduleName = restCell.car
                let symbols = ArrayList<String>()

                // 提取要导入的符号列表
                var current: LispValue = restCell.cdr
                while (!current.isNil()) {
                    match (current) {
                        case Cons(currCell) =>
                            match (currCell.car) {
                                case Symbol(sym) => symbols.add(sym)
                                case _ => ()
                            }
                            current = currCell.cdr
                        case _ => break
                    }
                }

                match (moduleName) {
                    case Symbol(name) =>
                        this.importModuleByName(name, None, symbols)
                    case _ => LispValue.Error(XispError(ErrorType.SyntaxError, "only requires a module name"))
                }
            case _ => LispValue.Error(XispError(ErrorType.SyntaxError, "only requires module name and symbols"))
        }
    }

    /**
     * 导入相对路径模块：("./local.module") 或 ("../parent.module")
     * 新逻辑：区分文件导入和目录包导入
     */
    private func importRelativeModule(relativePath: String): LispValue {
        // 获取当前文件路径
        match (this.currentFilePath) {
            case Some(filePath) =>
                // 获取当前文件所在目录
                let currentDir = Path(filePath).parent.toString()

                // 判断是文件导入还是目录包导入
                if (this.isFileImport(relativePath)) {
                    // 文件导入：无前缀，直接加载文件
                    this.loadRelativeFile(currentDir, relativePath)
                } else {
                    // 目录包导入：有前缀，加载目录下的所有文件
                    this.loadRelativePackage(currentDir, relativePath)
                }
            case None =>
                // REPL 环境：使用当前工作目录作为基准路径
                let currentDir = "."

                if (this.isFileImport(relativePath)) {
                    this.loadRelativeFile(currentDir, relativePath)
                } else {
                    this.loadRelativePackage(currentDir, relativePath)
                }
        }
    }

    /**
     * 判断是否是文件导入
     * 文件导入：以 .lisp 结尾
     * 目录包导入：不以 .lisp 结尾
     */
    private func isFileImport(relativePath: String): Bool {
        relativePath.endsWith(".lisp")
    }

    /**
     * 加载相对文件（无前缀）
     * @param baseDir 基础目录（当前文件所在目录）
     * @param relativePath 相对路径（如 "./utils.lisp"）
     */
    private func loadRelativeFile(baseDir: String, relativePath: String): LispValue {
        // 解析相对路径
        var normalizedPath = relativePath

        // 处理 ./ 开头
        if (normalizedPath.startsWith("./")) {
            normalizedPath = normalizedPath[2..normalizedPath.size]
        }

        // 构建完整路径
        let fullPath = Path(baseDir).join(normalizedPath).toString()

        // 检查文件是否存在
        if (!exists(fullPath)) {
            return LispValue.Error(XispError(ErrorType.IOError, "file not found at '${relativePath}'"))
        }

        // 检查文件是否是模块（不能导入模块）
        let parentDir = Path(fullPath).parent.toString()
        let moduleFile = Path(parentDir).join("module.lisp").toString()

        if (exists(moduleFile)) {
            return LispValue.Error(XispError(ErrorType.ModuleError, "Cannot import module '${relativePath}' using relative import. Use (import moduleName) or (import org::moduleName) instead."))
        }

        // 使用 ModuleLoader 加载单个文件
        let loader = this.getModuleLoader()

        // 直接加载文件到当前环境（无前缀）
        if (loader.loadFile(fullPath, this.env)) {
            Nil
        } else {
            LispValue.Error(XispError(ErrorType.IOError, "failed to load file '${relativePath}'"))
        }
    }

    /**
     * 加载相对目录包（有前缀）
     * @param baseDir 基础目录（当前文件所在目录）
     * @param relativePath 相对路径（如 "./helpers" 或 "./math.stats"）
     */
    private func loadRelativePackage(baseDir: String, relativePath: String): LispValue {
        // 解析相对路径
        var normalizedPath = relativePath

        // 处理 ./ 开头
        if (normalizedPath.startsWith("./")) {
            normalizedPath = normalizedPath[2..normalizedPath.size]
        }

        // 将 . 转换为路径分隔符
        let pathPart = normalizedPath.replace(".", "/")

        // 构建完整路径
        let fullPath = Path(baseDir).join(pathPart).toString()

        // 检查目录是否存在
        if (!exists(fullPath)) {
            return LispValue.Error(XispError(ErrorType.IOError, "directory not found at '${relativePath}'"))
        }

        // 检查是否是模块（不能相对导入模块）
        let moduleFile = Path(fullPath).join("module.lisp").toString()
        if (exists(moduleFile)) {
            return LispValue.Error(XispError(ErrorType.ModuleError, "Cannot import module '${relativePath}' using relative import. Use (import moduleName) or (import org::moduleName) instead."))
        }

        // 提取前缀（路径的最后一级）
        let prefix = this.extractPrefix(normalizedPath)

        // 创建一个新的环境用于包
        let packageEnv = this.env.createChild()

        // 使用 ModuleLoader 加载包目录下的所有文件
        let loader = this.getModuleLoader()

        if (loader.loadPackage(fullPath, packageEnv)) {
            // 将包环境中的导出符号绑定到当前环境（带前缀）
            this.bindPackageSymbols(packageEnv, prefix)
            Nil
        } else {
            LispValue.Error(XispError(ErrorType.IOError, "failed to load package '${relativePath}'"))
        }
    }

    /**
     * 从相对路径提取前缀
     * 例如：
     * - "helpers" → "helpers"
     * - "math.stats" → "stats"
     * - "utils" → "utils"
     */
    private func extractPrefix(relativePath: String): String {
        let parts = relativePath.split('.')
        if (parts.size > 0) {
            parts[parts.size - 1]
        } else {
            relativePath
        }
    }

    /**
     * 将包环境中的符号绑定到当前环境（带前缀）
     * @param packageEnv 包环境
     * @param prefix 前缀名
     */
    private func bindPackageSymbols(packageEnv: Environment, prefix: String) {
        // 获取包环境中所有导出的符号
        let exportedBindings = packageEnv.getExportedBindings()

        // 如果没有显式导出任何符号，导出所有符号
        if (exportedBindings.isEmpty()) {
            // 获取所有符号（除了特殊符号）
            let allKeys = packageEnv.getKeys()
            for (key in allKeys) {
                // 跳过内部符号（以下划线开头）
                if (!key.startsWith("_")) {
                    let value = packageEnv.lookup(key)
                    let prefixedKey = "${prefix}.${key}"
                    this.env.define(prefixedKey, value)
                }
            }
        } else {
            // 只绑定导出的符号
            for ((key, value) in exportedBindings) {
                let prefixedKey = "${prefix}.${key}"
                this.env.define(prefixedKey, value)
            }
        }
    }

    /**
     * 解析相对路径
     * @param baseDir 基础目录（当前文件所在目录）
     * @param relativePath 相对路径（如 "./lib.utils" 或 "../lib.parser"）
     * @return 解析后的绝对路径，或 None 如果路径无效
     */
    private func resolveRelativePath(baseDir: String, relativePath: String): Option<String> {
        // 标准化相对路径
        var normalizedPath = relativePath

        // 处理 ./ 开头
        if (normalizedPath.startsWith("./")) {
            // 去掉 ./（从索引2开始到结尾）
            normalizedPath = normalizedPath[2..normalizedPath.size]
        }

        // 处理 ../ 开头
        if (normalizedPath.startsWith("../")) {
            // 获取上级目录
            let parentDir = Path(baseDir).parent.toString()
            // 去掉 ../（从索引3开始到结尾）
            normalizedPath = normalizedPath[3..normalizedPath.size]
            // 拼接路径
            let fullPath = Path(parentDir).join(normalizedPath).toString()

            // 将路径转换为模块名
            this.pathToModuleName(fullPath)
        } else {
            // 当前目录下的路径
            let fullPath = Path(baseDir).join(normalizedPath).toString()
            this.pathToModuleName(fullPath)
        }
    }

    /**
     * 将文件路径转换为模块名
     * @param path 文件路径（如：/path/to/ystyle/log）
     * @return 模块名或 None
     */
    private func pathToModuleName(path: String): Option<String> {
        // 规范化路径
        let normalizedPath = canonicalize(path).toString()

        // 遍历所有搜索路径，找到匹配的包
        for (searchPath in this.getModuleRegistry().getSearchPaths()) {
            // 检查路径是否在搜索路径下
            if (normalizedPath.startsWith(searchPath)) {
                // 获取相对部分
                var relativePart = normalizedPath[searchPath.size..normalizedPath.size]

                // 去掉开头的 /
                if (relativePart.startsWith("/")) {
                    relativePart = relativePart[1..relativePart.size]
                }

                // 查找该路径的包目录（向上查找包含 package.lisp 的目录）
                match (this.findPackageDirectory(normalizedPath)) {
                    case Some(packagePath) =>
                        // 将包路径转换为模块名
                        let moduleName = this.packagePathToModuleName(packagePath)
                        return Some(moduleName)
                    case None =>
                        // 直接使用路径转换（作为备用方案）
                        return Some(this.convertPathToModuleName(relativePart))
                }
            }
        }

        None
    }

    /**
     * 向上查找包含 module.lisp 的目录
     * @param path 文件路径
     * @return 模块目录路径，或 None
     */
    private func findPackageDirectory(path: String): Option<String> {
        var currentPath = path

        while (!currentPath.isEmpty()) {
            // 检查当前目录是否有 module.lisp
            let packageFile = Path(currentPath).join("module.lisp").toString()

            if (exists(packageFile)) {
                // 找到了模块目录
                return Some(currentPath)
            }

            // 向上一级目录
            let parent = Path(currentPath).parent.toString()

            // 如果已经到达根目录，停止
            if (parent == currentPath) {
                break
            }

            currentPath = parent
        }

        None
    }

    /**
     * 将包路径转换为模块名
     * @param packagePath 包目录路径
     * @return 模块名
     */
    private func packagePathToModuleName(packagePath: String): String {
        // 遍历所有搜索路径，找到匹配的
        for (searchPath in this.getModuleRegistry().getSearchPaths()) {
            if (packagePath.startsWith(searchPath)) {
                // 获取相对部分
                var relativePart = packagePath[searchPath.size..packagePath.size]

                // 去掉开头的 /
                if (relativePart.startsWith("/")) {
                    relativePart = relativePart[1..relativePart.size]
                }

                // 将 / 替换为 ::
                return relativePart.replace("/", "::")
            }
        }

        // 如果没有找到，使用默认转换
        packagePath.replace("/", "::")
    }

    /**
     * 将路径字符串转换为模块名
     * @param path 路径（如：ystyle/log/zlog）
     * @return 模块名（如：ystyle::log.zlog）
     */
    private func convertPathToModuleName(path: String): String {
        // 简单的路径到模块名转换
        // 假设路径是相对于搜索路径的
        // 将 / 替换为 ::
        path.replace("/", "::")
    }

    /**
     * 根据路径查找对应的包
     * @param path 文件系统路径
     * @return 模块名或 None
     */
    private func findPackageByPath(path: String): Option<String> {
        // 向上查找包含 module.lisp 的目录
        match (this.findPackageDirectory(path)) {
            case Some(packageDir) =>
                // 使用 ModuleParser 解析模块目录获取模块名
                match (this.getModuleParser().parse(packageDir)) {
                    case Some(packageInfo) =>
                        // 构建模块名：org::packageName
                        Some(ModuleNamespace.buildModuleName(packageInfo.org, packageInfo.packageName))
                    case None => None
                }
            case None => None
        }
    }

    /**
     * 按名称导入模块
     * @param moduleName 模块名（如："ystyle::log.zlog"）
     * @param alias 可选的别名
     * @param symbols 要导入的符号列表（空表示全部）
     */
    private func importModuleByName(moduleName: String, alias: Option<String>, symbols: ArrayList<String>): LispValue {
        // 验证模块名格式
        if (!ModuleNamespace.isValidModuleName(moduleName)) {
            return LispValue.Error(XispError(ErrorType.ModuleError, "invalid module name '${moduleName}'"))
        }

        // 检查模块是否已加载
        match (this.getModuleRegistry().findModule(moduleName)) {
            case Some(module) =>
                // 模块已加载，直接使用
                this.bindModuleSymbols(module, alias, symbols)
            case None =>
                // 模块未加载，需要加载
                match (this.loadModule(moduleName)) {
                    case Some(module) =>
                        this.bindModuleSymbols(module, alias, symbols)
                    case None =>
                        LispValue.Error(XispError(ErrorType.ModuleError, "module '${moduleName}' not found"))
                }
        }
    }

    /**
     * 将模块的符号绑定到当前环境
     * @param module 模块
     * @param alias 可选的别名
     * @param symbols 要导入的符号列表（空表示全部）
     */
    private func bindModuleSymbols(module: Module, alias: Option<String>, symbols: ArrayList<String>): LispValue {
        let packageInfo = module.packageInfo
        let shortName = packageInfo.getShortName()

        // 确定使用的包名
        let bindingName = match (alias) {
            case Some(a) => a
            case None => shortName
        }

        if (symbols.isEmpty()) {
            // 导入所有导出的符号到包命名空间
            // 例如：(import ystyle::log.zlog) -> (zlog.init ...)
            for (symbolName in module.getAllExportedSymbols()) {
                match (module.getExportedSymbol(symbolName)) {
                    case Some(value) =>
                        // 使用 bindingName.symbolName 的形式（用 . 连接）
                        let qualifiedName = "${bindingName}.${symbolName}"
                        this.env.define(qualifiedName, value)
                    case None => ()
                }
            }

            Nil
        } else {
            // 只导入指定的符号
            for (symbolName in symbols) {
                match (module.getExportedSymbol(symbolName)) {
                    case Some(value) =>
                        // 使用 bindingName.symbolName 的形式（用 . 连接）
                        let qualifiedName = "${bindingName}.${symbolName}"
                        this.env.define(qualifiedName, value)
                    case None =>
                        // 符号未导出，忽略或报错
                        ()
                }
            }

            Nil
        }
    }

    /**
     * 加载模块
     * @param moduleName 模块名
     * @return 加载的模块，如果失败返回 None
     */
    private func loadModule(moduleName: String): Option<Module> {
        // 解析模块路径
        match (this.getModuleRegistry().resolveModulePath(moduleName)) {
            case Some(packagePath) =>
                // 解析 module.lisp
                match (this.parseModuleFile(packagePath)) {
                    case Some(packageInfo) =>
                        // 创建模块环境
                        let moduleEnv = this.env.createChild()

                        // 创建模块（先创建，用于 export）
                        let module = Module(moduleName, packageInfo, moduleEnv)

                        // 设置当前模块，以便 export 可以工作
                        let oldModule = this.currentModule
                        this.currentModule = Some(module)

                        // 加载包目录下的所有 .lisp 文件
                        let loaded = this.loadPackageFiles(packagePath, moduleEnv)

                        // 恢复之前的模块
                        this.currentModule = oldModule

                        if (loaded) {
                            module.isInitialized = true

                            // 注册模块
                            this.getModuleRegistry().registerModule(module)

                            Some(module)
                        } else {
                            None
                        }
                    case None => None
                }
            case None => None
        }
    }

    /**
     * 解析 module.lisp 文件
     * @param packagePath 模块目录路径
     * @return 解析的模块信息，失败返回 None
     */
    private func parseModuleFile(packagePath: String): Option<PackageInfo> {
        // 使用 ModuleParser 解析 module.lisp
        let parser = this.getModuleParser()
        parser.parse(packagePath)
    }

    /**
     * 加载包目录下的所有 .lisp 文件
     * @param packagePath 包目录路径
     * @param env 模块环境
     * @return 是否成功加载
     */
    private func loadPackageFiles(packagePath: String, env: Environment): Bool {
        // 使用 ModuleLoader 加载包
        let loader = ModuleLoader(this)

        // 加载包目录下的所有文件
        loader.loadPackage(packagePath, env)
    }

    /**
     * 特殊形式：export
     * 导出符号，使其在模块外可用
     *
     * 语法：(export symbol1 symbol2 ...)
     */
    func evalExport(expr: LispValue): LispValue {
        match (expr) {
            case Cons(cell) =>
                var current: LispValue = Cons(cell)
                let exportedSymbols = ArrayList<String>()

                while (!current.isNil()) {
                    if (let Cons(currCell) <- current && let Symbol(sym) <- currCell.car) {
                        exportedSymbols.add(sym)
                        current = currCell.cdr
                    } else {
                        break
                    }
                }

                if (exportedSymbols.isEmpty()) {
                    LispValue.Error(XispError(ErrorType.ArityMismatch, "export requires at least one symbol"))
                } else {
                    // 将符号添加到当前模块的导出列表
                    match (this.currentModule) {
                        case Some(module) =>
                            for (sym in exportedSymbols) {
                                module.exportSymbol(sym)
                            }
                            Nil
                        case None =>
                            // 如果没有当前模块，只返回 nil（在全局环境中 export 无效）
                            Nil
                    }
                }

            case _ => LispValue.Error(XispError(ErrorType.SyntaxError, "export requires a list of symbols"))
        }
    }
}

