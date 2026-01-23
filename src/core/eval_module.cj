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
     * 获取包解析器（如果未初始化则返回错误）
     */
    private func getPackageParser(): PackageParser {
        if (let Some(parser) <- this.packageParser) {
            parser
        } else {
            PackageParser(this)  // 不应该发生，但提供默认值
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
        match (expr) {
            case Cons(cell) =>
                if (cell.length() == 0) {
                    return Str("Error: import requires at least one argument")
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

                    case _ =>
                        Str("Error: invalid import syntax")
                }

            case _ => Str("Error: import requires a list")
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

            case _ => Str("Error: invalid import specification")
        }
    }

    /**
     * 解析简单导入：(module::name)
     * 或带别名的导入：(module::name :as alias)
     */
    private func parseSimpleImport(specCell: ConsCell): LispValue {
        let first = specCell.car
        let rest = specCell.cdr

        match (first) {
            case Symbol(moduleName) =>
                match (rest) {
                    case Cons(restCell) =>
                        // 检查是否有 :as
                        match (restCell.car) {
                            case Symbol(":as") =>
                                // (module::name :as alias)
                                match (restCell.cdr) {
                                    case Cons(aliasCell) =>
                                        match (aliasCell.car) {
                                            case Symbol(alias) =>
                                                this.importModuleByName(moduleName, Some(alias), ArrayList<String>())
                                            case _ => Str("Error: :as requires a symbol alias")
                                        }
                                    case _ => Str("Error: :as requires an alias")
                                }
                            case _ =>
                                // 只有模块名，普通导入
                                this.importModuleByName(moduleName, None, ArrayList<String>())
                        }
                    case _ =>
                        this.importModuleByName(moduleName, None, ArrayList<String>())
                }
            case _ => Str("Error: module name must be a symbol")
        }
    }

    /**
     * 解析别名导入：(module::name :as alias)
     */
    private func parseAliasImport(specCell: ConsCell): LispValue {
        // 这个方法实际上不会独立调用，因为在 parseSimpleImport 中已经处理
        Str("Error: :as must follow module name")
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
                    case _ => Str("Error: only requires a module name")
                }
            case _ => Str("Error: only requires module name and symbols")
        }
    }

    /**
     * 导入相对路径模块：("./local.module") 或 ("../parent.module")
     */
    private func importRelativeModule(relativePath: String): LispValue {
        // 获取当前文件的路径（从环境或 evaluator 获取）
        // 暂时返回错误，等文件系统集成
        Str("Error: relative import not yet implemented")
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
            return Str("Error: invalid module name '${moduleName}'")
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
                        Str("Error: module '${moduleName}' not found")
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
                        // 使用 bindingName.symbolName 的形式
                        let qualifiedName = "${bindingName}.${symbolName}"
                        this.env.define(qualifiedName, value)
                    case None => ()
                }
            }

            // 同时将包本身绑定为一个命名空间对象
            // 这样可以使用 zlog.init 的形式访问
            // TODO: 创建命名空间对象类型
            Nil
        } else {
            // 只导入指定的符号
            for (symbolName in symbols) {
                match (module.getExportedSymbol(symbolName)) {
                    case Some(value) =>
                        // 使用 bindingName.symbolName 的形式
                        let qualifiedName = "${bindingName}.${symbolName}"
                        this.env.define(qualifiedName, value)
                    case None =>
                        // 符号未导出，忽略或报错
                        ()
                }
            }
        }

        Nil
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
                // 解析 package.lisp
                match (this.parsePackageFile(packagePath)) {
                    case Some(packageInfo) =>
                        // 创建模块环境
                        let moduleEnv = this.env.createChild()

                        // 加载包目录下的所有 .lisp 文件
                        let loaded = this.loadPackageFiles(packagePath, moduleEnv)

                        if (loaded) {
                            // 创建模块
                            let module = Module(moduleName, packageInfo, moduleEnv)
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
     * 解析 package.lisp 文件
     * @param packagePath 包目录路径
     * @return 解析的包信息，失败返回 None
     */
    private func parsePackageFile(packagePath: String): Option<PackageInfo> {
        let packageFilePath = Path(packagePath).join("package.lisp").toString()

        // 读取文件内容
        if (!exists(packageFilePath)) {
            return None
        }

        // TODO: 实际解析 Lisp 文件
        // 暂时返回基本的包信息
        let (org, packageName) = this.getModuleRegistry().parseModuleName(packagePath)
        let packageInfo = PackageInfo(
            packageName,
            org,
            packageName,
            packagePath
        )

        Some(packageInfo)
    }

    /**
     * 加载包目录下的所有 .lisp 文件
     * @param packagePath 包目录路径
     * @param env 模块环境
     * @return 是否成功加载
     */
    private func loadPackageFiles(packagePath: String, env: Environment): Bool {
        // TODO: 实现文件遍历和加载逻辑
        // 1. 列出目录下所有 .lisp 文件
        // 2. 忽略以 . 开头的文件
        // 3. 按文件名排序
        // 4. 依次加载每个文件

        true  // 暂时返回 true
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
                    match (current) {
                        case Cons(currCell) =>
                            match (currCell.car) {
                                case Symbol(sym) =>
                                    exportedSymbols.add(sym)
                                case _ => ()
                            }
                            current = currCell.cdr
                        case _ => break
                    }
                }

                if (exportedSymbols.isEmpty()) {
                    Str("Error: export requires at least one symbol")
                } else {
                    // TODO: 将符号添加到当前模块的导出列表
                    // 需要追踪当前模块
                    Nil
                }

            case _ => Str("Error: export requires a list of symbols")
        }
    }

    /**
     * 特殊形式：package
     * 定义包元数据
     *
     * 语法：(package org.name
     *          (version "0.1.0")
     *          (description "..."))
     */
    func evalPackage(expr: LispValue): LispValue {
        match (expr) {
            case Cons(cell) =>
                let first = cell.car

                match (first) {
                    case Symbol(packageName) =>
                        // 解析包名：org.name 或 name
                        let parts = packageName.split('.')
                        let (org, pkg) = if (parts.size >= 2) {
                            (parts[0], packageName)
                        } else {
                            ("", packageName)
                        }

                        // TODO: 创建并注册包信息
                        // 需要解析后面的元数据字段
                        Nil
                    case _ => Str("Error: package requires a package name")
                }
            case _ => Str("Error: package requires a package name")
        }
    }
}
