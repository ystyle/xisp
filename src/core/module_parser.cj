package ystyle::xisp.core

import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import std.collection.ArrayList
import std.fs.*

/**
 * module.lisp 文件解析器
 *
 * 解析模块元数据文件，提取模块名、版本、依赖等信息
 */
public class ModuleParser {
    private var evaluator: Evaluator

    public init(evaluator: Evaluator) {
        this.evaluator = evaluator
    }

    /**
     * 解析 module.lisp 文件
     * @param modulePath 模块目录路径
     * @return 解析的模块信息，失败返回 None
     */
    public func parse(modulePath: String): Option<PackageInfo> {
        let moduleFilePath = Path(modulePath).join("module.lisp").toString()

        if (!exists(moduleFilePath)) {
            return None
        }

        // 读取文件内容
        try {
            let bytes = File.readFrom(moduleFilePath)
            let content = String.fromUtf8(bytes)
            this.parseModuleContent(content, modulePath)
        } catch (_: FSException) {
            None
        } catch (_: Exception) {
            None
        }
    }

    /**
     * 解析 module.lisp 内容
     */
    private func parseModuleContent(content: String, modulePath: String): Option<PackageInfo> {
        // 使用词法分析器和语法分析器
        let lexer = Lexer(content)
        let tokens = lexer.tokenize()
        let parser = Parser(tokens)

        // 解析所有表达式
        let exprs = parser.parse()

        // 处理所有表达式，找到 module 和 export 声明
        if (exprs.isEmpty()) {
            None
        } else {
            this.extractModuleInfoFromExprs(exprs, modulePath)
        }
    }

    /**
     * 从多个表达式提取模块信息
     */
    private func extractModuleInfoFromExprs(exprs: ArrayList<LispValue>, modulePath: String): Option<PackageInfo> {
        var moduleInfo: Option<PackageInfo> = None

        for (expr in exprs) {
            match (expr) {
                case Cons(cell) =>
                    let item = cell.car

                    match (item) {
                        case Symbol("module") =>
                            // 处理 (module moduleName ...)
                            moduleInfo = this.parseModuleDeclaration(cell, modulePath)
                        case _ => ()
                    }
                case _ => ()
            }
        }

        moduleInfo
    }

    /**
     * 从解析的表达式提取模块信息
     */
    private func extractModuleInfo(expr: LispValue, modulePath: String): Option<PackageInfo> {
        // module.lisp 格式：
        // (module name
        //   (version "0.1.0")
        //   (description "...")
        //   ...)
        //
        // (export sym1 sym2 ...)

        match (expr) {
            case Cons(cell) =>
                this.processModuleExpression(cell, modulePath)
            case _ => None
        }
    }

    /**
     * 处理 module 表达式
     */
    private func processModuleExpression(cell: ConsCell, modulePath: String): Option<PackageInfo> {
        var moduleInfo: Option<PackageInfo> = None
        var current: LispValue = Cons(cell)

        while (!current.isNil()) {
            match (current) {
                case Cons(currCell) =>
                    let item = currCell.car

                    match (item) {
                        case Cons(itemCell) =>
                            match (itemCell.car) {
                                case Symbol("module") =>
                                    // 处理 (module moduleName ...)
                                    moduleInfo = this.parseModuleDeclaration(itemCell, modulePath)
                                case _ => ()
                            }
                        case _ => ()
                    }

                    current = currCell.cdr
                case _ => break
            }
        }

        moduleInfo
    }

    /**
     * 解析 (module name ...) 声明
     * 注意：模块名可以是字符串或符号，组织从 organization 字段读取
     */
    private func parseModuleDeclaration(expr: ConsCell, modulePath: String): Option<PackageInfo> {
        let rest = expr.cdr

        match (rest) {
            case Cons(restCell) =>
                let moduleName = restCell.car

                match (moduleName) {
                    case Str(name) =>
                        // 模块名是字符串形式
                        this.parseModuleWithName(name, restCell.cdr, modulePath)
                    case Symbol(name) =>
                        // 模块名是符号形式（不含 . 的简单模块名）
                        this.parseModuleWithName(name, restCell.cdr, modulePath)
                    case _ => None
                }
            case _ => None
        }
    }

    /**
     * 使用模块名创建 PackageInfo
     */
    private func parseModuleWithName(name: String, metadata: LispValue, modulePath: String): Option<PackageInfo> {
        // 先扫描元数据找到 organization
        let org = this.extractOrganization(metadata)

        // 检测子目录来构造完整包名
        let fullPackageName = this.detectPackageName(name, modulePath)

        // 创建 PackageInfo（类名暂时保持不变）
        // name 是当前目录名（模块名），不是完整路径
        let moduleInfo = PackageInfo(
            fullPackageName,   // 完整包名（包含子目录）
            org,    // 从 organization 字段解析
            fullPackageName,   // packageName（完整包名）
            modulePath
        )

        // 解析其他元数据字段
        this.parseModuleMetadata(metadata, moduleInfo)

        Some(moduleInfo)
    }

    /**
     * 从元数据中提取 organization 字段
     */
    private func extractOrganization(expr: LispValue): String {
        var current: LispValue = expr

        while (!current.isNil()) {
            match (current) {
                case Cons(currCell) =>
                    let item = currCell.car

                    match (item) {
                        case Cons(itemCell) =>
                            match (itemCell.car) {
                                case Symbol("organization") =>
                                    match (this.parseStringField(itemCell.cdr)) {
                                        case Some(value) => return value
                                        case None => ()
                                    }
                                case _ => ()
                            }
                        case _ => ()
                    }

                    current = currCell.cdr
                case _ => break
            }
        }

        ""  // 默认空组织
    }

    /**
     * 解析模块元数据字段
     * 处理 (version "0.1.0")、(description "...") 等
     * 注意：organization 已经在创建 PackageInfo 之前处理
     */
    private func parseModuleMetadata(expr: LispValue, moduleInfo: PackageInfo) {
        var current: LispValue = expr

        while (!current.isNil()) {
            match (current) {
                case Cons(currCell) =>
                    let item = currCell.car

                    match (item) {
                        case Cons(itemCell) =>
                            match (itemCell.car) {
                                case Symbol("version") =>
                                    match (this.parseStringField(itemCell.cdr)) {
                                        case Some(value) => moduleInfo.version = value
                                        case None => ()
                                    }
                                case Symbol("description") =>
                                    match (this.parseStringField(itemCell.cdr)) {
                                        case Some(value) => moduleInfo.description = value
                                        case None => ()
                                    }
                                case Symbol("author") =>
                                    match (this.parseStringField(itemCell.cdr)) {
                                        case Some(value) => moduleInfo.author = value
                                        case None => ()
                                    }
                                case Symbol("homepage") =>
                                    match (this.parseStringField(itemCell.cdr)) {
                                        case Some(value) => moduleInfo.homepage = value
                                        case None => ()
                                    }
                                case Symbol("license") =>
                                    match (this.parseStringField(itemCell.cdr)) {
                                        case Some(value) => moduleInfo.license = value
                                        case None => ()
                                    }
                                case Symbol("dependencies") =>
                                    this.parseDependencies(itemCell.cdr, moduleInfo)
                                case _ => ()
                            }
                        case _ => ()
                    }

                    current = currCell.cdr
                case _ => break
            }
        }
    }

    /**
     * 解析字符串字段
     */
    private func parseStringField(expr: LispValue): Option<String> {
        match (expr) {
            case Cons(cell) =>
                match (cell.car) {
                    case Str(value) => Some(value)
                    case _ => None
                }
            case _ => None
        }
    }

    /**
     * 解析依赖列表
     * 新格式：(dependencies (org::module "version") ...)
     */
    private func parseDependencies(expr: LispValue, moduleInfo: PackageInfo) {
        match (expr) {
            case Cons(cell) =>
                var current: LispValue = Cons(cell)

                while (!current.isNil()) {
                    match (current) {
                        case Cons(currCell) =>
                            let dep = currCell.car

                            match (dep) {
                                case Cons(depCell) =>
                                    match (depCell.car) {
                                        case Symbol(depName) =>
                                            // depName 格式: org::module (新格式)
                                            match (this.parseStringField(depCell.cdr)) {
                                                case Some(version) =>
                                                    let dependency = Dependency(
                                                        depName,
                                                        version
                                                    )
                                                    moduleInfo.dependencies.add(dependency)
                                                case None => ()
                                            }
                                        case _ => ()
                                    }
                                case _ => ()
                            }

                            current = currCell.cdr
                        case _ => break
                    }
                }
            case _ => ()
        }
    }

    /**
     * 从目录路径推断模块信息
     * 用于没有 module.lisp 的情况（向后兼容）
     */
    public func inferModuleInfo(modulePath: String): Option<PackageInfo> {
        // 从路径提取模块名（目录名）
        let pathObj = Path(modulePath)
        let parentDir = pathObj.parent.toString()
        let dirName = pathObj.fileName

        // 判断是否有组织前缀
        let grandParent = Path(parentDir).fileName

        let (org, mod) = if (grandParent.isEmpty() || grandParent == ".") {
            ("", dirName)
        } else {
            (grandParent, dirName)
        }

        let moduleInfo = PackageInfo(
            mod,
            org,
            mod,
            modulePath
        )

        Some(moduleInfo)
    }

    /**
     * 检测包名（包括子目录）
     * 如果目录下只有一个子目录，则包名为 "name.子目录名"
     * 否则包名为 name
     */
    private func detectPackageName(name: String, modulePath: String): String {
        try {
            let entries = Directory.readFrom(Path(modulePath))
            var subDirs = ArrayList<String>()

            for (entry in entries) {
                if (entry.isDirectory() && !entry.name.startsWith(".")) {
                    subDirs.add(entry.name)
                }
            }

            // 如果只有一个子目录，则包名包含子目录
            if (subDirs.size == 1) {
                "${name}.${subDirs[0]}"
            } else {
                name
            }
        } catch (e: FSException) {
            name
        }
    }
}
