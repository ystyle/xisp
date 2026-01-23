package ystyle::xisp.core

import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import std.collection.ArrayList
import std.fs.*

/**
 * package.lisp 文件解析器
 *
 * 解析包元数据文件，提取包名、版本、依赖等信息
 */
public class PackageParser {
    private var evaluator: Evaluator

    public init(evaluator: Evaluator) {
        this.evaluator = evaluator
    }

    /**
     * 解析 package.lisp 文件
     * @param packagePath 包目录路径
     * @return 解析的包信息，失败返回 None
     */
    public func parse(packagePath: String): Option<PackageInfo> {
        let packageFilePath = Path(packagePath).join("package.lisp").toString()

        if (!exists(packageFilePath)) {
            return None
        }

        // 读取文件内容
        try {
            let bytes = File.readFrom(packageFilePath)
            let content = String.fromUtf8(bytes)
            this.parsePackageContent(content, packagePath)
        } catch (_: FSException) {
            None
        } catch (_: Exception) {
            None
        }
    }

    /**
     * 解析 package.lisp 内容
     */
    private func parsePackageContent(content: String, packagePath: String): Option<PackageInfo> {
        // 使用词法分析器和语法分析器
        let lexer = Lexer(content)
        let tokens = lexer.tokenize()
        let parser = Parser(tokens)

        // 解析所有表达式
        let exprs = parser.parse()

        // 处理所有表达式，找到 package 和 export 声明
        if (exprs.isEmpty()) {
            None
        } else {
            this.extractPackageInfoFromExprs(exprs, packagePath)
        }
    }

    /**
     * 从多个表达式提取包信息
     */
    private func extractPackageInfoFromExprs(exprs: ArrayList<LispValue>, packagePath: String): Option<PackageInfo> {
        var packageInfo: Option<PackageInfo> = None

        for (expr in exprs) {
            match (expr) {
                case Cons(cell) =>
                    let item = cell.car

                    match (item) {
                        case Symbol("package") =>
                            // 处理 (package "package.name" ...)
                            packageInfo = this.parsePackageDeclaration(cell, packagePath)
                        case _ => ()
                    }
                case _ => ()
            }
        }

        packageInfo
    }

    /**
     * 从解析的表达式提取包信息
     */
    private func extractPackageInfo(expr: LispValue, packagePath: String): Option<PackageInfo> {
        // package.lisp 格式：
        // (package org.name
        //   (version "0.1.0")
        //   (description "...")
        //   ...)
        //
        // (export sym1 sym2 ...)

        match (expr) {
            case Cons(cell) =>
                this.processPackageExpression(cell, packagePath)
            case _ => None
        }
    }

    /**
     * 处理 package 表达式
     */
    private func processPackageExpression(cell: ConsCell, packagePath: String): Option<PackageInfo> {
        var packageInfo: Option<PackageInfo> = None
        var current: LispValue = Cons(cell)

        while (!current.isNil()) {
            match (current) {
                case Cons(currCell) =>
                    let item = currCell.car

                    match (item) {
                        case Cons(itemCell) =>
                            match (itemCell.car) {
                                case Symbol("package") =>
                                    // 处理 (package "package.name" ...)
                                    packageInfo = this.parsePackageDeclaration(itemCell, packagePath)
                                case _ => ()
                            }
                        case _ => ()
                    }

                    current = currCell.cdr
                case _ => break
            }
        }

        packageInfo
    }

    /**
     * 解析 (package "package.name" ...) 声明
     * 注意：包名可以是字符串或符号，组织从 organization 字段读取
     */
    private func parsePackageDeclaration(expr: ConsCell, packagePath: String): Option<PackageInfo> {
        let rest = expr.cdr

        match (rest) {
            case Cons(restCell) =>
                let packageName = restCell.car

                match (packageName) {
                    case Str(name) =>
                        // 包名是字符串形式
                        this.parsePackageWithName(name, restCell.cdr, packagePath)
                    case Symbol(name) =>
                        // 包名是符号形式（不含 . 的简单包名）
                        this.parsePackageWithName(name, restCell.cdr, packagePath)
                    case _ => None
                }
            case _ => None
        }
    }

    /**
     * 使用包名创建 PackageInfo
     */
    private func parsePackageWithName(name: String, metadata: LispValue, packagePath: String): Option<PackageInfo> {
        // 先扫描元数据找到 organization
        let org = this.extractOrganization(metadata)

        // 创建 PackageInfo
        let packageInfo = PackageInfo(
            name,   // 完整包名（不含组织前缀）
            org,    // 从 organization 字段解析
            name,   // packageName
            packagePath
        )

        // 解析其他元数据字段
        this.parsePackageMetadata(metadata, packageInfo)

        Some(packageInfo)
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
     * 解析包元数据字段
     * 处理 (version "0.1.0")、(description "...") 等
     * 注意：organization 已经在创建 PackageInfo 之前处理
     */
    private func parsePackageMetadata(expr: LispValue, packageInfo: PackageInfo) {
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
                                        case Some(value) => packageInfo.version = value
                                        case None => ()
                                    }
                                case Symbol("description") =>
                                    match (this.parseStringField(itemCell.cdr)) {
                                        case Some(value) => packageInfo.description = value
                                        case None => ()
                                    }
                                case Symbol("author") =>
                                    match (this.parseStringField(itemCell.cdr)) {
                                        case Some(value) => packageInfo.author = value
                                        case None => ()
                                    }
                                case Symbol("homepage") =>
                                    match (this.parseStringField(itemCell.cdr)) {
                                        case Some(value) => packageInfo.homepage = value
                                        case None => ()
                                    }
                                case Symbol("license") =>
                                    match (this.parseStringField(itemCell.cdr)) {
                                        case Some(value) => packageInfo.license = value
                                        case None => ()
                                    }
                                case Symbol("dependencies") =>
                                    this.parseDependencies(itemCell.cdr, packageInfo)
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
     * 格式：(dependencies (org.dep "version") ...)
     */
    private func parseDependencies(expr: LispValue, packageInfo: PackageInfo) {
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
                                            match (this.parseStringField(depCell.cdr)) {
                                                case Some(version) =>
                                                    let dependency = Dependency(
                                                        depName,
                                                        version
                                                    )
                                                    packageInfo.dependencies.add(dependency)
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
     * 从目录路径推断包信息
     * 用于没有 package.lisp 的情况（向后兼容）
     */
    public func inferPackageInfo(packagePath: String): Option<PackageInfo> {
        // 从路径提取包名
        let pathObj = Path(packagePath)
        let parentDir = pathObj.parent.toString()
        let dirName = pathObj.fileName

        // 判断是否有组织前缀
        let grandParent = Path(parentDir).fileName

        let (org, pkg) = if (grandParent.isEmpty() || grandParent == ".") {
            ("", dirName)
        } else {
            (grandParent, dirName)
        }

        let packageInfo = PackageInfo(
            pkg,
            org,
            pkg,
            packagePath
        )

        Some(packageInfo)
    }
}
