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

        // 处理第一个表达式（应该是 package 表达式）
        if (exprs.isEmpty()) {
            None
        } else {
            this.extractPackageInfo(exprs[0], packagePath)
        }
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
                                    // 处理 (package org.name ...)
                                    packageInfo = this.parsePackageDeclaration(itemCell, packagePath)
                                case Symbol("export") =>
                                    // 处理 (export sym1 sym2 ...)
                                    if (let Some(info) <- packageInfo) {
                                        this.parseExportDeclaration(itemCell, info)
                                    }
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
     * 解析 (package org.name ...) 声明
     */
    private func parsePackageDeclaration(expr: ConsCell, packagePath: String): Option<PackageInfo> {
        let rest = expr.cdr

        match (rest) {
            case Cons(restCell) =>
                let packageName = restCell.car

                match (packageName) {
                    case Symbol(name) =>
                        // 解析包名：org.name 或 name
                        let parts = name.split('.')
                        let (org, pkg) = if (parts.size >= 2) {
                            (parts[0], name)
                        } else {
                            ("", name)
                        }

                        let packageInfo = PackageInfo(
                            name,
                            org,
                            pkg,
                            packagePath
                        )

                        // 解析元数据字段
                        this.parsePackageMetadata(restCell.cdr, packageInfo)

                        Some(packageInfo)
                    case _ => None
                }
            case _ => None
        }
    }

    /**
     * 解析包元数据字段
     * 处理 (version "0.1.0")、(description "...") 等
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
     * 解析 (export sym1 sym2 ...) 声明
     */
    private func parseExportDeclaration(expr: ConsCell, packageInfo: PackageInfo) {
        let rest = expr.cdr
        var current: LispValue = rest

        while (!current.isNil()) {
            match (current) {
                case Cons(currCell) =>
                    match (currCell.car) {
                        case Symbol(symName) =>
                            packageInfo.exports.add(symName)
                        case _ => ()
                    }
                    current = currCell.cdr
                case _ => break
            }
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
