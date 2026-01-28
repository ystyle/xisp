package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList
import std.fs.*

/**
 * ScriptLoader - 脚本/REPL 上下文的加载器
 *
 * 支持：
 * - (import module-name) → 同 ModuleLoader，走 ModuleSource
 * - (import "./xxx.lisp") → 文件导入（无前缀）
 * - (import "./xxx") → 目录包导入（有前缀）
 *
 * 使用场景：CLI 脚本、REPL、嵌入式 eval
 */
public class ScriptLoader <: Loader {
    private var moduleLoader: ModuleLoader
    private var evaluator: Evaluator

    public init(moduleLoader: ModuleLoader, evaluator: Evaluator) {
        this.moduleLoader = moduleLoader
        this.evaluator = evaluator
    }

    /**
     * Loader 接口：按模块名导入
     * 委托给 ModuleLoader
     */
    public func loadByName(moduleName: String): LispValue {
        this.moduleLoader.loadByName(moduleName)
    }

    /**
     * Loader 接口：按相对路径导入
     */
    public func loadRelative(relativePath: String): LispValue {
        let baseDir = match (this.evaluator.currentFilePath) {
            case Some(filePath) =>
                Path(filePath).parent.toString()
            case None => "."
        }

        if (relativePath.endsWith(".lisp")) {
            this.loadRelativeFile(baseDir, relativePath)
        } else {
            this.loadRelativePackage(baseDir, relativePath)
        }
    }

    /**
     * 加载单个文件（无前缀）
     */
    private func loadRelativeFile(baseDir: String, relativePath: String): LispValue {
        var normalizedPath = relativePath
        if (normalizedPath.startsWith("./")) {
            normalizedPath = normalizedPath[2..normalizedPath.size]
        }

        let fullPath = Path(baseDir).join(normalizedPath).toString()

        if (!exists(fullPath)) {
            return LispValue.Error(XispError(ErrorType.IOError,
                "file not found at '${relativePath}'"))
        }

        // 检查是否是模块（不能相对导入模块）
        let parentDir = Path(fullPath).parent.toString()
        let moduleFile = Path(parentDir).join("module.lisp").toString()
        if (exists(moduleFile)) {
            return LispValue.Error(XispError(ErrorType.ModuleError,
                "Cannot import module '${relativePath}' using relative import. Use (import moduleName) instead."))
        }

        if (this.moduleLoader.loadFile(fullPath, this.evaluator.env)) {
            Nil
        } else {
            LispValue.Error(XispError(ErrorType.IOError,
                "failed to load file '${relativePath}'"))
        }
    }

    /**
     * 加载目录包（有前缀）
     */
    private func loadRelativePackage(baseDir: String, relativePath: String): LispValue {
        var normalizedPath = relativePath
        if (normalizedPath.startsWith("./")) {
            normalizedPath = normalizedPath[2..normalizedPath.size]
        }

        let pathPart = normalizedPath.replace(".", "/")
        let fullPath = Path(baseDir).join(pathPart).toString()

        if (!exists(fullPath)) {
            return LispValue.Error(XispError(ErrorType.IOError,
                "directory not found at '${relativePath}'"))
        }

        // 检查是否是模块（不能相对导入模块）
        let moduleFile = Path(fullPath).join("module.lisp").toString()
        if (exists(moduleFile)) {
            return LispValue.Error(XispError(ErrorType.ModuleError,
                "Cannot import module '${relativePath}' using relative import. Use (import moduleName) instead."))
        }

        let prefix = this.extractPrefix(normalizedPath)
        let packageEnv = this.evaluator.env.createChild()

        if (this.moduleLoader.loadPackage(fullPath, packageEnv)) {
            this.bindPackageSymbols(packageEnv, prefix)
            Nil
        } else {
            LispValue.Error(XispError(ErrorType.IOError,
                "failed to load package '${relativePath}'"))
        }
    }

    /**
     * 从相对路径提取前缀（最后一级）
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
     */
    private func bindPackageSymbols(packageEnv: Environment, prefix: String): Unit {
        let exportedBindings = packageEnv.getExportedBindings()
        if (exportedBindings.isEmpty()) {
            let allKeys = packageEnv.getKeys()
            for (key in allKeys) {
                if (!key.startsWith("_")) {
                    let value = packageEnv.lookup(key)
                    let prefixedKey = "${prefix}.${key}"
                    this.evaluator.env.define(prefixedKey, value)
                }
            }
        } else {
            for ((key, value) in exportedBindings) {
                let prefixedKey = "${prefix}.${key}"
                this.evaluator.env.define(prefixedKey, value)
            }
        }
    }
}
