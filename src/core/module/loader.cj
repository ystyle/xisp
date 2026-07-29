package ystyle::xisp.core.module

import ystyle::xisp.types.*

/**
 * Loader 接口
 *
 * 定义模块/脚本加载的策略层接口。
 * - ModuleLoader：模块上下文，只支持按名导入 (import module-name)
 * - ScriptLoader：脚本/REPL 上下文，额外支持文件导入 (import "./xxx")
 */
public interface Loader {
    /**
     * 按模块名导入
     *
     * 所有 Loader 实现行为一致：
     * 通过 ModuleSource 查找模块元数据，加载文件，注册模块，绑定符号。
     *
     * @param moduleName 模块名（如 "pkg1", "ystyle::log"）
     * @return 导入结果（通常为 Nil）
     */
    func loadByName(moduleName: String): LispValue

    /**
     * 按相对路径导入
     *
     * - ModuleLoader：返回错误（模块上下文不支持相对导入）
     * - ScriptLoader：解析文件路径并加载
     *
     * @param relativePath 相对路径（如 "./utils.lisp", "./helpers"）
     * @return 导入结果
     */
    func loadRelative(relativePath: String): LispValue
}
