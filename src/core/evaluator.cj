package ystyle::xisp.core

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 求值器 - Lisp 表达式求值
 */
public class Evaluator {
    var env: Environment
    var stackDepth: Int64 = 0
    var maxStackDepth: Int64 = 1000  // 默认栈深度限制
    var funcChecker: ?((String) -> Bool) = None  // 函数权限检查回调
    var pathChecker: ?((String, Bool) -> Bool) = None  // 路径权限检查回调
    var moduleLoadChecker: ?(() -> Bool) = None  // 模块加载权限检查回调
    var macroExpandDepth: Int64 = 0  // 宏展开深度
    var maxMacroExpandDepth: Int64 = 100  // 最大宏展开深度

    // 当前执行的文件路径（用于相对路径导入）
    public var currentFilePath: Option<String> = None

    // 模块系统相关字段（延迟初始化以避免循环依赖）
    public var moduleRegistry: Option<ModuleRegistry> = None
    public var moduleLoader: Option<ModuleLoader> = None
    public var packageParser: Option<PackageParser> = None
    public var currentModule: Option<Module> = None  // 当前正在求值的模块

    public init(env: Environment) {
        this.env = env
        // 不在构造函数中初始化模块系统，避免循环依赖
    }

    /**
     * 初始化模块系统（在构造对象后调用）
     */
    public func initModuleSystem() {
        if (this.moduleRegistry.isNone()) {
            let registry = ModuleRegistry()

            // 添加默认搜索路径
            registry.addSearchPath("./examples")  // 添加 examples 目录用于测试和示例

            this.moduleRegistry = Some(registry)
            this.moduleLoader = Some(ModuleLoader(this))
            this.packageParser = Some(PackageParser(this))
        }
    }

    /**
     * 设置最大栈深度
     */
    public func setMaxStackDepth(depth: Int64) {
        this.maxStackDepth = depth
    }

    /**
     * 设置函数权限检查回调
     * @param checker 检查函数，返回 true 表示允许调用，false 表示禁止
     */
    public func setFuncChecker(checker: (String) -> Bool) {
        this.funcChecker = Some(checker)
    }

    /**
     * 设置路径权限检查回调
     * @param checker 检查函数，参数为 和 isWrite(是否写入)，返回 true 表示允许访问，false 表示禁止
     */
    public func setPathChecker(checker: (String, Bool) -> Bool) {
        this.pathChecker = Some(checker)
    }

    /**
     * 设置模块加载权限检查回调
     * @param checker 检查函数，返回 true 表示允许加载，false 表示禁止
     */
    public func setModuleLoadChecker(checker: () -> Bool) {
        this.moduleLoadChecker = Some(checker)
    }
}
