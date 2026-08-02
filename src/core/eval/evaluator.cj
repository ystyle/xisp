package ystyle::xisp.core.eval
import ystyle::xisp.core.module.*

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 求值器 - Lisp 表达式求值
 */
public class Evaluator {
    public var env: Environment
    var stackDepth: Int64 = 0
    var maxStackDepth: Int64 = 1000  // 默认栈深度限制
    var funcChecker: ?((String) -> Bool) = None  // 函数权限检查回调
    var pathChecker: ?((String, Bool) -> Bool) = None  // 路径权限检查回调
    var moduleLoadChecker: ?(() -> Bool) = None  // 模块加载权限检查回调
    var macroExpandDepth: Int64 = 0  // 宏展开深度
    var maxMacroExpandDepth: Int64 = 100  // 最大宏展开深度
    var inMacroExpandCall: Bool = false  // 是否在 macroexpand/macroexpand-all 调用中（不执行展开后的结果）

    // 性能探针（调试用）
    // 当前执行的文件路径（用于相对路径导入）
    public var currentFilePath: Option<String> = None

    // 模块系统相关字段（延迟初始化以避免循环依赖）
    public var moduleRegistry: Option<ModuleRegistry> = None
    public var moduleLoader: Option<ModuleLoader> = None
    public var moduleParser: Option<ModuleParser> = None
    public var currentModule: Option<Module> = None  // 当前正在求值的模块

    // 当前 Loader（模块上下文用 ModuleLoader，脚本/REPL 用 ScriptLoader）
    public var currentLoader: Option<Loader> = None

    // 编译器和虚拟机（预留）
    var compiler: Compiler = Compiler()
    var vm: VM = VM()

    // 算术操作符守卫缓存（+ - * / < > =）
    // arithGuardVersion 匹配 env.arithVersion.value 时缓存有效
    var arithGuardVersion: Int64 = -1
    var arithGuardCache: Array<Bool> = Array<Bool>(7) { _ => false }

    // 性能探针（调试用）

    /**
     * 设置编译器
     */
    public func setCompiler(c: Compiler) {
        this.compiler = c
    }

    /**
     * 设置虚拟机
     */
    public func setVM(v: VM) {
        this.vm = v
    }

    // 调用栈追踪
    public var callStack: ArrayList<String> = ArrayList<String>()

    /**
     * 压入调用栈帧
     */
    public func pushFrame(frame: String): Unit {
        this.callStack.add(frame)
    }

    /**
     * 弹出调用栈帧
     */
    public func popFrame(): Unit {
        if (!this.callStack.isEmpty()) {
            this.callStack.remove((this.callStack.size - 1)..this.callStack.size)
        }
    }

    /**
     * 将当前调用栈注入到错误中（如果错误栈为空）
     */
    public func injectStack(result: LispValue): LispValue {
        match (result) {
            case Error(err) =>
                if (err.stack.isEmpty()) {
                    for (frame in this.callStack) {
                        err.pushFrame(frame)
                    }
                }
                result
            case _ => result
        }
    }

    public init(env: Environment) {
        this.env = env
        // 不在构造函数中初始化模块系统，避免循环依赖
    }

    /**
     * 初始化模块系统（在构造对象后调用）
     */
    public func initModuleSystem(): Unit {
        if (this.moduleRegistry.isNone()) {
            let registry = ModuleRegistry()

            this.moduleRegistry = Some(registry)

            let parser = ModuleParser()
            let loader = ModuleLoader(this)
            loader.setModuleSystem(registry, parser)

            // 用 registry 的搜索路径创建默认 FileSystemSource
            let searchPaths = registry.getSearchPaths()
            let fsSource = FileSystemSource(searchPaths)
            loader.setSource(fsSource)

            this.moduleLoader = Some(loader)
            this.moduleParser = Some(parser)

            // 默认使用 ModuleLoader（模块上下文）
            this.currentLoader = Some(loader)
        }
    }

    /**
     * 设置最大栈深度
     */
    public func setMaxStackDepth(depth: Int64): Unit {
        this.maxStackDepth = depth
    }

    /**
     * 设置函数权限检查回调
     * @param checker 检查函数，返回 true 表示允许调用，false 表示禁止
     */
    public func setFuncChecker(checker: (String) -> Bool): Unit {
        this.funcChecker = Some(checker)
    }

    /**
     * 设置路径权限检查回调
     * @param checker 检查函数，参数为 和 isWrite(是否写入)，返回 true 表示允许访问，false 表示禁止
     */
    public func setPathChecker(checker: (String, Bool) -> Bool): Unit {
        this.pathChecker = Some(checker)
    }

    /**
     * 设置模块加载权限检查回调
     * @param checker 检查函数，返回 true 表示允许加载，false 表示禁止
     */
    public func setModuleLoadChecker(checker: () -> Bool): Unit {
        this.moduleLoadChecker = Some(checker)
    }
}
