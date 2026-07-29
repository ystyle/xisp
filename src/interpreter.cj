package ystyle::xisp

import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*
import ystyle::xisp.core.module.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.bridge.*
import ystyle::xisp.repl.*
import std.collection.ArrayList
import std.sync.*
import std.time.*
import std.runtime.*
import std.collection.HashSet
import std.fs.*

/**
 * Lisp 解释器
 * 提供完整的 Lisp 执行环境，包括求值器、环境和桥接层
 */
public class LispInterpreter {
    private var env: Environment
    private var evaluator: Evaluator
    public var bridge: BridgeManager

    // ==================== 配置字段 ====================

    /**
     * 是否显示 Banner
     */
    private var showBanner: Bool = true

    /**
     * 是否显示详细信息
     */
    private var verbose: Bool = false

    /**
     * 是否显示调试信息
     */
    private var debug: Bool = false

    /**
     * 自定义输出函数
     */
    private var outputFn: (String) -> Unit = { s => println(s) }

    // ==================== 沙箱安全字段 ====================

    /**
     * 是否启用沙箱
     */
    private var sandboxEnabled: Bool = false

    /**
     * 调用栈深度限制
     */
    private var maxStackDepth: Int64 = 1000

    /**
     * 执行超时限制
     */
    private var timeout: Option<Duration> = None

    /**
     * 内存限制（字节）
     */
    private var maxMemory: Option<Int64> = None

    /**
     * 初始内存（用于计算增量）
     */
    private var initialMemory: Int64 = 0

    /**
     * 禁止的函数集合（黑名单）
     */
    private var blockedFunctions: ArrayList<String> = ArrayList<String>()

    /**
     * 允许的函数集合（白名单，None 表示没有白名单）
     */
    private var allowedFunctions: Option<ArrayList<String>> = None

    /**
     * 是否允许文件写入
     */
    private var allowFileWrite: Bool = true

    /**
     * 是否允许加载模块
     */
    private var allowModuleLoad: Bool = true

    /**
     * 允许的文件路径（白名单）
     */
    private var allowedPaths: ArrayList<String> = ArrayList<String>()

    /**
     * 当前执行的 Future（用于超时控制）
     */
    private var currentFuture: ?Future<LispValue> = None

    /**
     * 创建新的 Lisp 解释器实例（默认配置）
     */
    public init() {
        this.env = Environment()
        this.evaluator = Evaluator(this.env)
        this.bridge = BridgeManager(this.env)

        // 设置 path 检查回调（用于 std.io 的路径权限）
        this.bridge.setPathChecker({ path, isWrite => this.isPathAllowed(path, isWrite) })

        // 设置函数权限检查回调
        this.evaluator.setFuncChecker({ funcName => this.isFunctionAllowed(funcName) })

        // 设置路径权限检查回调
        this.evaluator.setPathChecker({ path, isWrite => this.isPathAllowed(path, isWrite) })

        // 设置模块加载权限检查回调
        this.evaluator.setModuleLoadChecker({ => this.isModuleLoadAllowed() })

        // 注册内置函数
        BuiltinFunctions.registerAll(this.env)

        // 注册标准桥接函数
        this.bridge.registerStdIO()
        this.bridge.registerStdFS()
        Bridge.registerStdCollection(this.env)  // 注册现代化语法支持
    }

    /**
     * 使用选项配置创建解释器
     * @param opts 选项数组
     *
     * 示例：
     * ```lisp
     * let interpreter = LispInterpreter([
     *     withChineseKeywords(),
     *     withStdLib(),
     *     withVerboseMode()
     * ])
     * ```
     */
    public init(opts: Array<InterpreterOption>) {
        this()
        // 预初始化模块系统，确保 withModulePath 等选项能生效
        this.evaluator.initModuleSystem()
        // 应用所有选项
        for (opt in opts) {
            opt(this)
        }
    }

    // ==================== 关键字别名管理 ====================

    /**
     * 注册关键字别名
     * @param alias 别名（如："定义"）
     * @param original 原始关键字（如："define"）
     */
    public func registerKeywordAlias(alias: String, original: String): Unit {
        this.env.registerKeywordAlias(alias, original)
        if (this.verbose) {
            this.outputFn("注册关键字别名: ${alias} -> ${original}")
        }
    }

    // ==================== 标准库模块注册 ====================

    /**
     * 注册 std.io 模块
     */
    public func registerStdIO(): Unit {
        this.bridge.registerStdIO()
        if (this.verbose) {
            this.outputFn("启用 std.io 模块")
        }
    }

    /**
     * 注册 std.fs 模块
     */
    public func registerStdFS(): Unit {
        this.bridge.registerStdFS()
        if (this.verbose) {
            this.outputFn("启用 std.fs 模块")
        }
    }

    /**
     * 注册 std.collection 模块（预留）
     */
    public func registerStdCollection(): Unit {
        // TODO: 实现 collection 模块注册
        if (this.verbose) {
            this.outputFn("std.collection 模块尚未实现")
        }
    }

    // ==================== 配置方法 ====================

    /**
     * 设置是否显示 Banner
     */
    public func setShowBanner(show: Bool): Unit {
        this.showBanner = show
    }

    /**
     * 设置是否显示详细信息
     */
    public func setVerbose(verbose: Bool): Unit {
        this.verbose = verbose
    }

    /**
     * 设置是否显示调试信息
     */
    public func setDebug(debug: Bool): Unit {
        this.debug = debug
    }

    /**
     * 设置自定义输出函数
     */
    public func setOutputFn(fn: (String) -> Unit): Unit {
        this.outputFn = fn
    }

    // ==================== 沙箱安全配置方法 ====================

    /**
     * 启用沙箱
     */
    public func enableSandbox(): Unit {
        this.sandboxEnabled = true
        if (this.verbose) {
            this.outputFn("沙箱已启用")
        }
    }

    /**
     * 设置栈深度限制
     */
    public func setMaxStackDepth(depth: Int64): Unit {
        this.maxStackDepth = depth
        this.evaluator.setMaxStackDepth(depth)  // 同步到 Evaluator
        if (this.verbose) {
            this.outputFn("栈深度限制设置为: ${depth}")
        }
    }

    /**
     * 设置超时
     */
    public func setTimeout(timeout: Option<Duration>): Unit {
        this.timeout = timeout
        match (timeout) {
            case Some(d) => this.outputFn("超时设置为: ${d}")
            case None => this.outputFn("无超时限制")
        }
    }

    /**
     * 设置内存限制
     */
    public func setMaxMemory(bytes: Option<Int64>): Unit {
        this.maxMemory = bytes
        match (bytes) {
            case Some(b) => this.outputFn("内存限制设置为: ${b} 字节")
            case None => ()
        }
    }

    /**
     * 禁止单个函数
     */
    public func blockFunction(funcName: String): Unit {
        this.blockedFunctions.add(funcName)
        if (this.verbose) {
            this.outputFn("禁止函数: ${funcName}")
        }
    }

    /**
     * 批量禁止函数
     */
    public func blockFunctions(funcNames: Array<String>): Unit {
        for (funcName in funcNames) {
            this.blockedFunctions.add(funcName)
        }
        if (this.verbose) {
            this.outputFn("禁止 ${funcNames.size} 个函数")
        }
    }

    /**
     * 设置函数白名单
     */
    public func setAllowedFunctions(funcNames: Array<String>): Unit {
        var list = ArrayList<String>()
        for (funcName in funcNames) {
            list.add(funcName)
        }
        this.allowedFunctions = Some(list)
        if (this.verbose) {
            this.outputFn("设置函数白名单: ${funcNames.size} 个函数")
        }
    }

    /**
     * 禁止文件写入
     */
    public func blockFileWrite(): Unit {
        this.allowFileWrite = false
        if (this.verbose) {
            this.outputFn("禁止文件写入")
        }
    }

    /**
     * 允许文件写入
     */
    public func setFileWriteAllowed(allowed: Bool): Unit {
        this.allowFileWrite = allowed
        if (this.verbose) {
            if (allowed) {
                this.outputFn("允许文件写入")
            } else {
                this.outputFn("禁止文件写入")
            }
        }
    }

    /**
     * 禁止模块加载
     */
    public func blockModuleLoad(): Unit {
        this.allowModuleLoad = false
        if (this.verbose) {
            this.outputFn("禁止模块加载")
        }
    }

    /**
     * 允许模块加载
     */
    public func setModuleLoadAllowed(allowed: Bool): Unit {
        this.allowModuleLoad = allowed
        if (this.verbose) {
            if (allowed) {
                this.outputFn("允许模块加载")
            } else {
                this.outputFn("禁止模块加载")
            }
        }
    }

    /**
     * 设置允许的路径
     */
    public func setAllowedPaths(paths: Array<String>): Unit {
        for (path in paths) {
            this.allowedPaths.add(path)
        }
        if (this.verbose) {
            this.outputFn("设置允许路径: ${paths.size} 个")
        }
    }

    /**
     * 设置脚本模式加载器（允许 (import "./xxx")）
     *
     * CLI 脚本执行和 REPL 应使用此模式。
     */
    public func setScriptMode(): Unit {
        if (let Some(moduleLoader) <- this.evaluator.moduleLoader) {
            let scriptLoader = ScriptLoader(moduleLoader, this.evaluator)
            this.evaluator.currentLoader = Some(scriptLoader)
            if (this.verbose) {
                this.outputFn("切换到脚本模式加载器")
            }
        }
    }

    /**
     * 设置模块模式加载器（禁止 (import "./xxx")）
     *
     * 嵌入式场景或模块内部应使用此模式（默认）。
     */
    public func setModuleMode(): Unit {
        if (let Some(moduleLoader) <- this.evaluator.moduleLoader) {
            this.evaluator.currentLoader = Some(moduleLoader)
            if (this.verbose) {
                this.outputFn("切换到模块模式加载器")
            }
        }
    }

    /**
     * 设置自定义模块数据源
     *
     * @param source ModuleSource 实例（MemorySource、FileSystemSource 或自定义）
     *
     * 设置后 import 会优先使用该数据源加载模块。
     *
     * @example
     * ```
     * let source = MemorySource()
     * source.registerModule(...)
     * interpreter.setModuleSource(source)
     * ```
     */
    public func setModuleSource(source: ModuleSource): Unit {
        if (this.verbose) {
            this.outputFn("设置自定义模块数据源")
        }
        if (let Some(loader) <- this.evaluator.moduleLoader) {
            loader.setSource(source)
        }
    }

    /**
     * 添加模块搜索路径
     * 如果模块系统尚未初始化，缓存路径，初始化时自动添加
     */
    private var pendingSearchPaths: ArrayList<String> = ArrayList<String>()

    public func addModuleSearchPath(path: String): Unit {
        match (this.evaluator.moduleRegistry) {
            case Some(registry) =>
                registry.addSearchPath(path)
                if (this.verbose) {
                    this.outputFn("添加模块搜索路径: ${path}")
                }
            case None =>
                // 模块系统尚未初始化，缓存路径
                if (!this.pendingSearchPaths.contains(path)) {
                    this.pendingSearchPaths.add(path)
                }
        }
    }

    /**
     * 获取最大栈深度
     */
    public func getMaxStackDepth(): Int64 {
        this.maxStackDepth
    }

    /**
     * 检查函数是否允许调用
     */
    public func isFunctionAllowed(funcName: String): Bool {
        // 先检查黑名单
        for (blocked in this.blockedFunctions) {
            if (blocked == funcName) {
                return false
            }
        }

        // 如果有白名单，检查白名单
        match (this.allowedFunctions) {
            case Some(allowed) =>
                for (item in allowed) {
                    if (item == funcName) {
                        return true
                    }
                }
                false
            case None => true  // 没有白名单，都允许
        }
    }

    /**
     * 检查路径是否允许访问
     */
    public func isPathAllowed(path: String, isWrite: Bool): Bool {
        // 检查写入权限
        if (isWrite && !this.allowFileWrite) {
            return false
        }

        // 如果有路径白名单，检查
        if (this.allowedPaths.size > 0) {
            for (allowedPath in this.allowedPaths) {
                if (path.startsWith(allowedPath)) {
                    return true
                }
            }
            return false
        }

        return true
    }

    /**
     * 检查是否允许加载模块
     */
    public func isModuleLoadAllowed(): Bool {
        this.allowModuleLoad
    }

    /**
     * 检查内存限制
     */
    public func checkMemoryLimit(): Bool {
        match (this.maxMemory) {
            case Some(limit) =>
                let currentMem = getAllocatedHeapSize()
                let usedMem = currentMem - this.initialMemory
                usedMem < limit
            case None => true
        }
    }

    // ==================== 内部方法 ====================

    /**
     * 打印 Banner
     */
    private func printBanner() {
        this.outputFn("╔═══════════════════════════════════════════════════════════╗")
        this.outputFn("║           星枢 - 仓颉嵌入式 Lisp 脚本语言                 ║")
        this.outputFn("║                   版本 0.1.0 - MVP                        ║")
        this.outputFn("╚═══════════════════════════════════════════════════════════╝")
    }

    /**
     * 获取顶层环境
     */
    public func getEnvironment(): Environment {
        this.env
    }

    /**
     * 求值单个表达式
     * @param code Lisp 代码字符串
     * @return 求值结果，如果解析或求值失败返回 None
     */
    public func eval(code: String): LispValue {
        // 如果启用沙箱，使用 evalInSandbox
        if (this.sandboxEnabled) {
            match (this.evalInSandboxInternal(code)) {
                case Some(result) => result
                case None => Nil
            }
        } else {
            // 直接求值
            try {
                let lexer = Lexer(code)
                let tokens = lexer.tokenize()
                let parser = Parser(tokens)
                let exprs = parser.parse()

                if (exprs.size > 0) {

                    // 执行所有表达式，返回最后一个结果
                    var result: LispValue = Nil
                    var i = 0
                    for (expr in exprs) {
                        result = evaluator.eval(expr)
                        i = i + 1
                    }
                    result
                } else {
                    Nil
                }
            } catch (e: Exception) {
                println("Error: ${e.message}")
                Nil
            }
        }
    }

    /**
     * 在沙箱中执行代码（使用线程超时）
     * @param code Lisp 代码
     * @return 执行结果，超时或错误返回 None
     */
    public func evalInSandboxInternal(code: String): ?LispValue {
        // 记录初始内存
        this.initialMemory = getAllocatedHeapSize()

        // 在新线程中执行
        let fut = spawn {
            try {
                let lexer = Lexer(code)
                let tokens = lexer.tokenize()
                let parser = Parser(tokens)
                let exprs = parser.parse()

                if (exprs.size > 0) {
                    // 执行所有表达式，返回最后一个结果
                    var result: LispValue = Nil
                    for (expr in exprs) {
                        result = evaluator.eval(expr)
                    }
                    result
                } else {
                    Nil
                }
            } catch (e: Exception) {
                println("Sandbox error: ${e.message}")
                Nil
            }
        }

        this.currentFuture = Some(fut)

        try {
            match (this.timeout) {
                case Some(duration) =>
                    // 带超时等待
                    let result = fut.get(duration)
                    this.currentFuture = None
                    Some(result)
                case None =>
                    // 无超时限制
                    let result = fut.get()
                    this.currentFuture = None
                    Some(result)
            }
        } catch (e: Exception) {
            // 超时或其他错误
            match (this.currentFuture) {
                case Some(_) =>
                    // 尝试取消线程
                    fut.cancel()
                    this.outputFn("沙箱执行超时或被取消")
                case None => ()
            }
            this.currentFuture = None
            None
        }
    }

    /**
     * 求值多个表达式（顺序执行）
     * @param code 多个 Lisp 表达式
     * @return 最后一个表达式的求值结果
     */
    public func evalMultiple(code: String): LispValue {
        try {
            let lexer = Lexer(code)
            let tokens = lexer.tokenize()
            let parser = Parser(tokens)
            let exprs = parser.parse()

            var result: LispValue = Nil
            for (expr in exprs) {
                result = evaluator.eval(expr)
                // 如果是错误，打印错误信息（但继续执行）
                match (result) {
                    case Error(err) => eprintln(err.toString())
                    case _ => ()
                }
            }
            result
        } catch (e: Exception) {
            println("Error: ${e.message}")
            Nil
        }
    }

    /**
     * 从文件加载并执行 Lisp 脚本
     * @param filePath 文件路径
     * @return 最后一个表达式的求值结果
     */
    public func evalFile(filePath: String): LispValue {
        try {
            // 读取文件内容
            let bytes = File.readFrom(filePath)
            let content = String.fromUtf8(bytes)

            // 设置当前文件路径（支持 (import "./xxx") 相对导入）
            let oldFilePath = this.evaluator.currentFilePath
            this.evaluator.currentFilePath = Some(Path(filePath).normalize().toString())

            // 执行多个表达式
            let result = this.evalMultiple(content)

            // 恢复文件路径
            this.evaluator.currentFilePath = oldFilePath
            result
        } catch (e: FSException) {
            let errMsg = "无法读取文件: ${e.message}"
            eprintln(errMsg)
            LispValue.Error(XispError(ErrorType.IOError, errMsg))
        } catch (e: Exception) {
            let errMsg = "执行文件时出错: ${e.message}"
            eprintln(errMsg)
            LispValue.Error(XispError(ErrorType.IOError, errMsg))
        }
    }

    /**
     * 运行 REPL（交互式解释器）
     */
    public func runREPL(): Int64 {
        let repl = EnhancedRepl(this.env)
        if (this.showBanner) {
            repl.setShowBanner(true)
        }
        repl.run()
        0
    }

    public func runSimpleREPL(): Int64 {
        let repl = Repl(this.env)
        if (this.showBanner) {
            printBanner()
            repl.setShowBanner(false)
        }
        repl.run()
        0
    }

    /**
     * 注册桥接函数（便捷方法）
     * @param name 函数名（不带命名空间）
     * @param handler 函数处理器
     */
    public func registerBridgeFunction(
        name: String,
        handler: (ArrayList<LispValue>) -> LispValue
    ): Unit {
        this.bridge.register(name, handler)
    }

    /**
     * 注册带命名空间的桥接函数（便捷方法）
     * @param ns 命名空间前缀
     * @param name 函数名
     * @param handler 函数处理器
     */
    public func registerBridgeFunctionWithNS(
        ns: String,
        name: String,
        handler: (ArrayList<LispValue>) -> LispValue
    ): Unit {
        this.bridge.registerWithNS(ns, name, handler)
    }

    /**
     * 调用已定义的 Lisp 函数（支持变长参数）
     *
     * 使用示例：
     * ```
     * // 先定义 Lisp 函数
     * interpreter.eval("(define (square x) (* x x))")
     *
     * // 从仓颉调用
     * let result = interpreter.call("square", [5])
     * if (let Some(i) <- result.asInt()) {
     *     println(i)  // 25
     * }
     * ```
     *
     * @param funcName 函数名
     * @param args 0个或多个参数，自动调用 toLisp() 转换
     * @return 函数返回值（LispValue 格式）
     */
    public func call<T>(funcName: String, args: Array<T>): LispValue where T <: LispConvertible {
        // 将参数转换为 LispValue
        let lispArgs = ArrayList<LispValue>()
        for (arg in args) {
            lispArgs.add(arg.toLisp())
        }

        // 构造函数调用表达式：(funcName arg1 arg2 ...)
        let argsList = list(lispArgs)
        let callExpr = consValue(Symbol(funcName), argsList)

        // 求值并返回
        this.evaluator.eval(callExpr)
    }

    /**
     * 重置解释器状态（清空环境）
     */
    public func reset(): Unit {
        this.env = Environment()
        this.evaluator = Evaluator(this.env)

        // 重新注册内置函数
        BuiltinFunctions.registerAll(this.env)

        // 重新注册标准桥接函数
        this.bridge = BridgeManager(this.env)
        this.bridge.setPathChecker({ path, isWrite => this.isPathAllowed(path, isWrite) })
        this.bridge.registerStdIO()
        this.bridge.registerStdFS()

        // 注意：不重置配置字段（showBanner, verbose, debug, outputFn）
    }
}
