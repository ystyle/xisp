package ystyle::xisp

import ystyle::xisp.core.*
import ystyle::xisp.types.*
import ystyle::xisp.parser.*
import ystyle::xisp.bridge.*
import ystyle::xisp.repl.*
import std.collection.ArrayList
import std.sync.*
import std.time.*
import std.runtime.*
import std.collection.HashSet

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

        // 设置 bridge 的 interpreter 引用
        this.bridge.setInterpreter(this)

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
    public func registerKeywordAlias(alias: String, original: String) {
        this.env.registerKeywordAlias(alias, original)
        if (this.verbose) {
            this.outputFn("注册关键字别名: ${alias} -> ${original}")
        }
    }

    // ==================== 标准库模块注册 ====================

    /**
     * 注册 std.io 模块
     */
    public func registerStdIO() {
        this.bridge.registerStdIO()
        if (this.verbose) {
            this.outputFn("启用 std.io 模块")
        }
    }

    /**
     * 注册 std.fs 模块
     */
    public func registerStdFS() {
        this.bridge.registerStdFS()
        if (this.verbose) {
            this.outputFn("启用 std.fs 模块")
        }
    }

    /**
     * 注册 std.collection 模块（预留）
     */
    public func registerStdCollection() {
        // TODO: 实现 collection 模块注册
        if (this.verbose) {
            this.outputFn("std.collection 模块尚未实现")
        }
    }

    // ==================== 配置方法 ====================

    /**
     * 设置是否显示 Banner
     */
    public func setShowBanner(show: Bool) {
        this.showBanner = show
    }

    /**
     * 设置是否显示详细信息
     */
    public func setVerbose(verbose: Bool) {
        this.verbose = verbose
    }

    /**
     * 设置是否显示调试信息
     */
    public func setDebug(debug: Bool) {
        this.debug = debug
    }

    /**
     * 设置自定义输出函数
     */
    public func setOutputFn(fn: (String) -> Unit) {
        this.outputFn = fn
    }

    // ==================== 沙箱安全配置方法 ====================

    /**
     * 启用沙箱
     */
    public func enableSandbox() {
        this.sandboxEnabled = true
        if (this.verbose) {
            this.outputFn("沙箱已启用")
        }
    }

    /**
     * 设置栈深度限制
     */
    public func setMaxStackDepth(depth: Int64) {
        this.maxStackDepth = depth
        this.evaluator.setMaxStackDepth(depth)  // 同步到 Evaluator
        if (this.verbose) {
            this.outputFn("栈深度限制设置为: ${depth}")
        }
    }

    /**
     * 设置超时
     */
    public func setTimeout(timeout: Option<Duration>) {
        this.timeout = timeout
        match (timeout) {
            case Some(d) => this.outputFn("超时设置为: ${d}")
            case None => this.outputFn("无超时限制")
        }
    }

    /**
     * 设置内存限制
     */
    public func setMaxMemory(bytes: Option<Int64>) {
        this.maxMemory = bytes
        match (bytes) {
            case Some(b) => this.outputFn("内存限制设置为: ${b} 字节")
            case None => ()
        }
    }

    /**
     * 禁止单个函数
     */
    public func blockFunction(funcName: String) {
        this.blockedFunctions.add(funcName)
        if (this.verbose) {
            this.outputFn("禁止函数: ${funcName}")
        }
    }

    /**
     * 批量禁止函数
     */
    public func blockFunctions(funcNames: Array<String>) {
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
    public func setAllowedFunctions(funcNames: Array<String>) {
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
    public func blockFileWrite() {
        this.allowFileWrite = false
        if (this.verbose) {
            this.outputFn("禁止文件写入")
        }
    }

    /**
     * 允许文件写入
     */
    public func setFileWriteAllowed(allowed: Bool) {
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
    public func blockModuleLoad() {
        this.allowModuleLoad = false
        if (this.verbose) {
            this.outputFn("禁止模块加载")
        }
    }

    /**
     * 允许模块加载
     */
    public func setModuleLoadAllowed(allowed: Bool) {
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
    public func setAllowedPaths(paths: Array<String>) {
        for (path in paths) {
            this.allowedPaths.add(path)
        }
        if (this.verbose) {
            this.outputFn("设置允许路径: ${paths.size} 个")
        }
    }

    /**
     * 添加模块搜索路径
     */
    public func addModuleSearchPath(path: String) {
        match (this.evaluator.moduleRegistry) {
            case Some(registry) =>
                registry.addSearchPath(path)
                if (this.verbose) {
                    this.outputFn("添加模块搜索路径: ${path}")
                }
            case None => ()
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
            // TODO: 实现文件读取
            println("Error: evalFile not yet implemented")
            Nil
        } catch (e: Exception) {
            println("Error: ${e.message}")
            Nil
        }
    }

    /**
     * 运行 REPL（交互式解释器）
     */
    public func runREPL(): Int64 {
        let repl = Repl(this.env)
        // 如果 Interpreter 已经打印了 Banner，REPL 就不再打印
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
    ) {
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
    ) {
        this.bridge.registerWithNS(ns, name, handler)
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
        this.bridge.setInterpreter(this)
        this.bridge.registerStdIO()
        this.bridge.registerStdFS()

        // 注意：不重置配置字段（showBanner, verbose, debug, outputFn）
    }
}

/**
 * 桥接管理器
 * 管理解释器的桥接函数注册
 */
public class BridgeManager {
    private var env: Environment
    private var interpreter: ?LispInterpreter  // 改为可选，避免循环依赖

    internal init(env: Environment) {
        this.env = env
        this.interpreter = None
    }

    internal init(env: Environment, interpreter: LispInterpreter) {
        this.env = env
        this.interpreter = Some(interpreter)
    }

    /**
     * 设置解释器引用
     */
    internal func setInterpreter(interpreter: LispInterpreter) {
        this.interpreter = Some(interpreter)
    }

    /**
     * 注册桥接函数（不带命名空间）
     */
    public func register(
        name: String,
        handler: (ArrayList<LispValue>) -> LispValue
    ) {
        this.env.define(name, NativeFunc(handler))
    }

    /**
     * 注册带命名空间的桥接函数
     * @param ns 命名空间（如 "mymodule"）
     * @param name 函数名（如 "func"）
     * @param handler 函数处理器
     *
     * 最终函数名将是 "mymodule:func"
     */
    public func registerWithNS(
        ns: String,
        name: String,
        handler: (ArrayList<LispValue>) -> LispValue
    ) {
        let fullName = "${ns}:${name}"
        this.env.define(fullName, NativeFunc(handler))
    }

    /**
     * 批量注册 std.io 模块
     */
    public func registerStdIO(): Unit {
        // 创建路径权限检查回调
        match (this.interpreter) {
            case Some(interp) =>
                let checker: (String, Bool) -> Bool = { path, isWrite =>
                    interp.isPathAllowed(path, isWrite)
                }
                Bridge.registerStdIO(this.env, Some(checker))
            case None =>
                Bridge.registerStdIO(this.env)
        }
    }

    /**
     * 批量注册 std.fs 模块
     */
    public func registerStdFS(): Unit {
        // FS 模块暂时不需要权限检查，如果需要可以类似处理
        Bridge.registerStdFS(this.env)
    }
}
