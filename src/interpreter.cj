package ystyle::xisp

import ystyle::xisp.core.*
import ystyle::xisp.parser.*
import ystyle::xisp.bridge.*
import ystyle::xisp.repl.*
import std.collection.ArrayList

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

    /**
     * 创建新的 Lisp 解释器实例（默认配置）
     */
    public init() {
        this.env = Environment()
        this.evaluator = Evaluator(this.env)
        this.bridge = BridgeManager(this.env)

        // 注册内置函数
        BuiltinFunctions.registerAll(this.env)

        // 注册标准桥接函数
        this.bridge.registerStdIO()
        this.bridge.registerStdFS()
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

    // ==================== 内部方法 ====================

    /**
     * 打印 Banner
     */
    private func printBanner() {
        this.outputFn("╔═══════════════════════════════════════════════════════════╗")
        this.outputFn("║           星枢 - 仓颉嵌入式 Lisp 脚本语言          ║")
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
        try {
            let lexer = Lexer(code)
            let tokens = lexer.tokenize()
            let parser = Parser(tokens)
            let exprs = parser.parse()

            if (exprs.size > 0) {
                evaluator.eval(exprs[0])
            } else {
                Nil
            }
        } catch (e: Exception) {
            println("Error: ${e.message}")
            Nil
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

    internal init(env: Environment) {
        this.env = env
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
        Bridge.registerStdIO(this.env)
    }

    /**
     * 批量注册 std.fs 模块
     */
    public func registerStdFS(): Unit {
        Bridge.registerStdFS(this.env)
    }
}
