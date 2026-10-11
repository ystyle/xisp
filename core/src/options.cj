package ystyle::xisp

import ystyle::xisp.core.eval.*
import ystyle::xisp.core.builtin.*
import ystyle::xisp.core.module.*

/**
 * 解释器选项类型
 * 所有配置选项都使用这个类型，便于组合和扩展
 */
public type InterpreterOption = (LispInterpreter) -> Unit

// ==================== 关键字别名选项 ====================

/**
 * 注册单个关键字别名
 * @param alias 别名
 * @param original 原始关键字
 *
 * 示例：
 * ```lisp
 * let interpreter = LispInterpreter([
 *     withKeywordAlias("定义", "define")
 * ])
 * ```
 */
public func withKeywordAlias(alias: String, original: String): InterpreterOption {
    { interpreter => interpreter.registerKeywordAlias(alias, original) }
}

/**
 * 批量注册关键字别名
 * @param aliases 别名数组，每个元素是 (别名, 原始关键字) 对
 *
 * 示例：
 * ```lisp
 * let interpreter = LispInterpreter([
 *     withKeywordAliases([("def", "define"), ("fn", "lambda")])
 * ])
 * ```
 */
public func withKeywordAliases(aliases: Array<(String, String)>): InterpreterOption {
    { interpreter =>
        for ((alias, original) in aliases) {
            interpreter.registerKeywordAlias(alias, original)
        }
    }
}

/**
 * 中文关键字集合（预设）
 *
 * 包含常用的中文关键字别名：
 * - 定义 -> define
 * - 过程 -> lambda (使用"过程"避免与 func 混淆)
 * - 如果 -> if
 * - 让 -> let
 * - 打印 -> println
 * - 显示 -> print
 */
public func withChineseKeywords(): InterpreterOption {
    withKeywordAliases([
        ("定义", "define"),
        ("过程", "lambda"),
        ("如果", "if"),
        ("让", "let"),
        ("打印", "println"),
        ("显示", "print"),
        ("当", "when"),         // 未来功能
        ("除非", "unless"),     // 未来功能
        ("循环", "loop"),       // 未来功能
        ("递归", "recur")       // 未来功能
    ])
}

/**
 * 日语关键字集合（预设）
 *
 * 包含常用的日语关键字别名：
 * - 定義 -> define
 * - もし (moshi) -> if
 * - 让 -> let
 * - 表示 -> println
 */
public func withJapaneseKeywords(): InterpreterOption {
    withKeywordAliases([
        ("定義", "define"),
        ("もし", "if"),
        ("让", "let"),
        ("表示", "println"),
        ("λ", "lambda")
    ])
}

/**
 * 韩语关键字集合（预设）
 *
 * 包含常用的韩语关键字别名：
 * - 정의 (jeong-ui) -> define
 * - 만약 (man-yak) -> if
 * - 让 -> let
 * - 출력 (chul-ryeok) -> println
 */
public func withKoreanKeywords(): InterpreterOption {
    withKeywordAliases([
        ("정의", "define"),
        ("만약", "if"),
        ("让", "let"),
        ("출력", "println"),
        ("λ", "lambda")
    ])
}

// ==================== 标准库模块选项 ====================

/**
 * 启用 std.io 模块
 *
 * 注册文件读写相关的桥接函数：
 * - cangjie::io:read-file
 * - cangjie::io:write-file
 * - cangjie::io:append-file
 */
public func withStdIO(): InterpreterOption {
    { interpreter => interpreter.registerStdIO() }
}

/**
 * 启用 std.fs 模块
 *
 * 注册文件系统相关的桥接函数：
 * - cangjie::fs:exists?
 * - cangjie::fs:file?
 * - cangjie::fs:directory?
 * - cangjie::fs:list-dir
 */
public func withStdFS(): InterpreterOption {
    { interpreter => interpreter.registerStdFS() }
}

/**
 * 启用 std.collection 模块
 *
 * 注册集合相关的桥接函数：
 * - TODO: 添加具体的集合函数
 */
public func withStdCollection(): InterpreterOption {
    { interpreter => interpreter.registerStdCollection() }
}

/**
 * 启用所有标准库模块
 *
 * 包括 std.io、std.fs、std.collection
 */
public func withStdLib(): InterpreterOption {
    { interpreter =>
        interpreter.registerStdIO()
        interpreter.registerStdFS()
        interpreter.registerStdCollection()
    }
}

// ==================== 能力域装配 ====================

/**
 * 安装一组能力域（能力域设计 §4）
 *
 * 组合的语义是**并集**：按 `after` 约束排序后逐个安装。宿主要么用预置组合
 * （`withPreset(PRESET_FULL)`），要么自己拼：
 *
 * ```cangjie
 * LispInterpreter([
 *     withDomains([BuiltinFunctions.domainArithmetic(), BuiltinFunctions.domainList()])
 * ])
 * ```
 *
 * 也可用自定义域顶替 core 的同名域（域只是"注册集合"，后装者覆盖先装者）。
 */
public func withDomains(domains: Array<LispCapability>): InterpreterOption {
    withDomains(domains, replace: false)
}

/**
 * 安装一组能力域，并可指定是否**独占**。
 *
 * `replace = true` 时先清空环境（含 `LispInterpreter()` 默认装的全量内建），
 * 只留所声明的域 —— 这是"宿主自己选择要哪些能力"的完整语义。
 * `replace = false`（默认）为追加：在默认全量之上再装（适合只加自定义域）。
 *
 * 注意：即使 `replace = true`，①语言原语（`if`/`let`/`lambda`…）与②求值器拦截
 * （`map`/`eval`…）**仍然可用** —— 它们不在环境里，域装配动不了（设计文档 §5.3）。
 */
public func withDomains(domains: Array<LispCapability>, replace!: Bool): InterpreterOption {
    { interpreter =>
        if (replace) {
            interpreter.clearEnvironmentBindings()
            interpreter.markDomainsExclusive()
        } else if (interpreter.isDomainsExclusive()) {
            // 已经独占装配过：后续 withDomains 只做追加，不重装默认全量
            ()
        }
        BuiltinFunctions.installDomains(
            interpreter.getEnvironment(),
            domains,
            isInterrupted: Some({ => interpreter.isInterruptedNow() })
        )
    }
}

/**
 * 安装一个预置组合（`PRESET_FULL` / `PRESET_PURE`，或宿主自造的组合）
 */
public func withPreset(preset: Array<LispCapability>, replace!: Bool = false): InterpreterOption {
    withDomains(preset, replace: replace)
}

/**
 * 全量预置组合：所有 core 内置域
 */
public func presetFull(): Array<LispCapability> {
    BuiltinFunctions.presetFull()
}

/**
 * 纯函数子集预置组合：按 `!escapable && !hostPolicy` **推导**（非手写清单）
 *
 * 注意不含输出域 —— 导出通道由宿主用 `withOutputFn` / `registerPure(output:)` 接管，
 * 与既有语义一致。
 */
public func presetPure(): Array<LispCapability> {
    BuiltinFunctions.presetPure()
}

// ==================== 其他配置选项 ====================

/**
 * 静默模式（不显示 Banner）
 */
public func withQuietMode(): InterpreterOption {
    { interpreter => interpreter.setShowBanner(false) }
}

/**
 * 详细模式（显示更多信息）
 */
public func withVerboseMode(): InterpreterOption {
    { interpreter => interpreter.setVerbose(true) }
}

/**
 * 调试模式（显示调试信息）
 */
public func withDebugMode(): InterpreterOption {
    { interpreter => interpreter.setDebug(true) }
}

/**
 * 设置自定义输出函数
 * @param fn 输出函数，接收字符串参数
 *
 * 示例：
 * ```lisp
 * let interpreter = LispInterpreter([
 *     withOutputFn({ s => logger.log(s) })
 * ])
 * ```
 */
public func withOutputFn(fn: (String) -> Unit): InterpreterOption {
    { interpreter => interpreter.setOutputFn(fn) }
}

// ==================== 沙箱安全选项 ====================

/**
 * 启用沙箱模式（严格安全）
 *
 * 限制：
 * - 调用栈深度：500
 * - 执行时间：30 秒
 * - 禁止文件写入
 * - 禁止模块加载
 * - 禁止危险函数（eval, apply）
 *
 * 示例：
 * ```cangjie
 * let interpreter = LispInterpreter([
 *     withSandbox(),
 *     withStdIO()  // 但会被沙箱限制
 * ])
 * ```
 */
public func withSandbox(): InterpreterOption {
    { interpreter =>
        interpreter.enableSandbox()
        interpreter.setMaxStackDepth(500)
        interpreter.setTimeout(Some(Duration.second * 30))
        interpreter.blockFileWrite()
        interpreter.blockModuleLoad()
        interpreter.blockFunctions(["eval", "apply"])
    }
}

/**
 * 沙箱 - 懒惰模式（开发调试）
 *
 * 限制：
 * - 调用栈深度：5000
 * - 无执行时间限制
 * - 允许文件读写
 *
 * 示例：
 * ```cangjie
 * let interpreter = LispInterpreter([
 *     withSandboxLenient()
 * ])
 * ```
 */
public func withSandboxLenient(): InterpreterOption {
    { interpreter =>
        interpreter.enableSandbox()
        interpreter.setMaxStackDepth(5000)
        interpreter.setTimeout(None)
    }
}

/**
 * 设置执行超时
 * @param timeout 超时时间，None 表示无限制
 *
 * 示例：
 * ```cangjie
 * let interpreter = LispInterpreter([
 *     withTimeout(Some(Duration.second * 10))
 * ])
 * ```
 */
public func withTimeout(timeout: Option<Duration>): InterpreterOption {
    { interpreter => interpreter.setTimeout(timeout) }
}

/**
 * 设置调用栈深度限制
 * @param depth 最大栈深度，默认 1000
 *
 * 示例：
 * ```cangjie
 * let interpreter = LispInterpreter([
 *     withMaxStackDepth(2000)
 * ])
 * ```
 */
public func withMaxStackDepth(depth: Int64): InterpreterOption {
    { interpreter => interpreter.setMaxStackDepth(depth) }
}

/**
 * 设置内存限制
 * @param bytes 最大内存（字节），None 表示无限制
 *
 * 示例：
 * ```cangjie
 * let interpreter = LispInterpreter([
 *     withMaxMemory(Some(100 * 1024 * 1024))  // 100MB
 * ])
 * ```
 */
public func withMaxMemory(bytes: Option<Int64>): InterpreterOption {
    { interpreter => interpreter.setMaxMemory(bytes) }
}

/**
 * 禁止指定函数（黑名单）
 * @param funcNames 函数名列表
 *
 * 示例：
 * ```cangjie
 * let interpreter = LispInterpreter([
 *     withBlockedFunctions(["eval", "apply", "load"])
 * ])
 * ```
 */
public func withBlockedFunctions(funcNames: Array<String>): InterpreterOption {
    { interpreter =>
        for (funcName in funcNames) {
            interpreter.blockFunction(funcName)
        }
    }
}

/**
 * 只允许指定函数（白名单）
 * @param funcNames 函数名列表
 *
 * 示例：
 * ```cangjie
 * let interpreter = LispInterpreter([
 *     withAllowedFunctions(["+", "-", "*", "/", "println"])
 * ])
 * ```
 */
public func withAllowedFunctions(funcNames: Array<String>): InterpreterOption {
    { interpreter => interpreter.setAllowedFunctions(funcNames) }
}

/**
 * 禁止文件写入
 *
 * 示例：
 * ```cangjie
 * let interpreter = LispInterpreter([
 *     withNoFileWrite()
 * ])
 * ```
 */
public func withNoFileWrite(): InterpreterOption {
    { interpreter => interpreter.blockFileWrite() }
}

/**
 * 限制文件访问路径（白名单）
 * @param paths 允许的路径前缀
 *
 * 示例：
 * ```cangjie
 * let interpreter = LispInterpreter([
 *     withAllowedPaths(["/tmp/", "./user-data/"])
 * ])
 * ```
 */
public func withAllowedPaths(paths: Array<String>): InterpreterOption {
    { interpreter => interpreter.setAllowedPaths(paths) }
}

/**
 * 禁止模块加载
 *
 * 禁止使用 import 语句加载任何模块。
 * 适用于需要完全隔离代码执行的沙箱环境。
 *
 * 示例：
 * ```cangjie
 * let interpreter = LispInterpreter([
 *     withNoModuleLoad()
 * ])
 * ```
 */
public func withNoModuleLoad(): InterpreterOption {
    { interpreter => interpreter.blockModuleLoad() }
}

/**
 * 限制模块加载路径（白名单）
 * @param paths 允许加载模块的路径前缀
 *
 * 只允许从指定路径加载模块，配合 withNoFileWrite() 使用可实现严格隔离。
 *
 * 示例：
 * ```cangjie
 * let interpreter = LispInterpreter([
 *     withAllowedModulePaths(["./lib/", "/usr/share/xisp/modules/"])
 * ])
 * ```
 */
public func withAllowedModulePaths(paths: Array<String>): InterpreterOption {
    { interpreter => interpreter.setAllowedPaths(paths) }
}

/**
 * 添加模块搜索路径
 * @param path 要添加的模块搜索路径
 *
 * 向模块搜索路径中添加一个目录，用于查找模块。
 * 可以多次调用此选项来添加多个搜索路径。
 *
 * 示例：
 * ```cangjie
 * let interpreter = LispInterpreter([
 *     withModulePath("./lib/modules"),
 *     withModulePath("/usr/share/xisp/modules")
 * ])
 * ```
 */
public func withModulePath(path: String): InterpreterOption {
    { interpreter => interpreter.addModuleSearchPath(path) }
}

/**
 * 添加多个模块搜索路径
 * @param paths 要添加的模块搜索路径数组
 *
 * 向模块搜索路径中添加多个目录。
 *
 * 示例：
 * ```cangjie
 * let interpreter = LispInterpreter([
 *     withModulePaths(["./lib/modules", "/usr/share/xisp/modules"])
 * ])
 * ```
 */
public func withModulePaths(paths: Array<String>): InterpreterOption {
    { interpreter =>
        for (path in paths) {
            interpreter.addModuleSearchPath(path)
        }
    }
}

/**
 * 设置自定义模块数据源
 *
 * 通过 ModuleSource 接口支持从任意数据源加载模块。
 *
 * 适用场景：
 * - MemorySource：从内存加载模块（测试、动态生成）
 * - FileSystemSource：从文件系统加载（默认行为）
 * - 自定义实现：从数据库、网络等加载
 *
 * @param source ModuleSource 实例
 *
 * @example
 * ```cangjie
 * let source = MemorySource()
 * source.registerModule("my::mod", "(module mod ...)", HashMap<String, String> {
 *     ["core.lisp"] => "(export foo) (define foo 42)"
 * })
 *
 * let interpreter = LispInterpreter([
 *     withStdLib(),
 *     withModuleSource(source)
 * ])
 * interpreter.eval("(import my::mod)")
 * ```
 */
public func withModuleSource(source: ModuleSource): InterpreterOption {
    { interpreter => interpreter.setModuleSource(source) }
}

/**
 * 启用脚本模式（允许 (import "./xxx")）
 */
public func withScriptMode(): InterpreterOption {
    { interpreter => interpreter.setScriptMode() }
}

/**
 * 启用模块模式（禁止 (import "./xxx")，默认）
 */
public func withModuleMode(): InterpreterOption {
    { interp => interp.setModuleMode() }
}

// 字节码 / JIT 选项不在此处：它们属于可选后端模块 `ystyle::xisp_bc`
// （`withBytecodeCompiler()` / `withJit()`），核心不依赖字节码实现。
