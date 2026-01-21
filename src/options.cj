package ystyle::xisp

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
 * - cangjie:io:read-file
 * - cangjie:io:write-file
 * - cangjie:io:append-file
 */
public func withStdIO(): InterpreterOption {
    { interpreter => interpreter.registerStdIO() }
}

/**
 * 启用 std.fs 模块
 *
 * 注册文件系统相关的桥接函数：
 * - cangjie:fs:exists?
 * - cangjie:fs:file?
 * - cangjie:fs:directory?
 * - cangjie:fs:list-dir
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
