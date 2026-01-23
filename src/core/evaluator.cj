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
    var macroExpandDepth: Int64 = 0  // 宏展开深度
    var maxMacroExpandDepth: Int64 = 100  // 最大宏展开深度

    public init(env: Environment) {
        this.env = env
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
}
