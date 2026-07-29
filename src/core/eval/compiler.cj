package ystyle::xisp.core.eval

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 编译器
 *
 * 将 Lisp 表达式编译为 CompiledCode（字节码）。
 * 默认实现不做任何编译，返回 None，由 AST 解释器处理。
 * 后续可通过继承实现 BytecodeCompiler。
 */
public open class Compiler {
    /**
     * 编译单个表达式
     * @param expr 待编译的 Lisp 表达式
     * @return 编译结果，None 表示不编译（走 AST 解释）
     */
    public func compile(expr: LispValue): Option<CompiledCode> {
        None
    }

    /**
     * 编译多个表达式
     * @param exprs 待编译的表达式列表
     * @return 编译结果列表
     */
    public func compileAll(exprs: ArrayList<LispValue>): ArrayList<Option<CompiledCode>> {
        let results = ArrayList<Option<CompiledCode>>()
        for (expr in exprs) {
            results.add(this.compile(expr))
        }
        results
    }
}
