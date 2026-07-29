package ystyle::xisp.core.eval
import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 编译结果
 * 表示一个表达式被编译后的代码
 */
public enum CompiledCode {
    /// 字节码（预留）
    | Bytecode(ArrayList<Int64>, ArrayList<LispValue>)
}
