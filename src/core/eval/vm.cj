package ystyle::xisp.core.eval

import ystyle::xisp.types.*

/**
 * 虚拟机（预留）
 *
 * 执行编译后的 CompiledCode（字节码）。
 * 默认实现返回错误（尚未实现），后续可通过继承实现 BytecodeVM。
 */
public open class VM {
    /**
     * 执行编译后的代码
     * @param code 编译后的代码
     * @param env 当前环境
     * @return 执行结果
     */
    public func execute(code: CompiledCode, env: Environment): LispValue {
        LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode VM not yet implemented"))
    }
}
