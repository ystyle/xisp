package ystyle::xisp_bc

import ystyle::xisp.*
import ystyle::xisp.core.eval.*
import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 字节码执行后端
 *
 * 把字节码编译器与寄存器栈 VM 组合为核心要求的 `ExecutionBackend`。
 * 中断检查由核心统一注入（`Evaluator.setInterruptChecker` → 本类 → VM 热路径检查点），
 * 因此配置顺序不影响中断是否生效。
 */
public class BytecodeExecutionBackend <: ExecutionBackend {
    let compiler: BytecodeCompiler
    let vm: BytecodeVM

    public init() {
        this.compiler = BytecodeCompiler()
        this.vm = BytecodeVM()
        this.vm.setCompiler(this.compiler)
    }

    public func compile(expr: LispValue): Option<CompiledCode> {
        this.compiler.compile(expr)
    }

    public func execute(code: CompiledCode, env: Environment): LispValue {
        this.vm.execute(code, env)
    }

    public func callClosure(id: Int64, args: ArrayList<LispValue>, env: Environment): LispValue {
        this.vm.callClosure(id, args, env)
    }

    /**
     * 编译闭包 → AST 闭包（只读函数表，不碰 VM 的运行时状态）
     *
     * `Procedure(参数名列表, AST 体, 捕获环境)` 是纯数据，可以传给别的线程/求值器。
     */
    public func decompileClosure(id: Int64, closureEnv: Environment): Option<LispValue> {
        if (id < 0 || id >= BytecodeVM.functionTable.size) {
            return None
        }
        let f = BytecodeVM.functionTable[id]
        Some(LispValue.Procedure(f.params, f.body, closureEnv))
    }

    public func setInterruptChecker(checker: () -> Bool): Unit {
        this.vm.setInterruptChecker(checker)
    }
}

/**
 * 启用字节码编译器
 *
 * 将支持的表达式编译为字节码执行（提升性能）；不支持的表达式自动回退 AST 解释。
 */
public func withBytecodeCompiler(): InterpreterOption {
    { interp => interp.setExecutionBackend(BytecodeExecutionBackend()) }
}

/**
 * 启用 JIT（叠于字节码之上；未开字节码时无效）
 *
 * 未支持的指令 / 类型 → 函数级回退解释器（字节码保留）。
 */
public func withJit(): InterpreterOption {
    { _ =>
        JitRuntime()
        ()
    }
}
