package ystyle::xisp.core.eval

import ystyle::xisp.types.*
import std.collection.ArrayList

/**
 * 字节码执行后端
 *
 * 核心**不依赖任何字节码实现**：需要字节码能力的宿主自行依赖实现模块（`ystyle::xisp_bc`），
 * 再通过 `LispInterpreter.setExecutionBackend` 注入（或该模块提供的 `withBytecodeCompiler()` 选项）。
 *
 * IR 类型（`CompiledCode` / `CompiledFunction`）定义在本包，是核心与后端之间的**协议**；
 * 后端负责"编译 + 执行"，核心只负责在求值入口决定走 AST 还是后端。
 *
 * 约定：
 * - `compile` 返回 `None` 表示本表达式不支持编译，调用方回退 AST（并在本表达式树内禁用编译）；
 * - 后端的热路径有独立的中断检查点，注入后由 `setInterruptChecker` 同步。
 */
public interface ExecutionBackend {
    /**
     * 编译表达式；返回 None 表示不支持编译（调用方回退 AST）
     */
    func compile(expr: LispValue): Option<CompiledCode>

    /**
     * 执行编译产物
     */
    func execute(code: CompiledCode, env: Environment): LispValue

    /**
     * 调用编译闭包（AST 侧高阶函数把回调交给后端执行）
     */
    func callClosure(id: Int64, args: ArrayList<LispValue>, env: Environment): LispValue

    /**
     * 把编译闭包**降级**成等价的 AST 闭包（`Procedure`）
     *
     * 用于「拿到后端产出的闭包、但要在没有后端的地方调」的场景 —— 典型是子线程 /
     * 每请求一个求值器：字节码的 `Closure` 里存的是 VM 内部函数 id，跨求值器调不通，
     * 而降级成 `Procedure(参数名, AST 体, 捕获环境)` 后就是纯数据，哪里都能跑。
     *
     * 实现要保证**只读**后端状态（不执行、不分配 VM 资源），否则调用方线程会踩到
     * VM 的类级可变状态。
     */
    func decompileClosure(id: Int64, closureEnv: Environment): Option<LispValue>

    /**
     * 同步协作式中断检查回调（后端热路径的检查点）
     */
    func setInterruptChecker(checker: () -> Bool): Unit
}
