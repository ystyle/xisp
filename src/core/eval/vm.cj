package ystyle::xisp.core.eval
import ystyle::xisp.types.*
import std.collection.ArrayList
import std.collection.HashMap

/**
 * 虚拟机接口
 */
public interface VM {
    func execute(code: CompiledCode, env: Environment): LispValue
}

/**
 * 字节码虚拟机
 */
public class BytecodeVM <: VM {
    var registers: ArrayList<LispValue> = ArrayList<LispValue>()
    var sp: Int64 = -1
    var bp: Int64 = 0
    var currentEnv: Environment = Environment()
    static var functionTable: ArrayList<CompiledFunction> = ArrayList<CompiledFunction>()
    var curFunc: Option<CompiledFunction> = None
    var savedFrames: ArrayList<(CompiledFunction, Int64, Int64, Environment, Int64)> = ArrayList<(CompiledFunction, Int64, Int64, Environment, Int64)>()
    // 字节码编译器（供回退 AST 的子求值器使用，让 AST 边缘的子表达式仍可再编译进 VM）
    var compiler: Option<Compiler> = None
    // globalRefs 解析缓存：按编译函数 identity（从 0 连续）索引的数组缓存——
    // 免哈希查找；条目为 (解析环境, 环境版本, 值列表)
    static var globalRefCache: ArrayList<(Environment, Int64, ArrayList<LispValue>)> = ArrayList<(Environment, Int64, ArrayList<LispValue>)>()
    // 编译函数 identity → 全局闭包 id（同一函数只注册一次，避免 functionTable 膨胀）
    static var funcIds: HashMap<Int64, Int64> = HashMap<Int64, Int64>()



    /**
     * 设置字节码编译器（子求值器回退 AST 后仍可再编译）
     */
    public func setCompiler(c: Compiler) {
        this.compiler = Some(c)
    }

    /**
     * 创建子求值器（AST 回退路径）。
     * 注意：AST 求值器的局部变量（let/match/参数绑定）不在编译帧槽中，
     * 若注入编译器，子表达式编译后会把它们当作全局引用查找而导致
     * UnboundVariable——因此 AST 回退期间禁用编译（纯 AST 执行，语义正确）。
     * 编译上下文内的函数调用仍经帧切换/或编译产物自身的调用链接入 VM。
     */
    func makeSubEvaluator(env: Environment): Evaluator {
        Evaluator(env)
    }

    public func execute(code: CompiledCode, env: Environment): LispValue {
        match (code) {
            case Bytecode(func_) => this.executeFunc(func_, env)
        }
    }

    func executeFunc(func_: CompiledFunction, env: Environment): LispValue {
        // 嵌套执行安全：保存外层 VM 状态（AST 回退的子求值器可能重入本 VM）
        let savedRegisters = this.registers
        let savedSp = this.sp
        let savedBp = this.bp
        let savedEnv = this.currentEnv
        let savedFrames = this.savedFrames
        let savedCur = this.curFunc
        let result = this.runFunction(func_, env)
        this.registers = savedRegisters
        this.sp = savedSp
        this.bp = savedBp
        this.currentEnv = savedEnv
        this.savedFrames = savedFrames
        this.curFunc = savedCur
        result
    }

    func runFunction(func_: CompiledFunction, env: Environment): LispValue {
        this.registers = ArrayList<LispValue>()
        this.sp = -1
        this.currentEnv = env
        this.bp = 1000
        this.curFunc = Some(func_)
        this.savedFrames = ArrayList<(CompiledFunction, Int64, Int64, Environment, Int64)>()
        var ip: Int64 = 0
        var currentFunc = func_
        var running = true

        while (running) {
            while (ip < currentFunc.code.size && running) {
                let op = currentFunc.code[ip]
                ip++
                match (op) {
                    case 0 =>

                    if (ip < currentFunc.code.size) {
                        let idx = currentFunc.code[ip]; ip++
                        this.push(currentFunc.constants[idx])
                    } else {
                        return LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode: OP_PUSH_CONSTANT missing index"))
                    }
                

                    case 1 =>

                    this.push(Nil)
                

                    case 2 =>

                    this.push(LispValue.Boolean(true))
                

                    case 3 =>

                    this.push(LispValue.Boolean(false))
                

                    case 4 =>

                    if (this.sp >= 0) { this.sp-- }
                

                    case 8 =>
                    if (this.sp >= 1) {
                        let b = this.pop()
                        let a = this.pop()
                        if (let Int(ai) <- a && let Int(bi) <- b) {
                            this.push(LispValue.Int(ai + bi))
                        } else {
                            let result = match (a) {
                                case Int(ai) => match (b) {
                                    case Float(bf) => LispValue.Float(Float64(ai) + bf)
                                    case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: + expected number"))
                                }
                                case Float(af) => match (b) {
                                    case Int(bi) => LispValue.Float(af + Float64(bi))
                                    case Float(bf) => LispValue.Float(af + bf)
                                    case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: + expected number"))
                                }
                                case Str(s) => Str(s + b.toString())
                                case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: + expected number"))
                            }
                            this.push(result)
                        }
                    }
                

                    case 9 =>

                    if (this.sp >= 1) {
                        let b = this.pop()
                        let a = this.pop()
                        if (let Int(ai) <- a && let Int(bi) <- b) {
                            this.push(LispValue.Int(ai - bi))
                        } else {
                            let result = match (a) {
                                case Int(ai) => match (b) {
                                    case Float(bf) => LispValue.Float(Float64(ai) - bf)
                                    case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: - expected number"))
                                }
                                case Float(af) => match (b) {
                                    case Int(bi) => LispValue.Float(af - Float64(bi))
                                    case Float(bf) => LispValue.Float(af - bf)
                                    case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: - expected number"))
                                }
                                case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: - expected number"))
                            }
                            this.push(result)
                        }
                    }
                

                    case 12 =>

                    if (this.sp >= 1) {
                        let b = this.pop()
                        let a = this.pop()
                        if (let Int(ai) <- a && let Int(bi) <- b) {
                            this.push(LispValue.Int(ai * bi))
                        } else {
                            let result = match (a) {
                                case Int(ai) => match (b) {
                                    case Float(bf) => LispValue.Float(Float64(ai) * bf)
                                    case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: * expected number"))
                                }
                                case Float(af) => match (b) {
                                    case Int(bi) => LispValue.Float(af * Float64(bi))
                                    case Float(bf) => LispValue.Float(af * bf)
                                    case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: * expected number"))
                                }
                                case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: * expected number"))
                            }
                            this.push(result)
                        }
                    }
                

                    case 13 =>

                    if (this.sp >= 1) {
                        let b = this.pop()
                        let a = this.pop()
                        if (let Int(ai) <- a && let Int(bi) <- b) {
                            this.push(LispValue.Int(ai / bi))
                        } else {
                            let result = match (a) {
                                case Int(ai) => match (b) {
                                    case Float(bf) => LispValue.Float(Float64(ai) / bf)
                                    case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: / expected number"))
                                }
                                case Float(af) => match (b) {
                                    case Int(bi) => LispValue.Float(af / Float64(bi))
                                    case Float(bf) => LispValue.Float(af / bf)
                                    case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: / expected number"))
                                }
                                case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: / expected number"))
                            }
                            this.push(result)
                        }
                    }
                

                    case 14 =>

                    if (this.sp >= 1) {
                        let b = this.pop()
                        let a = this.pop()
                        if (let Int(ai) <- a && let Int(bi) <- b) {
                            this.push(LispValue.Boolean(ai < bi))
                        } else {
                            let result = match (a) {
                                case Int(ai) => match (b) {
                                    case Int(bi) => LispValue.Boolean(ai < bi)
                                    case Float(bf) => LispValue.Boolean(Float64(ai) < bf)
                                    case _ => LispValue.Boolean(false)
                                }
                                case Float(af) => match (b) {
                                    case Int(bi) => LispValue.Boolean(af < Float64(bi))
                                    case Float(bf) => LispValue.Boolean(af < bf)
                                    case _ => LispValue.Boolean(false)
                                }
                                case _ => LispValue.Boolean(false)
                            }
                            this.push(result)
                        }
                    }
                

                    case 15 =>

                    if (this.sp >= 1) {
                        let b = this.pop()
                        let a = this.pop()
                        if (let Int(ai) <- a && let Int(bi) <- b) {
                            this.push(LispValue.Boolean(ai > bi))
                        } else {
                            let result = match (a) {
                                case Int(ai) => match (b) {
                                    case Int(bi) => LispValue.Boolean(ai > bi)
                                    case Float(bf) => LispValue.Boolean(Float64(ai) > bf)
                                    case _ => LispValue.Boolean(false)
                                }
                                case Float(af) => match (b) {
                                    case Int(bi) => LispValue.Boolean(af > Float64(bi))
                                    case Float(bf) => LispValue.Boolean(af > bf)
                                    case _ => LispValue.Boolean(false)
                                }
                                case _ => LispValue.Boolean(false)
                            }
                            this.push(result)
                        }
                    }
                

                    case 16 =>

                    if (this.sp >= 1) {
                        let b = this.pop()
                        let a = this.pop()
                        if (let Int(ai) <- a && let Int(bi) <- b) {
                            this.push(LispValue.Boolean(ai == bi))
                        } else {
                            this.push(if (this.equalValue(a, b)) { Boolean(true) } else { Boolean(false) })
                        }
                    }
                

                    case 17 =>

                    if (this.sp >= 0) {
                        let v = this.pop()
                        let result = match (v) {
                            case Boolean(b) => LispValue.Boolean(!b)
                            case Nil => LispValue.Boolean(true)
                            case _ => LispValue.Boolean(false)
                        }
                        this.push(result)
                    }
                

                    case 23 =>

                    if (this.sp >= 0) {
                        this.push(this.registers[this.sp])
                    }
                

                    case 5 =>

                    if (ip < currentFunc.code.size) {
                        let nameIdx = currentFunc.code[ip]; ip++
                        let value = this.pop()
                        match (currentFunc.constants[nameIdx]) {
                            case Str(name) =>
                                this.currentEnv.define(name, value)
                                this.push(value)
                            case _ => return LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode: STORE_GLOBAL expected string"))
                        }
                    }
                

                    case 6 =>

                    if (ip < currentFunc.code.size) {
                        let nameIdx = currentFunc.code[ip]; ip++
                        match (currentFunc.constants[nameIdx]) {
                            case Str(name) =>
                                let val = env.lookup(name)
                                this.push(val)
                            case _ => return LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode: LOAD_GLOBAL expected string"))
                        }
                    }
                

                    case 10 =>

                    if (ip < currentFunc.code.size) {
                        let target = currentFunc.code[ip]; ip++
                        ip = target
                    }
                

                    case 11 =>

                    if (ip < currentFunc.code.size) {
                        let target = currentFunc.code[ip]; ip++
                        if (this.sp >= 0) {
                            let v = this.pop()
                            // 与 AST 的 isTruthy 语义一致：nil/false/0/0.0 为假
                            if (!v.isTruthy()) {
                                ip = target
                            }
                        }
                    }
                

                    case 48 =>

                    if (ip < currentFunc.code.size) {
                        let nameIdx = currentFunc.code[ip]; ip++
                        let value = this.pop()
                        match (currentFunc.constants[nameIdx]) {
                            case Str(name) =>
                                // set 语义：修改既有绑定（链查找），未定义返回 Nil（与 AST evalSet 一致）
                                if (this.currentEnv.set(name, value)) {
                                    this.push(value)
                                } else {
                                    this.push(Nil)
                                }
                            case _ => return LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode: SET_GLOBAL expected string"))
                        }
                    }
                

                    case 49 =>

                    if (this.sp >= 1) {
                        // 栈顶 = car，次顶 = cdr（序列构造：push 从右到左）
                        let a = this.pop()
                        let b = this.pop()
                        this.push(Cons(ConsCell(a, b)))
                    }
                

                    case 50 =>

                    if (this.sp >= 1) {
                        let b = this.pop()
                        let a = this.pop()
                        this.push(this.appendCopy(a, b))
                    }
                

                    case 51 =>

                    if (ip < currentFunc.code.size) {
                        let prefixCount = currentFunc.code[ip]; ip++
                        let result = this.applyCall(prefixCount, env)
                        match (result) {
                            case Error(_) => return result
                            case _ => this.push(result)
                        }
                    }
                

                    case 7 =>

                    if (ip < currentFunc.code.size) {
                        let argCount = currentFunc.code[ip]; ip++
                        let startIdx = this.sp - argCount + 1
                        if (startIdx >= 0) {
                            let funcVal = this.registers[startIdx]
                            // 字符串函数名 → 先解析为环境中的值（顶层编译压入的是名字常量；
                            // 宏/未定义仍需保留名字走 callFunction 的宏分支）
                            if (let Str(name) <- funcVal) {
                                let resolved = this.currentEnv.lookup(name)
                                match (resolved) {
                                    case Closure(_, _) | NativeFunc(_) | Procedure(_, _, _) | ProcedureFromParams(_, _, _) =>
                                        this.registers[startIdx] = resolved
                                    case _ => ()
                                }
                            }
                            // 帧切换快速路径：函数值（Closure）已就位 → 切换编译帧执行
                            if (let Some((nf, nip)) <- this.switchFrame(argCount, startIdx, currentFunc, ip)) {
                                currentFunc = nf
                                ip = nip
                                continue
                            }
                        }
                        // 普通函数调用（NativeFunc / Procedure / Macro / 错误等）
                        let result = this.callFunction(argCount, env)
                        match (result) {
                            case Error(_) => return result
                            case _ => this.push(result)
                        }
                    }
                

                    case 52 =>

                    if (ip + 2 < currentFunc.code.size) {
                        let binKind = currentFunc.code[ip]; ip++
                        let localIdx = currentFunc.code[ip]; ip++
                        let constIdx = currentFunc.code[ip]; ip++
                        this.push(this.binNum(this.getReg(this.bp + localIdx), currentFunc.constants[constIdx], binKind))
                    }
                

                    case 56 =>

                    if (ip + 3 < currentFunc.code.size) {
                        let cmpKind = currentFunc.code[ip]; ip++
                        let localIdx = currentFunc.code[ip]; ip++
                        let constIdx = currentFunc.code[ip]; ip++
                        let target = currentFunc.code[ip]; ip++
                        let a = this.getReg(this.bp + localIdx)
                        let b = currentFunc.constants[constIdx]
                        let cond = match (cmpKind) {
                            case 0 => this.compareLT(a, b)
                            case 1 => this.compareGT(a, b)
                            case _ => this.compareEQ(a, b)
                        }
                        if (!cond) {
                            ip = target
                        }
                    }
                

                    case 57 =>

                    if (ip < currentFunc.code.size) {
                        let target = currentFunc.code[ip]; ip++
                        if (this.sp >= 0) {
                            let v = this.registers[this.sp]
                            if (!v.isTruthy()) {
                                // and 假值语义：规范化为 Boolean(false)（与 AST NativeFunc 一致）
                                this.registers[this.sp] = LispValue.Boolean(false)
                                ip = target
                            }
                        }
                    }
                

                    case 58 =>

                    if (ip < currentFunc.code.size) {
                        let target = currentFunc.code[ip]; ip++
                        if (this.sp >= 0) {
                            let v = this.registers[this.sp]
                            if (v.isTruthy()) {
                                ip = target
                            }
                        }
                    }
                

                    case 59 =>

                    if (ip + 1 < currentFunc.code.size) {
                        let idx = currentFunc.code[ip]; ip++
                        let target = currentFunc.code[ip]; ip++
                        if (this.sp >= 0) {
                            // 不匹配：值留栈（供下一子句匹配），仅跳转；匹配：继续（值在栈，后续 POP）
                            if (!this.equalValue(this.registers[this.sp], currentFunc.constants[idx])) {
                                ip = target
                            }
                        }
                    }
                

                    case 60 =>

                    if (this.sp >= 0) {
                        let v = this.registers[this.sp]
                        if (!v.isTruthy()) {
                            this.registers[this.sp] = LispValue.Boolean(false)
                        }
                    }
                

                    case 99 =>

                    running = false
                    break
                

                    case 38 =>

                    if (ip + 1 < currentFunc.code.size) {
                        let idx = currentFunc.code[ip]; ip++
                        let target = currentFunc.code[ip]; ip++
                        if (this.sp >= 0) {
                            let val = this.pop()
                            let lit = currentFunc.constants[idx]
                            if (!this.equalValue(val, lit)) {
                                ip = target
                            }
                        }
                    }
                

                    case 39 =>

                    if (ip + 1 < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let idx = currentFunc.code[ip]; ip++
                        this.setReg(rd, this.getReg(this.bp + idx))
                    }
                

                    case 40 =>

                    if (ip + 1 < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let sIdx = currentFunc.code[ip]; ip++
                        match (currentFunc.constants[sIdx]) {
                            case Str(name) => this.setReg(rd, this.currentEnv.lookup(name))
                            case _ => this.setReg(rd, Nil)
                        }
                    }
                

                    case 30 =>

                    if (ip + 1 < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let idx = currentFunc.code[ip]; ip++
                        this.setReg(rd, currentFunc.constants[idx])
                    }
                

                    case 31 =>

                    if (ip + 2 < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let rs1 = currentFunc.code[ip]; ip++
                        let rs2 = currentFunc.code[ip]; ip++
                        this.binopReg(rd, rs1, rs2, 0)
                    }
                

                    case 32 =>

                    if (ip + 2 < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let rs1 = currentFunc.code[ip]; ip++
                        let rs2 = currentFunc.code[ip]; ip++
                        this.binopReg(rd, rs1, rs2, 1)
                    }
                

                    case 33 =>

                    if (ip + 2 < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let rs1 = currentFunc.code[ip]; ip++
                        let rs2 = currentFunc.code[ip]; ip++
                        this.binopReg(rd, rs1, rs2, 2)
                    }
                

                    case 34 =>

                    if (ip + 2 < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let rs1 = currentFunc.code[ip]; ip++
                        let rs2 = currentFunc.code[ip]; ip++
                        this.binopReg(rd, rs1, rs2, 3)
                    }
                

                    case 35 =>

                    if (ip + 1 < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let rs = currentFunc.code[ip]; ip++
                        this.unopNegReg(rd, rs)
                    }
                

                    case 41 =>

                    if (ip + 1 < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let rs = currentFunc.code[ip]; ip++
                        let v = this.getReg(rs)
                        this.setReg(rd, match (v) {
                            case Boolean(b) => LispValue.Boolean(!b)
                            case Nil => LispValue.Boolean(true)
                            case _ => LispValue.Boolean(false)
                        })
                    }
                

                    case 36 =>

                    if (ip + 1 < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let rs = currentFunc.code[ip]; ip++
                        this.setReg(rd, this.getReg(rs))
                    }
                

                    case 44 =>

                    if (ip + 2 < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let rs1 = currentFunc.code[ip]; ip++
                        let rs2 = currentFunc.code[ip]; ip++
                        let a = this.getReg(rs1)
                        let b = this.getReg(rs2)
                        this.setReg(rd, this.cmpLT(a, b))
                    }
                

                    case 45 =>

                    if (ip + 2 < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let rs1 = currentFunc.code[ip]; ip++
                        let rs2 = currentFunc.code[ip]; ip++
                        let a = this.getReg(rs1)
                        let b = this.getReg(rs2)
                        this.setReg(rd, this.cmpGT(a, b))
                    }
                

                    case 46 =>

                    if (ip + 2 < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let rs1 = currentFunc.code[ip]; ip++
                        let rs2 = currentFunc.code[ip]; ip++
                        let a = this.getReg(rs1)
                        let b = this.getReg(rs2)
                        this.setReg(rd, this.cmpEQ(a, b))
                    }
                

                    case 47 =>

                    if (ip < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        let val = this.pop()
                        this.setReg(rd, val)
                    }
                

                    case 37 =>

                    if (ip < currentFunc.code.size) {
                        let rd = currentFunc.code[ip]; ip++
                        this.push(this.getReg(rd))
                    }
                

                    case 24 =>

                    if (ip < currentFunc.code.size) {
                        let idx = currentFunc.code[ip]; ip++
                        if (this.bp + idx < this.registers.size) {
                            this.push(this.registers[this.bp + idx])
                        } else {
                            this.push(Nil)
                        }
                    }
                

                    case 25 =>

                    if (ip < currentFunc.code.size) {
                        let idx = currentFunc.code[ip]; ip++
                        let val = this.pop()
                        let targetIdx = this.bp + idx
                        while (targetIdx >= this.registers.size) {
                            this.registers.add(Nil)
                        }
                        this.registers[targetIdx] = val
                        // 绑定不是表达式值，不保留在栈上（保持栈平衡）
                    }
                

                    case 42 =>

                    if (ip < currentFunc.code.size) {
                        let idx = currentFunc.code[ip]; ip++
                        let capCount = currentFunc.code[ip]; ip++
                        // 捕获环境：child of currentEnv + 复制外层帧槽值（闭包捕获语义）
                        // 捕获环境：独立 env（不共享 defineVersion——捕获变量的定义不应使
                        // 其他环境的 globalRefs 缓存失效；闭包内 set! 仍通过自身版本正确失效）
                        let capEnv = Environment()
                        capEnv.parent = Some(this.currentEnv)
                        for (i in 0..capCount) {
                            let nameIdx = currentFunc.code[ip]; ip++
                            let slot = currentFunc.code[ip]; ip++
                            let val = if (this.bp + slot < this.registers.size) {
                                this.registers[this.bp + slot]
                            } else {
                                Nil
                            }
                            match (currentFunc.constants[nameIdx]) {
                                case Str(name) => capEnv.defineSilent(name, val)
                                case _ => return LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode: MAKE_CLOSURE expected string"))
                            }
                        }
                        if (let Some(f) <- this.curFunc && idx < f.functions.size) {
                            let subFunc = f.functions[idx]
                            let globalId = if (BytecodeVM.funcIds.contains(subFunc.identity)) {
                                BytecodeVM.funcIds[subFunc.identity]
                            } else {
                                let newId = BytecodeVM.functionTable.size
                                BytecodeVM.functionTable.add(subFunc)
                                BytecodeVM.funcIds[subFunc.identity] = newId
                                newId
                            }
                            this.push(Closure(globalId, capEnv))
                        } else {
                            this.push(Nil)
                        }
                    }
                

                    case 43 =>

                    let result = if (this.sp >= 0) { this.registers[this.sp] } else { Nil }
                    if (let Some((prevFunc, prevIp)) <- this.restoreFrame()) {
                        currentFunc = prevFunc
                        ip = prevIp
                        while (this.sp >= this.registers.size) {
                            this.registers.add(Nil)
                        }
                        this.registers[this.sp] = result
                        continue
                    } else {
                        running = false
                        break
                    }
                

                    case _ =>

                    return LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode: unknown opcode ${op}"))
                

                }
            }
            // 函数执行完毕，检查是否有保存的帧
            if (this.savedFrames.size > 0) {
                let result = if (this.sp >= 0) { this.registers[this.sp] } else { Nil }
                if (let Some((prevFunc, prevIp)) <- this.restoreFrame()) {
                    currentFunc = prevFunc
                    ip = prevIp
                    while (this.sp >= this.registers.size) {
                        this.registers.add(Nil)
                    }
                    this.registers[this.sp] = result
                } else {
                    break
                }
            } else {
                break
            }
        }

        if (this.sp >= 0) {
            this.registers[this.sp]
        } else {
            Nil
        }
    }

    func push(v: LispValue) {
        this.sp++
        // sp 可能跳过中间位置（帧切换直接定位到局部槽顶），补齐 gap 防越界
        while (this.sp >= this.registers.size) {
            this.registers.add(Nil)
        }
        this.registers[this.sp] = v
    }

    func pop(): LispValue {
        if (this.sp >= 0) {
            let v = this.registers[this.sp]
            this.sp--
            v
        } else {
            Nil
        }
    }

    /**
     * 帧切换快速路径：将栈上的 Closure 调用切换为编译帧执行。
     * 参数在寄存器中原地左移一位（覆盖函数值槽），避免参数复制分配；
     * 帧布局：局部槽区 [bp, bp+localSlots-1]（参数/全局引用/let 绑定），
     * 栈区从 bp+localSlots 之上开始，与局部槽区完全分离；
     * 全局引用每次调用实时解析（保证 set!/重新 define 语义正确，宏保留名字供展开）。
     * 成功返回 Some((下一函数, 下一指令指针 0))，失败返回 None。
     */
    func switchFrame(argCount: Int64, startIdx: Int64, currentFunc: CompiledFunction, ip: Int64): Option<(CompiledFunction, Int64)> {
        let funcVal = this.registers[startIdx]
        if (let Closure(id, closureEnv) <- funcVal) {
            if (id < BytecodeVM.functionTable.size) {
                let nextFunc = BytecodeVM.functionTable[id]
                let paramCount = argCount - 1
                // 参数原地左移一位：registers[startIdx+1..startIdx+paramCount] → registers[startIdx..startIdx+paramCount-1]
                var i: Int64 = 0
                while (i < paramCount) {
                    this.registers[startIdx + i] = this.registers[startIdx + i + 1]
                    i++
                }
                // 保存调用者帧（含当前环境与调用点函数槽位置——返回时结果写入该槽）
                this.savedFrames.add((currentFunc, ip, this.bp, this.currentEnv, startIdx))
                // 设置新帧：局部槽区 [bp, bp+localSlots-1]，栈区从 bp+localSlots 之上开始
                this.bp = startIdx
                this.sp = startIdx + nextFunc.localSlots - 1
                while (this.sp >= this.registers.size) {
                    this.registers.add(Nil)
                }
                this.currentEnv = closureEnv
                this.curFunc = Some(nextFunc)
                // 解析函数体的全局引用到帧槽（版本号缓存：环境未变时命中；正确性由
                // defineVersion（共享 cell）与解析环境引用（refEq）双重保证）
                if (nextFunc.globalRefs.size > 0) {
                    let cacheKey = nextFunc.identity
                    var cached: (Environment, Int64, ArrayList<LispValue>) = (this.currentEnv, -1, ArrayList<LispValue>())
                    if (cacheKey < BytecodeVM.globalRefCache.size) {
                        cached = BytecodeVM.globalRefCache[cacheKey]
                    }
                    var values: ArrayList<LispValue> = ArrayList<LispValue>()
                    var cacheHit = false
                    if (cached[1] >= 0 && refEq(cached[0], this.currentEnv) && cached[1] == Environment.defineVersion.value) {
                        values = cached[2]
                        cacheHit = true
                    }
                    if (!cacheHit) {
                        let resolved = ArrayList<LispValue>()
                        for (i in 0..nextFunc.globalRefs.size) {
                            let name = nextFunc.globalRefs[i]
                            let val = this.currentEnv.lookup(name)
                            // 宏值需要保留名字（callFunction 依赖名字重建调用表达式做展开）
                            let slotVal = match (val) {
                                case Macro(_, _, _) | MacroFromParams(_, _, _) => Str(name)
                                case _ => val
                            }
                            resolved.add(slotVal)
                        }
                        while (cacheKey >= BytecodeVM.globalRefCache.size) {
                            BytecodeVM.globalRefCache.add((this.currentEnv, -1, ArrayList<LispValue>()))
                        }
                        BytecodeVM.globalRefCache[cacheKey] = (this.currentEnv, Environment.defineVersion.value, resolved)
                        values = resolved
                    }
                    for (i in 0..values.size) {
                        let slotIdx = if (i < nextFunc.globalSlots.size) { nextFunc.globalSlots[i] } else { nextFunc.params.size + i }
                        let targetIdx = this.bp + slotIdx
                        while (targetIdx >= this.registers.size) {
                            this.registers.add(Nil)
                        }
                        this.registers[targetIdx] = values[i]
                    }
                }
                Some((nextFunc, 0))
            } else {
                None
            }
        } else {
            None
        }
    }

    /**
     * 弹出调用者帧并恢复其执行状态（bp/env/sp/curFunc）。
     * 恢复后 sp 指向调用点函数槽（prevCallIdx）——函数调用的结果槽：
     * 帧区域收缩为单值（结果写入该槽），使调用点栈上与后续操作数毗邻，
     * 若不存在调用者帧返回 None。
     */
    func restoreFrame(): Option<(CompiledFunction, Int64)> {
        if (this.savedFrames.size > 0) {
            let (prevFunc, prevIp, prevBp, prevEnv, prevCallIdx) = this.savedFrames[this.savedFrames.size - 1]
            this.savedFrames.remove((this.savedFrames.size - 1)..(this.savedFrames.size))
            this.bp = prevBp
            this.currentEnv = prevEnv
            this.curFunc = Some(prevFunc)
            this.sp = prevCallIdx
            Some((prevFunc, prevIp))
        } else {
            None
        }
    }

    /**
     * (apply f a b ... lastList)：弹出 funcVal + 前置参数 + 最后列表，
     * 展开列表后分派调用（Closure 走 AST 兜底——apply 是动态调用，无静态调用点）
     */
    func applyCall(prefixCount: Int64, env: Environment): LispValue {
        let lastList = this.pop()
        // 收集前置参数（栈上逆序）
        let reversed = ArrayList<LispValue>()
        for (i in 0..prefixCount) {
            reversed.add(this.pop())
        }
        let args = ArrayList<LispValue>()
        var i = reversed.size - 1
        while (i >= 0) {
            args.add(reversed[i])
            i--
        }
        // 展开最后列表
        var current = lastList
        while (let Cons(c) <- current) {
            args.add(c.car)
            current = c.cdr
        }
        let funcVal = this.pop()
        match (funcVal) {
            case Str(name) =>
                let value = this.currentEnv.lookup(name)
                this.callApplyValue(value, args)
            case _ => this.callApplyValue(funcVal, args)
        }
    }

    func callApplyValue(funcVal: LispValue, args: ArrayList<LispValue>): LispValue {
        match (funcVal) {
            case NativeFunc(f) => f(args)
            case Procedure(params, body, closureEnv) =>
                this.callProcedure(params, body, closureEnv, args)
            case ProcedureFromParams(paramInfos, body, closureEnv) =>
                this.callProcedureFromParams(paramInfos, body, closureEnv, args)
            case Closure(id, closureEnv) =>
                if (id < BytecodeVM.functionTable.size) {
                    let compiledFunc = BytecodeVM.functionTable[id]
                    this.callProcedure(compiledFunc.params, compiledFunc.body, closureEnv, args)
                } else {
                    LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode: invalid closure ID"))
                }
            case other =>
                LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode: apply expected function (got: ${other.toString()})"))
        }
    }

    /**
     * appendCopy(a, b)：复制 a 的链表（尾部接 b）；a 为 Nil → b；a 非列表 → b（splice 语义）
     */
    func appendCopy(a: LispValue, b: LispValue): LispValue {
        match (a) {
            case Nil => b
            case Cons(cell) => this.copyConsChain(cell, b)
            case _ => b
        }
    }

    func copyConsChain(cell: ConsCell, tail: LispValue): LispValue {
        let head = Cons(ConsCell(cell.car, Nil))
        var last: LispValue = head
        var current = cell.cdr
        while (let Cons(nc) <- current) {
            let next = Cons(ConsCell(nc.car, Nil))
            match (last) {
                case Cons(lc) => lc.cdr = next
                case _ => ()
            }
            last = next
            current = nc.cdr
        }
        match (last) {
            case Cons(lc) => lc.cdr = tail
            case _ => ()
        }
        head
    }

    func callFunction(argCount: Int64, env: Environment): LispValue {        let startIdx = this.sp - argCount + 1
        if (startIdx < 0) {
            return LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode: CALL with insufficient stack"))
        }
        let funcVal = this.registers[startIdx]
        let args = ArrayList<LispValue>()
        for (i in 1..argCount) {
            args.add(this.registers[startIdx + i])
        }
        this.sp -= argCount

        match (funcVal) {
            case Str(name) =>
                let value = this.currentEnv.lookup(name)
                match (value) {
                    case NativeFunc(f) => f(args)
                    case Procedure(params, body, closureEnv) =>
                        this.callProcedure(params, body, closureEnv, args)
                    case ProcedureFromParams(paramInfos, body, closureEnv) =>
                        this.callProcedureFromParams(paramInfos, body, closureEnv, args)
                    case Macro(_, _, _) | MacroFromParams(_, _, _) =>
                        this.callMacro(name, args)
                    case Closure(id, closureEnv) =>
                        if (id < BytecodeVM.functionTable.size) {
                            let compiledFunc = BytecodeVM.functionTable[id]
                            this.callProcedure(compiledFunc.params, compiledFunc.body, closureEnv, args)
                        } else {
                            LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode: invalid closure ID"))
                        }
                    case Error(_) => value
                    case _ =>
                        LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode: cannot call '${name}'"))
                }
            case NativeFunc(f) => f(args)
            case Procedure(params, body, closureEnv) =>
                this.callProcedure(params, body, closureEnv, args)
            case ProcedureFromParams(paramInfos, body, closureEnv) =>
                this.callProcedureFromParams(paramInfos, body, closureEnv, args)
            case Closure(id, closureEnv) =>
                if (id < BytecodeVM.functionTable.size) {
                    let compiledFunc = BytecodeVM.functionTable[id]
                    this.callProcedure(compiledFunc.params, compiledFunc.body, closureEnv, args)
                } else {
                    LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode: invalid closure ID"))
                }
            case other =>
                LispValue.Error(XispError(ErrorType.RuntimeError, "Bytecode: expected function (got: ${other.toString()})"))
        }
    }

    func callMacro(name: String, args: ArrayList<LispValue>): LispValue {
        let expr = this.buildCallExpr(name, args)
        let tempEval = this.makeSubEvaluator(this.currentEnv)
        tempEval.eval(expr)
    }

    /**
     * 构造宏调用表达式。
     * 编译路径下宏参数已被求值为值；宏在 AST 语义下收到的是未求值表达式（语法）。
     * 将参数 quote 化（(quote value)）恢复"值作为字面"语义——常见宏（参数原样嵌入
     * 展开并求值的位置）行为与 AST 完全等价。
     */
    func buildCallExpr(name: String, args: ArrayList<LispValue>): LispValue {
        var result: LispValue = Nil
        var i = args.size - 1
        while (i >= 0) {
            let quoted = Cons(ConsCell(Symbol("quote"), Cons(ConsCell(args[i], Nil))))
            result = Cons(ConsCell(quoted, result))
            i--
        }
        Cons(ConsCell(Symbol(name), result))
    }

    func callProcedure(params: ArrayList<String>, body: LispValue, closureEnv: Environment, args: ArrayList<LispValue>): LispValue {
        let childEnv = closureEnv.createChild()
        var i = 0
        var argIndex = 0
        while (i < params.size) {
            let param = params[i]
            if (param.startsWith("&rest-")) {
                let restName = param[6..]
                let restArgs = ArrayList<LispValue>()
                while (argIndex < args.size) {
                    restArgs.add(args[argIndex])
                    argIndex++
                }
                var restList: LispValue = Nil
                var j = restArgs.size - 1
                while (j >= 0) {
                    restList = Cons(ConsCell(restArgs[j], restList))
                    j--
                }
                childEnv.define(restName, restList)
                break
            } else {
                if (argIndex < args.size) {
                    childEnv.define(param, args[argIndex])
                    argIndex++
                } else {
                    childEnv.define(param, Nil)
                }
                i++
            }
        }
        let tempEval = this.makeSubEvaluator(childEnv)
        tempEval.evalBegin(body)
    }

    func callProcedureFromParams(paramInfos: ArrayList<ParamInfo>, body: LispValue, closureEnv: Environment, args: ArrayList<LispValue>): LispValue {
        let childEnv = closureEnv.createChild()
        var argIndex = 0
        var i = 0

        // 阶段一：位置参数绑定
        while (i < paramInfos.size && argIndex < args.size) {
            let paramInfo = paramInfos[i]
            if (paramInfo.isKey || paramInfo.isRest) { break }
            childEnv.define(paramInfo.name, args[argIndex])
            i++
            argIndex++
        }

        // 跳过未绑定的位置参数区域
        while (i < paramInfos.size && !paramInfos[i].isKey && !paramInfos[i].isRest) {
            i++
        }

        // 阶段二：命名参数绑定
        let keywordArgs = HashMap<String, LispValue>()
        var j = argIndex
        while (j < args.size - 1) {
            if (let Symbol(sym) <- args[j] && sym.startsWith(":")) {
                let keyName = sym[1..]
                keywordArgs[keyName] = args[j + 1]
                j += 2
            } else { break }
        }

        while (i < paramInfos.size) {
            let paramInfo = paramInfos[i]
            if (paramInfo.isRest) { break }
            if (paramInfo.isKey) {
                if (keywordArgs.contains(paramInfo.name)) {
                    match (keywordArgs.get(paramInfo.name)) {
                        case Some(v) => childEnv.define(paramInfo.name, v)
                        case None => ()
                    }
                } else {
                    match (paramInfo.defaultValue) {
                        case Some(defaultVal) => childEnv.define(paramInfo.name, defaultVal)
                        case None => childEnv.define(paramInfo.name, Nil)
                    }
                }
            }
            i++
        }

        // 阶段三：可变参数（简化处理）
        while (i < paramInfos.size) {
            let paramInfo = paramInfos[i]
            if (paramInfo.isRest) {
                let restArgs = ArrayList<LispValue>()
                while (j < args.size) { restArgs.add(args[j]); j++ }
                var restList: LispValue = Nil
                var ri = restArgs.size - 1
                while (ri >= 0) {
                    restList = Cons(ConsCell(restArgs[ri], restList))
                    ri--
                }
                childEnv.define(paramInfo.name, restList)
                break
            }
            i++
        }

        let tempEval = this.makeSubEvaluator(childEnv)
        tempEval.evalBegin(body)
    }

    // 寄存器操作
    func setReg(rd: Int64, v: LispValue) {
        while (rd >= this.registers.size) {
            this.registers.add(Nil)
        }
        this.registers[rd] = v
    }

    func getReg(rs: Int64): LispValue {
        if (rs < this.registers.size) {
            this.registers[rs]
        } else {
            Nil
        }
    }

    func binopReg(rd: Int64, rs1: Int64, rs2: Int64, opKind: Int64) {
        let a = this.getReg(rs1)
        let b = this.getReg(rs2)
        let result = match (a) {
            case LispValue.Int(ai) =>
                match (b) {
                    case LispValue.Int(bi) =>
                        Some(match (opKind) {
                            case 0 => LispValue.Int(ai + bi)
                            case 1 => LispValue.Int(ai - bi)
                            case 2 => LispValue.Int(ai * bi)
                            case _ => LispValue.Int(ai / bi)
                        })
                    case LispValue.Float(bf) =>
                        Some(match (opKind) {
                            case 0 => LispValue.Float(Float64(ai) + bf)
                            case 1 => LispValue.Float(Float64(ai) - bf)
                            case 2 => LispValue.Float(Float64(ai) * bf)
                            case _ => LispValue.Float(Float64(ai) / bf)
                        })
                    case _ => None<LispValue>
                }
            case LispValue.Float(af) =>
                match (b) {
                    case LispValue.Int(bi) =>
                        Some(match (opKind) {
                            case 0 => LispValue.Float(af + Float64(bi))
                            case 1 => LispValue.Float(af - Float64(bi))
                            case 2 => LispValue.Float(af * Float64(bi))
                            case _ => LispValue.Float(af / Float64(bi))
                        })
                    case LispValue.Float(bf) =>
                        Some(match (opKind) {
                            case 0 => LispValue.Float(af + bf)
                            case 1 => LispValue.Float(af - bf)
                            case 2 => LispValue.Float(af * bf)
                            case _ => LispValue.Float(af / bf)
                        })
                    case _ => None<LispValue>
                }
            case _ => None<LispValue>
        }
        match (result) {
            case Some(v) => this.setReg(rd, v)
            case None => this.setReg(rd, LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: expected number")))
        }
    }

    func unopNegReg(rd: Int64, rs: Int64) {
        let a = this.getReg(rs)
        match (a) {
            case LispValue.Int(ai) => this.setReg(rd, LispValue.Int(-ai))
            case LispValue.Float(af) => this.setReg(rd, LispValue.Float(-af))
            case _ => this.setReg(rd, LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: expected number")))
        }
    }

    /**
     * 数值二元运算（L+C 超指令用）：opKind 0=+ 1=- 2=* 3=/（Int 快路径）
     */
    func binNum(a: LispValue, b: LispValue, opKind: Int64): LispValue {
        if (let LispValue.Int(ai) <- a && let LispValue.Int(bi) <- b) {
            return match (opKind) {
                case 0 => LispValue.Int(ai + bi)
                case 1 => LispValue.Int(ai - bi)
                case 2 => LispValue.Int(ai * bi)
                case _ => LispValue.Int(ai / bi)
            }
        }
        match (a) {
            case LispValue.Int(ai) =>
                match (b) {
                    case LispValue.Float(bf) =>
                        match (opKind) {
                            case 0 => LispValue.Float(Float64(ai) + bf)
                            case 1 => LispValue.Float(Float64(ai) - bf)
                            case 2 => LispValue.Float(Float64(ai) * bf)
                            case _ => LispValue.Float(Float64(ai) / bf)
                        }
                    case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: expected number"))
                }
            case LispValue.Float(af) =>
                match (b) {
                    case LispValue.Int(bi) =>
                        match (opKind) {
                            case 0 => LispValue.Float(af + Float64(bi))
                            case 1 => LispValue.Float(af - Float64(bi))
                            case 2 => LispValue.Float(af * Float64(bi))
                            case _ => LispValue.Float(af / Float64(bi))
                        }
                    case LispValue.Float(bf) =>
                        match (opKind) {
                            case 0 => LispValue.Float(af + bf)
                            case 1 => LispValue.Float(af - bf)
                            case 2 => LispValue.Float(af * bf)
                            case _ => LispValue.Float(af / bf)
                        }
                    case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: expected number"))
                }
            case _ => LispValue.Error(XispError(ErrorType.TypeError, "Bytecode: expected number"))
        }
    }

    func compareLT(a: LispValue, b: LispValue): Bool {
        match (a) {
            case LispValue.Int(ai) =>
                match (b) {
                    case LispValue.Int(bi) => ai < bi
                    case LispValue.Float(bf) => Float64(ai) < bf
                    case _ => false
                }
            case LispValue.Float(af) =>
                match (b) {
                    case LispValue.Int(bi) => af < Float64(bi)
                    case LispValue.Float(bf) => af < bf
                    case _ => false
                }
            case _ => false
        }
    }

    func compareGT(a: LispValue, b: LispValue): Bool {
        match (a) {
            case LispValue.Int(ai) =>
                match (b) {
                    case LispValue.Int(bi) => ai > bi
                    case LispValue.Float(bf) => Float64(ai) > bf
                    case _ => false
                }
            case LispValue.Float(af) =>
                match (b) {
                    case LispValue.Int(bi) => af > Float64(bi)
                    case LispValue.Float(bf) => af > bf
                    case _ => false
                }
            case _ => false
        }
    }

    func compareEQ(a: LispValue, b: LispValue): Bool {
        match (a) {
            case LispValue.Int(ai) =>
                match (b) {
                    case LispValue.Int(bi) => ai == bi
                    case LispValue.Float(bf) => Float64(ai) == bf
                    case _ => false
                }
            case LispValue.Float(af) =>
                match (b) {
                    case LispValue.Int(bi) => af == Float64(bi)
                    case LispValue.Float(bf) => af == bf
                    case _ => false
                }
            case _ => false
        }
    }

    func equalValue(a: LispValue, b: LispValue): Bool {
        // Int 快路径（match 字面量模式最常见）
        if (let LispValue.Int(ai) <- a && let LispValue.Int(bi) <- b) {
            return ai == bi
        }
        if (let LispValue.Float(af) <- a && let LispValue.Float(bf) <- b) {
            return af == bf
        }
        match (a) {
            case LispValue.Int(ai) => match (b) { case LispValue.Int(bi) => ai == bi; case _ => false }
            case LispValue.Float(af) => match (b) { case LispValue.Float(bf) => af == bf; case _ => false }
            case Str(sa) => match (b) { case Str(sb) => sa == sb; case _ => false }
            case Boolean(ba) => match (b) { case Boolean(bb) => ba == bb; case _ => false }
            case Nil => match (b) { case Nil => true; case _ => false }
            case _ => false
        }
    }

    func cmpLT(a: LispValue, b: LispValue): LispValue {
        match (a) {
            case LispValue.Int(ai) =>
                match (b) {
                    case LispValue.Int(bi) => LispValue.Boolean(ai < bi)
                    case LispValue.Float(bf) => LispValue.Boolean(Float64(ai) < bf)
                    case _ => LispValue.Boolean(false)
                }
            case LispValue.Float(af) =>
                match (b) {
                    case LispValue.Int(bi) => LispValue.Boolean(af < Float64(bi))
                    case LispValue.Float(bf) => LispValue.Boolean(af < bf)
                    case _ => LispValue.Boolean(false)
                }
            case _ => LispValue.Boolean(false)
        }
    }

    func cmpGT(a: LispValue, b: LispValue): LispValue {
        match (a) {
            case LispValue.Int(ai) =>
                match (b) {
                    case LispValue.Int(bi) => LispValue.Boolean(ai > bi)
                    case LispValue.Float(bf) => LispValue.Boolean(Float64(ai) > bf)
                    case _ => LispValue.Boolean(false)
                }
            case LispValue.Float(af) =>
                match (b) {
                    case LispValue.Int(bi) => LispValue.Boolean(af > Float64(bi))
                    case LispValue.Float(bf) => LispValue.Boolean(af > bf)
                    case _ => LispValue.Boolean(false)
                }
            case _ => LispValue.Boolean(false)
        }
    }

    func cmpEQ(a: LispValue, b: LispValue): LispValue {
        match (a) {
            case LispValue.Int(ai) =>
                match (b) {
                    case LispValue.Int(bi) => LispValue.Boolean(ai == bi)
                    case LispValue.Float(bf) => LispValue.Boolean(Float64(ai) == bf)
                    case _ => LispValue.Boolean(false)
                }
            case LispValue.Float(af) =>
                match (b) {
                    case LispValue.Int(bi) => LispValue.Boolean(af == Float64(bi))
                    case LispValue.Float(bf) => LispValue.Boolean(af == bf)
                    case _ => LispValue.Boolean(false)
                }
            case _ => LispValue.Boolean(false)
        }
    }
}