package ystyle::xisp.core.eval
import ystyle::xisp.types.*
import std.collection.ArrayList
import std.collection.HashMap

/**
 * 编译器接口
 */
public interface Compiler {
    func compile(expr: LispValue): Option<CompiledCode>
}

/**
 * 字节码编译器
 */
public class BytecodeCompiler <: Compiler {
    var code: ArrayList<Int64> = ArrayList<Int64>()
    var constants: ArrayList<LispValue> = ArrayList<LispValue>()
    var functions: ArrayList<CompiledFunction> = ArrayList<CompiledFunction>()
    var locals: HashMap<String, Int64> = HashMap<String, Int64>()
    var localCount: Int64 = 0
    var regAlloc: Int64 = 0  // 寄存器分配
    // 全局引用捕获（函数体编译时使用）
    var globalNames: ArrayList<String> = ArrayList<String>()
    var globalSlots: ArrayList<Int64> = ArrayList<Int64>()  // 与 globalNames 平行的帧槽号
    var globalToLocal: HashMap<String, Int64> = HashMap<String, Int64>()
    var compilingFuncBody: Bool = false
    // 闭包捕获：当前函数编译期间登记的（外层变量名, 外层帧槽号）
    var captures: ArrayList<(String, Int64)> = ArrayList<(String, Int64)>()
    // 父帧槽表（编译子函数时快照的外层 locals/globalToLocal，用于识别闭包捕获）
    var parentSlots: HashMap<String, Int64> = HashMap<String, Int64>()
    // 祖先帧槽表（祖父及更外层）：跨层捕获不支持编译（运行时槽号不可达），回退 AST 保语义
    var ancestorSlots: HashMap<String, Int64> = HashMap<String, Int64>()

    public func compile(expr: LispValue): Option<CompiledCode> {
        this.reset()
        if (this.compileExpr(expr)) {
            Some(Bytecode(CompiledFunction(this.code, this.constants, this.functions, "<top>", Nil, ArrayList<String>(), ArrayList<String>(), ArrayList<Int64>(), 0, ArrayList<ParamInfo>())))
        } else {
            None
        }
    }

    func reset() {
        this.code = ArrayList<Int64>()
        this.constants = ArrayList<LispValue>()
        this.functions = ArrayList<CompiledFunction>()
        this.locals = HashMap<String, Int64>()
        this.localCount = 0
        this.regAlloc = 0
        this.globalNames = ArrayList<String>()
        this.globalSlots = ArrayList<Int64>()
        this.globalToLocal = HashMap<String, Int64>()
        this.compilingFuncBody = false
        this.captures = ArrayList<(String, Int64)>()
        this.parentSlots = HashMap<String, Int64>()
        this.ancestorSlots = HashMap<String, Int64>()
    }

    /// 分配寄存器（偏移 1000 避免与栈重叠）
    func allocReg(): Int64 {
        let r = 1000 + this.regAlloc
        this.regAlloc++
        r
    }

    func emitR(op: Int64, rd: Int64, rs1: Int64) {
        this.code.add(op)
        this.code.add(rd)
        this.code.add(rs1)
    }

    func emitR3(op: Int64, rd: Int64, rs1: Int64, rs2: Int64) {
        this.code.add(op)
        this.code.add(rd)
        this.code.add(rs1)
        this.code.add(rs2)
    }

    func addConst(v: LispValue): Int64 {
        this.constants.add(v)
        (this.constants.size - 1)
    }

    func emit(op: Int64) {
        this.code.add(op)
    }

    func emitWithArg(op: Int64, arg: Int64) {
        this.code.add(op)
        this.code.add(arg)
    }

    func compileExpr(expr: LispValue): Bool {
        match (expr) {
            case LispValue.Int(_) | LispValue.Float(_) | Str(_) =>
                this.emitWithArg(OP_PUSH_CONSTANT, this.addConst(expr))
                true
            case Boolean(true) =>
                this.emit(OP_PUSH_TRUE)
                true
            case Boolean(false) =>
                this.emit(OP_PUSH_FALSE)
                true
            case Nil =>
                this.emit(OP_PUSH_NIL)
                true
            case Symbol(s) =>
                if (this.locals.contains(s)) {
                    let idx = this.locals[s]
                    this.emitWithArg(OP_LOAD_LOCAL, idx)
                    true
                } else if (this.compilingFuncBody) {
                    if (this.globalToLocal.contains(s)) {
                        let idx = this.globalToLocal[s]
                        this.emitWithArg(OP_LOAD_LOCAL, idx)
                        true
                    } else {
                        match (this.regRefSlot(s)) {
                            case Some(idx) =>
                                this.emitWithArg(OP_LOAD_LOCAL, idx)
                                true
                            case None => false
                        }
                    }
                } else {
                    this.emitWithArg(OP_LOAD_GLOBAL, this.addConst(Str(s)))
                    true
                }
            case Cons(cell) => this.compileList(cell)
            case _ => false
        }
    }

    func compileList(cell: ConsCell): Bool {
        match (cell.car) {
            case Symbol(s) =>
                match (s) {
                    case "+" => this.compileAddOp(cell.cdr, OP_ADD, 0)
                    case "-" => this.compileMinus(cell.cdr)
                    case "*" => this.compileAddOp(cell.cdr, OP_MUL, 1)
                    case "/" => this.compileBinop(cell.cdr, OP_DIV)
                    case "<" => this.compileBinop(cell.cdr, OP_LT)
                    case ">" => this.compileBinop(cell.cdr, OP_GT)
                    case "=" => this.compileBinop(cell.cdr, OP_EQ)
                    case "not" => this.compileNot(cell.cdr)
                    case "define" => this.compileDefine(cell.cdr)
                    case "set!" => this.compileSet(cell.cdr)
                    case "if" => this.compileIf(cell.cdr)
                    case "begin" | "progn" => this.compileBegin(cell.cdr)
                    case "lambda" => this.compileLambda(cell.cdr)
                    case "condb" => this.compileCondb(cell.cdr)
                    case "match" => this.compileMatch(cell.cdr)
                    case "let" | "let*" => this.compileLet(cell.cdr)
                    case "and" => this.compileAnd(cell.cdr)
                    case "or" => this.compileOr(cell.cdr)
                    case "quote" => this.compileQuote(cell.cdr)
                    case "quasiquote" | "backquote" => this.compileQuasiquote(cell.cdr)
                    case "unquote" | "comma" | "comma-at" => false  // 脱离 backquote 无独立语义
                    case "defmacro" => false  // 编译期构造，回退 AST 展开
                    case "if-let" => this.compileIfLet(cell.cdr)
                    case "when-let*" => this.compileWhenLetStar(cell.cdr)
                    case "import" | "export" => false  // 宿主模块管理，回退 AST
                    case "apply" => this.compileApply(cell.cdr)
                    case "->" => this.compileThreadFirst(cell.cdr)
                    case "eval" | "macroexpand" | "macroexpand-all" => false  // 运行时求值/语法树操作，回退 AST
                    case "map" | "filter" | "reduce" => false  // AST 高阶循环（回调经 VM 执行）
                    case _ => this.compileCall(s, cell.cdr)
                }
            case _ => false
        }
    }

    func compileDefine(args: LispValue): Bool {
        match (args) {
            case Cons(cell) =>
                match (cell.car) {
                    case Symbol(name) =>
                        // (define name value)
                        match (cell.cdr) {
                            case Cons(cell2) =>
                                if (this.compileExpr(cell2.car)) {
                                    this.emitWithArg(OP_STORE_GLOBAL, this.addConst(Str(name)))
                                    true
                                } else { false }
                            case _ => false
                        }
                    case Cons(nameCell) =>
                        // (define (name params... ) body)
                        match (nameCell.car) {
                            case Symbol(name) =>
                                // 参数必须是纯位置参数（&rest/&key/默认值不支持编译，回退 AST 保证语义）
                                match (this.parseSimpleParams(nameCell.cdr)) {
                                    case Some(params) =>
                                        match (this.buildFunc(params, cell.cdr, name)) {
                                            case Some((bodyFunc, caps)) =>
                                                this.functions.add(bodyFunc)
                                                this.emitMakeClosure(this.functions.size - 1, caps)
                                                this.emitWithArg(OP_STORE_GLOBAL, this.addConst(Str(name)))
                                                true
                                            case None => false
                                        }
                                    case None => false
                                }
                            case _ => false
                        }
                    case _ => false
                }
            case _ => false
        }
    }

    /**
     * 为函数体内的符号登记帧槽（全局引用或闭包捕获）。
     * - 命中父槽表：登记为捕获（外层帧槽号，MAKE_CLOSURE 时复制进捕获环境）
     * - 否则：登记为全局引用（帧切换时从闭包环境链实时解析）
     */
    func regRefSlot(name: String): Option<Int64> {
        if (this.ancestorSlots.contains(name) && !this.parentSlots.contains(name)) {
            // 跨层捕获（祖父及更外层帧槽）：运行时不支持，回退 AST 保语义
            return None
        }
        let idx = this.localCount
        this.globalToLocal[name] = idx
        this.globalNames.add(name)
        this.globalSlots.add(idx)
        if (this.parentSlots.contains(name)) {
            this.captures.add((name, this.parentSlots[name]))
        }
        this.localCount++
        Some(idx)
    }

    /**
     * 发射 MAKE_CLOSURE（变长：funcIdx + 捕获对 (nameIdx, slot)）
     */
    func emitMakeClosure(funcIdx: Int64, caps: ArrayList<(String, Int64)>) {
        this.emit(OP_MAKE_CLOSURE)
        this.emit(funcIdx)
        this.emit(caps.size)
        for ((cn, cs) in caps) {
            this.emit(this.addConst(Str(cn)))
            this.emit(cs)
        }
    }

    /**
     * 解析参数列表为纯位置参数。
     * 支持：(x y z) 形式；&rest / &key / 默认值 / 点对 / 纯可变参数 → None（回退 AST）
     */
    func parseSimpleParams(params: LispValue): Option<ArrayList<String>> {
        let result = ArrayList<String>()
        var current = params
        while (let Cons(cell) <- current) {
            match (cell.car) {
                case Symbol(s) =>
                    if (s == "&rest" || s == "&key" || s == "." || s.startsWith(":")) {
                        return None
                    }
                    result.add(s)
                case _ => return None
            }
            current = cell.cdr
        }
        if (!current.isNil()) {
            return None  // 点对/纯可变参数
        }
        Some(result)
    }

    /**
     * 编译函数体为 CompiledFunction（子上下文编译，含参数槽布局与全局引用记录）。
     * 失败（体含不可编译表达式）返回 None，调用方负责整体回退。
     */
    func buildFunc(params: ArrayList<String>, body: LispValue, funcName: String): Option<(CompiledFunction, ArrayList<(String, Int64)>)> {
        let savedLocals = this.locals.clone()
        let savedCount = this.localCount
        let savedGlobalNames = this.globalNames
        let savedGlobalSlots = this.globalSlots
        let savedGlobalToLocal = this.globalToLocal
        let savedCompiling = this.compilingFuncBody
        let savedCode = this.code
        let savedConsts = this.constants
        let savedFuncs = this.functions
        let savedCaptures = this.captures
        let savedParentSlots = this.parentSlots
        let savedAncestorSlots = this.ancestorSlots

        // 祖先帧槽表 = 旧祖先表 ∪ 父表（父表升级为祖父表，用于识别跨层捕获）
        let mergedAncestor = savedParentSlots.clone()
        for ((k, v) in this.ancestorSlots) {
            mergedAncestor[k] = v
        }
        this.ancestorSlots = mergedAncestor
        // 父帧槽表：当前（外层）编译上下文的局部/全局引用槽，供子函数识别闭包捕获
        this.parentSlots = this.locals.clone()
        for ((k, v) in this.globalToLocal) {
            this.parentSlots[k] = v
        }
        this.captures = ArrayList<(String, Int64)>()

        this.locals = HashMap<String, Int64>()
        this.localCount = 0
        for (p in params) {
            this.locals[p] = this.localCount
            this.localCount++
        }
        this.code = ArrayList<Int64>()
        this.constants = ArrayList<LispValue>()
        this.functions = ArrayList<CompiledFunction>()
        this.globalNames = ArrayList<String>()
        this.globalSlots = ArrayList<Int64>()
        this.globalToLocal = HashMap<String, Int64>()
        this.compilingFuncBody = true

        var result: Option<(CompiledFunction, ArrayList<(String, Int64)>)> = None
        try {
            let bodyOk = this.compileBegin(body)
            if (bodyOk) {
                if (this.code.size == 0) {
                    this.emit(OP_PUSH_NIL)
                }
                result = Some((CompiledFunction(this.code, this.constants, this.functions, funcName, body, params, this.globalNames, this.globalSlots, this.localCount, ArrayList<ParamInfo>()), this.captures))
            }
        } finally {
            this.locals = savedLocals
            this.localCount = savedCount
            this.globalNames = savedGlobalNames
            this.globalSlots = savedGlobalSlots
            this.globalToLocal = savedGlobalToLocal
            this.compilingFuncBody = savedCompiling
            this.code = savedCode
            this.constants = savedConsts
            this.functions = savedFuncs
            this.captures = savedCaptures
            this.parentSlots = savedParentSlots
            this.ancestorSlots = savedAncestorSlots
        }
        result
    }

    /**
     * (lambda (params...) body...) → 编译为子函数 + MAKE_CLOSURE
     */
    func compileLambda(args: LispValue): Bool {
        match (args) {
            case Cons(cell) =>
                match (this.parseSimpleParams(cell.car)) {
                    case Some(params) =>
                        match (this.buildFunc(params, cell.cdr, "<lambda>")) {
                            case Some((bodyFunc, caps)) =>
                                this.functions.add(bodyFunc)
                                this.emitMakeClosure(this.functions.size - 1, caps)
                                true
                            case None => false
                        }
                    case None => false
                }
            case _ => false
        }
    }

    /**
     * (set! name value) → 修改绑定（env.set 语义）
     * - 局部/参数/let 变量：直接写帧槽（等价于环境中修改）
     * - 闭包捕获/全局变量：帧槽快照 + 环境链同步（保证本帧与本函数后续读取一致）
     */
    func compileSet(args: LispValue): Bool {
        match (args) {
            case Cons(cell) =>
                if (let Symbol(name) <- cell.car) {
                    match (cell.cdr) {
                        case Cons(valCell) =>
                            if (valCell.cdr.isNil()) {
                                if (!this.compileExpr(valCell.car)) { return false }
                                if (this.locals.contains(name)) {
                                    // 表达式值保留在栈（set! 返回值语义）
                                    this.emit(OP_DUP)
                                    this.emitWithArg(OP_STORE_LOCAL, this.locals[name])
                                } else if (this.compilingFuncBody) {
                                    let idxOpt = if (this.globalToLocal.contains(name)) {
                                        Some(this.globalToLocal[name])
                                    } else {
                                        this.regRefSlot(name)
                                    }
                                    let idx = match (idxOpt) {
                                        case Some(i) => i
                                        case None => return false
                                    }
                                    this.emit(OP_DUP)
                                    this.emitWithArg(OP_STORE_LOCAL, idx)
                                    this.emitWithArg(OP_SET_GLOBAL, this.addConst(Str(name)))
                                } else {
                                    this.emitWithArg(OP_SET_GLOBAL, this.addConst(Str(name)))
                                }
                                true
                            } else { false }
                        case _ => false
                    }
                } else { false }
            case _ => false
        }
    }

    /**
     * (quasiquote expr) / `expr → 编译为构造指令序列
     * 语义与 AST expandBackquote 对齐：
     * - 原子/符号 → 作为常量
     * - (comma e) → 求值 e 作为元素
     * - (comma-at e) → 求值 e（列表）拼接
     * - 列表 → 构造（普通元素段 + splice 段拼接，点对尾行为与 AST 一致）
     */
    func compileQuasiquote(args: LispValue): Bool {
        match (args) {
            case Cons(cell) =>
                if (cell.cdr.isNil()) {
                    this.compileQuasi(cell.car)
                } else { false }
            case _ => false
        }
    }

    func compileQuasi(expr: LispValue): Bool {
        match (expr) {
            case Nil =>
                this.emit(OP_PUSH_NIL)
                true
            case Symbol(_) | LispValue.Int(_) | LispValue.Float(_) | Str(_) | Boolean(_) =>
                this.emitWithArg(OP_PUSH_CONSTANT, this.addConst(expr))
                true
            case Cons(cell) =>
                match (cell.car) {
                    case Symbol("comma") =>
                        // (comma e) → 求值 e
                        match (cell.cdr) {
                            case Cons(v) where v.cdr.isNil() => this.compileExpr(v.car)
                            case _ => false
                        }
                    case Symbol("comma-at") =>
                        // 顶层 (comma-at e)：AST 原样返回该 cons cell（不求值）→ 按普通列表构造
                        this.compileQuasiList(cell)
                    case _ =>
                        this.compileQuasiList(cell)
                }
            case _ => false
        }
    }

    /**
     * quasiquote 列表构造：元素序列（从左到右），普通段 + splice 段，
     * 段间用 OP_CONCAT 拼接；普通段内右到左 OP_CONS 构造
     */
    func compileQuasiList(cell: ConsCell): Bool {
        // 收集元素链（点对尾：AST 行为为丢弃尾，这里保持对齐）
        let elems = ArrayList<LispValue>()
        var current: LispValue = Cons(cell)
        while (let Cons(c) <- current) {
            elems.add(c.car)
            current = c.cdr
        }
        var haveResult = false
        let pending = ArrayList<LispValue>()
        for (e in elems) {
            let isSplice = match (e) {
                case Cons(ec) =>
                    match (ec.car) {
                        case Symbol("comma-at") => true
                        case _ => false
                    }
                case _ => false
            }
            if (isSplice) {
                let (fok, fhr) = this.compileQuasiFlush(pending, haveResult)
                if (!fok) { return false }
                haveResult = fhr
                pending.clear()
                // splice 表达式求值 → 列表，与已有结果拼接
                match (e) {
                    case Cons(ec) =>
                        match (ec.cdr) {
                            case Cons(v) where v.cdr.isNil() =>
                                if (!this.compileExpr(v.car)) { return false }
                                if (haveResult) {
                                    this.emit(OP_CONCAT)
                                } else {
                                    haveResult = true
                                }
                            case _ => return false
                        }
                    case _ => return false
                }
            } else {
                pending.add(e)
            }
        }
        let (fok2, fhr2) = this.compileQuasiFlush(pending, haveResult)
        if (!fok2) { return false }
        haveResult = fhr2
        if (!haveResult) {
            this.emit(OP_PUSH_NIL)
        }
        true
    }

    /**
     * 构造普通段列表（右到左 OP_CONS，Nil 尾）并与已有结果拼接。
     * 返回 (是否成功, 新 haveResult)
     */
    func compileQuasiFlush(pending: ArrayList<LispValue>, haveResult: Bool): (Bool, Bool) {
        var idx = pending.size - 1
        if (idx < 0) {
            return (true, haveResult)
        }
        this.emit(OP_PUSH_NIL)
        while (idx >= 0) {
            if (!this.compileQuasi(pending[idx])) { return (false, false) }
            this.emit(OP_CONS)
            idx--
        }
        if (haveResult) {
            this.emit(OP_CONCAT)
        }
        (true, true)
    }

    /**
     * (if-let (var value) then else) → 展开为 (let ((var value)) (if var then else)) 编译
     */
    func compileIfLet(args: LispValue): Bool {
        match (args) {
            case Cons(cell) =>
                if (let Cons(bindCell) <- cell.car && let Cons(valCell) <- bindCell.cdr && valCell.cdr.isNil()) {
                    match (bindCell.car) {
                        case Symbol(varName) =>
                            match (cell.cdr) {
                                case Cons(restCell) =>
                                    let thenExpr = restCell.car
                                    var elseExpr: LispValue = Nil
                                    match (restCell.cdr) {
                                        case Cons(eCell) => elseExpr = eCell.car
                                        case _ => ()
                                    }
                                    // 构造 (let ((var value)) (if var then else))
                                    let binding = Cons(ConsCell(Symbol(varName), Cons(ConsCell(valCell.car, Nil))))
                                    let bindings = Cons(ConsCell(binding, Nil))
                                    let ifBody = Cons(ConsCell(Symbol("if"), Cons(ConsCell(Symbol(varName), Cons(ConsCell(thenExpr, elseExpr))))))
                                    let letExpr = Cons(ConsCell(Symbol("let"), Cons(ConsCell(bindings, Cons(ConsCell(ifBody, Nil))))))
                                    this.compileExpr(letExpr)
                                case _ => false
                            }
                        case _ => false
                    }
                } else { false }
            case _ => false
        }
    }

    /**
     * (when-let* ((v1 e1) (v2 e2) ...) body...) → 顺序绑定，最后一个值为真则执行 body
     * 语义与 AST evalWhenLetStar 对齐：只有最后一个绑定的值做真值判断（isTruthy），
     * 短路时返回 Nil；body 在绑定环境中执行
     */
    func compileWhenLetStar(args: LispValue): Bool {
        match (args) {
            case Cons(cell) =>
                let bindings = cell.car
                let body = cell.cdr
                let savedLocals = this.locals.clone()
                let savedCount = this.localCount
                var ok = true
                try {
                    let bindingCount = this.countArgs(bindings)
                    var idx: Int64 = 0
                    var current = bindings
                    var failed = false
                    while (let Cons(bindCell) <- current && !failed) {
                        match (bindCell.car) {
                            case Cons(pair) =>
                                match (pair.car) {
                                    case Symbol(name) =>
                                        match (pair.cdr) {
                                            case Cons(valCell) where valCell.cdr.isNil() =>
                                                if (!this.compileExpr(valCell.car)) { failed = true }
                                                if (!failed) {
                                                    this.locals[name] = this.localCount
                                                    let slot = this.localCount
                                                    this.localCount++
                                                    this.emit(OP_DUP)
                                                    this.emitWithArg(OP_STORE_LOCAL, slot)
                                                    if (idx < bindingCount - 1) {
                                                        // 非最后绑定：清掉影子值
                                                        this.emit(OP_POP)
                                                    }
                                                }
                                            case _ => failed = true
                                        }
                                    case _ => failed = true
                                }
                            case _ => failed = true
                        }
                        idx++
                        current = bindCell.cdr
                    }
                    if (failed) {
                        ok = false
                    } else {
                        // 最后一个绑定的值在栈上（DUP 影子）：真值判断
                        let jifPatch = this.code.size
                        this.emitWithArg(OP_JUMP_IF_FALSE, 0)
                        let bodyOk = this.compileBegin(body)
                        if (!bodyOk) {
                            ok = false
                        } else {
                            let endJump = this.code.size
                            this.emitWithArg(OP_JUMP, 0)
                            // 假路径：返回 Nil
                            this.code[jifPatch + 1] = this.code.size
                            this.emit(OP_PUSH_NIL)
                            this.code[endJump + 1] = this.code.size
                        }
                    }
                } finally {
                    this.locals = savedLocals
                    this.localCount = savedCount
                }
                ok
            case _ => false
        }
    }

    /**
     * (-> value form1 form2 ...) → 编译期展开：
     * value 依次作为每个 form 的第一个参数插入（thread-first）
     * 展开后的表达式可编译则编译，否则整个 -> 回退 AST
     */
    func compileThreadFirst(args: LispValue): Bool {
        match (args) {
            case Cons(cell) =>
                let value = cell.car
                let forms = cell.cdr
                var current = value
                var listCurrent = forms
                while (let Cons(fCell) <- listCurrent) {
                    match (fCell.car) {
                        case Cons(formCell) =>
                            // 特殊顺序（map/filter/reduce/apply）：(f args... current)
                            // 与 AST threadFirstList 语义一致（value 追加到 form 参数之后）
                            let needsSpecial = match (formCell.car) {
                                case Symbol(fn) =>
                                    fn == "map" || fn == "filter" || fn == "reduce" || fn == "apply"
                                case _ => false
                            }
                            if (needsSpecial) {
                                match (this.threadAppendArgs(formCell.car, formCell.cdr, current)) {
                                    case Some(expr) => current = expr
                                    case None => return false
                                }
                            } else {
                                // (f a b) → (f current a b)
                                current = Cons(ConsCell(formCell.car, Cons(ConsCell(current, formCell.cdr))))
                            }
                        case Symbol(fn) =>
                            // 符号形式 → (fn current)
                            current = Cons(ConsCell(Symbol(fn), Cons(ConsCell(current, Nil))))
                        case _ => return false
                    }
                    listCurrent = fCell.cdr
                }
                this.compileExpr(current)
            case _ => false
        }
    }

    /**
     * 构造 (f args... current)（thread-first 特殊顺序的追加参数语义）
     */
    func threadAppendArgs(fn: LispValue, formArgs: LispValue, value: LispValue): Option<LispValue> {
        var newArgs: LispValue = Cons(ConsCell(value, Nil))
        let elems = ArrayList<LispValue>()
        var p = formArgs
        while (let Cons(c) <- p) {
            elems.add(c.car)
            p = c.cdr
        }
        var i = elems.size - 1
        while (i >= 0) {
            newArgs = Cons(ConsCell(elems[i], newArgs))
            i--
        }
        Some(Cons(ConsCell(fn, newArgs)))
    }

    /**
     * (apply f a b ... lastlist) → 求值 f、前置参数与最后列表，动态展开调用
     * 编译为：函数值 + 前置参数 + 最后列表 → OP_APPLY n（n = 前置参数个数）
     * 此实现要求最后参数为列表形式（不支持 (apply f '(1 2)) 的 quote 展开语义以外的
     * 复杂形式——与 AST 语义一致：最后列表求值后展开）
     */
    func compileApply(args: LispValue): Bool {
        match (args) {
            case Cons(cell) =>
                // 函数表达式
                if (!this.compileExpr(cell.car)) { return false }
                var current = cell.cdr
                var prefix: Int64 = 0
                var haveLastList = false
                while (let Cons(peCell) <- current) {
                    if (peCell.cdr.isNil()) {
                        // 最后一个参数：展开源列表（求值，不参与 prefix）
                        if (!this.compileExpr(peCell.car)) { return false }
                        haveLastList = true
                        current = Nil
                    } else {
                        if (!this.compileExpr(peCell.car)) { return false }
                        prefix++
                        current = peCell.cdr
                    }
                }
                if (haveLastList) {
                    // 栈：[f, p1..pn, lastList] → OP_APPLY n（n = 前置参数个数）
                    this.emitWithArg(OP_APPLY, prefix)
                    true
                } else {
                    // (apply f) 无参数：等价于 (f) （AST 语义为 Nil——保留 AST 行为）
                    false
                }
            case _ => false
        }
    }

    func compileIf(args: LispValue): Bool {
        match (args) {
            case Cons(condCell) =>
                // 特判：条件为 (cmpOp <局部变量> <字面量>) → 比较+跳转单条超指令
                if (this.tryCmpLC(condCell.car)) {
                    // tryCmpLC 已发射 [OP_CMP_LC_JF, kind, localIdx, constIdx, target占位]
                    let cmpPatch = this.code.size - 5
                    match (condCell.cdr) {
                        case Cons(thenCell) =>
                            if (!this.compileExpr(thenCell.car)) { return false }
                            let jPatch = this.code.size
                            this.emitWithArg(OP_JUMP, 0)
                            this.code[cmpPatch + 4] = this.code.size
                            match (thenCell.cdr) {
                                case Cons(elseCell) =>
                                    if (!this.compileExpr(elseCell.car)) { return false }
                                case _ => this.emit(OP_PUSH_NIL)
                            }
                            this.code[jPatch + 1] = this.code.size
                            true
                        case _ => false
                    }
                } else {
                    // 编译条件
                    if (!this.compileExpr(condCell.car)) { return false }
                    match (condCell.cdr) {
                        case Cons(thenCell) =>
                            // 条件为 false 时的跳转（暂填 0）
                            let jifPatch = this.code.size
                            this.emitWithArg(OP_JUMP_IF_FALSE, 0)
                            // 编译 then 分支
                            if (!this.compileExpr(thenCell.car)) { return false }
                            // then 之后跳过 else（暂填 0）
                            let jPatch = this.code.size
                            this.emitWithArg(OP_JUMP, 0)
                            // 回填 JUMP_IF_FALSE → else 分支起始
                            this.code[jifPatch + 1] = this.code.size
                            // 编译 else 分支（如果有）
                            match (thenCell.cdr) {
                                case Cons(elseCell) =>
                                    if (!this.compileExpr(elseCell.car)) { return false }
                                case _ => this.emit(OP_PUSH_NIL)
                            }
                            // 回填 JUMP → 整个 if 表达式结束
                            this.code[jPatch + 1] = this.code.size
                            true
                        case _ => false
                    }
                }
            case _ => false
        }
    }

    /**
     * 特判 if 条件：(</>/= <局部变量> <字面量>) → 发射 OP_CMP_LC_JF（比较为假则跳）
     * 成功返回 true（指令已发射，target 为占位）；形式不匹配返回 false（走通用路径）
     */
    func tryCmpLC(cond: LispValue): Bool {
        match (cond) {
            case Cons(cell) =>
                let cmpKind = match (cell.car) {
                    case Symbol(s) where s == "<" => 0
                    case Symbol(s) where s == ">" => 1
                    case Symbol(s) where s == "=" => 2
                    case _ => -1
                }
                if (cmpKind < 0) { return false }
                match (cell.cdr) {
                    case Cons(firstCell) =>
                        match (firstCell.cdr) {
                            case Cons(secondCell) where secondCell.cdr.isNil() =>
                                match (firstCell.car) {
                                    case Symbol(name) where this.locals.contains(name) =>
                                        let val = secondCell.car
                                        if (let LispValue.Int(_) <- val || let LispValue.Float(_) <- val || let Str(_) <- val) {
                                            this.emit(OP_CMP_LC_JF)
                                            this.emit(cmpKind)
                                            this.emit(this.locals[name])
                                            this.emit(this.addConst(val))
                                            this.emit(0)  // target 占位
                                            true
                                        } else { false }
                                    case _ => false
                                }
                            case _ => false
                        }
                    case _ => false
                }
            case _ => false
        }
    }

    func compileBegin(args: LispValue): Bool {
        var current = args
        var exprCount: Int64 = 0
        // 第一遍：统计表达式数量
        while (let Cons(_) <- current) {
            exprCount++
            match (current) {
                case Cons(cell) => current = cell.cdr
                case _ => break
            }
        }
        if (exprCount == 0) {
            this.emit(OP_PUSH_NIL)
            return true
        }
        // 第二遍：编译每个表达式
        current = args
        var idx: Int64 = 0
        while (let Cons(cell) <- current) {
            if (!this.compileExpr(cell.car)) { return false }
            idx++
            // 非最后一个表达式需要 POP
            if (idx < exprCount) {
                this.emit(OP_POP)
            }
            current = cell.cdr
        }
        true
    }

    func compileBinop(args: LispValue, stackOp: Int64): Bool {
        // 左折叠多参数：(+ a b c) → a b ADD c ADD
        // 二元 (op <局部变量> <字面量>) → 超级指令（省 LOAD/PUSH 分派与栈操作）
        let superOp = if (stackOp == OP_ADD) { OP_ADD_LC }
            else if (stackOp == OP_SUB) { OP_SUB_LC }
            else if (stackOp == OP_MUL) { OP_MUL_LC }
            else if (stackOp == OP_DIV) { OP_DIV_LC }
            else { -1 }
        if (superOp >= 0) {
            match (this.tryBinopLC(args, superOp)) {
                case Some(ok) => return ok
                case None => ()
            }
        }
        match (args) {
            case Cons(firstCell) =>
                if (!this.compileExpr(firstCell.car)) { return false }
                var current = firstCell.cdr
                var count: Int64 = 1
                while (let Cons(cell) <- current) {
                    if (!this.compileExpr(cell.car)) { return false }
                    this.emit(stackOp)
                    count++
                    current = cell.cdr
                }
                count >= 2
            case _ => false
        }
    }

    /**
     * 尝试超指令化二元 (局部变量, 字面量) 运算。
     * 返回 Some(是否成功)——None 表示形式不匹配（走通用路径）
     */
    func tryBinopLC(args: LispValue, superOp: Int64): Option<Bool> {
        match (args) {
            case Cons(firstCell) =>
                match (firstCell.cdr) {
                    case Cons(secondCell) =>
                        if (!secondCell.cdr.isNil()) { return None }
                        match (firstCell.car) {
                            case Symbol(name) =>
                                if (!this.locals.contains(name)) { return None }
                                let val = secondCell.car
                                if (let LispValue.Int(_) <- val || let LispValue.Float(_) <- val || let Str(_) <- val) {
                                    // (op local const)：发射 [superOp, localIdx, constIdx]
                                    this.emitWithArg(superOp, this.locals[name])
                                    this.code.add(this.addConst(val))
                                    Some(true)
                                } else { None }
                            case _ => None
                        }
                    case _ => None
                }
            case _ => None
        }
    }

    /**
     * 编译 + / *（多参左折叠；单参恒等；空参返回单位元，与 AST 语义一致）
     */
    func compileAddOp(args: LispValue, stackOp: Int64, unit: Int64): Bool {
        match (args) {
            case Nil =>
                this.emitWithArg(OP_PUSH_CONSTANT, this.addConst(LispValue.Int(unit)))
                true
            case Cons(first) =>
                match (first.cdr) {
                    case Nil => this.compileExpr(first.car)
                    case _ => this.compileBinop(args, stackOp)
                }
            case _ => false
        }
    }

    /// (- x) — 取反
    func compileMinus(args: LispValue): Bool {
        match (args) {
            case Cons(cell) =>
                match (cell.cdr) {
                    case Nil =>
                        // 单参数：(- x) → 0 - x = -x
                        this.emitWithArg(OP_PUSH_CONSTANT, this.addConst(LispValue.Int(0)))
                        if (this.compileExpr(cell.car)) {
                            this.emit(OP_SUB)
                            true
                        } else { false }
                    case _ =>
                        this.compileBinop(args, OP_SUB)
                }
            case _ => false
        }
    }

    /// (not x) — 逻辑非
    func compileNot(args: LispValue): Bool {
        match (args) {
            case Cons(cell) =>
                match (cell.cdr) {
                    case Nil =>
                        if (this.compileExpr(cell.car)) {
                            this.emit(OP_NOT)
                            true
                        } else { false }
                    case _ => false
                }
            case _ => false
        }
    }

    /// (quote expr) — 直接压入表达式本身作为常量
    func compileQuote(args: LispValue): Bool {
        match (args) {
            case Cons(cell) =>
                this.emitWithArg(OP_PUSH_CONSTANT, this.addConst(cell.car))
                true
            case _ => false
        }
    }

    /// 编译一个值到寄存器，返回寄存器号。失败返回 -1。
    func compileRValue(expr: LispValue): Int64 {
        match (expr) {
            case LispValue.Int(_) | LispValue.Float(_) | Str(_) =>
                let rd = this.allocReg()
                this.emitR(OP_LOAD_CONST, rd, this.addConst(expr))
                rd
            case Boolean(b) =>
                let rd = this.allocReg()
                this.emitR(OP_LOAD_CONST, rd, this.addConst(Boolean(b)))
                rd
            case Nil =>
                let rd = this.allocReg()
                this.emitR(OP_LOAD_CONST, rd, this.addConst(Nil))
                rd
            case Symbol(s) =>
                if (this.locals.contains(s)) {
                    let rd = this.allocReg()
                    let idx = this.locals[s]
                    this.emitR(OP_LOAD_LOCAL_R, rd, idx)
                    rd
                } else {
                    let rd = this.allocReg()
                    this.emitR(OP_LOAD_GLOBAL_R, rd, this.addConst(Str(s)))
                    rd
                }
            case Cons(cell) =>
                // 嵌套表达式：尝试编译到寄存器
                let result = this.compileListToReg(cell)
                if (result >= 0) { return result }
                // 回退：用栈机编译，再 POP 到寄存器
                if (this.compileExpr(expr)) {
                    let rd = this.allocReg()
                    this.emitWithArg(OP_POP_R, rd)
                    rd
                } else {
                    -1
                }
            case _ => -1
        }
    }

    /// 编译列表表达式到寄存器。成功返回寄存器号，失败返回 -1。
    func compileListToReg(cell: ConsCell): Int64 {
        match (cell.car) {
            case Symbol(s) =>
                match (s) {
                    case "+" => this.compileBinopToReg(cell.cdr, OP_ADD_R)
                    case "-" => this.compileBinopToReg(cell.cdr, OP_SUB_R)
                    case "*" => this.compileBinopToReg(cell.cdr, OP_MUL_R)
                    case "/" => this.compileBinopToReg(cell.cdr, OP_DIV_R)
                    case _ => -1
                }
            case _ => -1
        }
    }

    /// 编译二元操作并返回结果寄存器号
    func compileBinopToReg(args: LispValue, regOp: Int64): Int64 {
        match (args) {
            case Cons(cell) =>
                let rs1 = this.compileRValue(cell.car)
                if (rs1 < 0) { return -1 }
                match (cell.cdr) {
                    case Cons(cell2) =>
                        let rs2 = this.compileRValue(cell2.car)
                        if (rs2 < 0) { return -1 }
                        let rd = this.allocReg()
                        this.emitR3(regOp, rd, rs1, rs2)
                        rd
                    case _ => -1
                }
            case _ => -1
        }
    }

    func compileCall(name: String, args: LispValue): Bool {
        // 推函数值（而非名字字符串），使 CALL 能直接使用闭包/原生函数
        if (this.locals.contains(name)) {
            let idx = this.locals[name]
            this.emitWithArg(OP_LOAD_LOCAL, idx)
        } else if (this.compilingFuncBody) {
            if (this.globalToLocal.contains(name)) {
                let idx = this.globalToLocal[name]
                this.emitWithArg(OP_LOAD_LOCAL, idx)
            } else {
                match (this.regRefSlot(name)) {
                    case Some(idx) => this.emitWithArg(OP_LOAD_LOCAL, idx)
                    case None => return false
                }
            }
        } else {
            let nameIdx = this.addConst(Str(name))
            this.emitWithArg(OP_PUSH_CONSTANT, nameIdx)
        }
        var current = args
        while (let Cons(cell) <- current) {
            // 关键字符号不做求值，作为符号直接压入
            if (let Symbol(s) <- cell.car && s.startsWith(":")) {
                this.emitWithArg(OP_PUSH_CONSTANT, this.addConst(cell.car))
            } else if (!this.compileExpr(cell.car)) {
                return false
            }
            current = cell.cdr
        }
        let argCount = this.countArgs(args) + 1
        this.emitWithArg(OP_CALL, argCount)
        true
    }

    func countArgs(args: LispValue): Int64 {
        var count: Int64 = 0
        var current = args
        while (let Cons(_) <- current) {
            count++
            match (current) {
                case Cons(cell) => current = cell.cdr
                case _ => break
            }
        }
        count
    }

    func compileAnd(args: LispValue): Bool {
        let argCount = this.countArgs(args)
        if (argCount == 0) {
            this.emit(OP_PUSH_TRUE)
            return true
        }
        let patches = ArrayList<Int64>()
        var current = args
        var idx: Int64 = 0
        while (let Cons(cell) <- current) {
            if (!this.compileExpr(cell.car)) { return false }
            idx++
            if (idx < argCount) {
                this.emit(OP_DUP)
                patches.add(this.code.size)
                this.emitWithArg(OP_JUMP_IF_FALSE, 0)
                this.emit(OP_POP)
            }
            current = cell.cdr
        }
        let endPos = this.code.size
        for (p in patches) {
            this.code[p + 1] = endPos
        }
        true
    }

    func compileOr(args: LispValue): Bool {
        let argCount = this.countArgs(args)
        if (argCount == 0) {
            this.emit(OP_PUSH_NIL)
            return true
        }
        let jumpPatches = ArrayList<Int64>()
        var current = args
        var idx: Int64 = 0
        while (let Cons(cell) <- current) {
            if (!this.compileExpr(cell.car)) { return false }
            idx++
            if (idx < argCount) {
                this.emit(OP_DUP)
                let elsePatch = this.code.size
                this.emitWithArg(OP_JUMP_IF_FALSE, 0)
                // 直接跳转到 end（值为真，DUP 已被 JUMP_IF_FALSE 消耗）
                jumpPatches.add(this.code.size)
                this.emitWithArg(OP_JUMP, 0)
                // else: 值为假，POP 掉 DUP 副本
                this.code[elsePatch + 1] = this.code.size
                this.emit(OP_POP)
            }
            current = cell.cdr
        }
        let endPos = this.code.size
        for (j in jumpPatches) {
            this.code[j + 1] = endPos
        }
        true
    }

    /// (condb test1 result1 test2 result2 else default)
    func compileCondb(args: LispValue): Bool {
        let endJumps = ArrayList<Int64>()
        var current = args
        var hasElse = false

        while (let Cons(testCell) <- current) {
            match (testCell.cdr) {
                case Cons(resultCell) =>
                    match (testCell.car) {
                        case Symbol("else") =>
                            // else 分支
                            if (!this.compileExpr(resultCell.car)) { return false }
                            hasElse = true
                            current = Nil
                        case Cons(letCell) =>
                            // (:let var expr) 绑定子句：AST 支持，编译侧回退（语义保真）
                            match (letCell.car) {
                                case Symbol(":let") => return false
                                case _ =>
                                    if (!this.compileExpr(testCell.car)) { return false }
                                    let elsePatch = this.code.size
                                    this.emitWithArg(OP_JUMP_IF_FALSE, 0)
                                    this.emit(OP_POP)
                                    if (!this.compileExpr(resultCell.car)) { return false }
                                    endJumps.add(this.code.size)
                                    this.emitWithArg(OP_JUMP, 0)
                                    this.code[elsePatch + 1] = this.code.size
                                    current = resultCell.cdr
                            }
                        case _ =>
                            // test → result 对
                            if (!this.compileExpr(testCell.car)) { return false }
                            let elsePatch = this.code.size
                            this.emitWithArg(OP_JUMP_IF_FALSE, 0)
                            this.emit(OP_POP)
                            if (!this.compileExpr(resultCell.car)) { return false }
                            endJumps.add(this.code.size)
                            this.emitWithArg(OP_JUMP, 0)
                            this.code[elsePatch + 1] = this.code.size
                            current = resultCell.cdr
                    }
                case _ => return false
            }
        }

        if (!hasElse) {
            // 没有匹配时返回 nil
            this.emit(OP_PUSH_NIL)
        }

        let endPos = this.code.size
        for (j in endJumps) {
            this.code[j + 1] = endPos
        }
        true
    }

    func compileMatch(args: LispValue): Bool {
        // (match expr (pattern body) (pattern when guard body) (else body))
        match (args) {
            case Cons(cell) =>
                // 编译值表达式
                if (!this.compileExpr(cell.car)) { return false }
                let clauses = cell.cdr
                let endJumps = ArrayList<Int64>()
                var current = clauses
                while (let Cons(clauseCell) <- current) {
                    match (clauseCell.car) {
                        case Cons(patternCell) =>
                            let pattern = patternCell.car
                            let rest = patternCell.cdr
                            // 仅支持 (pattern body) 子句形式：
                            // - 列表模式（(y z body...) 等多元素 clause）→ 回退 AST（AST 启发式语义保真）
                            // - 多 body 表达式 → 回退 AST（语义保真）
                            let restOk = match (rest) {
                                case Cons(restCell) => restCell.cdr.isNil()
                                case _ => false
                            }
                            if (!restOk) {
                                return false
                            }
                            match (pattern) {
                                case Symbol(sym) =>
                                    // 符号模式：总是匹配，且绑定到帧槽（AST 语义：变量绑定）
                                    // (x when guard ...) 类 guard 形式不支持编译，回退 AST
                                    if (this.isGuardClause(patternCell.cdr)) {
                                        return false
                                    }
                                    // 值在栈顶：绑定到帧槽后（body 通过槽引用）
                                    this.emit(OP_DUP)
                                    let slot = this.localCount
                                    this.locals[sym] = slot
                                    this.localCount++
                                    this.emitWithArg(OP_STORE_LOCAL, slot)
                                    this.emit(OP_POP)
                                    if (!this.compileMatchBody(patternCell.cdr)) { return false }
                                    endJumps.add(this.code.size)
                                    this.emitWithArg(OP_JUMP, 0)
                                case _ =>
                                    // 字面量模式：OP_MATCH_LIT + 跳转回填
                                    if (this.isGuardClause(patternCell.cdr)) {
                                        return false
                                    }
                                    this.emit(OP_DUP)
                                    let nextClause = this.compilePattern(pattern)
                                    if (nextClause == -1) { return false }
                                    // 匹配成功，POP 值
                                    this.emit(OP_POP)
                                    if (!this.compileMatchBody(patternCell.cdr)) { return false }
                                    endJumps.add(this.code.size)
                                    this.emitWithArg(OP_JUMP, 0)
                                    // 回填 nextClause 跳转（下一子句位置）
                                    this.code[nextClause] = this.code.size
                            }
                        case Symbol("else") =>
                            // else 分支：POP 值并执行体（多表达式）
                            this.emit(OP_POP)
                            if (!this.compileBegin(clauseCell.cdr)) { return false }
                            endJumps.add(this.code.size)
                            this.emitWithArg(OP_JUMP, 0)
                        case _ => return false
                    }
                    current = clauseCell.cdr
                }
                // 没有匹配的默认值
                this.emit(OP_POP)
                this.emitWithArg(OP_PUSH_CONSTANT, this.addConst(
                    LispValue.Error(XispError(ErrorType.RuntimeError, "match: no clause matched"))
                ))
                // 回填所有 end JUMP
                let endPos = this.code.size
                for (j in endJumps) {
                    this.code[j + 1] = endPos
                }
                true
            case _ => false
        }
    }

    /// 检测 guard 子句形式（pattern when guard ...）。guard 语义（匹配环境求值）
    /// 与模式变量绑定组合较复杂，本次回退 AST 保证语义一致。
    func isGuardClause(rest: LispValue): Bool {
        match (rest) {
            case Cons(bodyCell) =>
                if (let Symbol("when") <- bodyCell.car) {
                    true
                } else {
                    false
                }
            case _ => false
        }
    }

    /// 编译 match 子句体（无 guard 形式；guard 已由调用方拦截回退）
    /// 子句体允许多表达式（与 AST evalBody 语义一致）
    func compileMatchBody(rest: LispValue): Bool {
        if (let Cons(bodyCell) <- rest && !bodyCell.cdr.isNil()) {
            false  // 多表达式 body：由形状检查拦截（应不会走到）
        } else {
            this.compileBegin(rest)
        }
    }

    /// 编译模式（字面量）。返回回填位置（JUMP_IF_FALSE 的参数位置），-1 表示失败。
    func compilePattern(pattern: LispValue): Int64 {
        match (pattern) {
            case LispValue.Int(_) | LispValue.Float(_) | Str(_) | Boolean(_) | Nil =>
                // OP_MATCH_LIT litIdx, target
                // 弹出栈顶值，如果 != constants[idx] 则跳转到 target
                let litIdx = this.addConst(pattern)
                this.emitWithArg(OP_MATCH_LIT, litIdx)
                let targetIdx = this.code.size
                this.code.add(0)  // placeholder for target
                targetIdx
            case Symbol(s) =>
                if (s == "else") {
                    -2
                } else {
                    // 符号模式由 compileMatch 直接处理（绑定路径），此处不发射
                    -1
                }
            case _ => -1
        }
    }

    func compileLet(args: LispValue): Bool {
        // (let ((x 1) (y 2)) body)
        match (args) {
            case Cons(cell) =>
                let bindings = cell.car
                let body = cell.cdr
                // 绑定表必须是列表（或空）；命名 let (let name ((v e)...) body) 等
                // 非标准形式不支持编译，显式回退 AST，避免静默产出语义错误的字节码
                match (bindings) {
                    case Nil => ()
                    case Cons(_) => ()
                    case _ => return false
                }
                // 保存当前局部变量表（不保存 localCount，let 槽位在函数体内持续占用）
                let savedLocals = this.locals.clone()
                // 编译绑定并分配局部变量索引
                var current = bindings
                while (let Cons(bindCell) <- current) {
                    match (bindCell.car) {
                        case Cons(pair) =>
                            match (pair.car) {
                                case Symbol(name) =>
                                    match (pair.cdr) {
                                        case Cons(valCell) =>
                                            if (!this.compileExpr(valCell.car)) {
                                                this.locals = savedLocals
                                                return false
                                            }
                                            // 分配局部变量索引
                                            this.locals[name] = this.localCount
                                            this.emitWithArg(OP_STORE_LOCAL, this.localCount)
                                            this.localCount++
                                        case _ => return false
                                    }
                                case _ => return false
                            }
                        case _ => return false
                    }
                    current = bindCell.cdr
                }
                // 编译 body
                let result = this.compileBegin(body)
                // 恢复局部变量表（不恢复 localCount）
                this.locals = savedLocals
                result
            case _ => false
        }
    }
}
