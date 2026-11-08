package ystyle::xisp.core.eval

import ystyle::xisp.types.*

/**
 * 编译前的宏展开
 *
 * ## 为什么需要它
 *
 * 宏是**语法**层面的东西：`(when c body...)` 必须在"求值/编译"之前变成 `(if c (begin body…) nil)`。
 * 现状是把这个工作放在**运行时**做（AST 解释器在 `evalListInternal` 里遇到宏才展开；
 * 字节码后端的 VM 用 `callMacro` 每次调用重新展开），后果有两条：
 *
 * 1. **性能**：宏调用在编译产物里成了"调宏指令"，每执行一次就展开一次并起一个子求值器。
 *    实测递归函数体内含一个宏：字节码 10767us vs 不含宏 566us（**差 19 倍**）；
 * 2. **语义**：编译路径下宏参数**已被求值为值**，VM 只能把值 `quote` 回去冒充语法
 *    （`vm.cj` 的 `buildCallExpr` 注释自陈"常见宏…行为与 AST 完全等价"）。
 *    对"把参数当语法分析"的宏，这与 AST 语义**不等价** —— 是静默错误。
 *
 * 因此本模块把宏展开提到**编译前**：交给执行后端的表达式已"展开、无宏"，
 * 编译产物里不再有宏调用。展开在 AST 语义下进行，上面两条同时消掉。
 *
 * ## 与 `macroExpandAll` 的区别（不能复用它）
 *
 * `macroExpandAll` 是 `(macroexpand-all …)` 的语义：**递归展开一切**，包括：
 * - `quote` 内部（实测 `(quote (quote (when …)))` 会被展平）—— 那是**数据**，不该动；
 * - 与求值无关的位置。
 *
 * 本模块换一个判据：**只展开"会被求值"的位置**，遇到数据边界即停。
 */
extend Evaluator {
    /**
     * 快速探测：表达式里是否含有"会被求值的"宏调用。
     *
     * 用于给 `evalInner` 做短路 —— 绝大多数表达式不含宏，不必走完整展开（零分配）。
     * 边界规则与 `expandForCompile` 一致，保证两者判定不会互相矛盾。
     */
    func containsMacroCall(expr: LispValue): Bool {
        match (expr) {
            case Cons(cell) =>
                if (let Symbol(sym) <- cell.car) {
                    let resolvedSym = this.env.lookupKeyword(sym)
                    match (resolvedSym) {
                        // 数据边界：内部不展开，自然也不算"含宏"
                        case "quote" | "quasiquote" | "backquote" | "defmacro" => return false
                        case _ => ()
                    }
                    match (this.env.lookup(resolvedSym)) {
                        case Macro(_, _, _) | MacroFromParams(_, _, _) => return true
                        case _ => ()
                    }
                }
                if (this.containsMacroCall(cell.car)) {
                    return true
                }
                this.containsMacroCall(cell.cdr)
            case _ => false
        }
    }

    /**
     * 快速探测：表达式里是否含有"会被求值的"关键字别名调用。
     *
     * 与 `containsMacroCall` 同构 —— 别名同样必须在编译前解析成规范名，
     * 否则编译器会把它当普通函数调用（环境里并无此绑定）→ 报 `UnboundVariable`
     * 或静默什么都不做。实测 `(let ((t x)) (setq x y) …)` 在 BC 下失败而 AST 正常。
     */
    func containsAliasCall(expr: LispValue): Bool {
        match (expr) {
            case Cons(cell) =>
                if (let Symbol(sym) <- cell.car) {
                    let resolvedSym = this.env.lookupKeyword(sym)
                    match (resolvedSym) {
                        // 数据边界：内部不解析
                        case "quote" | "quasiquote" | "backquote" | "defmacro" => return false
                        case _ => ()
                    }
                    if (resolvedSym != sym) {
                        return true
                    }
                }
                if (this.containsAliasCall(cell.car)) {
                    return true
                }
                this.containsAliasCall(cell.cdr)
            case _ => false
        }
    }

    /**
     * 编译前展开：把表达式里**会被求值的**宏调用全部展开，返回无宏的等价表达式。
     *
     * 边界规则（哪些位置不下钻）：
     * - `(quote X)` / `'X`：X 是数据 → 整体保留；
     * - `(quasiquote X)` / `` `X ``：模板内除 `,` / `,@` 外是数据 → 整体保留
     *   （`backquote` 的既有求值路径自己会处理 `,` 部分）；
     * - `(defmacro …)`：宏定义体在**展开期**求值，不是运行期代码 → 整体保留。
     *
     * 其余位置一律下钻并展开，**包括 `lambda` / `define` 的函数体**：
     * 函数体是会被求值的代码，若不在编译前展开，该函数就编译不了（性能 + 语义双失）。
     *
     * @param expr 待展开表达式
     * @return 展开后的表达式；展开出错时返回 `Error`
     */
    func expandForCompile(expr: LispValue): LispValue {
        match (expr) {
            case Cons(cell) => this.expandForCompileCons(cell)
            case _ => expr
        }
    }

    private func expandForCompileCons(cell: ConsCell): LispValue {
        if (let Symbol(sym) <- cell.car) {
            // 关键字别名解析：编译器不认识别名表，`setq` 会被当普通函数调用
            // （环境里并无此绑定）→ 静默什么都不做或报 UnboundVariable。
            // 必须在**每个会被求值的位置**解析，而不只是顶层头部
            // —— 实测 (let … (setq …) …) 这类嵌套位置若不解析，BC 会报
            // `UnboundVariable: 'setq'` 且赋值不生效（AST 下正常）。
            let resolvedSym = this.env.lookupKeyword(sym)

            // 数据 / 展开期边界：不下钻
            match (resolvedSym) {
                case "quote" | "quasiquote" | "backquote" | "defmacro" => return Cons(cell)
                case _ => ()
            }

            // 宏调用：展开，并继续展开产物（产物里可能还有宏）
            let value = this.env.lookup(resolvedSym)
            match (value) {
                case Macro(_, _, _) | MacroFromParams(_, _, _) =>
                    if (this.macroExpandDepth >= this.maxMacroExpandDepth) {
                        return LispValue.Error(XispError(ErrorType.RuntimeError,
                            "Maximum macro expand depth exceeded (limit: ${this.maxMacroExpandDepth})"))
                    }
                    this.macroExpandDepth = this.macroExpandDepth + 1
                    let expanded = this.expandMacro(value, cell.cdr)
                    this.macroExpandDepth = this.macroExpandDepth - 1
                    match (expanded) {
                        case Error(_) => return expanded
                        case _ => return this.expandForCompile(expanded)
                    }
                case _ => ()
            }
        }

        // 一般位置：先解析头别名（若 car 是别名符号），再逐元素下钻
        if (let Symbol(sym) <- cell.car) {
            let canonical = this.env.lookupKeyword(sym)
            if (canonical != sym) {
                let newCdr0 = this.expandForCompileList(cell.cdr)
                match (newCdr0) {
                    case Error(_) => return newCdr0
                    case _ => return consValue(Symbol(canonical), newCdr0)
                }
            }
        }
        let newCar = this.expandForCompile(cell.car)
        match (newCar) {
            case Error(_) => return newCar
            case _ => ()
        }
        let newCdr = this.expandForCompileList(cell.cdr)
        match (newCdr) {
            case Error(_) => newCdr
            case _ => consValue(newCar, newCdr)
        }
    }

    /**
     * 逐元素展开列表（cdr 方向），非列表尾（点对）原样保留
     */
    private func expandForCompileList(list: LispValue): LispValue {
        match (list) {
            case Nil => Nil
            case Cons(cell) =>
                let newCar = this.expandForCompile(cell.car)
                match (newCar) {
                    case Error(_) => newCar
                    case _ =>
                        let newCdr = this.expandForCompileList(cell.cdr)
                        match (newCdr) {
                            case Error(_) => newCdr
                            case _ => consValue(newCar, newCdr)
                        }
                }
            case other => other
        }
    }
}
