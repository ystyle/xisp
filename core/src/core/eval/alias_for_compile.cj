package ystyle::xisp.core.eval

import ystyle::xisp.types.*

/**
 * 编译前的**关键字别名解析**
 *
 * ## 问题
 *
 * 关键字别名（`(registerKeywordAlias "setq" "set!")`）此前只在 **AST 侧**生效
 * （`evalListInternal` 的慢路径查 `lookupKeyword`）。字节码编译器完全不认识别名，
 * 于是同一个符号在两个后端下行为不同：
 *
 * ```
 * (define x 1) (setq x 2) x     AST → 2      BC → 1   ← 赋值静默失效，且不报错
 * ```
 *
 * 实测确认：编译器对 `setq` 走 `compileCall`，把它当普通函数调用 —— 而
 * `setq` 在环境里根本没有绑定（它只是别名），于是**静默什么都没做**。
 * 这比报错更危险：脚本在字节码模式下悄悄算错。
 *
 * ## 修法
 *
 * 在编译分派之前、与宏展开同一处，把**表达式头**的别名解析成规范名。
 * 编译器只需认识规范名（`set!` / `define` / …），不必知道别名表。
 *
 * 只解析**头位置**（car），不碰数据位置：
 * - 头位置是会被分派/调用的符号，别名在此等价于规范名；
 * - 参数位置与 `quote` 内部是数据或普通求值，改它们会改变语义。
 */
extend Evaluator {
    /**
     * 解析表达式头的关键字别名，返回规范名；无需解析时返回 `None`。
     *
     * 零分配探测：只在"确有别名且别名≠自身"时才返回 `Some`。
     */
    func resolveHeadAlias(expr: LispValue): Option<LispValue> {
        match (expr) {
            case Cons(cell) => match (cell.car) {
                case Symbol(s) =>
                    let canonical = this.env.lookupKeyword(s)
                    if (canonical != s) {
                        // 用规范名替换头，cdr 原样保留
                        Some(consValue(Symbol(canonical), cell.cdr))
                    } else {
                        None
                    }
                case _ => None
            }
            case _ => None
        }
    }
}
