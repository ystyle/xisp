package ystyle::xisp.core.builtin

import ystyle::xisp.core.eval.Evaluator
import ystyle::xisp.types.*
import ystyle::xisp.parser.*

/**
 * 内建宏包（④层）
 *
 * ## 这层的性质
 *
 * 本文件里的 16 个宏**全部用 Lisp 源码写成**，启动时装进环境 —— 它们不是
 * 仓颉函数，也不是求值器原语。这是"**lisp 实现 lisp 所有**"的直接体现：
 * `when` / `unless` / `do` / `defun` 这些"语法"不是硬编码的，是十几行 Lisp。
 *
 * 也是能力域 `macros.core` 的安装内容（`evalSource` 形态，见 §4.2）：
 *
 * | 实现形态 | 用在哪 |
 * |---|---|
 * | `ctx.register(...)` | 需要性能或宿主能力时（仓颉实现） |
 * | `ctx.evalSource(...)` | 纯语法糖、可用纯 Lisp 表达的能力（本文件） |
 *
 * ## 语义分组
 *
 * | 类别 | 宏 |
 * |---|---|
 * | 条件 | `when` `unless` `condb` `if-let` `when-let*` |
 * | 绑定 | `let*` |
 * | 逻辑 | `and` `or` |
 * | 赋值糖 | `incf` `decf` `swap` `negate` |
 * | 列表糖 | `push` `pop` |
 * | 过程/顺序 | `defun` `do` |
 *
 * ## 两条踩过的坑（写新宏时注意）
 *
 * 1. **宏收语法不收值**：`(%helper x)` 传给 helper 的是符号 `x` 本身，不是它的值。
 *    需要在展开期做值运算时用**普通函数**（`reverse` / `car` / `map`），
 *    如 `when-let*` 用 `(car (car (reverse bindings)))` 取末变量。
 * 2. **点对参数比固定参数深一层**：`(. cs)` 收到的 `cs` 是**子句列表**，
 *    判定子句头要 `(car (car cs))` 而不是 `(car cs)`。
 *
 * ## 非卫生性
 *
 * `defmacro` 与调用点共享符号空间，宏体引入的临时变量会捕获用户同名变量。
 * 需要临时符号时用 `gensym`（`or` 即如此）。
 */
public class BuiltinMacros {}

/** 内建宏包源码：`defmacro` 定义，按依赖顺序排列（被依赖者在前） */
public let BUILTIN_MACROS_SOURCE: String = '''
;; ---- 条件 ----
(defmacro when (test . body)
  (list (quote if) test (cons (quote begin) body) (quote nil)))
(defmacro unless (test . body)
  (list (quote if) test (quote nil) (cons (quote begin) body)))
;; condb：:let 子句 + (条件 结果) 对 + else；全假无 else 返回 nil
;; 自递归展开：:let → (let (binding) (condb 剩余…))；else → 其结果
(defmacro condb (. cs)
  (if (null? cs)
      nil
      (if (eq? (car (car cs)) :let)
          `(let (,(cdr (car cs))) (condb ,@(cdr cs)))
          (if (eq? (car cs) (quote else))
              (cadr cs)
              `(if ,(car cs) ,(cadr cs) (condb ,@(cddr cs)))))))
(defmacro if-let (binding then else)
  `(let (,(list (car binding) (cadr binding))) (if ,(car binding) ,then ,else)))
;; when-let*：全部绑定都建立，只判**最后一个**值的真值
(defmacro when-let* (bindings . body)
  (if (null? bindings)
      nil
      `(let* ,bindings (if ,(car (car (reverse bindings))) (begin ,@body) nil))))

;; ---- 绑定 ----
(defmacro let* (bindings . body)
  (if (null? bindings)
      `(begin ,@body)
      (if (null? (cdr bindings))
          `(let (,(car bindings)) ,@body)
          `(let (,(car bindings)) (let* ,(cdr bindings) ,@body)))))

;; ---- 逻辑（短路）----
(defmacro and (. args)
  (if (null? args)
      #t
      (if (null? (cdr args))
          `(if ,(car args) ,(car args) #f)
          `(if ,(car args) (and ,@(cdr args)) #f))))
;; or 用 gensym 暂存首项：非卫生宏里固定符号会被调用点同名变量捕获
(defmacro or (. args)
  (if (null? args)
      #f
      (if (null? (cdr args))
          `(if ,(car args) ,(car args) #f)
          (let ((g (gensym)))
            `(let ((,g ,(car args))) (if ,g ,g (or ,@(cdr args))))))))

;; ---- 赋值糖 ----
(defmacro incf (var) (list (quote set! ) var (list (quote +) var 1)))
(defmacro decf (var) (list (quote set! ) var (list (quote +) var (quote -1))))
(defmacro negate (x) (list (quote *) x (quote -1)))
(defmacro swap (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a)) (begin (set! ,a ,b) (set! ,b ,tmp)))))

;; ---- 列表糖 ----
(defmacro push (elem lst) (list (quote cons) elem lst))
(defmacro pop (lst) (list (quote cdr) lst))

;; ---- 过程 / 顺序 ----
(defmacro do (. body) (cons (quote begin) body))
(defmacro defun (name args . body) (cons (quote define) (list (cons name args) (car body))))
'''

extend BuiltinMacros {
    /** 宏包声明的全部符号（供能力域 `symbols` 与重名检出使用） */
    public static func allSymbols(): Array<String> {
        [
            "when", "unless", "condb", "if-let", "when-let*",
            "let*",
            "and", "or",
            "incf", "decf", "negate", "swap",
            "push", "pop",
            "do", "defun",
        ]
    }

    /**
     * 安装内建宏包（`evalSource` 形态）
     */
    public static func registerAll(env: Environment): Unit {
        let evaluator = Evaluator(env)
        let parser = Parser(Lexer(BUILTIN_MACROS_SOURCE).tokenize())
        for (expr in parser.parse()) {
            evaluator.eval(expr)
        }
    }
}
