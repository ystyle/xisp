package ystyle::xisp.core.builtin
import ystyle::xisp.core.eval.Evaluator

import ystyle::xisp.types.*
import ystyle::xisp.parser.Parser

/**
 * 内置宏库
 * 提供常用的预定义宏
 */
public class BuiltinMacros {}

extend BuiltinMacros {
    /**
     * 注册所有内置宏到环境
     */
    public static func registerAll(env: Environment): Unit {
        // 使用 eval 来定义宏，避免复杂的嵌套构造
        defineWhenMacro(env)
        defineUnlessMacro(env)
        defineIncfMacro(env)
        defineDecfMacro(env)
        defineSwapMacro(env)
        definePushMacro(env)
        definePopMacro(env)
        defineNegateMacro(env)
        defineDoMacro(env)
        defineDefunMacro(env)
        defineAndMacro(env)
        defineOrMacro(env)
    }

    /**
     * 定义 when 宏
     * (when test body...) → (if test (begin body...) nil)
     */
    private static func defineWhenMacro(env: Environment) {
        // (defmacro when (test . body)
        //   (if test (cons (quote begin) body) (quote 0)))
        let code = "(defmacro when (test . body) (list (quote if) test (cons (quote begin) body) (quote nil)))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }

    /**
     * 定义 unless 宏
     * (unless test body...) → (if test nil (begin body...))
     */
    private static func defineUnlessMacro(env: Environment) {
        // (defmacro unless (test . body)
        //   (if test (quote nil) (cons (quote begin) body)))
        let code = "(defmacro unless (test . body) (list (quote if) test (quote nil) (cons (quote begin) body)))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }

    /**
     * 定义 incf 宏
     */
    private static func defineIncfMacro(env: Environment) {
        // (defmacro incf (var)
        //   (list (quote setq) var (list (quote +) var 1)))
        let code = "(defmacro incf (var) (list (quote setq) var (list (quote +) var 1)))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }

    /**
     * 定义 decf 宏
     */
    private static func defineDecfMacro(env: Environment) {
        // (defmacro decf (var)
        //   (list (quote setq) var (list (quote +) var (quote -1))))
        let code = "(defmacro decf (var) (list (quote setq) var (list (quote +) var (quote -1))))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }

    /**
     * 定义 swap 宏
     */
    private static func defineSwapMacro(env: Environment) {
        // (defmacro swap (a b)
        //   (list (quote let)
        //         (list (list (quote temp) a))
        //         (list (quote setq) a b)
        //         (list (quote setq) b (quote temp))))
        let code = "(defmacro swap (a b) (list (quote let) (list (list (quote temp) a)) (list (quote setq) a b) (list (quote setq) b (quote temp))))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }

    /**
     * 定义 push 宏
     */
    private static func definePushMacro(env: Environment) {
        // (defmacro push (elem lst)
        //   (list (quote cons) elem lst))
        let code = "(defmacro push (elem lst) (list (quote cons) elem lst))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }

    /**
     * 定义 pop 宏
     */
    private static func definePopMacro(env: Environment) {
        // (defmacro pop (lst)
        //   (list (quote cdr) lst))
        let code = "(defmacro pop (lst) (list (quote cdr) lst))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }

    /**
     * 定义 negate 宏
     */
    private static func defineNegateMacro(env: Environment) {
        let code = "(defmacro negate (x) (list (quote *) x (quote -1)))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }

    /**
     * 定义 do 宏（begin 的别名）
     */
    private static func defineDoMacro(env: Environment) {
        let code = "(defmacro do body (cons (quote begin) body))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }

    /**
     * 定义 defun 宏（Common Lisp 兼容的函数定义）
     */
    private static func defineDefunMacro(env: Environment) {
        let code = "(defmacro defun (name args . body) (cons (quote define) (list (cons name args) (car body))))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }

    /**
     * 定义 and 宏 —— 短路求值
     *
     * (and)          → #t
     * (and a)        → (if a a #f)
     * (and a b ...)  → (if a (and b ...) #f)
     *
     * 为什么是宏而不是注册函数：注册函数会**先求值全部实参**，既不短路也不容忍
     * 后续实参出错。实测 (and 0 (undefined-fn)) 在 AST 下报 UndefinedFunction，
     * 而字节码后端（编译期展开成跳转）得到 false —— 两个后端语义不一致。
     * 改为宏后两后端走同一份展开产物，语义统一。
     */
    private static func defineAndMacro(env: Environment) {
        let code = "(defmacro and (. args) (if (null? args) #t (if (null? (cdr args)) `(if ,(car args) ,(car args) #f) `(if ,(car args) (and ,@(cdr args)) #f))))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }

    /**
     * 定义 or 宏 —— 短路求值，且首项只求值一次
     *
     * (or)          → #f
     * (or a)        → (if a a #f)
     * (or a b ...)  → (let ((g a)) (if g g (or b ...)))   ；g 由 gensym 生成
     *
     * 用 gensym 而非固定符号暂存首项：`defmacro` 是非卫生宏，固定符号会被调用点
     * 的同名变量捕获（实测 (let ((tmp 5)) (my-or #f tmp)) 返回 false 而非 5）。
     */
    private static func defineOrMacro(env: Environment) {
        let code = "(defmacro or (. args) (if (null? args) #f (if (null? (cdr args)) `(if ,(car args) ,(car args) #f) (let ((g (gensym))) `(let ((,g ,(car args))) (if ,g ,g (or ,@(cdr args))))))))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }
}
