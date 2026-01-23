package ystyle::xisp.core

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
    public static func registerAll(env: Environment) {
        // 使用 eval 来定义宏，避免复杂的嵌套构造
        defineWhenMacro(env)
        defineUnlessMacro(env)
        defineIncfMacro(env)
        defineDecfMacro(env)
        defineSwapMacro(env)
        definePushMacro(env)
        definePopMacro(env)
        defineNegateMacro(env)
    }

    /**
     * 定义 when 宏
     */
    private static func defineWhenMacro(env: Environment) {
        // (defmacro when (test then)
        //   (list (quote if) test then (quote 0)))
        let code = "(defmacro when (test then) (list (quote if) test then (quote 0)))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }

    /**
     * 定义 unless 宏
     */
    private static func defineUnlessMacro(env: Environment) {
        // (defmacro unless (test then)
        //   (list (quote if) test (quote 0) then))
        let code = "(defmacro unless (test then) (list (quote if) test (quote 0) then))"
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
        // (defmacro negate (x)
        //   (list (quote *) x (quote -1)))
        let code = "(defmacro negate (x) (list (quote *) x (quote -1)))"
        let exprs = Parser.parseFromString(code)
        if (exprs.size > 0) {
            let evaluator = Evaluator(env)
            evaluator.eval(exprs[0])
        }
    }
}
