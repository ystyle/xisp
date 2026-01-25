;; type-of 函数测试

(println "=== type-of 函数测试 ===")

(newline)
(println "--- 基础类型测试 ---")

;; 测试整数
(define int-result (type-of 42))
(println "type-of 42 =" int-result)
(if (string=? int-result "integer")
    (println "✓ 整数类型测试通过")
    (println "✗ 整数类型测试失败"))

;; 测试浮点数
(define float-result (type-of 3.14))
(println "type-of 3.14 =" float-result)
(if (string=? float-result "float")
    (println "✓ 浮点数类型测试通过")
    (println "✗ 浮点数类型测试失败"))

;; 测试字符串
(define str-result (type-of "hello"))
(println "type-of \"hello\" =" str-result)
(if (string=? str-result "string")
    (println "✓ 字符串类型测试通过")
    (println "✗ 字符串类型测试失败"))

;; 测试布尔值
(define bool-result1 (type-of #t))
(println "type-of #t =" bool-result1)
(if (string=? bool-result1 "boolean")
    (println "✓ 布尔值(#t)类型测试通过")
    (println "✗ 布尔值(#t)类型测试失败"))

(define bool-result2 (type-of #f))
(println "type-of #f =" bool-result2)
(if (string=? bool-result2 "boolean")
    (println "✓ 布尔值(#f)类型测试通过")
    (println "✗ 布尔值(#f)类型测试失败"))

;; 测试 nil
(define nil-result (type-of nil))
(println "type-of nil =" nil-result)
(if (string=? nil-result "nil")
    (println "✓ nil 类型测试通过")
    (println "✗ nil 类型测试失败"))

(newline)
(println "--- 复合类型测试 ---")

;; 测试符号
(define sym-result (type-of 'foo))
(println "type-of 'foo =" sym-result)
(if (string=? sym-result "symbol")
    (println "✓ 符号类型测试通过")
    (println "✗ 符号类型测试失败"))

;; 测试列表
(define list-result (type-of '(1 2 3)))
(println "type-of '(1 2 3) =" list-result)
(if (string=? list-result "list")
    (println "✓ 列表类型测试通过")
    (println "✗ 列表类型测试失败"))

;; 测试空列表
(define empty-list-result (type-of '()))
(println "type-of '() =" empty-list-result)
;; 在 Lisp 中，空列表和 nil 是同一个东西
(if (string=? empty-list-result "nil")
    (println "✓ 空列表类型测试通过（空列表即为 nil）")
    (println "✗ 空列表类型测试失败"))

;; 测试过程
(define proc-result (type-of +))
(println "type-of + =" proc-result)
(if (string=? proc-result "procedure")
    (println "✓ 过程类型测试通过")
    (println "✗ 过程类型测试失败"))

(newline)
(println "--- Lambda 函数测试 ---")

(define (square x) (* x x))
(define lambda-result (type-of square))
(println "type-of square =" lambda-result)
(if (string=? lambda-result "procedure")
    (println "✓ Lambda 函数类型测试通过")
    (println "✗ Lambda 函数类型测试失败"))

(newline)
(println "--- 测试总结 ---")
(println "type-of 函数测试完成")
