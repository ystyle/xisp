;; 模块系统测试脚本

;; 测试 1: 定义一个包
;; (package test.math
;;   (version "1.0.0")
;;   (description "Test math module"))

;; 测试 2: 导出符号
;; (export add subtract multiply divide)

;; 测试 3: 定义一些函数
(define add (lambda (a b) (+ a b)))
(define subtract (lambda (a b) (- a b)))
(define multiply (lambda (a b) (* a b)))
(define divide (lambda (a b) (/ a b)))

;; 测试这些函数是否工作
(print "Testing basic math functions:")
(print "add 2 3 = " (add 2 3))
(print "subtract 5 2 = " (subtract 5 2))
(print "multiply 3 4 = " (multiply 3 4))
(print "divide 10 2 = " (divide 10 2))

;; 测试 4: 尝试使用 import（暂时不会工作，但测试语法）
;; (import std::math)
;; (math.add 1 2)

(print "Module system basic tests completed!")
