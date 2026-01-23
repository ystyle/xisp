;; 主程序 - 演示模块导入

;; 注意：由于文件系统遍历尚未实现，
;; 我们暂时手动定义一些函数来模拟模块系统

;; 手动定义 math 模块的函数
(define math::add (lambda (a b) (+ a b)))
(define math::subtract (lambda (a b) (- a b)))
(define math::multiply (lambda (a b) (* a b)))
(define math::divide (lambda (a b) (/ a b)))
(define math::abs (lambda (x) (if (< x 0) (- 0 x) x)))

;; 测试这些函数
(println "=== Math Module Test ===")
(println "")
(println "Addition:")
(println "  math::add(2, 3) = " (math::add 2 3))
(println "")

(println "Subtraction:")
(println "  math::subtract(5, 2) = " (math::subtract 5 2))
(println "")

(println "Multiplication:")
(println "  math::multiply(3, 4) = " (math::multiply 3 4))
(println "")

(println "Division:")
(println "  math::divide(10, 2) = " (math::divide 10 2))
(println "")

(println "Absolute value:")
(println "  math::abs(-5) = " (math::abs -5))
(println "  math::abs(5) = " (math::abs 5))
(println "")

(println "=== Test Completed ===")
