;; std/math 核心数学函数

;; 加法
(define add (lambda (a b) (+ a b)))

;; 减法
(define subtract (lambda (a b) (- a b)))

;; 乘法
(define multiply (lambda (a b) (* a b)))

;; 除法
(define divide (lambda (a b) (/ a b)))

;; 幂运算
(define pow (lambda (base exp)
  (if (= exp 0)
    1
    (* base (pow base (- exp 1)))))

;; 绝对值
(define abs (lambda (x)
  (if (< x 0)
    (- 0 x)
    x)))

(print "Math module loaded successfully!")
