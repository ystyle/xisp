;; 性能场景（放大版：N=200 层外层循环 × 每层 40 次，累计校验）
(define (make-adder n) (lambda (x) (+ x n)))
(define (one) (let ((f (make-adder 7))) (f 3)))
(define (loop n acc)
  (if (= n 0) acc
    (loop (- n 1) (+ acc (+ (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one)))))))
(println (loop 200 0))
