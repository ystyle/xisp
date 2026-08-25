;; 性能场景（放大版：N=200 层外层循环 × 每层 1 次，累计校验）
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(define (loop n acc)
  (if (= n 0) acc
    (loop (- n 1) (+ acc (+ (let ((f fib)) (f 22)))))))
(println (loop 200 0))
