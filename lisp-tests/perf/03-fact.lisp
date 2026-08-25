;; 性能场景（放大版：N=200 层外层循环 × 每层 12 次，累计校验）
(define (fact n) (if (< n 2) 1 (* n (fact (- n 1)))))
(define (loop n acc)
  (if (= n 0) acc
    (loop (- n 1) (+ acc (+ (fact 17) (fact 17) (fact 17) (fact 17) (fact 17) (fact 17) (fact 17) (fact 17) (fact 17) (fact 17) (fact 17) (fact 17))))))
(println (loop 200 0))
