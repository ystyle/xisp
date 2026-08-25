;; 性能场景（放大版：N=200 层外层循环 × 每层 20 次，累计校验）
(define (sum-iter i acc n) (if (> i n) acc (sum-iter (+ i 1) (+ acc i) n)))
(define (loop n acc)
  (if (= n 0) acc
    (loop (- n 1) (+ acc (+ (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80) (sum-iter 1 0 80))))))
(println (loop 200 0))
