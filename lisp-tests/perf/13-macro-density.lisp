;; 性能场景（放大版：N=200 层外层循环 × 每层 40 次，累计校验）
(defmacro when (test then) `(if ,test ,then nil))
(define gx 0)
(define (one) (when (>= gx 0) 1))
(define (loop n acc)
  (if (= n 0) acc
    (loop (- n 1) (+ acc (+ (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one) (one)))))))
(println (loop 200 0))
