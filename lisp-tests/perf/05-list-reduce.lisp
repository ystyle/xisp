;; 性能场景（放大版：N=200 层外层循环 × 每层 20 次，累计校验）
(define (make-range n) (if (= n 0) '() (prepend n (make-range (- n 1)))))
(define data (make-range 150))
(define (loop n acc)
  (if (= n 0) acc
    (loop (- n 1) (+ acc (+ (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data) (reduce + 0 data))))))
(println (loop 200 0))
