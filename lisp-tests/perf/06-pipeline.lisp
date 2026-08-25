;; 性能场景（放大版：N=200 层外层循环 × 每层 8 次，累计校验）
(define (make-range n) (if (= n 0) '() (prepend n (make-range (- n 1)))))
(define data (make-range 120))
(define (pipeline d) (-> d (map (lambda (x) (* x x))) (filter (lambda (x) (= 0 (mod x 3)))) (reduce + 0)))
(define (loop n acc)
  (if (= n 0) acc
    (loop (- n 1) (+ acc (+ (pipeline data) (pipeline data) (pipeline data) (pipeline data) (pipeline data) (pipeline data) (pipeline data) (pipeline data))))))
(println (loop 200 0))
