;; 性能场景（放大版：N=200 层外层循环 × 每层 40 次，累计校验）
(define (make-counter) (let ((n 0)) (lambda () (set! n (+ n 1)) n)))
(define c (make-counter))
(define (loop n acc)
  (if (= n 0) acc
    (loop (- n 1) (+ acc (+ (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c) (c)))))))
(println (loop 200 0))
