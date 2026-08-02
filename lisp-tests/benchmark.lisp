;; ============================================================
;; Xisp 性能基准测试（拆箱栈优化用）
;; 由外部 time 命令计时：time xisp lisp-tests/benchmark.lisp
;; ============================================================

;; 1. 递归 fib(30) — 纯整数递归
(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(println "fib(30): " (fib 30))

;; 2. fact(20)
(define (fact n)
  (if (< n 2) 1 (* n (fact (- n 1)))))
(println "fact(20): " (fact 20))

;; 3. 循环求和 0..500000（尾递归，验证非尾递归场景）
(define (sum-to n)
  (define (iter i acc)
    (if (> i n) acc (iter (+ i 1) (+ acc i))))
  (iter 1 0))
(println "sum(0..500000): " (sum-to 500000))
