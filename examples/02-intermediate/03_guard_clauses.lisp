;; ======================================
;; 守卫条件 (Guard Clauses) 示例
;; ======================================

(println "=== Xisp 守卫条件演示 ===")
(println "")

(println "1. 基本守卫条件 - 大于 10")
(define r1 (match 15 (x when (> x 10) (str "large: " x)) (x when (< x 10) (str "small: " x)) _ "medium"))
(println r1)
(println "")

(println "2. 基本守卫条件 - 小于 10")
(define r2 (match 5 (x when (> x 10) (str "large: " x)) (x when (< x 10) (str "small: " x)) _ "medium"))
(println r2)
(println "")

(println "3. 守卫条件 - 正数")
(define r3 (match -5 (x when (> x 0) "positive") (x when (< x 0) "negative") _ "zero"))
(println r3)
(println "")

(println "4. 递归函数中使用模式匹配")
(define (my-length lst)
  (match lst
    () 0
    (head & tail) (+ 1 (my-length tail))))
(define r4 (my-length [1 2 3 4 5]))
(println r4)
