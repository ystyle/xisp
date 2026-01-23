(println "=== Testing when-let* ===")

;; Test 1: 基本功能
(println "Test 1: (when-let* ((x 5) (y (> x 3))) (+ x y))")
(define result1 (when-let* ((x 5) (y (> x 3))) (+ x y)))
(println "Result: " result1)
(println "Expected: 8.000000")
(println "")

;; Test 2: 条件为假
(println "Test 2: (when-let* ((x 1) (y (> x 3))) (+ x y))")
(define result2 (when-let* ((x 1) (y (> x 3))) (+ x y)))
(println "Result: " result2)
(println "Expected: nil")
(println "")

;; Test 3: 多个绑定
(println "Test 3: (when-let* ((a 10) (b (> a 5)) (c (* b 2))) c)")
(define result3 (when-let* ((a 10) (b (> a 5)) (c (* b 2))) c))
(println "Result: " result3)
(println "Expected: 2.000000")
(println "")

(println "=== Tests Complete ===")
