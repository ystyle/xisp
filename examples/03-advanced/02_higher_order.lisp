;; ======================================
;; 高阶函数深度 (Higher-Order Functions) 示例
;; ======================================

(println "=== Xisp 高阶函数深度演示 ===")
(println "")

(println "1. Map - 对每个元素应用函数")
(println "代码: (map (lambda (x) (* x x)) [1 2 3 4 5])")
(define r1 (map (lambda (x) (* x x)) [1 2 3 4 5]))
(println #"结果: #{r1}")
(println "期望: (1 4 9 16 25)")
(println "")

(println "2. Filter - 过滤元素")
(println "代码: (filter (lambda (x) (> x 10)) [5 15 8 20 3])")
(define r2 (filter (lambda (x) (> x 10)) [5 15 8 20 3]))
(println #"结果: #{r2}")
(println "期望: (15 20)")
(println "")

(println "3. Reduce - 归约操作")
(println "代码: (reduce + 0 [1 2 3 4 5])")
(define r3 (reduce + 0 [1 2 3 4 5]))
(println #"结果: #{r3}")
(println "期望: 15")
(println "")

(println "4. Reduce 用于求积")
(println "代码: (reduce * 1 [1 2 3 4])")
(define r4 (reduce * 1 [1 2 3 4]))
(println #"结果: #{r4}")
(println "期望: 24")
(println "")

(println "5. Reduce 用于找最大值")
(println "代码: (reduce (lambda (acc x) (if (> acc x) acc x)) 0 [3 1 4 1 5 9 2 6])")
(define r5 (reduce (lambda (acc x) (if (> acc x) acc x)) 0 [3 1 4 1 5 9 2 6]))
(println #"结果: #{r5}")
(println "期望: 9")
(println "")

(println "6. 链式高阶函数组合")
(println "代码: (reduce + 0 (map (lambda (x) (* x x)) (filter (lambda (x) (> x 2)) [1 2 3 4 5])))")
(println "说明: 过滤 > 2 => (3 4 5), 平方 => (9 16 25), 求和 => 50")
(define r6 (reduce + 0
    (map (lambda (x) (* x x))
        (filter (lambda (x) (> x 2)) [1 2 3 4 5]))))
(println #"结果: #{r6}")
(println "期望: 50")
(println "")

(println "7. 自定义高阶函数 - twice")
(println "代码: (define (twice f) (lambda (x) (f (f x))))")
(define (twice f)
  (lambda (x) (f (f x))))
(define inc (lambda (x) (+ x 1)))
(define inc-twice (twice inc))
(println #"(inc-twice 5): #{(inc-twice 5)}")
(println "期望: 7 (5 + 1 + 1)")
(println "")

(println "8. Currying 柯里化")
(println "代码: (define (add-curry a) (lambda (b) (+ a b)))")
(define (add-curry a)
  (lambda (b) (+ a b)))
(define add5 (add-curry 5))
(println #"(add5 10): #{(add5 10)}")
(println "期望: 15")
(println #"(add5 20): #{(add5 20)}")
(println "期望: 25")
(println "")

(println "=== 高阶函数演示完成 ===")
