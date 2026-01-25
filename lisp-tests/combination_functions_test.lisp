;; 传统组合函数测试
;; 测试 cadr, caddr, cddr, cdar, caar 等传统 Lisp 组合函数

(println "=== 测试 cadr (第二个元素) ===")
(define lst1 '(1 2 3 4 5))
(define result1 (cadr lst1))
(println "cadr '(1 2 3 4 5) =" result1)
(if (= result1 2)
    (println "✓ cadr test passed")
    (println "✗ cadr test failed"))

;; 与 second 比较
(define result2 (second lst1))
(if (= result1 result2)
    (println "✓ cadr equals second")
    (println "✗ cadr does not equal second"))

(newline)
(println "=== 测试 caddr (第三个元素) ===")
(define result3 (caddr lst1))
(println "caddr '(1 2 3 4 5) =" result3)
(if (= result3 3)
    (println "✓ caddr test passed")
    (println "✗ caddr test failed"))

;; 与 third 比较
(define result4 (third lst1))
(if (= result3 result4)
    (println "✓ caddr equals third")
    (println "✗ caddr does not equal third"))

(newline)
(println "=== 测试 cddr (除前两个外的剩余部分) ===")
(define result5 (cddr lst1))
(println "cddr '(1 2 3 4 5) =" result5)
(if (and (not (null? result5)) (= (first result5) 3) (= (length result5) 3))
    (println "✓ cddr test passed")
    (println "✗ cddr test failed"))

;; 与 (rest (rest lst)) 比较
(define result6 (rest (rest lst1))
(if (= (length result5) (length result6))
    (println "✓ cddr equals rest of rest")
    (println "✗ cddr does not equal rest of rest"))

(newline)
(println "=== 测试 caar (car of car) ===")
(define nested1 '((a b c) (d e f)))
(define result7 (caar nested1))
(println "caar '((a b c) (d e f)) =" result7)
(if (symbol? result7)
    (println "✓ caar returns symbol")
    (println "✗ caar does not return symbol"))

(newline)
(println "=== 测试 cdar (cdr of car) ===")
(define result8 (cdar nested1))
(println "cdar '((a b c) (d e f)) =" result8)
(if (and (not (null? result8)) (= (length result8) 2) (eq? (first result8) 'b))
    (println "✓ cdar test passed")
    (println "✗ cdar test failed"))

(newline)
(println "=== 测试手动嵌套组合 ===")
(define deep '((1 2 3) (4 5 6)))
(define result9 (car (car (cdr deep))))  ; = (car '(4 5 6)) = 4
(println "car (car (cdr deep)) =" result9)
(if (= result9 4)
    (println "✓ nested combination test 1 passed")
    (println "✗ nested combination test 1 failed"))

(define result10 (car (cdr (car deep))))  ; = (car '(2 3)) = 2
(println "car (cdr (car deep)) =" result10)
(if (= result10 2)
    (println "✓ nested combination test 2 passed")
    (println "✗ nested combination test 2 failed"))

(newline)
(println "=== 测试边界情况 ===")

;; 空列表
(define empty-list '())
(define e1 (cadr empty-list))
(define e2 (caddr empty-list))
(define e3 (cddr empty-list))
(if (and (null? e1) (null? e2) (null? e3))
    (println "✓ empty list edge cases passed")
    (println "✗ empty list edge cases failed"))

;; 单元素列表
(define single '(1))
(define s1 (cadr single))
(define s2 (caddr single))
(if (and (null? s1) (null? s2))
    (println "✓ single element list edge cases passed")
    (println "✗ single element list edge cases failed"))

;; 两元素列表
(define pair '(1 2))
(define p1 (cadr pair))
(define p2 (caddr pair))
(define p3 (cddr pair))
(if (and (= p1 2) (null? p2) (null? p3))
    (println "✓ pair edge cases passed")
    (println "✗ pair edge cases failed"))

;; 三元素列表
(define triple '(1 2 3))
(define t1 (cadr triple))
(define t2 (caddr triple))
(define t3 (cddr triple))
(if (and (= t1 2) (= t2 3) (null? t3))
    (println "✓ triple edge cases passed")
    (println "✗ triple edge cases failed"))

(newline)
(println "=== 所有传统组合函数测试通过 ===")
