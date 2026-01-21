; Xisp 快速功能验证
; 用于快速测试核心功能是否正常

(println "========== Xisp 快速功能验证 ==========")

; 1. 算术
(println "1. 算术运算:")
(println "  (+ 1 2 3) =" (+ 1 2 3))
(println "  (* 4 5) =" (* 4 5))
(println "  (- 10 3) =" (- 10 3))

; 2. 变量
(println "2. 变量:")
(define x 42)
(println "  (define x 42)")
(println "  x =" x)

; 3. 列表
(println "3. 列表:")
(define lst (list 1 2 3))
(println "  lst =" lst)
(println "  (length lst) =" (length lst))
(println "  (first lst) =" (first lst))

; 4. 函数
(println "4. 函数:")
(define (square n) (* n n))
(println "  (define (square n) (* n n))")
(println "  (square 5) =" (square 5))

; 5. 高阶函数
(println "5. 高阶函数:")
(println "  (sum lst) =" (sum lst))
(define squared (map square lst))
(println "  (map square lst) =" squared)

; 6. 比较和逻辑
(println "6. 比较和逻辑:")
(println "  (< 3 5) =" (< 3 5))
(println "  (and #t #t) =" (and #t #t))

; 7. 递归
(println "7. 递归:")
(define (fact n)
  (if (<= n 1) 1 (* n (fact (- n 1)))))
(println "  (fact 5) =" (fact 5))

; 8. 打印
(println "8. 打印:")
(print "  Print test: ")
(newline)
(println "  Println test: OK")

(newline)
(println "✓ 所有功能测试完成！")
