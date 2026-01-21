; Xisp 高级特性示例
; 展示闭包、高阶函数、函数式编程等高级特性

(println "╔═══════════════════════════════════════════════════════════╗")
(println "║           Xisp 高级特性示例                               ║")
(println "╚═══════════════════════════════════════════════════════════╝")

; ========== 1. 闭包 (Closures) ==========
(newline)
(println "1. 闭包 - 函数工厂:")
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(define add10 (make-adder 10))

(println "  (define add5 (make-adder 5))")
(println "  (add5 3) =" (add5 3))
(println "  (add10 3) =" (add10 3))

; ========== 2. 高阶函数组合 ==========
(newline)
(println "2. 函数组合:")
(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))
(define (increment x) (+ x 1))

(define square-then-inc (compose increment square))
(define inc-then-square (compose square increment))

(println "  (square-then-inc 3) =" (square-then-inc 3))
(println "  (inc-then-square 3) =" (inc-then-square 3))

; ========== 3. 柯里化 (Currying) ==========
(newline)
(println "3. 柯里化:")
(define (curried-add x)
  (lambda (y)
    (lambda (z)
      (+ x y z))))

(define add1 (curried-add 1))
(define add1-2 (add1 2))

(println "  (((curried-add 1) 2) 3) =" (((curried-add 1) 2) 3))

; ========== 4. 列表处理链 ==========
(newline)
(println "4. 列表处理管道:")
(define numbers (list 1 2 3 4 5 6 7 8 9 10))

(println "  numbers =" numbers)

(println "  偶数平方和 ="
  (sum
    (map square
      (filter (lambda (n) (= (mod n 2) 0))
        numbers))))

; ========== 5. 归约操作 ==========
(newline)
(println "5. 归约 (reduce):")
(define nums (list 1 2 3 4 5))

(println "  nums =" nums)
(println "  (reduce + 0 nums) =" (reduce + 0 nums))
(println "  (reduce * 1 nums) =" (reduce * 1 nums))

; ========== 6. 反向列表 ==========
(newline)
(println "6. 列表反转:")
(println "  (reverse (list 1 2 3)) ="
  (reverse (list 1 2 3)))

; ========== 7. 范围生成 ==========
(newline)
(println "7. 范围生成:")
(println "  (range 0 10) =" (range 0 10))
(println "  (range 0 10 2) =" (range 0 10 2))

(println "  偶数列表 =" (range 0 20 2))

; ========== 8. 映射多个操作 ==========
(newline)
(println "8. 链式 map:")
(define data (list 1 2 3 4 5))

(println "  data =" data)
(println "  处理后 ="
  (map (lambda (n) (* n 2))
    (filter (lambda (n) (> n 2))
      data)))

; ========== 9. 累加器模式 ==========
(newline)
(println "9. 累加器:")
(define (make-accumulator init)
  (let ((sum init))
    (lambda (n)
      (set! sum (+ sum n))
      sum)))

(define acc (make-accumulator 0))
(println "  (acc 10) =" (acc 10))
(println "  (acc 20) =" (acc 20))
(println "  (acc 30) =" (acc 30))

; ========== 10. 记忆化 (简单版) ==========
(newline)
(println "10. 缓存计算结果:")
(define memo-fib
  (let ((cache (list)))
    (lambda (n)
      (if (< n 2)
          n
          (+ (memo-fib (- n 1))
             (memo-fib (- n 2)))))))

(println "  (memo-fib 10) =" (memo-fib 10))

; ========== 11. 列表推导式风格 ==========
(newline)
(println "11. 列表生成模式:")
(define (squares-up-to n)
  (map square (range 1 (+ n 1))))

(println "  (squares-up-to 5) =" (squares-up-to 5))

; ========== 12. 函数式管道 ==========
(newline)
(println "12. 数据处理管道:")
(define process-data
  (compose
    (lambda (lst) (map square lst))
    (lambda (lst) (filter (lambda (n) (> n 10)) lst))
    (lambda (lst) (reverse lst))))

(println "  处理 (range 1 6):")
(println "  结果 =" (process-data (range 1 6)))

(newline)
(println "╔═══════════════════════════════════════════════════════════╗")
(println "║              高级特性展示完成！                        ║")
(println "╚═══════════════════════════════════════════════════════════╝")
