; Xisp 基础教程示例
; 适合初学者了解 Lisp 基本语法

(println "╔═══════════════════════════════════════════════════════════╗")
(println "║              Xisp 基础教程                                ║")
(println "╚═══════════════════════════════════════════════════════════╝")

; ========== 1. 基本运算 ==========
(newline)
(println "1. 基本算术运算:")
(println "  (+ 1 2 3) =" (+ 1 2 3))
(println "  (- 10 3) =" (- 10 3))
(println "  (* 4 5) =" (* 4 5))
(println "  (/ 20 4) =" (/ 20 4))
(println "  (mod 10 3) =" (mod 10 3))

; ========== 2. 变量定义 ==========
(newline)
(println "2. 变量定义:")
(define pi 3.14159)
(println "  (define pi 3.14159)")
(println "  pi =" pi)

(define name "Xisp")
(println "  (define name \"Xisp\")")
(println "  name =" name)

; ========== 3. 比较运算 ==========
(newline)
(println "3. 比较运算:")
(println "  (= 5 5) =" (= 5 5))
(println "  (< 3 5) =" (< 3 5))
(println "  (> 10 5) =" (> 10 5))
(println "  (<= 5 5) =" (<= 5 5))

; ========== 4. 逻辑运算 ==========
(newline)
(println "4. 逻辑运算:")
(println "  (and #t #t) =" (and #t #t))
(println "  (or #t #f) =" (or #t #f))
(println "  (not #t) =" (not #t))

; ========== 5. 列表基础 ==========
(newline)
(println "5. 列表操作:")
(define fruits (list "apple" "banana" "cherry"))
(println "  (list \"apple\" \"banana\" \"cherry\") =" fruits)

(println "  (first fruits) =" (first fruits))
(println "  (second fruits) =" (second fruits))
(println "  (rest fruits) =" (rest fruits))
(println "  (length fruits) =" (length fruits))

; ========== 6. 函数定义 ==========
(newline)
(println "6. 函数定义:")
(define (square x) (* x x))
(println "  (define (square x) (* x x))")
(println "  (square 7) =" (square 7))

(define (add a b) (+ a b))
(println "  (define (add a b) (+ a b))")
(println "  (add 3 4) =" (add 3 4))

; ========== 7. 匿名函数 (lambda) ==========
(newline)
(println "7. 匿名函数:")
(println "  ((lambda (x) (* x x)) 5) ="
  ((lambda (x) (* x x)) 5))

; ========== 8. 条件判断 ==========
(newline)
(println "8. 条件判断 (if):")
(define age 18)
(println "  (define age 18)")
(println "  (if (>= age 18) \"成年\" \"未成年\") ="
  (if (>= age 18) "成年" "未成年"))

; ========== 9. 局部绑定 (let) ==========
(newline)
(println "9. 局部绑定 (let):")
(println "  (let ((x 10) (y 20)) (+ x y)) ="
  (let ((x 10) (y 20)) (+ x y)))

; ========== 10. 高阶函数 ==========
(newline)
(println "10. 函数式编程:")
(define numbers (list 1 2 3 4 5))
(println "  numbers =" numbers)

; map - 映射
(println "  (map square numbers) ="
  (map square numbers))

; filter - 过滤
(println "  (filter (lambda (n) (> n 2)) numbers) ="
  (filter (lambda (n) (> n 2)) numbers))

; reduce - 归约
(println "  (reduce + 0 numbers) ="
  (reduce + 0 numbers))

; sum - 求和
(println "  (sum numbers) =" (sum numbers))

; ========== 11. 递归函数 ==========
(newline)
(println "11. 递归函数:")
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
(println "  (define (factorial n) ...)")
(println "  (factorial 5) =" (factorial 5))

; ========== 12. 组合示例 ==========
(newline)
(println "12. 综合示例:")
(define (calculate-stats nums)
  (list
    (sum nums)
    (product nums)
    (length nums)))

(println "  stats = (calculate-stats (list 2 3 4))")
(define stats (calculate-stats (list 2 3 4)))
(println "  sum =" (first stats))
(println "  product =" (second stats))
(println "  count =" (third stats))

(newline)
(println "╔═══════════════════════════════════════════════════════════╗")
(println "║              教程完成！                                ║")
(println "╚═══════════════════════════════════════════════════════════╝")
