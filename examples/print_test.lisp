; Xisp 打印功能测试脚本
; 运行方式: ./xisp < examples/print_test.lisp

; ========== 基本打印测试 ==========
(println "╔═══════════════════════════════════════════════════════════╗")
(println "║              Xisp 打印功能测试脚本                        ║")
(println "╚═══════════════════════════════════════════════════════════╝")

; ========== println - 多参数打印并换行 ==========
(newline)
(println ">>> 测试 println - 多参数打印:")
(println "Hello" "Xisp" "Lisp")
(println "Number:" 42 "Pi:" 3.14)

; ========== print - 不换行 ==========
(newline)
(println ">>> 测试 print - 不换行:")
(print "This is ")
(print "a ")
(print "test")
(newline)

; ========== princ - Lisp 风格打印 ==========
(newline)
(println ">>> 测试 princ:")
(princ "No newline here")
(newline)

; ========== display - Scheme 风格显示 ==========
(newline)
(println ">>> 测试 display:")
(display "Display test")
(newline)

; ========== newline - 换行 ==========
(newline)
(println ">>> 测试 newline:")
(print "Line 1")
(newline)
(print "Line 2")
(newline)

; ========== 结合变量和计算 ==========
(newline)
(println ">>> 测试变量和计算:")
(define x 42)
(println "x =" x)
(println "x * 2 =" (* x 2))

(define name "Xisp")
(println "Welcome to" name)

; ========== 列表打印 ==========
(newline)
(println ">>> 测试列表打印:")
(define numbers (list 1 2 3 4 5))
(println "Numbers:" numbers)
(println "Sum:" (sum numbers))
(println "Product:" (product numbers))

; ========== 高阶函数打印 ==========
(newline)
(println ">>> 测试高阶函数:")
(println "Squared:" (map (lambda (n) (* n n)) numbers))
(println "Filtered >2:" (filter (lambda (n) (> n 2)) numbers))

; ========== 函数定义和调用 ==========
(newline)
(println ">>> 测试函数定义:")
(define (square x) (* x x))
(println "square(7) =" (square 7))

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
(println "factorial(5) =" (factorial 5))

; ========== 嵌套打印 ==========
(newline)
(println ">>> 测试嵌套打印:")
(println "Result:" (+ 1 (print "Calculating...") 2))

; ========== 字符串插值效果 ==========
(newline)
(println ">>> 字符串和数字混合:")
(println "Values:" 1 "two" 3.0 "'four'" #t #f)

; ========== 测试完成 ==========
(newline)
(println "╔═══════════════════════════════════════════════════════════╗")
(println "║                   测试完成！                            ║")
(println "╚═══════════════════════════════════════════════════════════╝")
