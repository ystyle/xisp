;; ========================================
;; Xisp 5分钟快速体验
;; ========================================

(println "=== Xisp 快速体验 ===\n")

;; 1. 基本算术运算
(println "1. 基本算术运算:")
(println "  1 + 2 = " (+ 1 2))
(println "  10 - 3 = " (- 10 3))
(println "  4 * 5 = " (* 4 5))
(println "   20 / 4 = " (/ 20 4))
(newline)

;; 2. 变量定义
(println "2. 变量定义:")
(define name "Xisp")
(define version "1.0")
(println "  欢迎 " name " v" version)
(newline)

;; 3. 列表操作
(println "3. 列表操作:")
(define numbers (list 1 2 3 4 5))
(println "  列表: " numbers)
(println "  第一个元素: " (first numbers))
(println "  剩余元素: " (rest numbers))
(println "  列表长度: " (length numbers))
(newline)

;; 4. 函数定义
(println "4. 函数定义:")
(define (square x)
  (* x x))
(println "  square(5) = " (square 5))
(define (add-a b a)
  (+ a b))
(println "  add-a(3, 10) = " (add-a 3 10))
(newline)

;; 5. 高阶函数
(println "5. 高阶函数:")
(define numbers2 (list 1 2 3 4 5))
(println "  原始列表: " numbers2)
(println "  map square: " (map square numbers2))
(println "  filter even: " (filter (fn (x) (= (mod x 2) 0)) numbers2))
(println "  reduce sum: " (reduce + 0 numbers2))
(newline)

;; 6. 模式匹配
(println "6. 模式匹配:")
(define (describe-list lst)
  (match lst
    [] "empty"
    [x] "one"
    [x y] "two"
    _ "many"))
(println "  [] => " (describe-list []))
(println "  [1] => " (describe-list [1]))
(println "  [1 2] => " (describe-list [1 2]))
(println "  [1 2 3] => " (describe-list [1 2 3]))
(newline)

;; 7. 现代字面量语法
(println "7. 现代字面量语法:")
(println "  列表字面量: " [1 2 3 4 5])
(println "  嵌套列表: " [[1 2] [3 4]])
(println "  混合类型: " [1 "hello" 3.14])
(newline)

;; 8. 字符串插值
(println "8. 字符串插值:")
(define user "Alice")
(define count 42)
(println #"  Hello #{user}!")
(println #"  Count: #{count}")
(newline)

;; 9. 管道操作符
(println "9. 管道操作符:")
(println "  简单管道: " (-> 5 (+ 3) (* 2)))
(newline)

(println "=== 快速体验完成！===")
