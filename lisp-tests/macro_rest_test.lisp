;; 宏的纯可变参数测试
;; 测试 (. args) 语法在宏中的行为

(println "=== 测试宏的纯可变参数 ===")

;; 测试1: 宏的纯可变参数
(println "\n--- 测试1: 宏的纯可变参数 (. args) ---")
(defmacro print-all (. args)
  `(begin
     ,@(map (lambda (arg) `(println ,arg)) args)))

(println "调用 (print-all \"Hello\" \"World\" 42):")
(print-all "Hello" "World" 42)
;; 期望: 打印 Hello World 42
;; 实际: 什么都不打印（args 只绑定了 "Hello"）

;; 测试2: 宏的混合参数（固定参数 + 可变参数）
(println "\n--- 测试2: 宏的混合参数 (x y . rest) ---")
(defmacro print-first-and-rest (x y . rest)
  `(begin
     (println "x:" ,x)
     (println "y:" ,y)
     ,@(map (lambda (arg) `(println ,arg)) rest)))

(println "调用 (print-first-and-rest 1 2 \"three\" \"four\"):")
(print-first-and-rest 1 2 "three" "four")
;; 期望: 打印 x: 1, y: 2, three, four

;; 测试3: 检查宏的参数绑定
(println "\n--- 测试3: 检查宏的参数绑定 ---")
(defmacro inspect-args (. args)
  `(list 'args= args ,@(map (lambda (arg) `(list 'arg= ,arg)) args)))

(println "调用 (inspect-args \"A\" \"B\" \"C\"):")
(define result (inspect-args "A" "B" "C"))
(println "result =" result)
;; 期望: (args= (A B C) (arg= A) (arg= B) (arg= C))
;; 实际 bug: (args= A (arg= A)) - args 只绑定了 "A"

;; 测试4: 使用 macroexpand 检查展开
(println "\n--- 测试4: macroexpand 检查 ---")
(println "展开 (inspect-args 1 2 3):")
(macroexpand '(inspect-args 1 2 3))
;; 期望: 展开为包含所有参数的 list 表达式

(println "\n=== 所有测试完成 ===")
