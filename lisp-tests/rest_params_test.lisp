;; 可变参数测试
;; 测试 . args 和 &rest args 语法

(println "=== 测试可变参数功能 ===")

;; 测试1: Lambda 可变参数（Common Lisp 风格）
(println "\n--- 测试 Lambda 可变参数 (. rest) ---")
(define test-lambda
  (lambda (x y . rest)
    (list 'x=x x 'y=y 'rest=rest rest)))

(define result1 (test-lambda 1 2 3 4 5))
(println "result1 =" result1)
;; 期望: (x=x 1 y=y 2 rest=rest (3 4 5))

;; 测试2: Lambda 纯可变参数（无固定参数）
(println "\n--- 测试 Lambda 纯可变参数 (. all) ---")
(define test-lambda3
  (lambda (. all)
    all))

(define result3 (test-lambda3 'a 'b 'c 'd))
(println "result3 =" result3)
;; 期望: (a b c d)

;; 测试3: ,@ 拼接
(println "\n=== 测试 ,@ (comma-at) 展开 ===")

(println "\n--- 测试 ,@ 拼接 ---")
(define lst1 '(a b c))
(define lst2 '(1 2 3))

(define result7 `(x y z ,@lst1))
(println "result7 (x y z ,@lst1) =" result7)
;; 期望: (x y z a b c)

(define result8 `(1 2 ,@lst1 4 5))
(println "result8 (1 2 ,@lst1 4 5) =" result8)
;; 期望: (1 2 a b c 4 5)

(define result9 `(,@lst1 ,@lst2))
(println "result9 (,@lst1 ,@lst2) =" result9)
;; 期望: (a b c 1 2 3)

;; 测试4: 列表以 ,@ 开头
(println "\n--- 测试列表以 ,@ 开头 ---")
(define result10 `(,@lst2))
(println "result10 (,@lst2) =" result10)
;; 期望: (1 2 3)

(define result11 `(,@lst2 ,@lst1))
(println "result11 (,@lst2 ,@lst1) =" result11)
;; 期望: (1 2 3 a b c)

;; 测试5: 检测空可变参数
(println "\n--- 测试空可变参数 ---")
(define test-empty
  (lambda (x . rest)
    (list 'x x 'rest rest)))

(define result5 (test-empty 1))
(println "result5 (test-empty 1) =" result5)
;; 期望: (x 1 rest nil)

(define result6 (test-empty 1 2 3))
(println "result6 (test-empty 1 2 3) =" result6)
;; 期望: (x 1 rest (2 3))

(println "\n=== 所有测试完成 ===")
