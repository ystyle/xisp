; 测试 apply 特殊形式

; 测试 1: (apply + '(1 2 3))
; 应该返回 6
(define test1 (apply + '(1 2 3)))
(println "测试 1 - (apply + '(1 2 3)): ")
(println test1)

; 测试 2: (apply list 1 2 '(3 4))
; 应该返回 (1 2 3 4)
(define test2 (apply list 1 2 '(3 4)))
(println "测试 2 - (apply list 1 2 '(3 4)): ")
(println test2)

; 测试 3: (apply * '(2 3 4))
; 应该返回 24
(define test3 (apply * '(2 3 4)))
(println "测试 3 - (apply * '(2 3 4)): ")
(println test3)

; 测试 4: 使用 apply 调用 lambda
(define add-three (lambda (a b c) (+ a (+ b c))))
(define test4 (apply add-three '(10 20 30)))
(println "测试 4 - (apply add-three '(10 20 30)): ")
(println test4)

; 测试 5: cons 直接使用（不使用 apply）
; cons 只接受两个参数，所以不应该用 apply
(define my-list '(2 3 4))
(define test5 (cons 1 my-list))
(println "测试 5 - (cons 1 '(2 3 4)): ")
(println test5)

; 测试 6: apply 与 append 结合使用
; append 接受多个列表作为参数
(define test6 (apply append '((1 2) (3 4) (5 6))))
(println "测试 6 - (apply append '((1 2) (3 4) (5 6))): ")
(println test6)

(println "所有 apply 测试完成！")
