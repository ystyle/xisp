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

; 测试 5: apply 与 cons 结合使用
(define my-list '(2 3 4))
(define test5 (apply cons 1 my-list))
(println "测试 5 - (apply cons 1 '(2 3 4)): ")
(println test5)

(println "所有 apply 测试完成！")
