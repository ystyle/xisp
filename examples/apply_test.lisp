; 测试 apply 特殊形式

; 测试 1: (apply + '(1 2 3))
; 将列表展开为参数，调用 (+ 1 2 3)
; 应该返回 6
(define test1 (apply + '(1 2 3)))
(println "测试 1 - (apply + '(1 2 3)): ")
(println test1)

; 测试 2: (apply list 1 2 '(3 4))
; 前面参数 1 2 保持不变，最后列表 '(3 4) 被展开
; 等价于 (list 1 2 3 4)
; 应该返回 (1 2 3 4)
(define test2 (apply list 1 2 '(3 4)))
(println "测试 2 - (apply list 1 2 '(3 4)): ")
(println test2)

; 测试 3: (apply * '(2 3 4))
; 展开为 (* 2 3 4)
; 应该返回 24
(define test3 (apply * '(2 3 4)))
(println "测试 3 - (apply * '(2 3 4)): ")
(println test3)

; 测试 4: 使用 apply 调用 lambda
(define add-three (lambda (a b c) (+ a (+ b c))))
(define test4 (apply add-three '(10 20 30)))
(println "测试 4 - (apply add-three '(10 20 30)): ")
(println test4)

; 测试 5: apply 与 max/min 结合使用
; max/min 接受一个列表作为参数，所以直接 apply 不展开
(define numbers '(1 2 3 4 5))
(define test5a (apply max numbers))
(println "测试 5a - (apply max '(1 2 3 4 5)): ")
(println test5a)

(define test5b (apply min numbers))
(println "测试 5b - (apply min '(1 2 3 4 5)): ")
(println test5b)

; 测试 6: apply 与 append 结合使用
; append 接受多个列表参数，apply 展开列表的列表
(define test6 (apply append '((1 2) (3 4) (5 6))))
(println "测试 6 - (apply append '((1 2) (3 4) (5 6))): ")
(println test6)

(println "所有 apply 测试完成！")
