; Xisp apply 特殊形式演示
;
; apply 是一个强大的工具，它允许你动态地应用函数到参数列表
;
; 语法：(apply func arg1 arg2 ... args-list)
;
; 最后一个参数必须是一个列表，它会被展开作为函数的参数

; 基础示例：应用加法到列表
(println "=== 基础示例 ===")
(println "(apply + '(1 2 3)) =")
(println (apply + '(1 2 3)))

; 应用乘法到列表
(println "\n(apply * '(2 3 4)) =")
(println (apply * '(2 3 4)))

; 混合使用：前面的参数 + 列表
(println "\n=== 混合参数示例 ===")
(println "(apply list 1 2 '(3 4)) =")
(println (apply list 1 2 '(3 4)))

; 与 lambda 结合使用
(println "\n=== Lambda + apply ===")
(define add-three (lambda (a b c) (+ a (+ b c))))
(println "(define add-three (lambda (a b c) (+ a (+ b c))))")
(println "(apply add-three '(10 20 30)) =")
(println (apply add-three '(10 20 30)))

; 高阶函数示例
(println "\n=== 高阶函数示例 ===")
(define numbers '(1 2 3 4 5))
(println "(define numbers '(1 2 3 4 5))")

; 使用 apply 调用 max
(println "(apply max numbers) =")
(println (apply max numbers))

; 使用 apply 调用 min
(println "(apply min numbers) =")
(println (apply min numbers))

; 实用示例：求和函数
(println "\n=== 实用示例 ===")
(define sum (lambda (lst) (apply + lst)))
(println "(define sum (lambda (lst) (apply + lst)))")
(println "(sum '(1 2 3 4 5)) =")
(println (sum '(1 2 3 4 5)))

; 实用示例：求积函数
(define product (lambda (lst) (apply * lst)))
(println "(define product (lambda (lst) (apply * lst)))")
(println "(product '(1 2 3 4)) =")
(println (product '(1 2 3 4)))

; 实用示例：构建列表
(println "\n(apply append '((1 2) (3 4) (5 6))) =")
(println (apply append '((1 2) (3 4) (5 6))))

(println "\n=== apply 演示完成 ===")
