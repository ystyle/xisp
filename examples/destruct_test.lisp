; 解构绑定测试

; 测试 1：简单解构
(println "=== 测试 1：简单解构 ===")
(define result1 (let ((x . y) '(1 2 3)) x))
(println "x should be 1:")
(println result1)

; 测试 2：绑定剩余部分
(println "\n=== 测试 2：绑定剩余部分 ===")
(define result2 (let ((x . y) '(1 2 3)) y))
(println "y should be (2 3):")
(println result2)

; 测试 3：普通绑定（对比）
(println "\n=== 测试 3：普通绑定 ===")
(define result3 (let ((z 10)) z))
(println "z should be 10:")
(println result3)

(println "\n=== 所有测试完成 ===")
