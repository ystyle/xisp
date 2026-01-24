(println "=== 测试目录包导入（有前缀）===")

;; 导入同级目录 - 应该有前缀
(import "./helpers")

;; 调用函数 - 带 helpers. 前缀
(println "调用 helpers.validateEmail:")
(println "  " (helpers.validateEmail "test@example.com"))

;; 调用另一个函数
(println "调用 helpers.formatName:")
(println "  " (helpers.formatName "张三"))

;; 访问常量 - 带 helpers. 前缀
(println "")
(println "访问 helpers.MAX_SIZE:")
(println "  " helpers.MAX_SIZE)

(println "访问 helpers.MIN_VALUE:")
(println "  " helpers.MIN_VALUE)

(println "")
(println "=== 测试完成 ===")
