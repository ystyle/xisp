(println "=== 测试文件导入（无前缀）===")

;; 导入同级文件 - 应该无前缀
(import "./utils.lisp")

;; 调用函数 - 无前缀
(println "调用 processData:")
(println "  " (processData "测试数据"))

;; 验证未导出符号不可访问
(println "")
(println "测试未导出符号:")
(try
  (println "调用未导出的符号 internalHelper:")
  (println "  " (internalHelper "test"))
  (println "ERROR: 应该报错")
) catch (e)
  (println "✓ 正确：未导出符号不可访问")
  (println "  错误: " e)
)

(println "")
(println "=== 测试完成 ===")
