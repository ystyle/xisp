(println "=== 测试 Export 功能 ===")

(import "./funcs.lisp")

;; 测试导出的函数
(println "调用导出的函数 publicFunc:")
(println "  " (publicFunc))

;; 测试未导出的函数
(println "")
(println "测试未导出的函数 privateFunc:")
(try
  (privateFunc)
  (println "ERROR: 应该报错")
) catch (e)
  (println "✓ 正确：未导出函数不可访问")
  (println "  错误: " e)
)

(println "")
(println "=== 测试完成 ===")
