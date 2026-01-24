(println "=== 测试文件不存在错误 ===")

(try
  (import "./notexist.lisp")
  (println "ERROR: 应该报错")
) catch (e)
  (println "✓ 正确：文件不存在时返回错误")
  (println "  错误: " e)
)

(println "")
(println "=== 测试完成 ===")
