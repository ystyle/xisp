(println "=== 测试目录不存在错误 ===")

(try
  (import "./notexist-dir")
  (println "ERROR: 应该报错")
) catch (e)
  (println "✓ 正确：目录不存在时返回错误")
  (println "  错误: " e)
)

(println "")
(println "=== 测试完成 ===")
