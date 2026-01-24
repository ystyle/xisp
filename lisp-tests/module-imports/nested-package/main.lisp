(println "=== 测试嵌套目录包导入 ===")

;; 导入嵌套目录 - 前缀应该是最后一级
(import "./math.stats")

;; 调用函数 - 带 stats. 前缀（不是 math.stats.）
(println "调用 stats.average:")
(println "  " (stats.average [1 2 3 4 5]))

(println "")
(println "说明: 前缀是 'stats.' 而不是 'math.stats.'")
(println "      因为嵌套导入只使用最后一级作为前缀")

(println "")
(println "=== 测试完成 ===")
