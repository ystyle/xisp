; 解构绑定测试 - 展示现代向量语法和传统点对语法

; ==================== 现代向量语法（推荐） ====================

; 测试 1：简单向量解构 [x y]
(println "=== 测试 1：现代向量解构 [x y] ===")
(define result1 (let [[x y] (quote (1 2 3 4))] (list x y)))
(println "(let [[x y] '(1 2 3 4)] (list x y))")
(println "结果: (1 2)")
(println result1)
(println)

; 测试 2：使用 & 收集剩余元素
(println "=== 测试 2：使用 & 收集剩余元素 ===")
(define result2 (let [[x y & rest] (quote (1 2 3 4 5))] (list x y rest)))
(println "(let [[x y & rest] '(1 2 3 4 5)] (list x y rest))")
(println "结果: (1 2 (3 4 5))")
(println result2)
(println)

; 测试 3：嵌套向量解构
(println "=== 测试 3：嵌套向量解构 [[a b] c] ===")
(define result3 (let [[[a b] c] (quote ((1 2) 3))] (list a b c)))
(println "(let [[[a b] c] '((1 2) 3)] (list a b c))")
(println "结果: (1 2 3)")
(println result3)
(println)

; 测试 4：复杂的嵌套解构
(println "=== 测试 4：复杂嵌套解构 ===")
(define result4 (let [[[a b] [c d & rest]] (quote ((1 2) (3 4 5 6)))] (list a b c d rest)))
(println "(let [[[a b] [c d & rest]] '((1 2) (3 4 5 6))] (list a b c d rest))")
(println "结果: (1 2 3 4 (5 6))")
(println result4)
(println)

; ==================== 传统点对语法（向后兼容） ====================

; 测试 5：传统点对解构 (x . y)
(println "=== 测试 5：传统点对解构 (x . y) ===")
(define result5 (let ((x . y) (quote (1 2 3))) x))
(println "(let ((x . y) '(1 2 3)) x)")
(println "结果: 1")
(println result5)
(println)

; 测试 6：传统嵌套点对解构
(println "=== 测试 6：传统嵌套点对解构 ===")
(define result6 (let (((x . y) . z) (quote ((1 2) 3 4))) (list x y z)))
(println "(let (((x . y) . z) '((1 2) 3 4)) (list x y z))")
(println "结果: (1 2 (3 4))")
(println result6)
(println)

; ==================== 对比示例 ====================

; 测试 7：普通绑定（两种语法都支持）
(println "=== 测试 7：普通绑定 ===")
(define result7 (let ((z 10)) z))
(println "(let ((z 10)) z)")
(println "结果: 10")
(println result7)
(println)

; 测试 8：现代语法的优势 - 更清晰
(println "=== 测试 8：现代语法对比传统语法 ===")
(println "现代: [first second & rest]  vs  传统: (first . second)")
(println "现代语法更直观，& 符号明确表示'收集剩余'")
(define result8-modern (let [[first second & rest] (quote (1 2 3 4 5))] (list first second rest)))
(define result8-traditional (let ((first . second) (quote (1 2 3 4 5))) (list first second)))
(println "现代语法结果: (1 2 (3 4 5))")
(println result8-modern)
(println "传统语法结果: (1 (2 3 4 5))")
(println result8-traditional)
(println)

(println "=== 所有测试完成 ===")
(println "✅ 推荐使用现代向量语法：[x y & rest]")
(println "✅ 传统点对语法继续有效：(x . y)")
(println "✅ 两种语法可以共存")
