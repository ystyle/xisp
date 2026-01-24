;; ======================================
;; 解构绑定 (Destructuring) 示例
;; ======================================

(println "=== Xisp 解构绑定演示 ===")
(println "")

(println "1. 基本列表解构")
(println "代码: (let [[x y z] [1 2 3]] (list x y z))")
(define r1 (let [[x y z] [1 2 3]] (list x y z)))
(println #"结果: #{r1}")
(println "期望: (1 2 3)")
(println "")

(println "2. 嵌套列表解构")
(println "代码: (let [((a b) (c d)) [[1 2] [3 4]]] (list a b c d))")
(define r2 (let [((a b) (c d)) [[1 2] [3 4]]]
  (list a b c d)))
(println #"结果: #{r2}")
(println "期望: (1 2 3 4)")
(println "")

(println "3. 混合解构")
(println "代码: (let [(x y) [1 2] (a b) [3 4]] (list x y a b))")
(define r3 (let [(x y) [1 2]
              (a b) [3 4]]
  (list x y a b)))
(println #"结果: #{r3}")
(println "期望: (1 2 3 4)")
(println "")

(println "4. Rest 参数解构")
(println "代码: (let [[x y & rest] [1 2 3 4 5]] (list x y rest))")
(define r4 (let [[x y & rest] [1 2 3 4 5]]
  (list x y rest)))
(println #"结果: #{r4}")
(println "期望: (1 2 (3 4 5))")
(println "")

(println "5. 向量解构")
(println "代码: (let [[x y z] [1 2 3]] (list x y z))")
(define r5 (let [[x y z] [1 2 3]]
  (list x y z)))
(println #"结果: #{r5}")
(println "期望: (1 2 3)")
(println "")

(println "6. 嵌套向量解构")
(println "代码: (let [[[a b] c] [[1 2] 3]] (list a b c))")
(define r6 (let [[[a b] c] [[1 2] 3]]
  (list a b c)))
(println #"结果: #{r6}")
(println "期望: (1 2 3)")
(println "")

(println "7. 复杂嵌套解构")
(println "代码: (let [[[a b] [c d & rest]] [[1 2] [3 4 5 6]]] (list a b c d rest))")
(define r7 (let [[[a b] [c d & rest]] [[1 2] [3 4 5 6]]]
  (list a b c d rest)))
(println #"结果: #{r7}")
(println "期望: (1 2 3 4 (5 6))")
(println "")

(println "=== 解构绑定演示完成 ===")
