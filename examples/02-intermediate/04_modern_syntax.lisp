;; ======================================
;; 现代化语法 (Modern Syntax) 示例
;; ======================================
;; Xisp 支持现代化的 Lisp 语法特性

;; ==================== 1. 向量字面量 [] ====================
(println "=== 1. 向量字面量 [] ===")
(println "创建向量 [1 2 3]:")
(define v1 [1 2 3])
(println v1)

(println "\n向量可以包含任意类型 [1 \"hello\" #t]:")
(define v2 [1 "hello" #t])
(println v2)

(println "\n嵌套向量 [[1 2] [3 4]]:")
(define v3 [[1 2] [3 4]])
(println v3)

;; ==================== 2. 哈希映射字面量 {} ====================
(println "\n=== 2. 哈希映射字面量 {} ===")
(println "创建哈希映射 {:name \"Alice\" :age 30 :city \"Beijing\"}:")
(define m1 {:name "Alice" :age 30 :city "Beijing"})
(println m1)

(println "\n嵌套哈希映射 {:user {:name \"Bob\" :id 123} :role :admin}:")
(define m2 {:user {:name "Bob" :id 123} :role :admin})
(println m2)

;; ==================== 2.1 Redis 风格哈希操作 ====================
(println "\n=== 2.1 Redis 风格哈希操作 ===")
(println "创建配置:")
(define config {:host "localhost" :port 8080 :debug #f})
(println "Config: " config)

(println "\nHGET - 获取值:")
(println "  host: " (hget config :host))
(println "  port: " (hget config :port))

(println "\nHSET - 设置值:")
(hset config :port 9090)
(println "  After HSET port: " (hget config :port))

(println "\nHEXISTS - 检查键:")
(println "  debug exists? " (hexists config :debug))
(println "  ssl exists? " (hexists config :ssl))

(println "\nHLEN - 获取大小:")
(println "  size: " (hlen config))

(println "\nHKEYS - 获取所有键:")
(println "  keys: " (hkeys config))

(println "\nHVALS - 获取所有值:")
(println "  values: " (hvals config))

(println "\nHDEL - 删除键:")
(hdel config :debug)
(println "  After HDEL debug, exists? " (hexists config :debug))

;; ==================== 3. 哈希集合字面量 #{} ====================
(println "\n=== 3. 哈希集合字面量 #{} ===")
(println "创建集合 #{1 2 3 4 5}:")
(define s1 #{1 2 3 4 5})
(println s1)

(println "\n符号集合 #{:red :green :blue}:")
(define s2 #{:red :green :blue})
(println s2)

;; ==================== 4. 字符串插值 #"" ====================
(println "\n=== 4. 字符串插值 #\"\" ===")
(define x 10)
(define y 20)
(println "简单插值 #\"Value is: #{x}\":")
(println #"Value is: #{x}")

(println "\n插入多个值 #\"x=#{x}, y=#{y}, sum=#{+ x y}\":")
(println #"x=#{x}, y=#{y}, sum=#{+ x y}")

(println "\n复杂表达式插值 #\"Result: #{* x x}\" (x 的平方):")
(println #"Result: #{* x x}")

;; ==================== 5. 解构绑定 (let) ====================
(println "\n=== 5. 解构绑定 (let) ===")
(println "解构 [x y z]:")
(define r1 (let [[x y z] [1 2 3]]
  (str "x=" x ", y=" y ", z=" z)))
(println r1)

(println "\n使用 & 收集剩余元素 [x y & rest]:")
(define r2 (let [[x y & rest] [1 2 3 4 5]]
  (str "x=" x ", y=" y ", rest=" rest)))
(println r2)

(println "\n嵌套解构 [[a b] [c d]]:")
(define r3 (let [[[a b] [c d]] [[1 2] [3 4]])]
  (str "a=" a ", b=" b ", c=" c ", d=" d)))
(println r3)

;; ==================== 6. 管道操作符 -> ====================
(println "\n=== 6. 管道操作符 -> ===")
(println "简单管道 (-> 10 (+ 5) (* 2)):")
(define p1 (-> 10 (+ 5) (* 2)))
(println p1)

(println "\n复杂管道操作 (map square, filter > 5, reduce +):")
(define p2 (-> [1 2 3 4 5]
  (map (lambda (x) (* x x)))
  (filter (lambda (x) (> x 5)))
  (reduce + 0)))
(println p2)
(println "期望: 50 (9 + 16 + 25)")

;; ==================== 7. 高阶函数 lambda 支持 ====================
(println "\n=== 7. 高阶函数 lambda 支持 ===")
(println "map lambda 平方:")
(define map1 (map (lambda (x) (* x x)) [1 2 3 4 5]))
(println map1)

(println "\nfilter lambda 过滤 > 3:")
(define filter1 (filter (lambda (x) (> x 3)) [1 2 3 4 5]))
(println filter1)

(println "\nreduce lambda 求和:")
(define reduce1 (reduce (lambda (acc x) (+ acc x)) 0 [1 2 3 4 5]))
(println reduce1)

;; ==================== 8. 组合使用 ====================
(println "\n=== 8. 组合使用 ===")
(define numbers [1 2 3 4 5])
(println "管道 + lambda + 字面量:")
(define combo (-> numbers
    (map (lambda (n) (* n n)))
    (filter (lambda (n) (> n 5)))
    (reduce + 0)))
(println combo)
(println "期望: 50 (9 + 16 + 25)")

(println "\n=== 所有测试完成 ===")
