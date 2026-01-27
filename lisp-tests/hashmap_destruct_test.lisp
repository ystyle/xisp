; HashMap 解构测试

; 测试 1：基本 HashMap 解构
(println "=== 测试 1：基本 HashMap 解构 ===")
(define config {:host "localhost" :port 8080})
(let [{:host h :port p} config]
  (println "host:")
  (println h)
  (println "port:")
  (println p))
(println)

; 测试 2：嵌套 HashMap 解构（如果 HashMap 的值也是 HashMap）
(println "=== 测试 2：部分键解构 ===")
(define data {:name "Alice" :age 30 :city "Beijing"})
(let [{:name n :age a} data]
  (println "name:")
  (println n)
  (println "age:")
  (println a))
(println)

; 测试 3：嵌套 HashMap 解构（先解构向量，再解构 HashMap）
(println "=== 测试 3：嵌套解构 ===")
(define users [{:name "Bob" :email "bob@example.com"}
              {:name "Carol" :email "carol@example.com"}])
(let [[user1 user2] users]
  (let [{:name n1 :email e1} user1]
    (let [{:name n2 :email e2} user2]
      (println "n1:")
      (println n1)
      (println "e1:")
      (println e1)
      (println "n2:")
      (println n2)
      (println "e2:")
      (println e2))))
(println)
