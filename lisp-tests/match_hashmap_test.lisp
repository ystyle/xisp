; match HashMap 模式匹配测试

(println "=== 测试 1：基本 HashMap 匹配 ===")
(define test1 (match {:name "Alice" :age 30}
  {:name n :age a} (list "matched" n a)
  _ "not matched"))
(println test1)

(println "=== 测试 2：部分键匹配 ===")
(define test2 (match {:name "Bob" :age 25 :city "Beijing"}
  {:name n} (list "name-only" n)
  _ "not matched"))
(println test2)

(println "=== 测试 3：不匹配的情况 ===")
(define test3 (match {:name "Charlie"}
  {:name n :age a} "should not match"
  {:name n} (list "matched-name-only" n)))
(println test3)

(println "=== 测试 4：多个分支 ===")
(define test4 (match {:type "user" :name "Dave"}
  {:type "admin"} "admin"
  {:type "user" :name n} (list "user" n)
  _ "unknown"))
(println test4)

(println "=== 测试 5：空 HashMap ===")
(define test5 (match {}
  {} "empty-map"
  _ "not empty"))
(println test5)

(println "=== 测试 6：通配符 _ ===")
(define test6 (match {:x 1 :y 2}
  _ "wildcard matched"))
(println test6)

(println "=== 测试 7：嵌套匹配 ===")
(define test7 (match {:user {:name "Eve" :email "eve@example.com"}}
  {:user {:name n}} (list "user-name" n)
  _ "not matched"))
(println test7)

(println "=== 测试 8：值类型匹配 ===")
(define test8 (match {:count 42 :active true}
  {:count c} (list "count" c)
  _ "not matched"))
(println test8)
