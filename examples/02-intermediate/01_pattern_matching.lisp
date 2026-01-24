;; ======================================
;; 模式匹配 (Pattern Matching) 示例
;; ======================================

(println "=== Xisp 模式匹配演示 ===")
(println "")

(println "1. 常量匹配")
(println "代码: (match 5 1 \"one\" 2 \"two\" 5 \"five\" _ \"other\")")
(define r1 (match 5 1 "one" 2 "two" 5 "five" _ "other"))
(println #"结果: #{r1}")
(println "期望: \"five\"")
(println "")

(println "2. 变量绑定")
(println "代码: (match 42 x (str \"Value is: \" x))")
(define r2 (match 42 x (str "Value is: " x)))
(println #"结果: #{r2}")
(println "期望: \"Value is: 42\"")
(println "")

(println "3. 列表模式匹配")
(println "代码: (match [1 2 3] (x y z) (str \"x=\" x \", y=\" y \", z=\" z) _ \"not a list of 3\")")
(define r3 (match [1 2 3] (x y z) (str "x=" x ", y=" y ", z=" z) _ "not a list of 3"))
(println #"结果: #{r3}")
(println "期望: 解构绑定 x=1, y=2, z=3")
(println "")

(println "4. 通配符")
(println "代码: (match \"hello\" _ \"matched anything\")")
(define r4 (match "hello" _ "matched anything"))
(println #"结果: #{r4}")
(println "期望: \"matched anything\"")
(println "")

(println "5. 守卫条件")
(println "代码: (match 15 (x when (> x 10) (str \"large: \" x)) (x when (< x 10) (str \"small: \" x)) _ \"medium\")")
(define r5 (match 15 (x when (> x 10) (str "large: " x)) (x when (< x 10) (str "small: " x)) _ "medium"))
(println #"结果: #{r5}")
(println "期望: \"large: 15\"")
(println "")

(println "6. 嵌套列表匹配")
(println "代码: (match [[1 2] [3 4]]) ((a b) (c d)) (str \"a=\" a \", b=\" b \", c=\" c \", d=\" d) _ \"no match\")")
(define r6 (match [[1 2] [3 4]] ((a b) (c d)) (str "a=" a ", b=" b ", c=" c ", d=" d) _ "no match"))
(println #"结果: #{r6}")
(println "期望: 解构嵌套列表")
(println "")

(println "7. Rest 参数")
(println "代码: (match [1 2 3 4 5] (x & rest) (str \"first: \" x \", rest: \" rest) _ \"no match\")")
(define r7 (match [1 2 3 4 5] (x & rest) (str "first: " x ", rest: " rest) _ "no match"))
(println #"结果: #{r7}")
(println "期望: \"first: 1, rest: (2 3 4 5)\"")
(println "")

(println "8. 复杂模式组合")
(println "代码: (define (describe-data data) (match data 0 \"zero\" 1 \"one\" (x y) (str \"Pair: \" x \", \" y) (x y z) (str \"Triple: \" x \", \" y \", \" z) _ \"unknown\"))")
(define (describe-data data)
  (match data
    0 "zero"
    1 "one"
    (x y) (str "Pair: " x ", " y)
    (x y z) (str "Triple: " x ", " y ", " z)
    _ "unknown"))

(println "测试 (describe-data [1 2]):")
(define r8 (describe-data [1 2]))
(println #"结果: #{r8}")
(println "期望: \"Pair: 1, 2\"")

