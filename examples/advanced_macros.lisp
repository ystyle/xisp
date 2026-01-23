;; Xisp 高级宏特性演示
;; 演示 let*, if-let, when-let* 的使用

(println "========================================")
(println "Xisp 高级宏特性演示")
(println "========================================")
(println "")

;; ============================================================================
;; let* - 顺序绑定
;; ============================================================================

(println "【let* - 顺序绑定】")
(println "")

(println "示例 1: 基本用法 - 后面的绑定可以使用前面的变量")
(println "代码: (let* ((a 1) (b (+ a 10)) (c (* b 2))) c)")
(define result1 (let* ((a 1) (b (+ a 10)) (c (* b 2))) c))
(println "结果: " result1)
(println "说明: a=1, b=11, c=22")
(println "")

(println "示例 2: 没有绑定时直接执行 body")
(println "代码: (let* () (+ 1 2))")
(define result2 (let* () (+ 1 2)))
(println "结果: " result2)
(println "")

(println "示例 3: 单个绑定（退化为普通 let）")
(println "代码: (let* ((x 5)) (* x x))")
(define result3 (let* ((x 5)) (* x x)))
(println "结果: " result3)
(println "")

(println "示例 4: 多个表达式（使用 begin）")
(println "代码: (let* ((a 1) (b 2)) (begin (println a) (println b) (+ a b)))")
(println "执行:")
(define result4 (let* ((a 1) (b 2)) (begin (println "  a = " a) (println "  b = " b) (+ a b))))
(println "结果: " result4)
(println "")

(println "示例 5: 词法作用域")
(println "代码: (define x 100) (let* ((x 1)) (+ x 10))")
(define x 100)
(println "外层 x = 100")
(define result5 (let* ((x 1)) (+ x 10)))
(println "let* 结果: " result5)
(println "外层 x = " x)
(println "说明: let* 内的 x 遮蔽了外层的 x")
(println "")

;; ============================================================================
;; if-let - 条件绑定
;; ============================================================================

(println "========================================")
(println "【if-let - 条件绑定】")
(println "")

(println "示例 1: 条件为真时执行 then 分支")
(println "代码: (if-let (x 5) x nil)")
(define result6 (if-let (x 5) x nil))
(println "结果: " result6)
(println "说明: x 绑定为 5（真），返回 x")
(println "")

(println "示例 2: 条件为假时执行 else 分支")
(println "代码: (if-let (x 0) x 100)")
(define result7 (if-let (x 0) x 100))
(println "结果: " result7)
(println "说明: x 绑定为 0（假），返回 else 分支 100")
(println "")

(println "示例 3: 使用表达式")
(println "代码: (if-let (x (+ 2 3)) (* x x) nil)")
(define result8 (if-let (x (+ 2 3)) (* x x) nil))
(println "结果: " result8)
(println "说明: x 绑定为 5，返回 25")
(println "")

(println "示例 4: 复杂的条件判断")
(println "代码: (if-let (value (get-value)) (process value) (handle-error))")
(println "模拟:")
(define get-value (lambda () 42))
(define process (lambda (x) (* x 2)))
(define handle-error (lambda () "Error: no value"))
(define result9 (if-let (value (get-value)) (process value) (handle-error)))
(println "结果: " result9)
(println "说明: 模拟从一个可能返回假值的函数获取数据")
(println "")

;; ============================================================================
;; when-let* - 条件+顺序绑定
;; ============================================================================

(println "========================================")
(println "【when-let* - 条件+顺序绑定】")
(println "")

(println "示例 1: 最后一个值为真时执行")
(println "代码: (when-let* ((x 5) (y (* x 2))) (+ x y))")
(define result10 (when-let* ((x 5) (y (* x 2))) (+ x y)))
(println "结果: " result10)
(println "说明: x=5, y=10, y 为真，执行 (+ x y) = 15")
(println "")

(println "示例 2: 最后一个值为假时返回 nil")
(println "代码: (when-let* ((x 5) (y 0)) (+ x y))")
(define result11 (when-let* ((x 5) (y 0)) (+ x y)))
(println "结果: " result11)
(println "说明: y=0 为假，返回 nil")
(println "")

(println "示例 3: 多个绑定的链式处理")
(println "代码:")
(println "  (when-let* ((a 10)")
(println "              (b (* a 2))")
(println "              (c (+ b 5)))")
(println "    c)")
(define result12 (when-let* ((a 10) (b (* a 2)) (c (+ b 5))) c))
(println "结果: " result12)
(println "说明: a=10, b=20, c=25")
(println "")

(println "示例 4: 没有绑定时返回 nil")
(println "代码: (when-let* () 42)")
(define result13 (when-let* () 42))
(println "结果: " result13)
(println "说明: 没有绑定，不执行")
(println "")

(println "示例 5: 实际应用 - 链式数据处理")
(println "模拟数据处理流水线:")
(println "  (when-let* ((data (fetch-data))")
(println "              (parsed (parse data))")
(println "              (validated (validate parsed))")
(println "              (result (process validated)))")
(println "    (handle result))")
(println "")

; 模拟函数
(define fetch-data (lambda () "raw data"))
(define parse (lambda (s) (str "parsed: " s)))
(define validate (lambda (s) (if (s.contains "parsed") s nil)))
(define process (lambda (s) (str "processed: " s)))
(define handle (lambda (s) (println "最终结果: " s)))

(define result14 (when-let* ((data (fetch-data))
                            (parsed (parse data))
                            (validated (validate parsed))
                            (result (process validated)))
  (handle result)))
(println "说明: 每一步都依赖上一步，任何一步返回假值都会中断")
(println "")

;; ============================================================================
;; 综合示例
;; ============================================================================

(println "========================================")
(println "【综合示例】")
(println "")

(println "示例: 使用 if-let 处理可选值")
(println "代码:")
(println "  (define user-id (get-user-id-input))")
(println "  (if-let (user (lookup-user user-id))")
(println "    (display-user user)")
(println "    (show-error \"User not found\"))")
(println "")

; 模拟
(define get-user-id-input (lambda () 123))
(define lookup-user (lambda (id) (if (= id 123) "Alice" nil)))
(define display-user (lambda (name) (str "Welcome, " name)))
(define show-error (lambda (msg) (str "Error: " msg)))

(define user-id (get-user-id-input))
(define result15 (if-let (user (lookup-user user-id)) (display-user user) (show-error "User not found")))
(println "结果: " result15)
(println "")

(println "示例: 使用 when-let* 链式处理")
(println "代码:")
(println "  (when-let* ((input (read-input))")
(println "              (number (parse-number input))")
(println "              (result (calculate number)))")
(println "    (output result))")
(println "")

; 模拟
(define read-input (lambda () "42"))
(define parse-number (lambda (s) 42.0))
(define calculate (lambda (n) (* n 2)))
(define output (lambda (r) (println "计算结果: " r)))

(when-let* ((input (read-input))
            (number (parse-number input))
            (result (calculate number)))
  (output result))
(println "")

;; ============================================================================
;; 总结
;; ============================================================================

(println "========================================")
(println "【总结】")
(println "")
(println "let*      - 顺序绑定，后面的绑定可以使用前面的变量")
(println "if-let    - 条件绑定，类似三元运算符")
(println "when-let* - 条件+顺序绑定，适合链式处理")
(println "")
(println "演示完成！")
(println "========================================")
