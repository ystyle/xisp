;; ======================================
;; 守卫条件 (Guard Clauses) 示例
;; ======================================
;; 守卫条件允许在模式匹配时添加额外的条件判断
;; 使用 when 关键字

;; ==================== 1. 基本守卫条件 ====================
;; 只匹配大于 10 的数字
(match 15
  (x when (> x 10) (str "large: " x))
  (x when (< x 10) (str "small: " x))
  _ "medium")
;; => "large: 15.000000"

(match 5
  (x when (> x 10) (str "large: " x))
  (x when (< x 10) (str "small: " x))
  _ "medium")
;; => "small: 5.000000"

;; ==================== 2. 多个守卫条件 ====================
;; 链式条件判断
(match 10
  (x when (> x 20) "very large")
  (x when (> x 10) "large")
  (x when (= x 10) "exactly 10")
  _ "small")
;; => "exactly 10"

;; ==================== 3. 复杂守卫条件 ====================
;; 组合多个条件
(match (quote (1 2 3))
  (x y z when (and (> x 0) (> y 0) (> z 0))
    (str "all positive: " x ", " y ", " z)
  _ "not all positive")
;; => "all positive: 1.000000, 2.000000, 3.000000"

;; ==================== 4. 守卫条件与 rest 参数 ====================
;; 匹配列表并检查条件
(match (quote (1 2 3 4 5))
  (x & rest when (> (length rest) 3)
    (str "first: " x ", rest has more than 3 elements")
  _ "other")
;; => "first: 1.000000, rest has more than 3 elements"

;; ==================== 5. 在递归中使用守卫条件 ====================
;; 快速排序示例
(define (quicksort lst)
  (match lst
    () ()
    (pivot & rest when (not (null? rest))
      (let [(less-or-equal greater)
            (partition pivot rest)]
        (append (quicksort (cons pivot less-or-equal))
                (quicksort greater)))
    _ lst))

;; 辅助函数
(define (partition pivot lst)
  (filter (lambda (x) (<= x pivot)) lst))

;; ==================== 6. 守卫条件与嵌套模式 ====================
;; 匹配嵌套结构并验证条件
(match (quote ((1 2) (3 4) (5 6)))
  ((a b) (c d) (e f) when (and (= a 1) (= e 5))
    (str "matched: a=" a ", e=" e)
  _ "no match")
;; => "matched: a=1.000000, e=5.000000"

;; ==================== 7. 类型检查守卫 ====================
;; 根据类型进行分支
(match 42
  (x when (number? x) (str "number: " x))
  (x when (string? x) (str "string: " x))
  (x when (list? x) (str "list"))
  _ "other type")
;; => "number: 42.000000"

;; ==================== 8. 复杂业务逻辑示例 ====================
;; 定义一个处理订单的函数
(define (process-order order)
  (match order
    (id items status when (and (symbol? status) (= status :pending))
      (str "Processing order " id " with " (length items) " items")
    (id items status when (and (symbol? status) (= status :shipped))
      (str "Order " id " has been shipped")
    _ "Invalid order status"))

;; 使用示例
(process-order (quote (ORD001 (item1 item2) :pending)))
;; => "Processing order ORD001 with 2.000000 items"
