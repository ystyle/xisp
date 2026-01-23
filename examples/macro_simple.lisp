;; ======================================
;; Xisp 宏系统演示（使用数字代替布尔值）
;; ======================================

(println "=== Xisp 宏系统演示 ===")
(println "")
(println "注意：反引号 (backquote) 和逗号 (comma) 语法还未实现")
(println "当前宏需要使用 list/quote 构造表达式")
(println "布尔值使用 1 (真) 和 0 (假) 代替")
(println "")

;; ======================================
;; 1. 定义简单的 when 宏
;; ======================================

(println "1. 定义 when 宏")
(println "代码: (defmacro when-simple (test then)")
(println "        (list (quote if) test then (quote 0)))")

(defmacro when-simple (test then)
  (list (quote if) test then (quote 0)))

;; 测试 when-simple 宏
(println "测试 1: (when-simple 1 100)")
(define r1 (when-simple 1 100))
(println #"结果: #{r1}")
(println "期望: 100.000000")
(println "")

(println "测试 2: (when-simple 0 100)")
(define r2 (when-simple 0 100))
(println #"结果: #{r2}")
(println "期望: 0.000000 (nil)")
(println "")

;; ======================================
;; 2. 定义 incf 宏
;; ======================================

(println "2. 定义 incf 宏")
(println "代码: (defmacro incf (var)")
(println "        (list (quote setq) var (list (quote +) var 1)))")

(defmacro incf (var)
  (list (quote setq) var (list (quote +) var 1)))

;; 测试 incf 宏
(println "测试: (define counter 0) (incf counter) (incf counter)")
(define counter 0)
(println "初始 counter = 0")
(incf counter)
(println #"incf counter => #{counter}")
(incf counter)
(println #"incf counter => #{counter}")
(println "期望: 1.000000, 2.000000")
(println "")

;; ======================================
;; 3. 定义 negate 宏（取反）
;; ======================================

(println "3. 定义 negate 宏")
(println "代码: (defmacro negate (x)")
(println "        (list (quote *) x (quote -1)))")

(defmacro negate (x)
  (list (quote *) x (quote -1)))

;; 测试 negate 宏
(println "测试: (negate 5)")
(define r3 (negate 5))
(println #"结果: #{r3}")
(println "期望: -5.000000")
(println "")

;; ======================================
;; 4. 定义 swap 宏（交换两个变量）
;; ======================================

(println "4. 定义 swap 宏")
(println "代码: (defmacro swap (a b)")
(println "        (list (quote let)")
(println "              (list (list (quote temp) a))")
(println "              (list (quote setq) a b)")
(println "              (list (quote setq) b (quote temp))))")

(defmacro swap (a b)
  (list (quote let)
        (list (list (quote temp) a))
        (list (quote setq) a b)
        (list (quote setq) b (quote temp))))

;; 测试 swap 宏
(println "测试: (define x 1) (define y 2) (swap x y)")
(define x 1)
(define y 2)
(println "初始: x = " x ", y = " y)
(swap x y)
(println "交换后: x = " x ", y = " y)
(println "期望: x = 2, y = 1")
(println "")

(println "=== 宏系统演示完成 ===")
