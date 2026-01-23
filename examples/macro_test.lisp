;; ======================================
;; Xisp 宏系统基础测试
;; ======================================

(println "=== Xisp 宏系统基础测试 ===")
(println "")

;; ======================================
;; 1. 定义 when 宏
;; ======================================

(println "1. 定义 when 宏")
(println "(defmacro when (test then) `(if ,test ,then nil))")

(defmacro when (test then)
  `(if ,test ,then nil))

;; 测试 when 宏
(println "测试: (when (> 5 3) (println \"5 > 3\") (println \"条件成立\"))")
(when (> 5 3)
  (println "5 > 3")
  (println "条件成立"))
(println "")

;; ======================================
;; 2. 定义 unless 宏
;; ======================================

(println "2. 定义 unless 宏")
(println "(defmacro unless (test then) `(if (not ,test) ,then nil))")

(defmacro unless (test then)
  `(if (not ,test) ,then nil))

;; 测试 unless 宏
(println "测试: (unless (< 5 3) (println \"5 不小于 3\"))")
(unless (< 5 3)
  (println "5 不小于 3"))
(println "")

;; ======================================
;; 3. 测试宏展开
;; ======================================

(println "3. 测试 macroexpand")
(println "代码: (macroexpand '(when (> x 10) (println \"large\")))")
(define expanded (macroexpand '(when (> x 10) (println "large")))
(println "展开结果:")
(println expanded)
(println "")

;; ======================================
;; 4. 定义 incf 宏
;; ======================================

(println "4. 定义 incf 宏（自增）")
(println "(defmacro incf (var) `(setq ,var (+ ,var 1)))")

(defmacro incf (var)
  `(setq ,var (+ ,var 1)))

;; 测试 incf 宏
(println "测试: (define counter 0) (incf counter)")
(define counter 0)
(println "初始 counter = 0")
(incf counter)
(println #"incf counter => #{counter}")
(println "")

;; ======================================
;; 5. 定义 incf-by 宏（带增量参数）
;; ======================================

(println "5. 定义 incf-by 宏（指定增量）")
(println "(defmacro incf-by (var delta) `(setq ,var (+ ,var ,delta)))")

(defmacro incf-by (var delta)
  `(setq ,var (+ ,var ,delta)))

;; 测试 incf-by 宏
(println "测试: (incf-by counter 5)")
(incf-by counter 5)
(println #"incf-by counter 5 => #{counter}")
(println "")

;; ======================================
;; 6. 定义 negate 宏（数值取反）
;; ======================================

(println "6. 定义 negate 宏")
(println "(defmacro negate (x) `(* ,x -1))")

(defmacro negate (x)
  `(* ,x -1))

;; 测试 negate 宏
(println "测试: (negate 5)")
(define r1 (negate 5))
(println #"结果: #{r1}")
(println "期望: -5.000000")
(println "")

;; ======================================
;; 7. 定义 swap 宏（交换变量）
;; ======================================

(println "7. 定义 swap 宏")
(println "(defmacro swap (a b)")
(println "  `(let ((temp ,a))")
(println "     (setq ,a ,b)")
(println "     (setq ,b temp)))")

(defmacro swap (a b)
  `(let ((temp ,a))
     (setq ,a ,b)
     (setq ,b temp)))

;; 测试 swap 宏
(println "测试: (define x 1) (define y 2) (swap x y)")
(define x 1)
(define y 2)
(println "初始: x = " x ", y = " y)
(swap x y)
(println "交换后: x = " x ", y = " y)
(println "期望: x = 2, y = 1")
(println "")

;; ======================================
;; 8. 定义 push 宏（列表头部插入）
;; ======================================

(println "8. 定义 push 宏")
(println "(defmacro push (elem lst) `(cons ,elem ,lst))")

(defmacro push (elem lst)
  `(cons ,elem ,lst))

;; 测试 push 宏
(println "测试: (define lst '(2 3)) (push 1 lst)")
(define lst '(2 3))
(println "初始 lst = " lst)
(push 1 lst)
(println "push 后 lst = " lst)
(println "期望: (1 2 3)")
(println "")

;; ======================================
;; 9. 嵌套宏调用测试
;; ======================================

(println "9. 嵌套宏调用测试")
(println "(defmacro add-one (x) `(+ ,x 1))")
(println "(defmacro add-two (x) `(+ (add-one ,x) 1))")

(defmacro add-one (x)
  `(+ ,x 1))

(defmacro add-two (x)
  `(+ (add-one ,x) 1))

(println "测试: (add-two 10)")
(define r2 (add-two 10))
(println #"结果: #{r2}")
(println "期望: 12.000000")
(println "")

(println "=== 宏系统基础测试完成 ===")
