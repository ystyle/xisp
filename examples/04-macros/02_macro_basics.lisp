;; ======================================
;; Xisp 宏系统演示
;; ======================================

(println "=== Xisp 宏系统演示 ===")
(println "")

;; ======================================
;; 1. 基础宏定义
;; ======================================

(println "1. 定义简单的宏: when")
(println "代码: (defmacro when (test then) `(if ,test ,then nil))")

;; when 宏：条件为真时执行表达式
(defmacro when (test then)
  `(if ,test ,then nil))

;; 测试 when 宏
(println "测试: (when (> 5 3) (println \"5 > 3\"))")
(when (> 5 3)
  (println "5 > 3"))
(println "")

;; unless 宏：条件为假时执行表达式
(println "2. 定义 unless 宏")
(println "代码: (defmacro unless (test then) `(if (not ,test) ,then nil))")

(defmacro unless (test then)
  `(if (not ,test) ,then nil))

;; 测试 unless 宏
(println "测试: (unless (< 5 3) (println \"5 不小于 3\"))")
(unless (< 5 3)
  (println "5 不小于 3"))
(println "")

;; ======================================
;; 2. 宏与反引号语法
;; ======================================

(println "3. 反引号语法演示")
(println "代码: (defmacro double (x) `(* ,x 2))")

(defmacro double (x)
  `(* ,x 2))

(println "测试: (double 5)")
(define r1 (double 5))
(println #"结果: #{r1}")
(println "期望: 10")
(println "")

;; ======================================
;; 3. 逗号-at 拼接列表
;; ======================================

(println "4. 逗号-at 列表拼接演示")
(println "代码: (defmacro wrap-list (lst) `(a ,@lst b))")

(defmacro wrap-list (lst)
  `(a ,@lst b))

(println "测试: (wrap-list '(x y z))")
(define r2 (wrap-list '(x y z)))
(println "结果: " r2)
(println "期望: (a x y z b)")
(println "")

;; ======================================
;; 4. 自定义宏示例
;; ======================================

(println "5. 自定义宏: incf（自增）")
(println "代码: (defmacro incf (var) `(setq ,var (+ ,var 1)))")

(defmacro incf (var)
  `(setq ,var (+ ,var 1)))

;; 测试 incf 宏
(println "测试: (define counter 0) (incf counter)")
(define counter 0)
(println "初始 counter = 0")
(incf counter)
(println #"incf counter => #{counter}")
(println "期望: 1")
(println "")

;; ======================================
;; 5. 带多个参数的宏
;; ======================================

(println "6. 多参数宏: incf-by（指定增量）")
(println "代码: (defmacro incf-by (var delta) `(setq ,var (+ ,var ,delta)))")

(defmacro incf-by (var delta)
  `(setq ,var (+ ,var ,delta)))

;; 测试 incf-by 宏
(println "测试: (incf-by counter 5)")
(incf-by counter 5)
(println #"incf-by counter 5 => #{counter}")
(println "期望: 6")
(println "")

;; ======================================
;; 6. 宏展开测试
;; ======================================

(println "7. 宏展开测试")
(println "代码: (macroexpand '(when (> x 10) (println \"large\")))")
(define expanded (macroexpand '(when (> x 10) (println "large")))
(println "展开结果:")
(println expanded)
(println "")

(println "代码: (macroexpand-all '(when (> x 10) (println \"large\")))")
(define expanded-all (macroexpand-all '(when (> x 10) (println "large")))
(println "完全展开结果:")
(println expanded-all)
(println "")

;; ======================================
;; 7. 复杂宏示例：三元运算符
;; ======================================

(println "8. 三元运算符宏")
(println "代码: (defmacro ternary (test a b) `(if ,test ,a ,b))")

(defmacro ternary (test a b)
  `(if ,test ,a ,b))

;; 测试 ternary 宏
(println "测试: (ternary (> 5 3) \"yes\" \"no\")")
(define r3 (ternary (> 5 3) "yes" "no"))
(println "结果: " r3)
(println "期望: \"yes\"")
(println "")

;; ======================================
;; 8. 实用宏示例
;; ======================================

(println "9. 实用宏: negate（数值取反）")
(println "代码: (defmacro negate (x) `(* ,x -1))")

(defmacro negate (x)
  `(* ,x -1))

;; 测试 negate 宏
(println "测试: (negate 42)")
(define r4 (negate 42))
(println #"结果: #{r4}")
(println "期望: -42.000000")
(println "")

;; ======================================
;; 9. 变量操作宏
;; ======================================

(println "10. 变量交换宏: swap")
(println "代码: (defmacro swap (a b)")
(println "        `(let ((temp ,a))")
(println "           (setq ,a ,b)")
(println "           (setq ,b temp)))")

(defmacro swap (a b)
  `(let ((temp ,a))
     (setq ,a ,b)
     (setq ,b temp)))

;; 测试 swap 宏
(println "测试: (define x 100) (define y 200) (swap x y)")
(define x 100)
(define y 200)
(println "初始: x = " x ", y = " y)
(swap x y)
(println "交换后: x = " x ", y = " y)
(println "期望: x = 200, y = 100")
(println "")

;; ======================================
;; 10. 列表操作宏
;; ======================================

(println "11. 列表操作宏: push")
(println "代码: (defmacro push (elem lst) `(cons ,elem ,lst))")

(defmacro push (elem lst)
  `(cons ,elem ,lst))

;; 测试 push 宏
(println "测试: (define mylist '(2 3 4)) (push 1 mylist)")
(define mylist '(2 3 4))
(println "初始 mylist = " mylist)
(push 1 mylist)
(println "push 后 mylist = " mylist)
(println "期望: (1 2 3 4)")
(println "")

(println "=== 宏系统演示完成 ===")
