;; ======================================
;; Xisp 宏系统演示
;; ======================================

(println "=== Xisp 宏系统演示 ===")
(println "")

;; ======================================
;; 1. 基础宏定义
;; ======================================

(println "1. 定义简单的宏: when")
(println "代码: (defmacro when (test & body) `(if test (begin ,@body) nil))")

;; when 宏：条件为真时执行多个表达式
(defmacro when (test & body)
  `(if test
       (begin ,@body)
     nil))

;; 测试 when 宏
(println "测试: (when (> 5 3) (println \"5 > 3\") (println \"条件成立\"))")
(when (> 5 3)
  (println "5 > 3")
  (println "条件成立"))
(println "")

;; unless 宏：条件为假时执行多个表达式
(println "2. 定义 unless 宏")
(println "代码: (defmacro unless (test & body) `(if (not test) (begin ,@body) nil))")

(defmacro unless (test & body)
  `(if (not test)
       (begin ,@body)
     nil))

;; 测试 unless 宏
(println "测试: (unless (< 5 3) (println \"5 不小于 3\"))")
(unless (< 5 3)
  (println "5 不小于 3"))
(println "")

;; ======================================
;; 2. let* 宏（顺序绑定）
;; ======================================

(println "3. 定义 let* 宏（顺序绑定）")
(println "代码: (defmacro let* (bindings & body)")
(println "        (if (null? bindings)")
(println "            `(begin ,@body)")
(println "          `((let ((,(caar bindings) ,(cadar bindings)))")
(println "              (let* ,(cdr bindings) ,@body)))))")

(defmacro let* (bindings & body)
  (if (null? bindings)
      `(begin ,@body)
    `((let ((,(caar bindings) ,(cadar bindings)))
        (let* ,(cdr bindings) ,@body)))))

;; 测试 let* 宏
(println "测试: (let* ((a 1) (b (+ a 10))) (+ a b))")
(define result (let* ((a 1) (b (+ a 10))) (+ a b)))
(println #"结果: #{result}")
(println "期望: 12.000000")
(println "")

;; ======================================
;; 3. cond 宏（多分支条件）
;; ======================================

(println "4. 定义 cond 宏（多分支条件）")

(defmacro cond (& clauses)
  (if (null? clauses)
      nil
    (if (eq? (caar clauses) 'else)
        `(begin ,@(cdar clauses))
      `(if ,(caar clauses)
           (begin ,@(cdar clauses))
         (cond ,@(cdr clauses))))))

;; 测试 cond 宏
(println "测试: (cond ((< x 5) \"小\") ((> x 10) \"大\") (else \"中等\"))")
(define x 7)
(define cond-result (cond
                      ((< x 5) "小")
                      ((> x 10) "大")
                      (else "中等")))
(println #"结果: #{cond-result}")
(println "期望: \"中等\"")
(println "")

;; ======================================
;; 4. 自定义宏示例
;; ======================================

(println "5. 自定义宏: incf（自增）")
(println "代码: (defmacro incf (place &optional (delta 1))")
(println "        `(setq ,place (+ ,place ,delta)))")

(defmacro incf (place &optional (delta 1))
  `(setq ,place (+ ,place ,delta)))

;; 测试 incf 宏
(println "测试: (define counter 0) (incf counter) (incf counter 5)")
(define counter 0)
(println "初始 counter = 0")
(incf counter)
(println #"incf counter => #{counter}")
(incf counter 5)
(println #"incf counter 5 => #{counter}")
(println "期望: 1, 6")
(println "")

;; ======================================
;; 5. 宏展开测试
;; ======================================

(println "6. 宏展开测试")
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
;; 6. 复杂宏示例：管道操作符的简化版
;; ======================================

(println "7. 宏实现管道操作符简化版")

(defmacro ==> (& forms)
  (if (null? (cdr forms))
      (car forms)
    `((,(car forms)) (==>,@(cdr forms)))))

;; 测试 ==> 宏
(println "测试: (==> 1 (+ 2) (* 3) (- 5))")
(define pipe-result (==> 1 (+ 2) (* 3) (- 5)))
(println #"结果: #{pipe-result}")
(println "期望: ((- 5) (* 3 (+ 2 1))) 展开后的求值结果")
(println "")

(println "=== 宏系统演示完成 ===")
