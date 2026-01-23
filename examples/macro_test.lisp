;; ======================================
;; Xisp 宏系统基础测试
;; ======================================

(println "=== Xisp 宏系统基础测试 ===")
(println "")

;; ======================================
;; 1. 定义 when 宏
;; ======================================

(println "1. 定义 when 宏")
(println "(defmacro when (test & body) `(if test (begin ,@body) nil))")

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

;; ======================================
;; 2. 定义 unless 宏
;; ======================================

(println "2. 定义 unless 宏")
(println "(defmacro unless (test & body) `(if (not test) (begin ,@body) nil))")

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
(println "(defmacro incf (place &optional (delta 1)) `(setq ,place (+ ,place ,delta)))")

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
(println "")

;; ======================================
;; 5. 定义 let* 宏
;; ======================================

(println "5. 定义 let* 宏（顺序绑定）")
(println "(defmacro let* (bindings & body)")
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

(println "=== 宏系统基础测试完成 ===")
