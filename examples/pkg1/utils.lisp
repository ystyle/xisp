;; pkg1/utils.lisp
;; 提供 utility 函数

(export greet)

(define greet
  (lambda (name)
    "Greet someone"
    (println "Hello from pkg1, " name "!")))
