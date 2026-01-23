;; pkg2/main.lisp
;; 使用相对路径导入 pkg1

(println "pkg2/main.lisp 正在加载...")

(println "pkg2/main.lisp 正在导出符号...")
(export call-pkg1)
(println "pkg2/main.lisp 导出完成")

;; 导入 pkg1
(println "pkg2/main.lisp 正在导入 pkg1...")
(import pkg1)

(println "pkg2/main.lisp 正在定义 call-pkg1...")
(define call-pkg1
  (lambda (name)
    (println "  -> call-pkg1 被调用，参数: " name)
    (pkg1.greet name)
    (str "pkg2.call-pkg1 完成")))

(println "pkg2/main.lisp 加载完成")
