;; reltest/main.lisp
(println "reltest/main: 开始加载")

(println "reltest/main: 定义 reltest-func...")
(define (reltest-func x)
  (str "reltest 处理: " x))

(println "reltest/main: reltest-func 定义完成，值为: " reltest-func)

(println "reltest/main: 导出 reltest-func...")
(export reltest-func)

(println "reltest/main: 加载完成")
