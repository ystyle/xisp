;; rel-test/main.lisp
(println "rel-test/main: 开始加载")

;; 相对导入子目录下的包
(println "rel-test/main: 导入 ./sub.inner-pkg...")
(import "./sub.inner-pkg")

(println "rel-test/main: 加载完成")

(export rel-test-func)

(define (rel-test-func)
  "来自 rel-test")
