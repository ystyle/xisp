;; test-rel/main.lisp
;; 测试相对路径导入

(println "test-rel/main.lisp: 开始加载")

;; 相对导入当前目录下的 pkg1
(println "test-rel/main.lisp: 尝试相对路径导入 ./pkg1...")
(import "./pkg1")

(println "test-rel/main.lisp: 导入成功")

;; 调用 pkg1 的函数
(println "test-rel/main.lisp: 调用 pkg1.greet:")
(pkg1.greet "来自 test-rel")

;; 导出测试函数
(export test-rel-func)

(define (test-rel-func x)
  (str "test-rel 处理: " x))

(println "test-rel/main.lisp: 加载完成")
