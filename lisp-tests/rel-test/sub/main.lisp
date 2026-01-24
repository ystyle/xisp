;; sub/main.lisp
(println "sub/main: 开始加载")

;; 相对导入父目录的 pkg1 (../pkg1)
(println "sub/main: 尝试相对导入 ../pkg1...")
(import "../pkg1")

(println "sub/main: 导入成功")

;; 调用 pkg1 的函数
(println "sub/main: 调用 pkg1.greet:")
(pkg1.greet "来自 sub")

(println "sub/main: 加载完成")

(export sub-func)

(define (sub-func)
  "来自 sub")
