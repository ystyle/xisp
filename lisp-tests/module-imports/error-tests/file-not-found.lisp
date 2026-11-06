(println "=== 测试文件不存在错误 ===")

;; 同 directory-not-found：不做捕获，期望 stderr 出现
;; `[IOError] file not found at './notexist.lisp'`。
(import "./notexist.lisp")

(println "")
(println "=== 测试完成 ===")
