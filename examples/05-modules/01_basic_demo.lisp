;; modules_demo/demo.lisp
;; 演示模块系统功能

(println "=== 模块系统演示 ===")

;; 导入 pkg2
(println "正在导入 pkg2...")
(import pkg2)

;; 直接引用符号看看是否存在
(println "检查符号 pkg2.call-pkg1:")
(println pkg2.call-pkg1)

;; 调用它
(println "调用 pkg2.call-pkg1:")
(pkg2.call-pkg1 "World")

;; 导出符号
(export demo-result)
(define demo-result
  (lambda ()
    (str "Module syste  demo completed!")))
