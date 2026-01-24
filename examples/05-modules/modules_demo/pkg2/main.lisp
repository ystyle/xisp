;; ======================================
;; pkg2/main.lisp
;; 包 2 的主文件（导入 pkg1）
;; ======================================

(println "正在加载 pkg2/main.lisp...")

;; 导入 pkg1 模块
(import pkg1)

;; 导出符号列表
(export greet calculate call-pkg1-util show-pkg1-info)

;; 导出：问候函数（使用 pkg1 的功能）
(define (greet name)
  (println #"Greetings from pkg2!")
  (println #"pkg1 says:")
  (pkg1.greet name))

;; 导出：计算函数（使用 pkg1 的 add）
(define (calculate x y)
  (println #"pkg2 calculating #{x} + #{y}...")
  (define result (pkg1.add x y))
  (println #"Using pkg1.add, result is #{result}")
  result)

;; 导出：调用 pkg1 工具函数
(define (call-pkg1-util)
  (println "Calling pkg1 utility functions:")
  (println (pkg1.get-info))
  (println #"pkg1.add(5, 3) = #{(pkg1.add 5 3)}")
  (println #"pkg1.multiply(4, 7) = #{(pkg1.multiply 4 7)}"))

;; 导出：显示 pkg1 信息
(define (show-pkg1-info)
  (println "")
  (println "=== pkg1 信息 ===")
  (println (pkg1.get-info))
  (println "=================")
  (println ""))
