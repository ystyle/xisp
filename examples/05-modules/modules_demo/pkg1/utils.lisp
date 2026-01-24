;; ======================================
;; pkg1/utils.lisp
;; 包 1 的工具函数实现
;; ======================================

(println "正在加载 pkg1/utils.lisp...")

;; 导出符号
(export greet add multiply get-info)

;; 导出：问候函数
(define (greet name)
  (println #"Hello from pkg1! 你好, #{name}!"))

;; 导出：加法函数
(define (add a b)
  (+ a b))

;; 导出：乘法函数
(define (multiply x y)
  (* x y))

;; 导出：获取包信息
(define (get-info)
  "This is pkg1 - a utility package")

;; 内部函数（不导出）
(define (internal-helper)
  "This is a private function in pkg1")
