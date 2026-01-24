;; funcs.lisp - 用于测试 Export 功能

(define (publicFunc)
  "这是公开函数")

(define (privateFunc)
  "这是私有函数")

;; 只导出 publicFunc
(export publicFunc)
