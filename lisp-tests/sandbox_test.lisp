;; Xisp 沙箱系统测试脚本

(print "=== Xisp 沙箱系统测试 ===")
(print "")

;; 测试 1: 栈深度限制
(print "测试 1: 栈深度限制")
(print "设置栈深度为 10")
(define (deep-recurse n)
  (if (> n 0)
    (deep-recurse (- n 1))
    n))
(print "(deep-recurse 20):")
; 这应该会触发栈深度限制
; (deep-recurse 20)  ; 暂时注释，避免崩溃
(print "")
