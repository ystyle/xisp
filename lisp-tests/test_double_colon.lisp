;; 测试双冒号符号

(define zlog::init (lambda (name) (print "Initializing: " name)))
(define zlog::write (lambda (msg) (print "Writing: " msg)))

;; 测试
(zlog::init "test")
(zlog::write "hello")

;; 也测试单冒号
(define test:symbol 42)
(print "test:symbol = " test:symbol)
