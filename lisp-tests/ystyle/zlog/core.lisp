;; ystyle.zlog 实现

(export info debug error warn)

(define info
  (lambda (msg)
    "Log info message"
    (print "[INFO] " msg)))

(define debug
  (lambda (msg)
    "Log debug message"
    (print "[DEBUG] " msg)))

(define error
  (lambda (msg)
    "Log error message"
    (print "[ERROR] " msg)))

(define warn
  (lambda (msg)
    "Log warning message"
    (print "[WARN] " msg)))
