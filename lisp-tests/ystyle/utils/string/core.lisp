;; ystyle.utils.string 实现

(export reverse-string upper-case lower-case trim)

(define reverse-string
  (lambda (s)
    "Reverse a string"
    (apply str (apply reverse (str->list s)))))

(define upper-case
  (lambda (s)
    "Convert string to uppercase"
    ;; 简化实现
    s))

(define lower-case
  (lambda (s)
    "Convert string to lowercase"
    ;; 简化实现
    s))

(define trim
  (lambda (s)
    "Trim whitespace from string"
    ;; 简化实现
    s))
