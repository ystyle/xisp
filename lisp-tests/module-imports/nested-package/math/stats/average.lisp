;; math/stats/average.lisp - 平均值计算

(define (average numbers)
  (let ((sum (apply + numbers))
        (count (length numbers)))
    (/ sum count)))

(export average)
