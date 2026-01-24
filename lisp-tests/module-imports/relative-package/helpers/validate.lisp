;; helpers/validate.lisp - 验证函数

(define (validateEmail email)
  (str "验证邮箱: " email))

(define (formatName name)
  (str "格式化名字: " name))

(export validateEmail)
(export formatName)
