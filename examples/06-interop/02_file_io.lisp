;; ======================================
;; 文件 I/O 实践 (File I/O Practice) 示例
;; ======================================

(println "=== Xisp 文件 I/O 实践演示 ===")
(println "")

(println "1. 创建日志文件")
(println "代码: (cangjie::write-file \"app.log\" \"[INFO] Application started\\n\")")
(cangjie::write-file "app.log" "[INFO] Application started\n")
(println "日志文件已创建")
(println "")

(println "2. 追加日志")
(println "代码: (cangjie::append-file \"app.log\" \"[INFO] Processing data\\n\")")
(cangjie::append-file "app.log" "[INFO] Processing data\n")
(cangjie::append-file "app.log" "[INFO] Data processed successfully\n")
(println "日志已追加")
(println "")

(println "3. 读取完整日志")
(println "代码: (cangjie::read-file \"app.log\")")
(define logs (cangjie::read-file "app.log"))
(println #"日志内容:\n#{logs}")
(println "")

(println "4. 创建配置文件")
(println "代码: (cangjie::write-file \"config.ini\" \"[settings]\\nkey=value\")")
(cangjie::write-file "config.ini" "[settings]\nkey=value\ncount=42\n")
(println "配置文件已创建")
(println "")

(println "5. 读取配置")
(println "代码: (cangjie::read-file \"config.ini\")")
(define config (cangjie::read-file "config.ini"))
(println #"配置内容:\n#{config}")
(println "")

(println "6. 文件操作模式对比")
(println "write-file: 覆盖模式 (每次写入会清空原文件)")
(cangjie::write-file "mode_test.txt" "First content\n")
(define content1 (cangjie::read-file "mode_test.txt"))
(println "第一次写入后:")
(println #"  #{content1}")
(cangjie::write-file "mode_test.txt" "Second content\n")
(define content2 (cangjie::read-file "mode_test.txt"))
(println "第二次写入后 (覆盖):")
(println #"  #{content2}")
(println "")

(println "append-file: 追加模式 (在文件末尾添加)")
(cangjie::write-file "mode_test.txt" "Base content\n")
(cangjie::append-file "mode_test.txt" "Appended 1\n")
(cangjie::append-file "mode_test.txt" "Appended 2\n")
(define content3 (cangjie::read-file "mode_test.txt"))
(println "写入+追加后:")
(println #"  #{content3}")
(println "")

(println "7. 实用示例 - 统计 Lisp 文件")
(println "代码: 获取所有 .lisp 文件")
(define all-files (cangjie::list-dir "examples/01-basics"))
(define (is-lisp-file? name)
  (and (> (length name) 5)
       (= (first (rest (rest (rest (rest (rest name)))))) 46)  ; ASCII for '.'
       (= (first (rest (rest (rest (rest (rest (rest name))))))) 108)  ; 'l'
       (= (first (rest (rest (rest (rest (rest (rest (rest name)))))))) 105)))  ; 'i'
(println "找到的 Lisp 文件:")
(define (print-lisp-files lst)
  (if (null? lst)
      ()
      (do
        (if (is-lisp-file? (first lst))
            (println #"  - #{(first lst)}")
            ())
        (print-lisp-files (rest lst)))))
(print-lisp-files all-files)
(println "")

(println "8. 文件安全检查示例")
(println "读取前检查文件是否存在")
(define target "README.md")
(if (cangjie::exists? target)
    (do
      (println #"#{target} 存在，类型判断:")
      (println #"  是文件: #{(cangjie::file? target)}")
      (println #"  是目录: #{(cangjie::directory? target)}"))
    (println #"#{target} 不存在"))
(println "")

(println "=== 文件 I/O 实践演示完成 ===")
(println "")
(println "测试文件已创建:")
(println "  - app.log (日志文件)")
(println "  - config.ini (配置文件)")
(println "  - mode_test.txt (模式测试)")
