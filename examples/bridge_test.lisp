; 仓颉桥接功能示例
; 演示 Lisp 如何调用仓颉标准库函数

(println "╔═══════════════════════════════════════════════════════════╗")
(println "║         仓颉桥接功能测试 - Xisp Bridge                    ║")
(println "╚═══════════════════════════════════════════════════════════╝")

; ========== std.io 模块测试 ==========
(newline)
(println ">>> std.io 模块测试:")

(newline)
(println "1. 文件读取:")
(println "  (cangjie::read-file \"config.json\")")
(define file-content (cangjie::read-file "config.json"))
(println "  结果:" file-content)

(newline)
(println "2. 文件写入:")
(println "  (cangjie::write-file \"output.txt\" \"Hello from Lisp!\")")
(define write-result (cangjie::write-file "output.txt" "Hello from Lisp!"))
(println "  结果:" write-result)

; ========== std.fs 模块测试 ==========
(newline)
(println ">>> std.fs 模块测试:")

(newline)
(println "1. 检查文件存在:")
(println "  (cangjie::exists? \"/tmp\")")
(define tmp-exists (cangjie::exists? "/tmp"))
(println "  /tmp 存在?" tmp-exists)

(newline)
(println "2. 检查文件不存在:")
(println "  (cangjie::exists? \"/nonexistent\")")
(define not-exists (cangjie::exists? "/nonexistent"))
(println "  结果:" not-exists)

(newline)
(println "3. 列出目录:")
(println "  (cangjie::list-dir \".\")")
(define dir-list (cangjie::list-dir "."))
(println "  结果:" dir-list)

(newline)
(println "4. 遍历目录列表:")
(define (print-list lst)
  (if (not (null? lst))
      (begin
        (println "  -" (first lst))
        (print-list (rest lst)))
      ()))

(print-list dir-list)

; ========== 综合示例 ==========
(newline)
(println ">>> 综合示例 - 处理文件列表:")

(define files (cangjie::list-dir "."))
(println "文件数量:" (length files))
(println "文件列表:" files)

; ========== 说明 ==========
(newline)
(println "╔═══════════════════════════════════════════════════════════╗")
(println "║              桥接功能说明                              ║")
(println "╠═══════════════════════════════════════════════════════════╣")
(println "║ 命名空间:                                               ║")
(println "║   cangjie:: - 文件和文件系统操作                          ║")
(println "║                                                         ║")
(println "║ 可用函数:                                               ║")
(println "║   cangjie::read-file    - 读取文件内容                ║")
(println "║   cangjie::write-file   - 写入文件                    ║")
(println "║   cangjie::exists?      - 检查文件/目录是否存在        ║")
(println "║   cangjie::list-dir     - 列出目录内容                ║")
(println "║                                                         ║")
(println "║ 注意: 当前为演示版本，实际文件操作待实现               ║")
(println "╚═══════════════════════════════════════════════════════════╝")

(newline)
(println "✓ 桥接功能测试完成！")
