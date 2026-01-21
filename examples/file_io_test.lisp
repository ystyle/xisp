; 文件 I/O 功能测试
; 测试真实的文件读写和目录操作

(println "╔═══════════════════════════════════════════════════════════╗")
(println "║         文件 I/O 功能测试 - 真实文件操作                  ║")
(println "╚═══════════════════════════════════════════════════════════╝")

; ========== 文件写入测试 ==========
(newline)
(println ">>> 测试 1: 文件写入")
(define write-result (cangjie:io:write-file "test_output.txt" "Hello from Xisp!\nThis is a test file."))
(println "  写入结果:" write-result)

; ========== 文件读取测试 ==========
(newline)
(println ">>> 测试 2: 文件读取")
(define file-content (cangjie:io:read-file "test_output.txt"))
(println "  文件内容:")
(println "  " file-content)

; ========== 文件追加测试 ==========
(newline)
(println ">>> 测试 3: 文件追加")
(define append-result (cangjie:io:append-file "test_output.txt" "\nAppended text!"))
(println "  追加结果:" append-result)

; ========== 重新读取验证追加 ==========
(newline)
(println ">>> 测试 4: 读取追加后的内容")
(define updated-content (cangjie:io:read-file "test_output.txt"))
(println "  更新后的内容:")
(println "  " updated-content)

; ========== 文件存在性检查 ==========
(newline)
(println ">>> 测试 5: 文件存在性检查")
(println "  test_output.txt 存在?" (cangjie:fs:exists? "test_output.txt"))
(println "  nonexistent.txt 存在?" (cangjie:fs:exists? "nonexistent.txt"))

; ========== 判断文件/目录 ==========
(newline)
(println ">>> 测试 6: 判断文件类型")
(println "  test_output.txt 是文件?" (cangjie:fs:file? "test_output.txt"))
(println "  test_output.txt 是目录?" (cangjie:fs:directory? "test_output.txt"))
(println "  . 是目录?" (cangjie:fs:directory? "."))
(println "  . 是文件?" (cangjie:fs:file? "."))

; ========== 列出当前目录 ==========
(newline)
(println ">>> 测试 7: 列出当前目录内容")
(define files (cangjie:fs:list-dir "."))
(println "  文件数量:" (length files))
(println "  前 5 个文件:")
(define (print-first-n lst n)
  (if (or (null? lst) (<= n 0))
      ()
      (begin
        (println "   -" (first lst))
        (print-first-n (rest lst) (- n 1)))))
(print-first-n files 5)

; ========== 列出 examples 目录 ==========
(newline)
(println ">>> 测试 8: 列出 examples 目录")
(define example-files (cangjie:fs:list-dir "examples"))
(println "  examples 文件数量:" (length example-files))

; ========== 错误处理测试 ==========
(newline)
(println ">>> 测试 9: 错误处理")
(println "  读取不存在的文件:")
(define error-result (cangjie:io:read-file "nonexistent_file.txt"))
(println "   " error-result)

; ========== 总结 ==========
(newline)
(println "╔═══════════════════════════════════════════════════════════╗")
(println "║              文件 I/O 测试完成！                        ║")
(println "║                                                        ║")
(println "║  已测试功能:                                            ║")
(println "║  - cangjie:io:read-file    ✓ 文件读取                   ║")
(println "║  - cangjie:io:write-file   ✓ 文件写入                   ║")
(println "║  - cangjie:io:append-file  ✓ 文件追加                   ║")
(println "║  - cangjie:fs:exists?      ✓ 存在性检查                 ║")
(println "║  - cangjie:fs:file?        ✓ 文件判断                   ║")
(println "║  - cangjie:fs:directory?   ✓ 目录判断                   ║")
(println "║  - cangjie:fs:list-dir     ✓ 目录列表                   ║")
(println "╚═══════════════════════════════════════════════════════════╝")

(newline)
(println "✓ 所有测试完成！")
(println "  测试文件 'test_output.txt' 已创建，请检查当前目录。")
