(println "=== 测试 Export 功能 ===")

(import "./funcs.lisp")

;; 测试导出的函数
(println "调用导出的函数 publicFunc:")
(println "  " (publicFunc))

;; 测试未导出的函数：语言没有 try/catch，故不捕获。
;; 期望：stderr 出现 `[UndefinedFunction] Undefined function: 'privateFunc'`。
;; 该行为由 `lisp-tests/check-examples.sh`（三模式一致性，含 stderr）与
;; `xisp_tests` 的模块用例共同钉住。
(println "")
(println "测试未导出的函数 privateFunc:")
(privateFunc)

(println "")
(println "=== 测试完成 ===")
