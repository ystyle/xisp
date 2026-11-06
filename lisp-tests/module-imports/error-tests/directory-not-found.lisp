(println "=== 测试目录不存在错误 ===")

;; 语言目前**没有 `try/catch`**（错误是"值"而不是可捕获的异常），
;; 所以这里不做捕获：`import` 失败会返回错误值并中止本条表达式。
;; 期望：stderr 出现 `[IOError] directory not found at './notexist-dir'`。
;; 三模式一致性由 `lisp-tests/check-examples.sh` 比对（它同时收 stderr），
;; 错误种类由 `xisp_tests` 的模块用例断言。
(import "./notexist-dir")

(println "")
(println "=== 测试完成 ===")
