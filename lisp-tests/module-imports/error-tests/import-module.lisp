(println "=== 测试相对导入模块错误 ===")

;; bad-module/ 有 module.lisp，不应该用相对导入。
;; 期望：stderr 出现 `[ModuleError] Cannot import module './bad-module' ...`。
(import "./bad-module")

(println "")
(println "说明: 相对导入不应该导入有 module.lisp 的目录")
(println "      应该使用绝对导入 (import bad-module)")

(println "")
(println "=== 测试完成 ===")
