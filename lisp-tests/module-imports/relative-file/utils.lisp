;; utils.lisp - 工具函数
;; 这个文件将被相对导入，符号应该无前缀

(define (processData data)
  (str "处理数据: " data))

(define (internalHelper data)
  (str "内部助手: " data))

;; 只导出 processData，不导出 internalHelper
(export processData)
