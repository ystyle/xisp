;; ======================================
;; pkg2/module.lisp
;; 包 2 的模块元数据
;; ======================================

(module pkg2
  (version "1.0.0")
  (description "Package 2 - Main functions that use pkg1")
  (author "Xisp Demo")
  (dependencies
    (pkg1 "1.0.0")))
