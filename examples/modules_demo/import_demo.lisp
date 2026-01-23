;; 模块导入演示
;; 演示如何使用 import 语句导入模块

(println "========================================")
(println "模块导入演示")
(println "========================================")
(println "")

;; 演示1: 导入标准库模块
(println "【演示1: 导入标准库模块】")
(println "代码: (import std::math)")
(println "注意: 实际导入功能需要完整实现")
(println "")

;; 预期用法（模块系统完整实现后）:
;; (import std::math)
;; (math.add 1 2)       ; => 3
;; (math.square 5)      ; => 25

;; 演示2: 导入并使用别名
(println "【演示2: 导入并使用别名】")
(println "代码: (import std::math :as m)")
(println "预期: (m.add 1 2) => 3")
(println "")

;; 演示3: 限定导入
(println "【演示3: 限定导入】")
(println "代码: (import (only std::math add subtract))")
(println "预期: 只导入 add 和 subtract 函数")
(println "")

;; 演示4: 相对路径导入
(println "【演示4: 相对路径导入】")
(println "代码: (import ./local.module)")
(println "预期: 导入当前目录下的 local.module")
(println "")

(println "========================================")
(println "演示完成！")
(println "注意: 模块系统的基础框架已实现，")
(println "      文件系统访问需要仓颉标准库支持")
(println "========================================")
