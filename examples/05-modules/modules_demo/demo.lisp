;; ======================================
;; 模块系统演示 - 顶层文件
;; ======================================

(println "=== Xisp 模块系统演示 ===")
(println "")

(println "1. 导入 pkg2 模块")
(println "代码: (import pkg2)")
(import pkg2)
(println "模块导入成功")
(println "")

(println "2. 调用 pkg2 中导出的函数")
(println "代码: (pkg2.greet \"Xisp\")")
(pkg2.greet "Xisp")
(println "")

(println "3. 使用 pkg2 中的计算函数")
(println "代码: (pkg2.calculate 10 20)")
(define result (pkg2.calculate 10 20))
(println #"结果: #{result}")
(println "期望: 30")
(println "")

(println "4. 调用 pkg2 中使用 pkg1 功能的函数")
(println "代码: (pkg2.call-pkg1-util)")
(pkg2.call-pkg1-util)
(println "")

(println "5. 显示 pkg1 信息")
(println "代码: (pkg2.show-pkg1-info)")
(pkg2.show-pkg1-info)
(println "")

(println "6. 直接访问 pkg1 的符号")
(println "代码: (import pkg1)")
(import pkg1)
(println "代码: (pkg1.greet \"Direct\")")
(pkg1.greet "Direct")
(println #"代码: (pkg1.multiply 6 7) = #{(pkg1.multiply 6 7)}")
(println "")

(println "=== 模块系统演示完成 ===")
