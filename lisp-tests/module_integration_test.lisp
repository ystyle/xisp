;; 模块系统完整功能测试
;; 运行方式: XISP_PATH=examples ../target/release/bin/ystyle::xisp.cli module_integration_test.lisp

(println "╔═══════════════════════════════════════════════════════════╗")
(println "║           模块系统完整功能测试                          ║")
(println "╚═══════════════════════════════════════════════════════════╝")
(println "")

;; ==================== 测试1: 基本导入和调用 ====================
(println "【测试1】基本导入和调用")
(import pkg1)
(println "  导入 pkg1 成功")

(println "  调用 pkg1.greet:")
(pkg1.greet "Alice")
(println "")

;; ==================== 测试2: 跨包依赖 ====================
(println "【测试2】跨包依赖（pkg2 导入 pkg1）")
(import pkg2)
(println "  导入 pkg2 成功")

(println "  调用 pkg2.call-pkg1:")
(define result1 (pkg2.call-pkg1 "Bob"))
(println "  返回值: " result1)
(println "")

;; ==================== 测试3: 平方函数 ====================
(println "【测试3】数学函数调用")
(println "  pkg2.square(5) = " (pkg2.square 5))
(println "  pkg2.square(10) = " (pkg2.square 10))
(println "")

;; ==================== 测试4: 符号导出验证 ====================
(println "【测试4】符号导出验证")
(println "  pkg1.greet = " pkg1.greet)
(println "  pkg2.call-pkg1 = " pkg2.call-pkg1)
(println "  pkg2.square = " pkg2.square)
(println "")

;; ==================== 测试5: 函数组合 ====================
(println "【测试5】函数组合调用")
(define (process-square n)
  (pkg2.square n))

(println "  (process-square 7) = " (process-square 7))
(println "")

;; ==================== 测试6: 高阶函数 ====================
(println "【测试6】高阶函数")
(define numbers [1 2 3 4 5])
(define squared (map pkg2.square numbers))
(println "  (map pkg2.square [1 2 3 4 5]) = " squared)
(println "")

;; ==================== 测试7: Lambda 多表达式 ====================
(println "【测试7】Lambda 多表达式（使用 begin）")
(define multi-expr
  (lambda (x)
    (begin
      (println "  开始计算...")
      (println "  输入: " x)
      (define result (* x x))
      (println "  输出: " result)
      result)))

(println "  调用多表达式 lambda:")
(multi-expr 6)
(println "")

;; ==================== 测试8: 模块符号命名空间 ====================
(println "【测试8】模块符号命名空间")
(println "  测试 pkg1 和 pkg2 的符号隔离")
(define greet "local-greet")
(println "  本地 greet = " greet)
(println "  pkg1.greet = " pkg1.greet)
(println "  两者不冲突")
(println "")

;; ==================== 测试总结 ====================
(println "╔═══════════════════════════════════════════════════════════╗")
(println "║              所有测试完成！                            ║")
(println "╚═══════════════════════════════════════════════════════════╝")
