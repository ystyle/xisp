;; 相等性比较函数测试
;; 测试 eq?, string=?, string<, string> 函数

(println "=== 测试 eq? 函数 ===")

;; 测试符号比较
(println "\n--- 符号比较 ---")
(define eq-sym-1 (eq? :ok :ok))
(println "(eq? :ok :ok) =" eq-sym-1)
(if eq-sym-1
    (println "✓ 相同符号比较测试通过")
    (println "✗ 相同符号比较测试失败"))

(define eq-sym-2 (eq? :ok :error))
(println "(eq? :ok :error) =" eq-sym-2)
(if (not eq-sym-2)
    (println "✓ 不同符号比较测试通过")
    (println "✗ 不同符号比较测试失败"))

;; 测试字符串比较
(println "\n--- 字符串比较 ---")
(define eq-str-1 (eq? "a" "a"))
(println "(eq? \"a\" \"a\") =" eq-str-1)
(if eq-str-1
    (println "✓ 相同字符串比较测试通过")
    (println "✗ 相同字符串比较测试失败"))

(define eq-str-2 (eq? "a" "b"))
(println "(eq? \"a\" \"b\") =" eq-str-2)
(if (not eq-str-2)
    (println "✓ 不同字符串比较测试通过")
    (println "✗ 不同字符串比较测试失败"))

;; 测试整数比较
(println "\n--- 整数比较 ---")
(define eq-int-1 (eq? 42 42))
(println "(eq? 42 42) =" eq-int-1)
(if eq-int-1
    (println "✓ 相同整数比较测试通过")
    (println "✗ 相同整数比较测试失败"))

(define eq-int-2 (eq? 42 43))
(println "(eq? 42 43) =" eq-int-2)
(if (not eq-int-2)
    (println "✓ 不同整数比较测试通过")
    (println "✗ 不同整数比较测试失败"))

;; 测试 nil 比较
(println "\n--- nil 比较 ---")
(define eq-nil (eq? nil nil))
(println "(eq? nil nil) =" eq-nil)
(if eq-nil
    (println "✓ nil 比较测试通过")
    (println "✗ nil 比较测试失败"))

;; 测试布尔值比较
(println "\n--- 布尔值比较 ---")
(define eq-bool-1 (eq? #t #t))
(println "(eq? #t #t) =" eq-bool-1)
(if eq-bool-1
    (println "✓ 相同布尔值比较测试通过")
    (println "✗ 相同布尔值比较测试失败"))

(define eq-bool-2 (eq? #t #f))
(println "(eq? #t #f) =" eq-bool-2)
(if (not eq-bool-2)
    (println "✓ 不同布尔值比较测试通过")
    (println "✗ 不同布尔值比较测试失败"))

;; 测试不同类型比较
(println "\n--- 不同类型比较 ---")
(define eq-diff (eq? :ok "ok"))
(println "(eq? :ok \"ok\") =" eq-diff)
(if (not eq-diff)
    (println "✓ 不同类型比较测试通过")
    (println "✗ 不同类型比较测试失败"))

(println "\n=== 测试 string<? 函数 ===")

(define str-less-1 (string< "a" "b"))
(println "(string< \"a\" \"b\") =" str-less-1)
(if str-less-1
    (println "✓ string< 字典序测试通过")
    (println "✗ string< 字典序测试失败"))

(define str-less-2 (string< "b" "a"))
(println "(string< \"b\" \"a\") =" str-less-2)
(if (not str-less-2)
    (println "✓ string< 反向测试通过")
    (println "✗ string< 反向测试失败"))

(define str-less-3 (string< "abc" "abc"))
(println "(string< \"abc\" \"abc\") =" str-less-3)
(if (not str-less-3)
    (println "✓ string< 相等字符串测试通过")
    (println "✗ string< 相等字符串测试失败"))

;; 测试数字字符串
(define str-less-4 (string< "10" "2"))
(println "(string< \"10\" \"2\") =" str-less-4)
(println "说明：字符串比较是字典序，\"10\" < \"2\" 因为 '1' < '2'")

(println "\n=== 测试 string>? 函数 ===")

(define str-greater-1 (string> "b" "a"))
(println "(string> \"b\" \"a\") =" str-greater-1)
(if str-greater-1
    (println "✓ string> 字典序测试通过")
    (println "✗ string> 字典序测试失败"))

(define str-greater-2 (string> "a" "b"))
(println "(string> \"a\" \"b\") =" str-greater-2)
(if (not str-greater-2)
    (println "✓ string> 反向测试通过")
    (println "✗ string> 反向测试失败"))

(define str-greater-3 (string> "abc" "abc"))
(println "(string> \"abc\" \"abc\") =" str-greater-3)
(if (not str-greater-3)
    (println "✓ string> 相等字符串测试通过")
    (println "✗ string> 相等字符串测试失败"))

(println "\n=== 测试 string=? 函数 ===")

(define str-eq-1 (string=? "abc" "abc"))
(println "(string=? \"abc\" \"abc\") =" str-eq-1)
(if str-eq-1
    (println "✓ string=? 相同字符串测试通过")
    (println "✗ string=? 相同字符串测试失败"))

(define str-eq-2 (string=? "abc" "def"))
(println "(string=? \"abc\" \"def\") =" str-eq-2)
(if (not str-eq-2)
    (println "✓ string=? 不同字符串测试通过")
    (println "✗ string=? 不同字符串测试失败"))

(println "\n=== 实际应用场景测试 ===")

;; 场景1：使用符号作为状态码
(println "\n--- 场景1：状态码匹配 ---")
(define current-status :ok)
(if (eq? current-status :ok)
    (println "✓ 状态正常")
    (println "✗ 状态异常"))

(if (eq? current-status :error)
    (println "✓ 状态错误")
    (println "✓ 状态不是错误"))

;; 场景2：字符串排序
(println "\n--- 场景2：字符串排序比较 ---")
(define name1 "alice")
(define name2 "bob")
(if (string< name1 name2)
    (println "✓ " name1 " 在 " name2 " 前面")
    (println "✗ 排序错误"))

;; 场景3：组合使用
(println "\n--- 场景3：多条件判断 ---")
(define result1 (and (eq? :ok :ok) (string< "a" "b")))
(println "(and (eq? :ok :ok) (string< \"a\" \"b\")) =" result1)
(if result1
    (println "✓ 组合条件测试通过")
    (println "✗ 组合条件测试失败"))

(println "\n=== 所有测试完成 ===")
