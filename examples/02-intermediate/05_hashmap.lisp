;; ======================================
;; 哈希映射 (Hash Map) 完整示例
;; ======================================
;; Redis 风格的哈希操作 API

(println "=== Xisp 哈希映射完整示例 ===\n")

;; ==================== 1. 创建哈希映射 ====================
(println "1. 创建哈希映射")
(println "---")

(println "空映射:")
(define empty-map {})
(println "  {} => " empty-map)

(println "\n简单映射:")
(define user {:name "Alice" :age 30 :city "Beijing"})
(println "  {:name \"Alice\" :age 30 :city \"Beijing\"}")
(println "  => " user)

(println "\n嵌套映射:")
(define config
  {:server {:host "localhost" :port 8080}
   :database {:host "db.example.com" :port 5432}
   :debug #t})
(println "  嵌套配置:")
(println "  => " config)

;; ==================== 2. Redis 风格操作 ====================
(println "\n2. Redis 风格操作")
(println "---")

(define app-config {:host "localhost" :port 8080 :debug #f})

(println "初始配置:")
(println "  " app-config)

;; HGET - 获取值
(println "\nHGET - 获取值:")
(println "  (hget app-config :host) => " (hget app-config :host))
(println "  (hget app-config :port) => " (hget app-config :port))
(println "  (hget app-config :debug) => " (hget app-config :debug))

;; HSET - 设置值
(println "\nHSET - 设置值:")
(println "  修改端口 8080 -> 9090")
(hset app-config :port 9090)
(println "  (hget app-config :port) => " (hget app-config :port))

(println "  添加新字段 :timeout")
(hset app-config :timeout 30)
(println "  (hget app-config :timeout) => " (hget app-config :timeout))

;; HEXISTS - 检查键
(println "\nHEXISTS - 检查键:")
(println "  (hexists app-config :host) => " (hexists app-config :host))
(println "  (hexists app-config :ssl) => " (hexists app-config :ssl))

;; HLEN - 获取大小
(println "\nHLEN - 获取大小:")
(println "  (hlen app-config) => " (hlen app-config))

;; HKEYS - 获取所有键
(println "\nHKEYS - 获取所有键:")
(println "  (hkeys app-config) => " (hkeys app-config))

;; HVALS - 获取所有值
(println "\nHVALS - 获取所有值:")
(println "  (hvals app-config) => " (hvals app-config))

;; HGETALL - 获取全部
(println "\nHGETALL - 获取全部:")
(println "  (hgetall app-config) => " (hgetall app-config))

;; HDEL - 删除键
(println "\nHDEL - 删除键:")
(println "  删除 :debug")
(hdel app-config :debug)
(println "  (hexists app-config :debug) => " (hexists app-config :debug))
(println "  (hlen app-config) => " (hlen app-config))

;; ==================== 3. 关键字自求值 ====================
(println "\n3. 关键字自求值特性")
(println "---")

(println "关键字 :name 不需要 quote:")
(println "  :name => " :name)

(println "\n在哈希映射中直接使用:")
(println "  {:name \"Alice\" :age 30}")
(println "  关键字自动识别，无需 '" (quote :name))

(println "\n访问时也直接使用:")
(println "  (hget user :name) => " (hget user :name))
(println "  而不是 (hget user ':name)")

;; ==================== 4. 实用场景 ====================
(println "\n4. 实用场景")
(println "---")

;; 场景1: 配置管理
(println "场景1: 配置管理")
(define server-config
  {:host "0.0.0.0"
   :port 3000
   :env "production"
   :features #{:auth :logging :cache}})

(println "  服务器配置:")
(println "    host => " (hget server-config :host))
(println "    port => " (hget server-config :port))
(println "    env => " (hget server-config :env))

;; 场景2: 数据存储
(println "\n场景2: 数据存储")
(define users
  {:u1 {:name "Alice" :score 100}
   :u2 {:name "Bob" :score 85}
   :u3 {:name "Charlie" :score 95}})

(println "  用户数据:")
(println "    Alice => " (hget (hget users :u1) :score))
(println "    Bob => " (hget (hget users :u2) :score))

;; 场景3: 动态更新
(println "\n场景3: 动态更新:")
(define counter {:count 0})
(println "  初始: " counter)
(hset counter :count (+ (hget counter :count) 1))
(println "  +1: " counter)
(hset counter :count (+ (hget counter :count) 1))
(println "  +1: " counter)

;; ==================== 5. 完整函数名（向后兼容）====================
(println "\n5. 完整函数名（向后兼容）")
(println "---")

(println "除了 Redis 风格的简短名称，也可以使用完整函数名:")
(println "  hget        <=> hashmap-get")
(println "  hset        <=> hashmap-set!")
(println "  hdel        <=> hashmap-remove!")
(println "  hexists     <=> hashmap-contains?")
(println "  hlen        <=> hashmap-size")
(println "  hkeys       <=> hashmap-keys")
(println "  hvals       <=> hashmap-values")

(println "\n示例:")
(println "  (hashmap-get user :name) => " (hashmap-get user :name))
(println "  (hashmap-size user) => " (hashmap-size user))

;; ==================== 6. 类型检查 ====================
(println "\n6. 类型检查")
(println "---")

(println "hashmap? 谓词:")
(println "  (hashmap? {:a 1}) => " (hashmap? {:a 1}))
(println "  (hashmap? '(1 2 3)) => " (hashmap? '(1 2 3)))
(println "  (hashmap? 42) => " (hashmap? 42))

(println "\n=== 示例完成 ===")
