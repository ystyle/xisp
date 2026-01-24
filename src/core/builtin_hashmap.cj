package ystyle::xisp.core
import ystyle::xisp.types.*
import std.collection.*

/**
 * 哈希映射操作
 *
 * 提供哈希映射的创建、访问、修改、删除等操作
 */

/**
 * 将 LispValue 转换为字符串键
 */
public func lispValueToKey(v: LispValue): String {
    match (v) {
        case Str(s) => s
        case Symbol(s) =>
            // 如果符号以 : 开头，去掉冒号（关键字语法）
            if (s.size > 0 && s.startsWith(":")) {
                s[1..]
            } else {
                s
            }
        case _ => v.toString()
    }
}

extend BuiltinFunctions {

    /**
     * 注册 Redis 风格的哈希操作别名
     */
    public static func registerRedisAliases(env: Environment) {
        // hget - 获取哈希字段
        let hgetFunc = env.lookup("hashmap-get")
        env.define("hget", hgetFunc)

        // hset - 设置哈希字段
        let hsetFunc = env.lookup("hashmap-set!")
        env.define("hset", hsetFunc)

        // hdel - 删除哈希字段
        let hdelFunc = env.lookup("hashmap-remove!")
        env.define("hdel", hdelFunc)

        // hexists - 检查字段是否存在
        let hexistsFunc = env.lookup("hashmap-contains?")
        env.define("hexists", hexistsFunc)

        // hkeys - 获取所有字段名
        let hkeysFunc = env.lookup("hashmap-keys")
        env.define("hkeys", hkeysFunc)

        // hvals - 获取所有值
        let hvalsFunc = env.lookup("hashmap-values")
        env.define("hvals", hvalsFunc)

        // hlen - 获取字段数量
        let hlenFunc = env.lookup("hashmap-size")
        env.define("hlen", hlenFunc)

        // hgetall - 获取所有键值对（返回哈希映射本身）
        env.define("hgetall", NativeFunc({ args =>
            if (args.size != 1) {
                println("Error: hgetall requires 1 argument")
                return Nil
            }
            match (args[0]) {
                case HashMap(_) => args[0]  // 直接返回哈希映射本身
                case _ =>
                    println("Error: argument must be a hashmap")
                    Nil
            }
        }))
    }

    /**
     * 注册哈希映射相关的内置函数
     */
    public static func registerHashMap(env: Environment) {
        // (hashmap key1 val1 key2 val2 ...)
        // 创建哈希映射
        env.define("hashmap", NativeFunc({ args =>
            if (args.size % 2 != 0) {
                println("Error: hashmap requires even number of arguments")
                return Nil
            }

            let map = HashMap<String, LispValue>()
            var i = 0
            while (i < args.size) {
                let keyStr = lispValueToKey(args[i])
                map[keyStr] = args[i + 1]
                i = i + 2
            }

            LispValue.HashMap(map)
        }))

        // (hashmap-get map key)
        // 获取哈希映射中的值
        env.define("hashmap-get", NativeFunc({ args =>
            if (args.size != 2) {
                println("Error: hashmap-get requires 2 arguments")
                return Nil
            }

            match (args[0]) {
                case HashMap(map) =>
                    map.get(lispValueToKey(args[1])) ?? Nil
                case _ =>
                    println("Error: first argument must be a hashmap")
                    Nil
            }
        }))

        // (hashmap-set! map key value)
        // 设置哈希映射中的值（修改）
        env.define("hashmap-set!", NativeFunc({ args =>
            if (args.size != 3) {
                println("Error: hashmap-set! requires 3 arguments")
                return Nil
            }

            match (args[0]) {
                case HashMap(map) =>
                    map[lispValueToKey(args[1])] = args[2]
                    args[0]
                case _ =>
                    println("Error: first argument must be a hashmap")
                    Nil
            }
        }))

        // (hashmap-remove! map key)
        // 删除哈希映射中的键
        env.define("hashmap-remove!", NativeFunc({ args =>
            if (args.size != 2) {
                println("Error: hashmap-remove! requires 2 arguments")
                return Nil
            }

            match (args[0]) {
                case HashMap(map) =>
                    map.remove(lispValueToKey(args[1]))
                    args[0]
                case _ =>
                    println("Error: first argument must be a hashmap")
                    Nil
            }
        }))

        // (hashmap-contains? map key)
        // 检查哈希映射是否包含某个键
        env.define("hashmap-contains?", NativeFunc({ args =>
            if (args.size != 2) {
                println("Error: hashmap-contains? requires 2 arguments")
                return LispValue.Boolean(false)
            }

            match (args[0]) {
                case HashMap(map) =>
                    LispValue.Boolean(map.contains(lispValueToKey(args[1])))
                case _ =>
                    println("Error: first argument must be a hashmap")
                    LispValue.Boolean(false)
            }
        }))

        // (hashmap-keys map)
        // 获取哈希映射的所有键
        env.define("hashmap-keys", NativeFunc({ args =>
            if (args.size != 1) {
                println("Error: hashmap-keys requires 1 argument")
                return Nil
            }

            match (args[0]) {
                case HashMap(map) =>
                    let keys = ArrayList<LispValue>()
                    for ((key, _) in map) {
                        keys.add(Str(key))
                    }
                    makeList(keys.toArray())
                case _ =>
                    println("Error: argument must be a hashmap")
                    Nil
            }
        }))

        // (hashmap-values map)
        // 获取哈希映射的所有值
        env.define("hashmap-values", NativeFunc({ args =>
            if (args.size != 1) {
                println("Error: hashmap-values requires 1 argument")
                return Nil
            }

            match (args[0]) {
                case HashMap(map) =>
                    let values = ArrayList<LispValue>()
                    for ((_, value) in map) {
                        values.add(value)
                    }
                    makeList(values.toArray())
                case _ =>
                    println("Error: argument must be a hashmap")
                    Nil
            }
        }))

        // (hashmap-size map)
        // 获取哈希映射的大小
        env.define("hashmap-size", NativeFunc({ args =>
            if (args.size != 1) {
                println("Error: hashmap-size requires 1 argument")
                return Nil
            }

            match (args[0]) {
                case HashMap(map) =>
                    LispValue.Int(map.size)
                case _ =>
                    println("Error: argument must be a hashmap")
                    Nil
            }
        }))

        // (hashmap? value)
        // 判断是否为哈希映射
        env.define("hashmap?", NativeFunc({ args =>
            if (args.size != 1) {
                return LispValue.Boolean(false)
            }
            LispValue.Boolean(args[0].isHashMap())
        }))
    }
}
