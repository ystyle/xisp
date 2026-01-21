package ystyle::xisp.bridge

import ystyle::xisp.core.*
import std.collection.ArrayList
import std.fs.*
import std.io.*

/**
 * 桥接层管理器
 * 管理仓颉和 Lisp 之间的互操作
 */
public class Bridge {
    /**
     * 注册仓颉函数到 Lisp 环境
     * @param env Lisp 环境
     * @param name 函数名
     * @param handler 仓颉函数，接收 LispValue 列表，返回 LispValue
     */
    public static func registerFunc(env: Environment, name: String, handler: (ArrayList<LispValue>) -> LispValue) {
        env.define(name, NativeFunc(handler))
    }

    /**
     * 注册仓颉函数（带命名空间前缀）
     */
    public static func registerFuncWithNS(env: Environment, ns: String, name: String, handler: (ArrayList<LispValue>) -> LispValue) {
        let fullName = "${ns}:${name}"
        registerFunc(env, fullName, handler)
    }

    /**
     * 批量注册 std.io 模块
     */
    public static func registerStdIO(env: Environment) {
        // 文件读取
        registerFuncWithNS(env, "cangjie:io", "read-file", { args =>
            if (args.size > 0 && let Str(path) <- args[0]) {
                try {
                    let bytes = File.readFrom(path)
                    let content = String.fromUtf8(bytes)
                    Str(content)
                } catch (e: FSException) {
                    Str("Error: ${e.message}")
                } catch (e: Exception) {
                    Str("Error: ${e.message}")
                }
            } else {
                Str("Error: invalid arguments")
            }
        })

        // 文件写入
        registerFuncWithNS(env, "cangjie:io", "write-file", { args =>
            if (args.size >= 2 && let Str(path) <- args[0] && let Str(content) <- args[1]) {
                try {
                    // 如果文件已存在，先删除
                    if (exists(path)) {
                        remove(path)
                    }
                    // 创建新文件并写入
                    var file = File.create(path)
                    file.write(content.toArray())
                    file.close()
                    Str("Success: written to ${path}")
                } catch (e: FSException) {
                    Str("Error: ${e.message}")
                } catch (e: Exception) {
                    Str("Error: ${e.message}")
                }
            } else {
                Str("Error: invalid arguments")
            }
        })

        // 文件追加
        registerFuncWithNS(env, "cangjie:io", "append-file", { args =>
            if (args.size >= 2 && let Str(path) <- args[0] && let Str(content) <- args[1]) {
                try {
                    File.appendTo(path, content.toArray())
                    Str("Success: appended to ${path}")
                } catch (e: FSException) {
                    Str("Error: ${e.message}")
                } catch (e: Exception) {
                    Str("Error: ${e.message}")
                }
            } else {
                Str("Error: invalid arguments")
            }
        })
    }

    /**
     * 批量注册 std.fs 模块
     */
    public static func registerStdFS(env: Environment) {
        // 检查文件或目录是否存在
        registerFuncWithNS(env, "cangjie:fs", "exists?", { args =>
            if (args.size > 0 && let Str(path) <- args[0]) {
                try {
                    Boolean(exists(path))
                } catch (e: Exception) {
                    Boolean(false)
                }
            } else {
                Boolean(false)
            }
        })

        // 列出目录内容
        registerFuncWithNS(env, "cangjie:fs", "list-dir", { args =>
            if (args.size > 0 && let Str(path) <- args[0]) {
                try {
                    let files = Directory.readFrom(path)
                    // 将 FileInfo 数组转换为 Lisp 列表
                    let lispFiles = ArrayList<LispValue>()
                    for (fileInfo in files) {
                        // 获取文件名
                        lispFiles.add(Str(fileInfo.name))
                    }
                    list(lispFiles)
                } catch (e: FSException) {
                    Str("Error: ${e.message}")
                } catch (e: Exception) {
                    Str("Error: ${e.message}")
                }
            } else {
                Nil
            }
        })

        // 判断是否为目录
        registerFuncWithNS(env, "cangjie:fs", "directory?", { args =>
            if (args.size > 0 && let Str(path) <- args[0]) {
                try {
                    let fileInfo = FileInfo(path)
                    Boolean(fileInfo.isDirectory())
                } catch (e: Exception) {
                    Boolean(false)
                }
            } else {
                Boolean(false)
            }
        })

        // 判断是否为文件
        registerFuncWithNS(env, "cangjie:fs", "file?", { args =>
            if (args.size > 0 && let Str(path) <- args[0]) {
                try {
                    let fileInfo = FileInfo(path)
                    Boolean(fileInfo.isRegular())
                } catch (e: Exception) {
                    Boolean(false)
                }
            } else {
                Boolean(false)
            }
        })
    }

    /**
     * 注册所有桥接函数
     */
    public static func registerAll(env: Environment) {
        registerStdIO(env)
        registerStdFS(env)
    }
}
