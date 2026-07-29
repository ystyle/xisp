package ystyle::xisp.repl

import std.collection.ArrayList
import std.fs.*
import std.io.*

/**
 * HistoryManager - REPL 历史记录管理
 * 内存环形缓冲 + ~/.xisp_history 文件持久化
 */
public class HistoryManager {
    private var entries: ArrayList<String> = ArrayList()
    private let maxEntries: Int64
    private var currentIndex: Int64 = -1
    private var savedCount: Int64 = 0

    public init(maxEntries!: Int64 = 1000) {
        this.maxEntries = maxEntries
    }

    /**
     * 从文件加载历史
     */
    public func load(path: String): Unit {
        if (!exists(Path(path))) {
            return
        }
        try {
            let bytes = File.readFrom(path)
            if (bytes.size == 0) {
                return
            }
            let content = String.fromUtf8(bytes)
            let lines = content.split("\n")
            for (line in lines) {
                let trimmed = line.trimAscii()
                if (trimmed.size > 0) {
                    entries.add(trimmed)
                }
            }
            savedCount = entries.size
            currentIndex = entries.size
        } catch (_: Error) {}
    }

    /**
     * 追加保存新条目到文件
     */
    public func save(path: String): Unit {
        if (savedCount >= entries.size) {
            return
        }
        try {
            let file = File(path, Append)
            for (i in savedCount..entries.size) {
                file.write((entries[i] + "\n").toArray())
            }
            file.close()
            savedCount = entries.size
        } catch (_: Error) {}
    }

    /**
     * 添加条目（去重）
     */
    public func add(entry: String): Unit {
        if (maxEntries <= 0) {
            return
        }
        let trimmed = entry.trimAscii()
        if (trimmed.size == 0) {
            return
        }
        if (entries.size > 0 && entries[entries.size - 1] == trimmed) {
            return
        }
        if (entries.size >= maxEntries) {
            entries.remove(at: 0)
        }
        entries.add(trimmed)
        currentIndex = entries.size
    }

    /**
     * 历史导航
     * @param offset -1 上一条，+1 下一条
     * @return ?String 条目内容
     */
    public func navigate(offset: Int64): ?String {
        let newIndex = currentIndex + offset
        if (newIndex >= 0 && newIndex < entries.size) {
            currentIndex = newIndex
            return Some(entries[currentIndex])
        }
        None
    }

    /**
     * 重置导航索引到最新位置
     */
    public func resetNavigation(): Unit {
        currentIndex = entries.size
    }

    /**
     * 获取所有条目
     */
    public func getAll(): ArrayList<String> {
        entries
    }

    /**
     * 获取当前导航索引
     */
    public func getCurrentIndex(): Int64 {
        currentIndex
    }
}
