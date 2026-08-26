package ystyle::xisp.terminal

import std.collection.ArrayList

public enum KeyCode {
    | Backspace | Enter | Left | Right | Up | Down
    | Home | End | PageUp | PageDown | Tab | BackTab
    | Delete | Insert
    | Char(Rune)
    | Null | Esc
}

public class KeyModifiers {
    public var data: UInt8
    public static let SHIFT: UInt8 = 0b0000_0001
    public static let CONTROL: UInt8 = 0b0000_0010
    public static let ALT: UInt8 = 0b0000_0100
    public static let SUPER: UInt8 = 0b0000_1000
    public static let NONE: UInt8 = 0b0000_0000

    public init() { this.data = 0 }
    public init(data: UInt8) { this.data = data }
    public func contains(flag: UInt8): Bool { (this.data & flag) != 0 }
    public func insert(flag: UInt8): Unit { this.data = this.data | flag }
    public func remove(flag: UInt8): Unit { this.data = this.data & (flag ^ UInt8.Max) }
}

public class KeyEvent {
    public let code: KeyCode
    public let modifiers: KeyModifiers

    public init(code: KeyCode, modifiers: KeyModifiers) {
        this.code = code
        this.modifiers = modifiers
    }
}

// ==================== UTF-8 多字节解码（终端输入中文/emoji 用） ====================
// 由 UTF-8 引导字节推断序列总长（1-4）；非法引导（孤立续字节 0x80-0xC1、过窄 0xC0-0xC1、超范围 0xF5+）→ 1（按单字节容错）
public func utf8SeqLen(lead: Byte): Int64 {
    if (lead < 0x80) { return 1 }
    if (lead >= 0xC2 && lead <= 0xDF) { return 2 }
    if (lead >= 0xE0 && lead <= 0xEF) { return 3 }
    if (lead >= 0xF0 && lead <= 0xF4) { return 4 }
    1
}

// 终端显示宽度：CJK/宽字符 = 2，其余 = 1（简化版 wcwidth，覆盖中文/日文假名/谚文/emoji）
public func runeDisplayWidth(r: Rune): Int64 {
    let v = UInt32(r)
    if (v >= 0x1100 && v <= 0x115F) { return 2 }   // Hangul Jamo
    if (v >= 0x2E80 && v <= 0xA4CF) { return 2 }   // CJK 部首/符号/假名/统一表意/谚文
    if (v >= 0xAC00 && v <= 0xD7A3) { return 2 }   // Hangul 音节
    if (v >= 0xF900 && v <= 0xFAFF) { return 2 }   // CJK 兼容表意
    if (v >= 0xFE30 && v <= 0xFE4F) { return 2 }   // CJK 兼容形式
    if (v >= 0xFF00 && v <= 0xFF60) { return 2 }   // 全角 ASCII/片假名
    if (v >= 0xFFE0 && v <= 0xFFE6) { return 2 }   // 全角符号
    if (v >= 0x1F300 && v <= 0x1FAFF) { return 2 } // emoji/杂项符号与补充
    1
}

// 从字节序列解码多字节 UTF-8 字符（seq.size = 2..4）；续字节非法或长度不足 → 替换符 U+FFFD
public func decodeUtf8Rune(seq: ArrayList<Byte>): Rune {
    if (seq.size < 2) { return Rune(0xFFFD) }
    let b0 = UInt32(seq[0])
    let b1 = UInt32(seq[1])
    if ((b1 & 0xC0) != 0x80) { return Rune(0xFFFD) }
    if (seq.size == 2) {
        return Rune(((b0 & 0x1F) << 6) | (b1 & 0x3F))
    }
    let b2 = UInt32(seq[2])
    if ((b2 & 0xC0) != 0x80) { return Rune(0xFFFD) }
    if (seq.size == 3) {
        return Rune(((b0 & 0x0F) << 12) | ((b1 & 0x3F) << 6) | (b2 & 0x3F))
    }
    let b3 = UInt32(seq[3])
    if ((b3 & 0xC0) != 0x80) { return Rune(0xFFFD) }
    Rune(((b0 & 0x07) << 18) | ((b1 & 0x3F) << 12) | ((b2 & 0x3F) << 6) | (b3 & 0x3F))
}
