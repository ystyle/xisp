package ystyle::xisp.terminal

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
