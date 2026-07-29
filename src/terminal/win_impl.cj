package ystyle::xisp.terminal

@When[os == "Windows"]
let STD_INPUT_HANDLE: UInt32 = 0xFFFFFFF6
@When[os == "Windows"]
let STD_OUTPUT_HANDLE: UInt32 = 0xFFFFFFF5
@When[os == "Windows"]
let ENABLE_LINE_INPUT: UInt32 = 0x0002
@When[os == "Windows"]
let ENABLE_ECHO_INPUT: UInt32 = 0x0004
@When[os == "Windows"]
let ENABLE_PROCESSED_INPUT: UInt32 = 0x0001
@When[os == "Windows"]
let ENABLE_VIRTUAL_TERMINAL_PROCESSING: UInt32 = 0x0004
@When[os == "Windows"]
let ENABLE_PROCESSED_OUTPUT: UInt32 = 0x0001
@When[os == "Windows"]
let KEY_EVENT: UInt16 = 0x0001
@When[os == "Windows"]
let LEFT_CTRL: UInt32 = 0x0008
@When[os == "Windows"]
let RIGHT_CTRL: UInt32 = 0x0004
@When[os == "Windows"]
let LEFT_ALT: UInt32 = 0x0002
@When[os == "Windows"]
let RIGHT_ALT: UInt32 = 0x0001
@When[os == "Windows"]
let SHIFT_PRESSED: UInt32 = 0x0010

@When[os == "Windows"]
@C
foreign func GetStdHandle(nStdHandle: UInt32): CPointer<Byte>
@When[os == "Windows"]
@C
foreign func GetConsoleMode(hConsole: CPointer<Byte>, lpMode: CPointer<UInt32>): Int32
@When[os == "Windows"]
@C
foreign func SetConsoleMode(hConsole: CPointer<Byte>, dwMode: UInt32): Int32
@When[os == "Windows"]
@C
foreign func ReadConsoleInputW(hConsole: CPointer<Byte>, lpBuffer: CPointer<Byte>, nLength: UInt32, lpNumRead: CPointer<UInt32>): Int32
@When[os == "Windows"]
@C
foreign func GetConsoleScreenBufferInfo(hConsole: CPointer<Byte>, lpInfo: CPointer<Byte>): Int32

@When[os == "Windows"]
public class PlatformTerminal <: Terminal {
    private var rawMode: Bool = false
    private var stdinHandle: CPointer<Byte> = CPointer<Byte>(0)
    private var stdoutHandle: CPointer<Byte> = CPointer<Byte>(0)
    private var originalMode: UInt32 = 0

    public func enableRawMode(): Bool {
        if (this.rawMode) { return true }
        this.stdinHandle = GetStdHandle(STD_INPUT_HANDLE)
        this.stdoutHandle = GetStdHandle(STD_OUTPUT_HANDLE)
        let modePtr = unsafe { LibC.malloc<UInt32>(count: 1) }
        if (GetConsoleMode(this.stdinHandle, modePtr) == 0) {
            unsafe { LibC.free(modePtr) }
            return false
        }
        this.originalMode = unsafe { modePtr.read() }
        let newMode = this.originalMode & (!(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT))
        SetConsoleMode(this.stdinHandle, newMode)
        let outModePtr = unsafe { LibC.malloc<UInt32>(count: 1) }
        if (GetConsoleMode(this.stdoutHandle, outModePtr) != 0) {
            let outMode: UInt32 = unsafe { outModePtr.read() }
            SetConsoleMode(this.stdoutHandle, outMode | ENABLE_VIRTUAL_TERMINAL_PROCESSING | ENABLE_PROCESSED_OUTPUT)
        }
        unsafe { LibC.free(outModePtr) }
        unsafe { LibC.free(modePtr) }
        this.rawMode = true
        true
    }

    public func disableRawMode(): Bool {
        if (!this.rawMode) { return true }
        SetConsoleMode(this.stdinHandle, this.originalMode | ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT)
        this.rawMode = false
        true
    }

    public func isRawModeEnabled(): Bool { this.rawMode }

    public func readKey(): ?KeyEvent {
        let recBuf = unsafe { LibC.malloc<Byte>(count: 20) }
        let numRead = unsafe { LibC.malloc<UInt32>(count: 1) }
        while (true) {
            if (ReadConsoleInputW(this.stdinHandle, recBuf, 1, numRead) == 0) {
                unsafe { LibC.free(recBuf); LibC.free(numRead) }
                return None
            }
            let num: UInt32 = unsafe { numRead.read() }
            if (num == 0) {
                unsafe { LibC.free(recBuf); LibC.free(numRead) }
                return None
            }
            let b0: Byte = unsafe { (recBuf).read() }
            let b1: Byte = unsafe { (recBuf + 1).read() }
            let eventType = UInt16(b0) | (UInt16(b1) << 8)
            if (eventType != KEY_EVENT) { continue }
            let keyDown: Byte = unsafe { (recBuf + 4).read() }
            if ((keyDown & 0x01) == 0) { continue }
            let ch_l0: Byte = unsafe { (recBuf + 6).read() }
            let ch_l1: Byte = unsafe { (recBuf + 7).read() }
            let uChar = UInt16(ch_l0) | (UInt16(ch_l1) << 8)
            let vk_l0: Byte = unsafe { (recBuf + 10).read() }
            let vk_l1: Byte = unsafe { (recBuf + 11).read() }
            let vkCode = UInt16(vk_l0) | (UInt16(vk_l1) << 8)
            let ck0: UInt32 = UInt32(unsafe { (recBuf + 14).read() })
            let ck1: UInt32 = UInt32(unsafe { (recBuf + 15).read() })
            let ck2: UInt32 = UInt32(unsafe { (recBuf + 16).read() })
            let ck3: UInt32 = UInt32(unsafe { (recBuf + 17).read() })
            let ctrlState = ck0 | (ck1 << 8) | (ck2 << 16) | (ck3 << 24)
            let mods = parseWinMods(ctrlState)
            if (uChar != 0) {
                if (ctrlState & (LEFT_CTRL | RIGHT_CTRL) != 0 && uChar >= 1 && uChar <= 26) {
                    unsafe { LibC.free(recBuf); LibC.free(numRead) }
                    return Some(KeyEvent(KeyCode.Char(Rune(UInt32(uChar + 0x60))), KeyModifiers(KeyModifiers.CONTROL)))
                }
                unsafe { LibC.free(recBuf); LibC.free(numRead) }
                return Some(KeyEvent(KeyCode.Char(Rune(UInt32(uChar))), mods))
            }
            unsafe { LibC.free(recBuf); LibC.free(numRead) }
            if (vkCode == 0x08) { return Some(KeyEvent(KeyCode.Backspace, mods)) }
            if (vkCode == 0x09) { return Some(KeyEvent(KeyCode.Tab, mods)) }
            if (vkCode == 0x0D) { return Some(KeyEvent(KeyCode.Enter, mods)) }
            if (vkCode == 0x1B) { return Some(KeyEvent(KeyCode.Esc, mods)) }
            if (vkCode == 0x21) { return Some(KeyEvent(KeyCode.PageUp, mods)) }
            if (vkCode == 0x22) { return Some(KeyEvent(KeyCode.PageDown, mods)) }
            if (vkCode == 0x23) { return Some(KeyEvent(KeyCode.End, mods)) }
            if (vkCode == 0x24) { return Some(KeyEvent(KeyCode.Home, mods)) }
            if (vkCode == 0x25) { return Some(KeyEvent(KeyCode.Left, mods)) }
            if (vkCode == 0x26) { return Some(KeyEvent(KeyCode.Up, mods)) }
            if (vkCode == 0x27) { return Some(KeyEvent(KeyCode.Right, mods)) }
            if (vkCode == 0x28) { return Some(KeyEvent(KeyCode.Down, mods)) }
            if (vkCode == 0x2E) { return Some(KeyEvent(KeyCode.Delete, mods)) }
            if (vkCode == 0x2D) { return Some(KeyEvent(KeyCode.Insert, mods)) }
        }
    }

    public func getTerminalSize(): (Int64, Int64) {
        let buf = unsafe { LibC.malloc<Byte>(count: 22) }
        if (GetConsoleScreenBufferInfo(this.stdoutHandle, buf) == 0) {
            unsafe { LibC.free(buf) }
            return (80, 24)
        }
        let b8: Byte = unsafe { (buf + 8).read() }
        let b9: Byte = unsafe { (buf + 9).read() }
        let b10: Byte = unsafe { (buf + 10).read() }
        let b11: Byte = unsafe { (buf + 11).read() }
        let b12: Byte = unsafe { (buf + 12).read() }
        let b13: Byte = unsafe { (buf + 13).read() }
        let b14: Byte = unsafe { (buf + 14).read() }
        let b15: Byte = unsafe { (buf + 15).read() }
        unsafe { LibC.free(buf) }
        let left = Int64(UInt16(b8) | (UInt16(b9) << 8))
        let right = Int64(UInt16(b10) | (UInt16(b11) << 8))
        let top = Int64(UInt16(b12) | (UInt16(b13) << 8))
        let bottom = Int64(UInt16(b14) | (UInt16(b15) << 8))
        let cols = right - left + 1
        let rows = bottom - top + 1
        if (cols <= 0 || rows <= 0) { (80, 24) } else { (cols, rows) }
    }

    public func isTTY(): Bool {
        let modePtr = unsafe { LibC.malloc<UInt32>(count: 1) }
        let result = GetConsoleMode(this.stdinHandle, modePtr)
        unsafe { LibC.free(modePtr) }
        result != 0
    }
}

@When[os == "Windows"]
func rb(ptr: CPointer<Byte>, off: Int64): Byte { unsafe { (ptr + off).read() } }

@When[os == "Windows"]
func parseWinMods(ctrlState: UInt32): KeyModifiers {
    let mods = KeyModifiers()
    if (ctrlState & SHIFT_PRESSED != 0) { mods.insert(KeyModifiers.SHIFT) }
    if (ctrlState & (LEFT_CTRL | RIGHT_CTRL) != 0) { mods.insert(KeyModifiers.CONTROL) }
    if (ctrlState & (LEFT_ALT | RIGHT_ALT) != 0) { mods.insert(KeyModifiers.ALT) }
    mods
}
