package ystyle::xisp.terminal

@When[os != "Windows"]
@C
foreign func tcgetattr(fd: Int32, buf: CPointer<Byte>): Int32

@When[os != "Windows"]
@C
foreign func tcsetattr(fd: Int32, act: Int32, buf: CPointer<Byte>): Int32

@When[os != "Windows"]
@C
foreign func cfmakeraw(buf: CPointer<Byte>): Int32

@When[os != "Windows"]
@C
foreign func ioctl(fd: Int32, req: UIntNative, buf: CPointer<Byte>): Int32

@When[os != "Windows"]
@C
foreign func read(fd: Int32, buf: CPointer<Byte>, count: UIntNative): IntNative

@When[os != "Windows"]
@C
foreign func isatty(fd: Int32): Int32

@When[os != "Windows"]
let TIOCGWINSZ: UIntNative = 0x5413

@When[os != "Windows"]
let STDIN_FD: Int32 = 0

@When[os != "Windows"]
let TCSANOW: Int32 = 0

@When[os != "Windows"]
public class PlatformTerminal <: Terminal {
    private var rawMode: Bool = false
    private var originalBuf: CPointer<Byte> = unsafe { LibC.malloc<Byte>(count: 64) }
    private var readBuf: CPointer<Byte> = unsafe { LibC.malloc<Byte>(count: 64) }
    private var readLen: Int64 = 0
    private var readPos: Int64 = 0

    public func enableRawMode(): Bool {
        if (this.rawMode) { return true }
        this.readLen = 0; this.readPos = 0
        let buf = unsafe { LibC.malloc<Byte>(count: 64) }
        if (unsafe { tcgetattr(STDIN_FD, buf) } != 0) {
            unsafe { LibC.free(buf) }
            return false
        }
        for (i in 0..64) {
            unsafe { (this.originalBuf + i).write((buf + i).read()) }
        }
        unsafe { cfmakeraw(buf) }
        let result = unsafe { tcsetattr(STDIN_FD, TCSANOW, buf) }
        unsafe { LibC.free(buf) }
        if (result != 0) { return false }
        this.rawMode = true
        true
    }

    public func disableRawMode(): Bool {
        if (!this.rawMode) { return true }
        let result = unsafe { tcsetattr(STDIN_FD, TCSANOW, this.originalBuf) }
        this.rawMode = false
        result == 0
    }

    public func isRawModeEnabled(): Bool { this.rawMode }

    public func readKey(): ?KeyEvent {
        if (this.readPos >= this.readLen) {
            this.readLen = Int64(unsafe { read(STDIN_FD, this.readBuf, 64) })
            this.readPos = 0
        }
        if (this.readLen <= 0) { return None }
        let b0: Byte = unsafe { (this.readBuf + this.readPos).read() }
        if (b0 == 0x1B) {
            let tryLen = scanSeqLen(this.readBuf, this.readPos, this.readLen)
            if (tryLen < 0) {
                if (this.readPos > 0 || this.readLen >= 64) {
                    this.readPos = this.readPos + 1
                    return Some(KeyEvent(KeyCode.Esc, KeyModifiers()))
                }
                let remaining = this.readLen
                for (i in 0..remaining) {
                    let val: Byte = unsafe { (this.readBuf + this.readPos + i).read() }
                    unsafe { (this.readBuf + i).write(val) }
                }
                let more = Int64(unsafe { read(STDIN_FD, this.readBuf + remaining, UIntNative(64 - remaining)) })
                this.readLen = remaining + more
                this.readPos = 0
                if (this.readLen <= 0) { return None }
                let tryAgain = scanSeqLen(this.readBuf, this.readPos, this.readLen)
                if (tryAgain < 0) {
                    this.readPos = this.readPos + 1
                    return Some(KeyEvent(KeyCode.Esc, KeyModifiers()))
                }
                let event = parseSeq(this.readBuf, this.readPos, tryAgain)
                this.readPos = this.readPos + tryAgain
                return event
            }
            let event = parseSeq(this.readBuf, this.readPos, tryLen)
            this.readPos = this.readPos + tryLen
            return event
        }
        this.readPos = this.readPos + 1
        if (b0 == 0x7F || b0 == 0x08) { return Some(KeyEvent(KeyCode.Backspace, KeyModifiers())) }
        if (b0 == 0x09) { return Some(KeyEvent(KeyCode.Tab, KeyModifiers())) }
        if (b0 == 0x0A || b0 == 0x0D) { return Some(KeyEvent(KeyCode.Enter, KeyModifiers())) }
        if (b0 >= 0x01 && b0 <= 0x1A) {
            return Some(KeyEvent(KeyCode.Char(Rune(UInt32(b0 + 0x60))), KeyModifiers(KeyModifiers.CONTROL)))
        }
        if (b0 >= 0x20) {
            return Some(KeyEvent(KeyCode.Char(Rune(UInt32(b0))), KeyModifiers()))
        }
        None
    }

    public func getTerminalSize(): (Int64, Int64) {
        let buf = unsafe { LibC.malloc<Byte>(count: 8) }
        if (unsafe { ioctl(STDIN_FD, TIOCGWINSZ, buf) } != 0) { unsafe { LibC.free(buf) }; return (80, 24) }
        let b0: Byte = unsafe { (buf).read() }
        let b1: Byte = unsafe { (buf + 1).read() }
        let b2: Byte = unsafe { (buf + 2).read() }
        let b3: Byte = unsafe { (buf + 3).read() }
        unsafe { LibC.free(buf) }
        let rows = Int64(UInt16(UInt8(b0)) | (UInt16(UInt8(b1)) << 8))
        let cols = Int64(UInt16(UInt8(b2)) | (UInt16(UInt8(b3)) << 8))
        if (rows <= 0 || cols <= 0) { (80, 24) } else { (cols, rows) }
    }

    public func isTTY(): Bool { unsafe { isatty(STDIN_FD) } != 0 }
}

@When[os != "Windows"]


func scanSeqLen(buf: CPointer<Byte>, off: Int64, max: Int64): Int64 {
    if (max - off < 2) { return -1 }
    let b1: Byte = unsafe { (buf + off + 1).read() }
    if (b1 == 0x5B || b1 == 0x4F) {
        var i: Int64 = 2
        while (off + i < max) {
            let cb: Byte = unsafe { (buf + off + i).read() }
            i = i + 1
            if (cb >= 0x40 && cb <= 0x7E) { return i }
        }
        return -1
    }
    if (b1 >= 0x01 && b1 <= 0x1F || b1 >= 0x20) { return 2 }
    return -1
}

func parseSeq(buf: CPointer<Byte>, off: Int64, n: Int64): Option<KeyEvent> {
    let b1: Byte = unsafe { (buf + off + 1).read() }
    if (b1 == 0x5B || b1 == 0x4F) {
        if (n < 3) { return Some(KeyEvent(KeyCode.Esc, KeyModifiers())) }
        let b2: Byte = unsafe { (buf + off + 2).read() }
        if (b2 == 0x41) { return Some(KeyEvent(KeyCode.Up, KeyModifiers())) }
        if (b2 == 0x42) { return Some(KeyEvent(KeyCode.Down, KeyModifiers())) }
        if (b2 == 0x43) { return Some(KeyEvent(KeyCode.Right, KeyModifiers())) }
        if (b2 == 0x44) { return Some(KeyEvent(KeyCode.Left, KeyModifiers())) }
        if (b2 == 0x48) { return Some(KeyEvent(KeyCode.Home, KeyModifiers())) }
        if (b2 == 0x46) { return Some(KeyEvent(KeyCode.End, KeyModifiers())) }
        if (b2 == 0x5A) { return Some(KeyEvent(KeyCode.BackTab, KeyModifiers(KeyModifiers.SHIFT))) }
        if (n < 4) { return None }
        let b3: Byte = unsafe { (buf + off + 3).read() }
        if (b2 == 0x31 && b3 == 0x7E) { return Some(KeyEvent(KeyCode.Home, KeyModifiers())) }
        if (b2 == 0x32 && b3 == 0x7E) { return Some(KeyEvent(KeyCode.Insert, KeyModifiers())) }
        if (b2 == 0x33 && b3 == 0x7E) { return Some(KeyEvent(KeyCode.Delete, KeyModifiers())) }
        if (b2 == 0x34 && b3 == 0x7E) { return Some(KeyEvent(KeyCode.End, KeyModifiers())) }
        if (b2 == 0x35 && b3 == 0x7E) { return Some(KeyEvent(KeyCode.PageUp, KeyModifiers())) }
        if (b2 == 0x36 && b3 == 0x7E) { return Some(KeyEvent(KeyCode.PageDown, KeyModifiers())) }
        if (n < 6) { return None }
        if (b2 == 0x31 && b3 == 0x3B) {
            let modCode: Byte = unsafe { (buf + off + 4).read() }
            let keyByte: Byte = unsafe { (buf + off + 5).read() }
            let mods = if (modCode == 0x32) { KeyModifiers(KeyModifiers.SHIFT) }
                else if (modCode == 0x33) { KeyModifiers(KeyModifiers.ALT) }
                else if (modCode == 0x35) { KeyModifiers(KeyModifiers.CONTROL) }
                else { KeyModifiers() }
            if (keyByte == 0x41) { return Some(KeyEvent(KeyCode.Up, mods)) }
            if (keyByte == 0x42) { return Some(KeyEvent(KeyCode.Down, mods)) }
            if (keyByte == 0x43) { return Some(KeyEvent(KeyCode.Right, mods)) }
            if (keyByte == 0x44) { return Some(KeyEvent(KeyCode.Left, mods)) }
            if (keyByte == 0x48) { return Some(KeyEvent(KeyCode.Home, mods)) }
            if (keyByte == 0x46) { return Some(KeyEvent(KeyCode.End, mods)) }
        }
        return None
    }
    if (b1 >= 0x01 && b1 <= 0x1F) {
        return Some(KeyEvent(KeyCode.Char(Rune(UInt32(b1 + 0x40))), KeyModifiers(KeyModifiers.ALT)))
    }
    Some(KeyEvent(KeyCode.Char(Rune(UInt32(b1))), KeyModifiers(KeyModifiers.ALT)))
}
