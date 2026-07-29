package ystyle::xisp.terminal

public interface Terminal {
    func enableRawMode(): Bool
    func disableRawMode(): Bool
    func isRawModeEnabled(): Bool
    func readKey(): ?KeyEvent
    func getTerminalSize(): (Int64, Int64)
    func isTTY(): Bool
}

public func createTerminal(): Terminal { PlatformTerminal() }

public func moveTo(col: Int64, row: Int64): Unit {
    print("\u{1b}[${row + 1};${col + 1}H")
}

public func clearLine(): Unit {
    print("\u{1b}[2K")
}

public func clearScreen(): Unit {
    print("\u{1b}[2J")
}

public func cursorHide(): Unit {
    print("\u{1b}[?25l")
}

public func cursorShow(): Unit {
    print("\u{1b}[?25h")
}
