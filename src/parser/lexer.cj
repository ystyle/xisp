package ystyle::xisp.parser

import std.collection.ArrayList
import std.convert.*
import std.unicode.UnicodeRuneExtension  // 用于 Unicode 字母判断（支持中文）

/**
 * 词法分析器（Lexer）
 *
 * 将 Lisp 源代码字符串转换为 Token 流
 */
public class Lexer {
    /**
     * 输入的 Rune 数组
     */
    private var runeArray: Array<Rune>

    /**
     * 当前位置
     */
    private var pos: Int64

    /**
     * 构造函数
     */
    public init(input: String) {
        this.runeArray = input.toRuneArray()
        this.pos = 0
    }

    /**
     * 获取当前位置的 Rune
     */
    private func peek(): ?Rune {
        if (this.pos < this.runeArray.size) {
            Some(this.runeArray[this.pos])
        } else {
            None
        }
    }

    /**
     * 获取当前位置之后的 Rune
     */
    private func peekAhead(offset: Int64): ?Rune {
        let newPos = this.pos + offset
        if (newPos < this.runeArray.size) {
            Some(this.runeArray[newPos])
        } else {
            None
        }
    }

    /**
     * 前进一个 Rune
     */
    private func advance(): ?Rune {
        let r = this.peek()
        this.pos = this.pos + 1
        r
    }

    /**
     * 跳过空白字符
     */
    private func skipWhitespace() {
        while (let Some(r) <- this.peek()) {
            if (!r.isAsciiWhiteSpace()) {
                break
            }
            this.advance()
        }
    }

    /**
     * 跳过注释（分号到行尾）
     */
    private func skipComment() {
        while (let Some(r) <- this.peek()) {
            this.advance()
            if (r == r'\n') {
                break
            }
        }
    }

    /**
     * 跳过空白和注释
     */
    private func skipWhitespaceAndComments() {
        while (true) {
            this.skipWhitespace()
            if (let Some(r) <- this.peek() && r == r';') {
                this.advance()
                this.skipComment()
            } else {
                break
            }
        }
    }

    /**
     * 读取数字
     * 支持整数和小数，包括负数
     * 前提：当前位置是数字或负号后跟数字
     */
    private func readNumber(): Token {
        let start = this.pos

        // 处理负号
        if (let Some(r) <- this.peek() && r == r'-') {
            this.advance()
        }

        // 读取整数部分
        while (let Some(r) <- this.peek()) {
            if (r.isAsciiNumber()) {
                this.advance()
            } else {
                break
            }
        }

        // 读取小数部分
        if (let Some(r1) <- this.peek() && r1 == r'.') {
            if (let Some(r2) <- this.peekAhead(1) && r2.isAsciiNumber()) {
                this.advance()  // 跳过 '.'
                while (let Some(r) <- this.peek() && r.isAsciiNumber()) {
                    this.advance()
                }
            }
        }

        let runeSlice = this.runeArray[start..this.pos]
        let numStr = String(runeSlice)
        let num = Float64.parse(numStr)
        Token.Number(num)
    }

    /**
     * 判断是否为符号字符
     */
    private func isSymbolChar(r: Rune): Bool {
        r == r'_' || r == r'+' || r == r'-' || r == r'*' || r == r'/' ||
        r == r'<' || r == r'>' || r == r'=' || r == r'!' || r == r'?' ||
        r == r'%' || r == r'$' || r == r'#' || r == r'@' || r == r':' ||
        r == r'[' || r == r']' ||
        r.isAsciiNumber() || r.isLetter()  // 使用 isLetter() 支持 Unicode（包括中文）
    }

    /**
     * 读取符号
     * 符号可以包含字母、数字和特殊字符（+ - * / < > = ! ? _ 等）
     */
    private func readSymbol(): Token {
        let start = this.pos

        while (let Some(r) <- this.peek()) {
            if (this.isSymbolChar(r)) {
                this.advance()
            } else {
                break
            }
        }

        let runeSlice = this.runeArray[start..this.pos]
        let symStr = String(runeSlice)

        // 检查是否为布尔值
        if (symStr == "#t" || symStr == "#true") {
            return Token.Boolean(true)
        } else if (symStr == "#f" || symStr == "#false") {
            return Token.Boolean(false)
        }

        Token.Symbol(symStr)
    }

    /**
     * 读取字符串
     * 支持转义字符
     */
    private func readString(): Token {
        this.advance()  // 跳过开始的 '"'

        var result = ""
        while (let Some(r) <- this.peek()) {
            if (r == r'"') {
                this.advance()  // 跳过结束的 '"'
                break
            }

            this.advance()
            if (r == r'\\') {
                let nextR = this.advance()
                if (let Some(nr) <- nextR) {
                    // 处理转义字符
                    if (nr == r'n') {
                        result += "\n"
                    } else if (nr == r't') {
                        result += "\t"
                    } else if (nr == r'r') {
                        result += "\r"
                    } else if (nr == r'"') {
                        result += "\""
                    } else if (nr == r'\\') {
                        result += "\\"
                    } else {
                        result += nr.toString()
                    }
                }
            } else {
                result += r.toString()
            }
        }

        Token.String(result)
    }

    /**
     * 读取下一个 Token
     */
    public func nextToken(): Token {
        this.skipWhitespaceAndComments()

        if (let Some(r) <- this.peek()) {
            if (r == r'(') {
                this.advance()
                Token.LeftParen
            } else if (r == r')') {
                this.advance()
                Token.RightParen
            } else if (r == r'.') {
                this.advance()
                Token.Dot
            } else if (r == r'\'') {
                this.advance()
                Token.Quote
            } else if (r == r'"') {
                this.readString()
            } else if (r.isAsciiNumber()) {
                this.readNumber()
            } else if (r == r'-') {
                // 检查是否是负数
                if (let Some(nextR) <- this.peekAhead(1) && nextR.isAsciiNumber()) {
                    this.readNumber()
                } else {
                    this.readSymbol()
                }
            } else {
                this.readSymbol()
            }
        } else {
            Token.EOF
        }
    }

    /**
     * 获取所有 Token
     */
    public func tokenize(): ArrayList<Token> {
        let tokens = ArrayList<Token>()
        var token = this.nextToken()

        while (token != Token.EOF) {
            tokens.add(token)
            token = this.nextToken()
        }

        tokens
    }
}
