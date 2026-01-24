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
        var hasDecimalPoint = false

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
                hasDecimalPoint = true
                while (let Some(r) <- this.peek() && r.isAsciiNumber()) {
                    this.advance()
                }
            }
        }

        let runeSlice = this.runeArray[start..this.pos]
        let numStr = String(runeSlice)

        if (hasDecimalPoint) {
            // 浮点数
            let num = Float64.parse(numStr)
            Token.Float(num)
        } else {
            // 整数
            let num = Int64.parse(numStr)
            Token.Int(num)
        }
    }

    /**
     * 判断是否为符号字符
     */
    private func isSymbolChar(r: Rune): Bool {
        r == r'_' || r == r'+' || r == r'-' || r == r'*' || r == r'/' ||
        r == r'<' || r == r'>' || r == r'=' || r == r'!' || r == r'?' ||
        r == r'%' || r == r'$' || r == r'#' || r == r'@' || r == r':' ||
        r == r'&' || r == r'.' ||  // 添加 & 符号用于 rest 参数，添加 . 用于模块符号
        r.isAsciiNumber() || r.isLetter()  // 使用 isLetter() 支持 Unicode（包括中文）
        // 移除了 []{} 因为它们是独立的 token
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
     * 读取插值字符串
     * 格式：#"text #{expr} more text"
     * 返回包含字符串片段和插值表达式的标记列表
     */
    private func readInterpolateString(): Token {
        this.advance()  // 跳过开始的 '"'

        var parts = ArrayList<String>()
        var hasInterpolation = false

        while (let Some(r) <- this.peek()) {
            if (r == r'"') {
                this.advance()  // 跳过结束的 '"'
                break
            }

            // 检查插值开始标记 #{
            if (r == r'#') {
                if (let Some(nextR) <- this.peekAhead(1) && nextR == r'{') {
                    // 保存之前的字符串片段
                    parts.add("")
                    hasInterpolation = true

                    // 跳过 #{
                    this.advance()
                    this.advance()

                    // 读取插值表达式直到 }
                    var exprDepth = 1
                    var exprStr = ""
                    while (exprDepth > 0 && let Some(er) <- this.peek()) {
                        this.advance()
                        if (er == r'{') {
                            exprDepth = exprDepth + 1
                            exprStr += er.toString()
                        } else if (er == r'}') {
                            exprDepth = exprDepth - 1
                            if (exprDepth > 0) {
                                exprStr += er.toString()
                            }
                        } else {
                            exprStr += er.toString()
                        }
                    }
                    parts.add("#{" + exprStr + "}")

                    // 继续读取字符串
                    var remainingStr = ""
                    while (let Some(rr) <- this.peek() && rr != r'"' && rr != r'#') {
                        this.advance()
                        remainingStr += rr.toString()
                    }
                    parts.add(remainingStr)
                    continue
                }
            }

            this.advance()
            if (r == r'\\') {
                let nextR = this.advance()
                if (let Some(nr) <- nextR) {
                    // 处理转义字符
                    if (nr == r'n') {
                        parts[parts.size - 1] += "\n"
                    } else if (nr == r't') {
                        parts[parts.size - 1] += "\t"
                    } else if (nr == r'r') {
                        parts[parts.size - 1] += "\r"
                    } else if (nr == r'"') {
                        parts[parts.size - 1] += "\""
                    } else if (nr == r'\\') {
                        parts[parts.size - 1] += "\\"
                    } else {
                        parts[parts.size - 1] += nr.toString()
                    }
                }
            } else {
                if (parts.size == 0) {
                    parts.add(r.toString())
                } else {
                    parts[parts.size - 1] += r.toString()
                }
            }
        }

        // 将所有部分组合成一个特殊标记的字符串
        // 格式: ::INTERPOLATE::part1::#{expr1}::part2::#{expr2}::part3
        var combined = "::INTERPOLATE::"
        for (i in 0..parts.size) {
            combined += parts[i]
            if (i < parts.size - 1) {
                combined += "::"
            }
        }
        Token.String(combined)
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
            } else if (r == r'`') {
                this.advance()
                Token.Backquote
            } else if (r == r',') {
                this.advance()
                // 检查是否是 ,@
                if (let Some(nextR) <- this.peek() && nextR == r'@') {
                    this.advance()
                    Token.CommaAt
                } else {
                    Token.Comma
                }
            } else if (r == r'"') {
                this.readString()
            } else if (r == r'[') {
                this.advance()
                Token.VectorStart
            } else if (r == r']') {
                this.advance()
                Token.VectorEnd
            } else if (r == r'{') {
                this.advance()
                Token.HashMapStart
            } else if (r == r'}') {
                this.advance()
                Token.HashMapEnd
            } else if (r == r'#') {
                this.advance()
                // 检查下一个字符
                if (let Some(nextR) <- this.peek()) {
                    if (nextR == r'{') {
                        this.advance()
                        Token.HashSetStart
                    } else if (nextR == r'"') {
                        this.readInterpolateString()
                    } else if (nextR == r't' || nextR == r'f') {
                        // 布尔值 #t #f 或 #true #false
                        this.pos = this.pos - 1  // 回退到 #
                        this.readSymbol()
                    } else {
                        // 其他情况，将 # 和后续字符作为符号
                        this.pos = this.pos - 1  // 回退
                        this.readSymbol()
                    }
                } else {
                    // 只有 #，作为符号
                    this.pos = this.pos - 1
                    this.readSymbol()
                }
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
