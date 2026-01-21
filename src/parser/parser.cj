package ystyle::xisp.parser

import ystyle::xisp.core.*
import std.collection.ArrayList

/**
 * 语法分析器（Parser）
 *
 * 将 Token 流转换为 LispValue AST
 */
public class Parser {
    /**
     * Token 列表
     */
    private var tokens: ArrayList<Token>

    /**
     * 当前位置
     */
    private var pos: Int64

    /**
     * 构造函数
     */
    public init(tokens: ArrayList<Token>) {
        this.tokens = tokens
        this.pos = 0
    }

    /**
     * 获取当前 Token
     */
    private func peek(): Token {
        if (this.pos < this.tokens.size) {
            this.tokens[this.pos]
        } else {
            Token.EOF
        }
    }

    /**
     * 获取之后的 Token
     */
    private func peekAhead(offset: Int64): Token {
        let newPos = this.pos + offset
        if (newPos < this.tokens.size) {
            this.tokens[newPos]
        } else {
            Token.EOF
        }
    }

    /**
     * 前进到下一个 Token
     */
    private func advance(): Token {
        let token = this.peek()
        this.pos = this.pos + 1
        token
    }

    /**
     * 检查当前 Token 是否为指定类型
     */
    private func check(tokenType: Token): Bool {
        this.peek() == tokenType
    }

    /**
     * 消费指定类型的 Token
     */
    private func consume(tokenType: Token): Token {
        if (this.check(tokenType)) {
            this.advance()
        } else {
            Token.EOF  // 错误情况，简化处理
        }
    }

    /**
     * 解析原子值
     */
    private func parseAtom(): LispValue {
        let token = this.advance()

        match (token) {
            case Token.Number(n) => Number(n)
            case Token.String(s) => Str(s)
            case Token.Boolean(b) => Boolean(b)
            case Token.Symbol(s) => Symbol(s)
            case _ => Nil  // 错误情况
        }
    }

    /**
     * 解析表达式
     */
    public func parseExpression(): LispValue {
        let token = this.peek()

        // 处理引用符号 '
        if (token == Token.Quote) {
            this.advance()  // 消费 Quote
            let expr = this.parseExpression()
            // 创建 (quote expr) 的等价形式
            consValue(Symbol("quote"), consValue(expr, Nil))
        } else if (token == Token.LeftParen) {
            this.parseList()
        } else {
            this.parseAtom()
        }
    }

    /**
     * 解析列表
     * 支持规则列表和不规则列表（点对）
     */
    private func parseList(): LispValue {
        this.advance()  // 消费 LeftParen

        // 检查是否为空列表
        if (this.peek() == Token.RightParen) {
            this.advance()  // 消费 RightParen
            Nil
        } else {
            this.parseListBody()
        }
    }

    /**
     * 解析列表体
     */
    private func parseListBody(): LispValue {
        let first = this.parseExpression()

        // 检查是否为点对
        if (this.peek() == Token.Dot) {
            this.advance()  // 消费 Dot
            let cdr = this.parseExpression()
            if (this.peek() == Token.RightParen) {
                this.advance()  // 消费 RightParen
            }
            // 创建点对
            Cons(ConsCell(first, cdr))
        } else if (this.peek() == Token.RightParen) {
            this.advance()  // 消费 RightParen
            // 单元素列表
            consValue(first, Nil)
        } else {
            // 递归解析剩余元素
            let rest = this.parseListBody()
            consValue(first, rest)
        }
    }

    /**
     * 解析所有表达式
     */
    public func parse(): ArrayList<LispValue> {
        let exprs = ArrayList<LispValue>()

        while (this.peek() != Token.EOF) {
            exprs.add(this.parseExpression())
        }

        exprs
    }

    /**
     * 解析单个表达式（入口函数）
     */
    public static func parseFromString(input: String): ArrayList<LispValue> {
        let lexer = Lexer(input)
        let tokens = lexer.tokenize()
        let parser = Parser(tokens)
        parser.parse()
    }

    /**
     * 解析单个表达式（返回第一个）
     */
    public static func parseSingleFromString(input: String): LispValue {
        let exprs = parseFromString(input)
        if (exprs.size > 0) {
            exprs[0]
        } else {
            Nil
        }
    }
}
