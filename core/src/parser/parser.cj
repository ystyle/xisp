package ystyle::xisp.parser

import ystyle::xisp.types.*
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
     * 严格模式：未闭合的括号/向量/哈希字面量记为语法错误，
     * 而不是静默补全为完整表达式。默认关闭——REPL 的连续输入
     * 与既有调用方依赖宽松行为。
     */
    private var strict: Bool = false

    /**
     * 严格模式下记录的首个语法错误。非空表示本次解析未完整覆盖输入。
     */
    private var syntaxError: Option<XispError> = None

    /**
     * 构造函数
     */
    public init(tokens: ArrayList<Token>) {
        this.tokens = tokens
        this.pos = 0
    }

    /**
     * 启用或关闭严格模式。见 {@link strict}。
     */
    public func setStrict(strict: Bool): Unit {
        this.strict = strict
    }

    /**
     * 严格模式下记录的首个语法错误；未出错或非严格模式时为 None。
     */
    public func getSyntaxError(): Option<XispError> {
        this.syntaxError
    }

    /**
     * 记录语法错误（仅首次，保持最早的出错位置）。
     */
    private func recordSyntaxError(message: String): Unit {
        if (this.strict && this.syntaxError.isNone()) {
            this.syntaxError = Some(XispError(ErrorType.SyntaxError, message))
        }
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
            case Token.Int(i) => LispValue.Int(i)
            case Token.Float(f) => LispValue.Float(f)
            case Token.String(s) => this.parseStringOrInterpolation(s)
            case Token.Boolean(b) => Boolean(b)
            case Token.Keyword(k) => Symbol(":${k}")  // 关键字是自求值符号
            case Token.Symbol(s) => Symbol(s)
            case Token.Dot => Symbol(".")  // 点号应该被解析为符号 "."
            case _ => Nil  // 错误情况
        }
    }

    /**
     * 解析字符串或插值字符串
     * 如果是普通字符串，返回 Str
     * 如果是插值字符串（以 ::INTERPOLATE:: 开头），转换为 (cangjie::interpolate ...)
     */
    private func parseStringOrInterpolation(s: String): LispValue {
        if (s.startsWith("::INTERPOLATE::")) {
            // 插值字符串，解析并转换为函数调用
            // 格式: ::INTERPOLATE::part1::#{expr1}::part2::#{expr2}::part3
            let runeArray = s.toRuneArray()
            let prefixLen = 15  // "::INTERPOLATE::" 的长度
            let contentRunes = runeArray[prefixLen..runeArray.size]
            let content = String(contentRunes)
            let parts = content.split("::")

            // 创建 (cangjie::interpolate ...)
            let result = consValue(Symbol("cangjie::interpolate"), Nil)
            var current = result

            for (part in parts) {
                if (part.startsWith("#{") && part.endsWith("}")) {
                    // 这是插值表达式，提取 #{...} 内的表达式
                    let partRunes = part.toRuneArray()
                    let exprStr = String(partRunes[2..partRunes.size - 1])

                    // 递归解析表达式
                    let exprLexer = Lexer(exprStr)
                    let exprTokens = exprLexer.tokenize()

                    // 如果有多个 token 且不以括号开头，自动包裹在括号中
                    let finalExprStr = if (exprTokens.size > 1 && !exprStr.startsWith("(")) {
                        "(${exprStr})"
                    } else {
                        exprStr
                    }

                    // 重新解析
                    let finalLexer = Lexer(finalExprStr)
                    let finalTokens = finalLexer.tokenize()
                    if (finalTokens.size > 0) {
                        let exprParser = Parser(finalTokens)
                        let parsedExpr = exprParser.parseExpression()

                        // 添加到结果列表
                        let newCell = Cons(ConsCell(parsedExpr, Nil))
                        match (current) {
                            case Cons(cell) => cell.cdr = newCell
                            case _ => ()
                        }
                        current = newCell
                    }
                } else {
                    // 普通字符串片段
                    let newCell = Cons(ConsCell(Str(part), Nil))
                    match (current) {
                        case Cons(cell) => cell.cdr = newCell
                        case _ => ()
                    }
                    current = newCell
                }
            }

            result
        } else {
            // 普通字符串
            Str(s)
        }
    }

    /**
     * 解析表达式
     */
    public func parseExpression(): LispValue {
        let token = this.peek()

        // 处理词法错误
        if (let Token.Error(msg) <- token) {
            this.advance()  // 消费 Error token
            this.recordSyntaxError(msg)
            return LispValue.Error(XispError(ErrorType.SyntaxError, msg))
        }

        // 处理引用符号 '
        if (token == Token.Quote) {
            this.advance()  // 消费 Quote
            let expr = this.parseExpression()
            // 创建 (quote expr) 的等价形式
            consValue(Symbol("quote"), consValue(expr, Nil))
        } else if (token == Token.Backquote) {
            this.advance()  // 消费 Backquote
            let expr = this.parseExpression()
            // 创建 (backquote expr) 的等价形式
            consValue(Symbol("backquote"), consValue(expr, Nil))
        } else if (token == Token.Comma) {
            this.advance()  // 消费 Comma
            let expr = this.parseExpression()
            // 创建 (comma expr) 的等价形式
            consValue(Symbol("comma"), consValue(expr, Nil))
        } else if (token == Token.CommaAt) {
            this.advance()  // 消费 CommaAt
            let expr = this.parseExpression()
            // 创建 (comma-at expr) 的等价形式
            consValue(Symbol("comma-at"), consValue(expr, Nil))
        } else if (token == Token.LeftParen) {
            this.parseList()
        } else if (token == Token.VectorStart) {
            this.parseVector()
        } else if (token == Token.HashMapStart) {
            this.parseHashMap()
        } else if (token == Token.HashSetStart) {
            this.parseHashSet()
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
            let result = this.parseListBody()
            // 消费外层的 RightParen（parseListBody 只负责解析内容，不消费外层的括号）
            if (this.peek() == Token.RightParen) {
                this.advance()
            } else {
                // 输入在这里结束：宽松模式静默补全，严格模式记为语法错误
                this.recordSyntaxError("Syntax error: unexpected end of input, expected ')'")
            }
            result
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
            // 创建点对
            Cons(ConsCell(first, cdr))
        } else if (this.peek() == Token.RightParen) {
            // 单元素列表，不消费 RightParen，由 parseList() 消费
            consValue(first, Nil)
        } else if (this.peek() == Token.EOF) {
            // 防止无限递归：到达输入末尾时停止
            this.recordSyntaxError("Syntax error: unexpected end of input, expected ')'")
            consValue(first, Nil)
        } else {
            // 递归解析剩余元素
            let rest = this.parseListBody()
            consValue(first, rest)
        }
    }

    /**
     * 解析向量
     * [1 2 3] → (cangjie::vector 1 2 3)
     */
    private func parseVector(): LispValue {
        this.advance()  // 消费 VectorStart [

        // 收集所有元素
        let elements = ArrayList<LispValue>()
        while (this.peek() != Token.VectorEnd && this.peek() != Token.EOF) {
            elements.add(this.parseExpression())
        }

        if (this.peek() == Token.VectorEnd) {
            this.advance()  // 消费 VectorEnd ]
        } else {
            this.recordSyntaxError("Syntax error: unexpected end of input, expected ']'")
        }

        // 创建 (cangjie::vector elem1 elem2 ...)
        let result = consValue(Symbol("cangjie::vector"), Nil)
        var current = result
        for (elem in elements) {
            let newCell = Cons(ConsCell(elem, Nil))
            match (current) {
                case Cons(cell) => cell.cdr = newCell
                case _ => ()
            }
            current = newCell
        }

        result
    }

    /**
     * 解析哈希映射
     * {:a 1 :b 2} → (cangjie::hashmap (quote :a) 1 (quote :b) 2)
     *
     * 注意：键（特别是符号）需要被 quote，防止被求值为 nil
     */
    private func parseHashMap(): LispValue {
        this.advance()  // 消费 HashMapStart {

        // 收集所有键值对
        let pairs = ArrayList<LispValue>()
        var isKey = true
        while (this.peek() != Token.HashMapEnd && this.peek() != Token.EOF) {
            let expr = this.parseExpression()
            // 对键进行 quote，防止符号被求值为 nil
            if (isKey) {
                // 创建 (quote expr)
                let quotedExpr = consValue(Symbol("quote"), consValue(expr, Nil))
                pairs.add(quotedExpr)
            } else {
                pairs.add(expr)
            }
            isKey = !isKey  // 切换键/值状态
        }

        if (this.peek() == Token.HashMapEnd) {
            this.advance()  // 消费 HashMapEnd }
        } else {
            this.recordSyntaxError("Syntax error: unexpected end of input, expected '}'")
        }

        // 创建 (hashmap (quote key1) val1 (quote key2) val2 ...)
        let result = consValue(Symbol("hashmap"), Nil)
        var current = result
        for (pair in pairs) {
            let newCell = Cons(ConsCell(pair, Nil))
            match (current) {
                case Cons(cell) => cell.cdr = newCell
                case _ => ()
            }
            current = newCell
        }

        result
    }

    /**
     * 解析哈希集合
     * #{1 2 3} → (cangjie::hashset 1 2 3)
     * #{:red :green} → (cangjie::hashset (quote :red) (quote :green))
     *
     * 注意：符号元素需要被 quote，防止被求值为 nil
     */
    private func parseHashSet(): LispValue {
        this.advance()  // 消费 HashSetStart #{

        // 收集所有元素
        let elements = ArrayList<LispValue>()
        while (this.peek() != Token.HashMapEnd && this.peek() != Token.EOF) {
            let expr = this.parseExpression()
            // 对符号元素进行 quote，防止被求值为 nil
            let finalExpr = match (expr) {
                case Symbol(s) => consValue(Symbol("quote"), consValue(expr, Nil))  // quote 符号
                case _ => expr  // 非符号直接使用
            }
            elements.add(finalExpr)
        }

        if (this.peek() == Token.HashMapEnd) {
            this.advance()  // 消费 HashMapEnd }
        } else {
            this.recordSyntaxError("Syntax error: unexpected end of input, expected '}'")
        }

        // 创建 (cangjie::hashset elem1 elem2 ...)
        let result = consValue(Symbol("cangjie::hashset"), Nil)
        var current = result
        for (elem in elements) {
            let newCell = Cons(ConsCell(elem, Nil))
            match (current) {
                case Cons(cell) => cell.cdr = newCell
                case _ => ()
            }
            current = newCell
        }

        result
    }

    /**
     * 解析所有表达式
     */
    public func parse(): ArrayList<LispValue> {
        let exprs = ArrayList<LispValue>()
        var i = 0

        // 严格模式下一旦记录到语法错误就停止：继续解析只会堆积残缺表达式
        while (this.peek() != Token.EOF && this.syntaxError.isNone()) {
            let expr = this.parseExpression()
            exprs.add(expr)
            i = i + 1
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

    /**
     * 严格解析：把「输入不完整」作为错误报告，而不是静默补全。
     *
     * 宽松解析（{@link parseFromString}）会补全未闭合的括号
     * （`(+ 1` → `(+ 1)`）并把未闭合字符串的剩余输入当作内容——
     * 这对 REPL 的连续输入是必要的，但宿主执行模型生成的代码时，
     * 静默补全意味着**残缺程序会照常运行**，错误直到行为异常才暴露。
     *
     * @param input 源代码
     * @return 表达式列表与语法错误；error 非空时应整体拒绝 exprs
     */
    public static func parseChecked(input: String): ParseOutcome {
        let lexer = Lexer(input)
        lexer.setStrict(true)
        let tokens = lexer.tokenize()
        let parser = Parser(tokens)
        parser.setStrict(true)
        let exprs = parser.parse()
        ParseOutcome(exprs, parser.getSyntaxError())
    }
}

/**
 * 一次解析的结果。
 *
 * `error` 非空表示输入不完整或存在语法错误；此时 `exprs` 只含出错前
 * 成功解析的表达式，调用方应当整体拒绝，不要执行残缺结果。
 */
public struct ParseOutcome {
    public let exprs: ArrayList<LispValue>
    public let error: Option<XispError>

    public init(exprs: ArrayList<LispValue>, error: Option<XispError>) {
        this.exprs = exprs
        this.error = error
    }
}
