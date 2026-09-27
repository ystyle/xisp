package ystyle::xisp.parser

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList

/**
 * Token 测试项
 */
class TokenTestItem {
    TokenTestItem(let token: Token, let expectedStr: String) {}
}

var tokenTests = ArrayList<TokenTestItem>([
    TokenTestItem(Token.LeftParen, "("),
    TokenTestItem(Token.RightParen, ")"),
    TokenTestItem(Token.Dot, "."),
    TokenTestItem(Token.Quote, "'"),
    TokenTestItem(Token.EOF, "<EOF>"),
    TokenTestItem(Token.Symbol("foo"), "<symbol: foo>"),
    TokenTestItem(Token.Int(42), "int:"),
    TokenTestItem(Token.Float(3.14), "float:"),
    TokenTestItem(Token.String("hello"), "<string: \"hello\">"),
    TokenTestItem(Token.Boolean(true), "#t"),
    TokenTestItem(Token.Boolean(false), "#f"),
])

/**
 * Token 测试
 */
@Test
class TokenTest {
    @TestCase
    func testToString() {
        for (i in 0..tokenTests.size) {
            let test = tokenTests[i]
            let result = test.token.toString()
            println("[${i}] token.toString() = ${result}, expected contains: ${test.expectedStr}")

            if (test.expectedStr.size > 0 && !result.contains(test.expectedStr)) {
                @Fail("[${i}] Expected '${result}' to contain '${test.expectedStr}'")
            }

            // 额外检查：Int 和 Float 类型应该包含 "int:" 或 "float:" 关键字
            match (test.token) {
                case Token.Int(_) =>
                    if (!result.contains("int:")) {
                        @Fail("[${i}] Int token should contain 'int:' in '${result}'")
                    }
                case Token.Float(_) =>
                    if (!result.contains("float:")) {
                        @Fail("[${i}] Float token should contain 'float:' in '${result}'")
                    }
                case _ => ()
            }
        }
    }

    @TestCase
    func testEquality() {
        @Assert(Token.LeftParen == Token.LeftParen)
        @Assert(Token.RightParen == Token.RightParen)
        @Assert(Token.Dot == Token.Dot)
        @Assert(Token.Quote == Token.Quote)
        @Assert(Token.EOF == Token.EOF)
        @Assert(Token.Symbol("foo") == Token.Symbol("foo"))
        @Assert(Token.Int(42) == Token.Int(42))
        @Assert(Token.Float(3.14) == Token.Float(3.14))
        @Assert(Token.String("hello") == Token.String("hello"))
        @Assert(Token.Boolean(true) == Token.Boolean(true))

        @Assert(!(Token.LeftParen == Token.RightParen))
        @Assert(!(Token.Symbol("foo") == Token.Symbol("bar")))
    }

    @TestCase
    func testIsLiteral() {
        @Assert(Token.Int(42).isLiteral())
        @Assert(Token.Float(3.14).isLiteral())
        @Assert(Token.String("hello").isLiteral())
        @Assert(Token.Boolean(true).isLiteral())

        @Assert(!Token.LeftParen.isLiteral())
        @Assert(!Token.RightParen.isLiteral())
        @Assert(!Token.Symbol("foo").isLiteral())
        @Assert(!Token.Dot.isLiteral())
        @Assert(!Token.Quote.isLiteral())
        @Assert(!Token.EOF.isLiteral())
    }
}
