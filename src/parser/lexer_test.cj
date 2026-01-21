package ystyle::xisp.parser

import std.unittest.*
import std.unittest.testmacro.*
import std.collection.ArrayList

/**
 * Lexer 测试项
 */
class LexerTestItem {
    LexerTestItem(let input: String, let expectedTokens: Array<Token>) {}
}

var lexerTests = ArrayList<LexerTestItem>([
    // 基本括号
    LexerTestItem(
        "()",
        [Token.LeftParen, Token.RightParen]
    ),
    // 数字
    LexerTestItem(
        "42 3.14",
        [Token.Number(42.0), Token.Number(3.14)]
    ),
    // 符号
    LexerTestItem(
        "foo bar+baz? test123 中文",
        [Token.Symbol("foo"), Token.Symbol("bar+baz?"), Token.Symbol("test123"), Token.Symbol("中文")]
    ),
    // 布尔值
    LexerTestItem(
        "#t #f #true #false",
        [Token.Boolean(true), Token.Boolean(false), Token.Boolean(true), Token.Boolean(false)]
    ),
    // 字符串
    LexerTestItem(
        "\"hello\" \"world\\n\"",
        [Token.String("hello"), Token.String("world\n")]
    ),
    // 注释
    LexerTestItem(
        "42 ; comment\n100",
        [Token.Number(42.0), Token.Number(100.0)]
    ),
    // 空白符
    LexerTestItem(
        "  (  foo  bar  )  ",
        [Token.LeftParen, Token.Symbol("foo"), Token.Symbol("bar"), Token.RightParen]
    ),
    // 点对
    LexerTestItem(
        "(a . b)",
        [Token.LeftParen, Token.Symbol("a"), Token.Dot, Token.Symbol("b"), Token.RightParen]
    ),
    // 引用
    LexerTestItem(
        "'foo",
        [Token.Quote, Token.Symbol("foo")]
    ),

    // ===== 中文测试用例 =====

    // 纯中文符号
    LexerTestItem(
        "中文符号 测试变量",
        [Token.Symbol("中文符号"), Token.Symbol("测试变量")]
    ),
    // 中英文混合符号
    LexerTestItem(
        "my变量 foo函数 test123测试",
        [Token.Symbol("my变量"), Token.Symbol("foo函数"), Token.Symbol("test123测试")]
    ),
    // 中文数字混合
    LexerTestItem(
        "数值1 值2 第三个值",
        [Token.Symbol("数值1"), Token.Symbol("值2"), Token.Symbol("第三个值")]
    ),
    // 中文带特殊字符
    LexerTestItem(
        "变量+值 测试?结果",
        [Token.Symbol("变量+值"), Token.Symbol("测试?结果")]
    ),
    // 中文在字符串中
    LexerTestItem(
        "\"你好世界\" \"测试\\n字符串\"",
        [Token.String("你好世界"), Token.String("测试\n字符串")]
    ),
    // 中文在注释中
    LexerTestItem(
        "42 ; 这是一个中文注释\n100",
        [Token.Number(42.0), Token.Number(100.0)]
    ),
    // 中文符号表达式
    LexerTestItem(
        "(定义 函数 (参数) 函数体)",
        [Token.LeftParen, Token.Symbol("定义"), Token.Symbol("函数"), Token.LeftParen, Token.Symbol("参数"), Token.RightParen, Token.Symbol("函数体"), Token.RightParen]
    ),
    // 中文解构绑定
    LexerTestItem(
        "(让 ((首 . 其余) 列表))",
        [Token.LeftParen, Token.Symbol("让"), Token.LeftParen, Token.LeftParen, Token.Symbol("首"), Token.Dot, Token.Symbol("其余"), Token.RightParen, Token.Symbol("列表"), Token.RightParen, Token.RightParen]
    ),
    // 多个中文符号
    LexerTestItem(
        "加法 减法 乘法 除法",
        [Token.Symbol("加法"), Token.Symbol("减法"), Token.Symbol("乘法"), Token.Symbol("除法")]
    ),
    // 中文带下划线
    LexerTestItem(
        "my_变量 测试_数据 foo_bar函数",
        [Token.Symbol("my_变量"), Token.Symbol("测试_数据"), Token.Symbol("foo_bar函数")]
    ),
    // 日文符号
    LexerTestItem(
        "日本語 変数 テスト",
        [Token.Symbol("日本語"), Token.Symbol("変数"), Token.Symbol("テスト")]
    ),
    // 韩文符号
    LexerTestItem(
        "한국어 변수 함수",
        [Token.Symbol("한국어"), Token.Symbol("변수"), Token.Symbol("함수")]
    ),
    // 混合Unicode
    LexerTestItem(
        "test测试 変数foo 변수bar",
        [Token.Symbol("test测试"), Token.Symbol("変数foo"), Token.Symbol("변수bar")]
    ),
])

/**
 * Lexer 测试
 */
@Test
class LexerTest {
    @TestCase
    func testTokenize() {
        for (testIdx in 0..lexerTests.size) {
            let test = lexerTests[testIdx]
            println("[${testIdx}] Testing: ${test.input}")

            let lexer = Lexer(test.input)
            let tokens = lexer.tokenize()

            for (i in 0..test.expectedTokens.size) {
                if (i >= tokens.size) {
                    @Fail("[${testIdx}/${i}] Token stream exceeded expected length. Expected ${test.expectedTokens.size} tokens, got ${tokens.size}")
                }

                let actual = tokens[i]
                let expected = test.expectedTokens[i]

                println("[${testIdx}/${i}] actual: ${actual.toString()}, expected: ${expected.toString()}")

                if (!(actual == expected)) {
                    @Fail("[${testIdx}/${i}] Expected ${expected.toString()}, got ${actual.toString()}")
                }
            }

            // 检查是否有额外的 token
            if (tokens.size > test.expectedTokens.size) {
                @Fail("[${testIdx}] Expected ${test.expectedTokens.size} tokens, got ${tokens.size}")
            }
        }
    }
}
