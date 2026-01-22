package ystyle::xisp.parser

/**
 * Token 类型
 *
 * 表示 Lisp 代码的词法单元
 */
public enum Token <: Equatable<Token> {
    /**
     * 左括号 (
     */
    | LeftParen

    /**
     * 右括号 )
     */
    | RightParen

    /**
     * 点号 .
     */
    | Dot

    /**
     * 引用符号 '
     */
    | Quote

    /**
     * 符号（变量名、函数名等）
     */
    | Symbol(String)

    /**
     * 数字
     */
    | Number(Float64)

    /**
     * 字符串
     */
    | String(String)

    /**
     * 布尔值 #t 或 #f
     */
    | Boolean(Bool)

    /**
     * 向量开始 [
     */
    | VectorStart

    /**
     * 向量结束 ]
     */
    | VectorEnd

    /**
     * 哈希集合开始 #{
     */
    | HashSetStart

    /**
     * 哈希映射开始 {
     */
    | HashMapStart

    /**
     * 哈希映射结束 }
     */
    | HashMapEnd

    /**
     * 字符串插值标记 #"
     */
    | InterpolateString

    /**
     * 文件结束
     */
    | EOF

    /**
     * 实现 Equal 接口
     */
    public operator func ==(rhs: Token): Bool {
        match ((this, rhs)) {
            case (LeftParen, LeftParen) => true
            case (RightParen, RightParen) => true
            case (Dot, Dot) => true
            case (Quote, Quote) => true
            case (Symbol(s1), Symbol(s2)) => s1 == s2
            case (Number(n1), Number(n2)) => n1 == n2
            case (String(s1), String(s2)) => s1 == s2
            case (Boolean(b1), Boolean(b2)) => b1 == b2
            case (VectorStart, VectorStart) => true
            case (VectorEnd, VectorEnd) => true
            case (HashSetStart, HashSetStart) => true
            case (HashMapStart, HashMapStart) => true
            case (HashMapEnd, HashMapEnd) => true
            case (InterpolateString, InterpolateString) => true
            case (EOF, EOF) => true
            case _ => false
        }
    }

    /**
     * 获取 Token 的位置信息（用于错误报告）
     */
    public func toString(): String {
        match (this) {
            case LeftParen => "("
            case RightParen => ")"
            case Dot => "."
            case Quote => "'"
            case Symbol(s) => "<symbol: ${s}>"
            case Number(n) => "<number: ${n}>"
            case String(s) => "<string: \"${s}\">"
            case Boolean(b) => if (b) { "#t" } else { "#f" }
            case VectorStart => "["
            case VectorEnd => "]"
            case HashSetStart => "#{"
            case HashMapStart => "{"
            case HashMapEnd => "}"
            case InterpolateString => "#\""
            case EOF => "<EOF>"
        }
    }

    /**
     * 判断是否为字面量（数字、字符串、布尔值）
     */
    public func isLiteral(): Bool {
        match (this) {
            case Number(_) => true
            case String(_) => true
            case Boolean(_) => true
            case _ => false
        }
    }

    /**
     * 判断是否为符号（包括布尔值的 #t/#f）
     */
    public func isSymbolLike(): Bool {
        match (this) {
            case Symbol(_) => true
            case Boolean(_) => true
            case _ => false
        }
    }
}
