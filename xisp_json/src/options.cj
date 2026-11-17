package ystyle::xisp_json

import ystyle::xisp.*

/**
 * 启用 JSON 能力（追加到默认装配之上）
 *
 * 用法：
 *
 * ```cangjie
 * let interp = LispInterpreter([withJson()])
 * interp.eval(#"(cangjie::json-get "{\"a\":1}" "a")"#)
 * ```
 *
 * 只装 JSON 一档时用 `withDomains([jsonDomain()], replace: true)` —— 但那样
 * 连 `println` 都没有，需要自己补域。
 */
public func withJson(): InterpreterOption {
    withDomains([jsonDomain()])
}
