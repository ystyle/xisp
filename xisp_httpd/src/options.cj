package ystyle::xisp_httpd

import ystyle::xisp.*

/**
 * 启用 HTTP 服务端能力（**默认不注册**）
 *
 * ```cangjie
 * let interp = LispInterpreter([withHttpd()])
 * ```
 *
 * ## 沙箱默认
 *
 * - **只绑 `127.0.0.1`** —— 对外监听要宿主显式 `bind: "0.0.0.0"`；
 * - **拒绝特权端口**（< 1024），由 `portRange` 限制；
 * - 脚本能选的只有端口，绑定地址是宿主决策。
 *
 * ```cangjie
 * // 对外提供服务
 * let interp = LispInterpreter([withHttpd(bind: "0.0.0.0", portRange: (8080, 8090))])
 * ```
 */
public func withHttpd(bind!: String = "127.0.0.1",
                      portRange!: (UInt16, UInt16) = (1024, 65535),
                      maxBodyBytes!: Int64 = 4 * 1024 * 1024,
                      maxConns!: Int64 = 16384): InterpreterOption {
    let cfg = HttpdConfig()
    cfg.bind = bind
    cfg.minPort = portRange[0]
    cfg.maxPort = portRange[1]
    cfg.maxBodyBytes = maxBodyBytes
    cfg.maxConns = maxConns
    withDomains([httpdDomain(cfg)])
}
