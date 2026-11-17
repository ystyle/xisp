package ystyle::xisp_net

import ystyle::xisp.*

/**
 * 启用网络能力（**默认不注册**，需要显式给域名白名单）
 *
 * ```cangjie
 * let interp = LispInterpreter([withNetwork(["api.example.com", "*.example.org"])])
 * ```
 *
 * ## 白名单规则
 *
 * - 精确匹配 `api.example.com`
 * - 后缀通配 `*.example.org` 匹配 `a.example.org`、`a.b.example.org`，
 *   **不匹配** `example.org` 本身（要连主域就把它也写进去）
 * - 匹配目标是 URL 的 **host**，不是整条 URL
 * - **传空数组 = 全部拒绝**（等价于不启用网络）。这是安全默认 ——
 *   "没配白名单"绝不能被理解成"允许所有"
 *
 * ## 连不上的情况
 *
 * 域名在白名单内但网络不通（DNS 失败、超时、TLS 失败）：
 * 返回 `Error(NetworkError)`，不是脚本异常。
 *
 * ## 构建前提
 *
 * 本模块依赖 `stdx`，构建前必须 `eval "$(cjvs stdx-env zsh)"`。
 * 缺 `CANGJIE_STDX_PATH` 时 cjpm 会报"cjpm.toml 没加依赖"，
 * 那是指错误方向 —— 实际原因是环境变量没配。
 */
public func withNetwork(allowedHosts: Array<String>): InterpreterOption {
    let cfg = NetConfig()
    if (allowedHosts.size > 0) {
        cfg.urlChecker = Some({ host => hostAllowed(host, allowedHosts) })
    }
    withDomains([netDomain(cfg)])
}

/**
 * 启用网络但不设域名限制（**仅供测试与受控环境**）
 *
 * 与 `withNetwork([])` 的区别：后者全部拒绝，这个全部放行。
 * 生产环境不要用它 —— 那等于让脚本能访问任意地址。
 */
public func withNetworkUnrestricted(): InterpreterOption {
    let cfg = NetConfig()
    cfg.urlChecker = Some({ _ => true })
    withDomains([netDomain(cfg)])
}
