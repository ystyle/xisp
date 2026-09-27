package ystyle::xisp_repl

import cjpminfo.*

/**
 * 星枢版本号
 *
 * 版本号只服务于界面展示（REPL/CLI 的 banner），因此放在界面模块，
 * 核心不为此引入外部依赖。
 *
 * 编译时从本模块 `cjpm.toml` 读取，无需手动维护；工作区已发布成员共用产品版本（lockstep）。
 */
public const XISP_VERSION: String = @ModuleVer("0.5.0")

/**
 * 打印版本 Banner
 *
 * 界面职责：核心只提供「是否显示」的配置查询（`LispInterpreter.shouldShowBanner`）。
 */
public func printBanner() {
    println("╔═══════════════════════════════════════════════════════════╗")
    println("║           星枢 - 仓颉嵌入式 Lisp 脚本语言                 ║")
    println("║                  版本 ${XISP_VERSION}                           ║")
    println("╚═══════════════════════════════════════════════════════════╝")
}
