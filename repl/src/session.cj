package ystyle::xisp_repl

import ystyle::xisp.*

/**
 * REPL 会话编排
 *
 * 编排放在 REPL 模块内：核心包（`ystyle::xisp`）不依赖 REPL，
 * 宿主按需依赖 `ystyle::xisp_repl` 才能启动交互式会话。
 */

/**
 * 启动交互式 REPL（含行编辑 / 补全 / 高亮 / 历史）
 *
 * @param interp 已配置好的解释器
 * @return 退出码（恒为 0）
 */
public func runRepl(interp: LispInterpreter): Int64 {
    let repl = EnhancedRepl(interp.getEnvironment())
    if (interp.shouldShowBanner()) {
        repl.setShowBanner(true)
    }
    repl.run()
    0
}

/**
 * 启动简易 REPL（无行编辑，逐行读取）
 *
 * @param interp 已配置好的解释器
 * @return 退出码（恒为 0）
 */
public func runSimpleRepl(interp: LispInterpreter): Int64 {
    let repl = Repl(interp.getEnvironment())
    if (interp.shouldShowBanner()) {
        printBanner()
        repl.setShowBanner(false)
    }
    repl.run()
    0
}
