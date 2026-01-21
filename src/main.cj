package ystyle::xisp

main(): Int64 {
    // 默认配置：英文关键字，启用所有标准库
    //
    // 使用方法：
    // 1. 在 REPL 中使用 ,lang zh 启用中文关键字
    // 2. 或者修改下面注释的选项来启用特定功能
    //
    let interpreter = LispInterpreter([
        // 取消注释以下行来启用对应功能：
        // withChineseKeywords(),    // 启用中文关键字
        // withJapaneseKeywords(),   // 启用日文关键字
        // withQuietMode(),          // 不显示 Banner
        // withVerboseMode(),        // 详细输出
    ])
    interpreter.runREPL()
}