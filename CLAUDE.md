# 星枢
一个仓颉嵌入式 Lisp 脚本语言

## 设计文档
- 设计文档在`docs/design.md`里
- 核心必要功能列表在`docs/core.md`里

## 仓颉语言
- 在上下文压缩后，如果没有仓颉语法相关的，需要马上使用`cangjie_mem_list`工具加载所有仓颉语言级记忆
- 在仓颉api和手册可以使用`cangjie_docs`相关工具查找，在`cangjie-mem`没有的直接在文档里查找，不要猜api和语法
- 在提示语法错误时重新使用 `cangjie-mem` 加载语言级记忆
- match 的 case 后不能接`{}`, case后直接写多行列表式而不需要`{}`

## 任务指南
- **不要考虑时间，不要简化算法，不要简化测试，这项目是自己的产品，按最好的来搞**
- 需要在完成任务后更新`task.md`
- 在实现功能总结后，把总结的内容记录到`cangjie-mem`项目级记忆里
- 新功能、新特性一定要写单元测试
- 不要想在项目外创建仓颉单文件测试，非cjpm项目没法导入当前项目
- 新功能需要做好，且有单元测试后提交, 以仓颉单元测试为主
- 测试发现的新问题需要解决，且要添加新用例到仓颉的单元测试里

## 常用命令
```shell
# 编译
cjpm build 
# 运行可执行包
cjpm run --name ystyle::xisp.examples.match_demo
# 手动执行，先查看 target/release/bin 目录有哪些可执行文件
./target/release/bin/ystyle::xisp.cli

# 运行所有测试
cjpm test 

# 运行指定测试
cjpm test --show-all-output --filter 'ParserTest.testParseRestParameter'
cjpm test --show-all-output --filter 'ParserTest.*'

# 清理构建缓存 
cjpm clean
```

### 项目结构
```shell
.
├── cjpm.lock
├── cjpm.toml
├── docs                             # 文档目录
│   ├── integration                  # 仓颉嵌入式使用手册
│   ├── modules.md                   # xisp 模块文档
│   ├── README.md                    # 文档目录
│   ├── syntax                       # xisp 实现的语法手册
│   └── unicode                      # unicode 支持说明
├── examples                         # 用户示例文件
├── lisp-tests                       # lisp 测试文件，用于仓颉的单元测试
├── README.md
├── src                              # 仓颉代码目录
│   ├── bridge
│   ├── cli
│   ├── core
│   ├── examples                     # 仓颉示例
│   ├── parser
│   ├── repl
│   └── types
├── target                           # 编译结果
├── task.md
├── temp-test                        # 临时测试-不提交git
├── temp-docs                        # 临时文档-不提交git
├── UNICODE_SUPPORT.md
```
