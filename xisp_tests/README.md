# ystyle::xisp_tests

契约 / 集成测试（**不发布**）：只用 `ystyle::xisp` 的 public API 验证语言行为。

- 本模块**只测 public API**——它是宿主视角的契约测试；
- 白盒测试仍与被测包同目录（如 `core/src/parser/parser_test.cj`、`core/src/types/lispvalue_test.cj`），
  这样才可访问该包 `internal` 成员（仓颉 `internal` 为包级可见：当前包及子包）；
- 跨模块测试（如字节码 / JIT 与解释器的集成）也放这里，避免让被拆出的模块反向依赖核心。
