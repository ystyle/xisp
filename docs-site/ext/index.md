# 扩展模块

Xisp 的核心只做「求值」——解释器、内建函数、模块系统、桥接。**它零外部依赖**，
不依赖任何第三方包。

**扩展模块**是另外一批能力：它们要额外的依赖，因此单独成 cjpm 成员，**按需依赖**。
不用它们的宿主完全不会接触这些依赖。

---

## 核心 vs 扩展

| | 核心 `ystyle::xisp` | 扩展模块 |
|---|---|---|
| 外部依赖 | **无** | 各自不同（gjson / stdx …） |
| 默认装配 | 内置函数默认可用 | **默认不注册**，要显式 `withXxx()` |
| 能力域 | 14 个内置域 | 各自提供自己的域（可装配掉） |
| 版本 | 与产品同版本（lockstep） | 同左 |

**判断标准很简单**：要写 `import` 第三方包的能力，就属于扩展模块。
所以并发（`spawn` / `Future`）**不是**扩展 —— 它用 `std.core` 的线程，在核心里。

---

## 现有模块

| 模块 | 内容 | 外部依赖 | 构建前提 |
|---|---|---|---|
| [`ystyle::xisp_json`](json) | JSON 路径取值与解析成树 | `gjson`（零依赖） | 无 |
| [`ystyle::xisp_net`](net) | HTTP **客户端** | `stdx.net.http` / `stdx.net.tls` | **需 `CANGJIE_STDX_PATH`** |
| [`ystyle::xisp_httpd`](httpd) | HTTP **服务端** | `tachyon`（零 stdx） | 无 |

```toml
[dependencies]
  "ystyle::xisp" = "0.7.0"
  "ystyle::xisp_json" = "0.7.0"   # 0.7.0 起提供
  "ystyle::xisp_net" = "0.7.0"    # 同上；另需 stdx 环境
```

---

## 共同约定

**一、默认不注册。** 扩展模块的能力不会自动出现，要显式装配：

```cangjie
import ystyle::xisp_json.*
import ystyle::xisp_net.*

let it = LispInterpreter([
    withJson(),
    withNetwork(["api.example.com"]),
])
```

**二、每个模块提供自己的能力域。** 所以它们也能被 `withDomains(..., replace: true)`
独占装配，或按需从预置组合里剔除。

**三、构建前提各自独立。** 只有 `xisp_net` 需要平台相关配置（stdx 的二进制依赖）；
`xisp_json` 虽然依赖 gjson，但 gjson 本身零依赖，构建与核心一样简单。

::: tip 将来
科学计算族（`xisp_array` / `xisp_linalg` / …）落地后也会归到这里，
见 `docs/plans/2026-10-11-sci-family-design.md`。
:::

---

## 各模块怎么选

| 你要什么 | 看哪页 |
|---|---|
| 读 JSON、按路径取字段、把 JSON 解析成 Lisp 值 | [JSON](json) |
| 发 HTTP 请求、调 REST 接口 | [HTTP 客户端](net) |
| 起一个 HTTP 服务、给别的东西提供接口 | [HTTP 服务端](httpd) |
| 并行执行多个求值任务 | [并发](/api/async)（**核心内置**，不在这里） |
| 字节码 / JIT 加速 | [字节码与 JIT](/integration/options#字节码与-jit可选后端) |
