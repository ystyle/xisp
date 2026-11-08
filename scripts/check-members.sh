#!/usr/bin/env bash
# 工作区成员配置闸门（CI 用）
#
# 用法: bash scripts/check-members.sh
# 退出码: 0 = 通过；1 = 有问题；2 = 用法/环境问题
#
# 它检查的是 **`cjpm ws check` 不覆盖**的那部分：ws check 只管版本 lockstep，
# 而发版失败/制品装错的主要原因是下面这几类（都踩过）：
#   · `include` 没写或没锚定 → 制品把 docs-site / examples/src 一起打进去（0.4.0 中招）
#   · 库成员里写 `[replace]` → 只对本仓生效，下游照样冲突（工作区硬规则）
#   · 发布成员缺 README/metadata → 中心仓制品不合格
#   · 引入 stdx 之类的平台相关配置却没有登记 → 消费方在新机器上构建失败

set -u
cd "$(dirname "$0")/.."

python3 - <<'PY'
import sys, os, glob, tomllib

fail = []
warn = []
ok = []

def load(path):
    with open(path, "rb") as f:
        return tomllib.load(f)

if not os.path.exists("workspace.toml"):
    print("错误: 找不到 workspace.toml", file=sys.stderr)
    sys.exit(2)

ws = load("workspace.toml")
ws_ver = ws.get("workspace", {}).get("package", {}).get("version")
if not ws_ver:
    print("错误: workspace.toml 缺 [workspace.package] version", file=sys.stderr)
    sys.exit(2)

members = sorted(glob.glob("*/cjpm.toml"))
if not members:
    print("错误: 没找到任何成员 cjpm.toml", file=sys.stderr)
    sys.exit(2)

# 约定：测试包与示例包不发布
def is_publishable(dirname, pkg):
    name = pkg.get("name", dirname)
    return not (name.endswith("_tests") or name.endswith("_examples"))

# 允许出现平台相关配置（stdx / bin-dependencies 等）的成员白名单。
# 当前为空：xisp 全工作区零外部原生依赖。将来引入时必须先在这里登记，
# 并同步给 CI 与文档 —— 否则配置会悄悄渗进不需要它的成员。
NATIVE_ALLOWED = set()

cjc_versions = {}
for path in members:
    dirname = os.path.dirname(path)
    cfg = load(path)
    pkg = cfg.get("package")
    if not pkg:
        fail.append(f"{path}: 缺 [package] 段")
        continue
    name = pkg.get("name", "?")
    ver = pkg.get("version")

    # 1. lockstep 版本（与 workspace.toml 一致）
    if ver != ws_ver:
        fail.append(f"{path}: version={ver} 与 workspace.toml 的 {ws_ver} 不一致")

    # 2. cjc-version 全体一致
    cjc_versions.setdefault(str(pkg.get("cjc-version")), []).append(path)

    # 3. 库成员禁用 [replace]
    if pkg.get("output-type") == "static" and "replace" in cfg:
        fail.append(f"{path}: 库成员（static）不得使用 [replace]（只对本仓生效，下游照样冲突）")

    # 4. 平台相关配置必须在白名单里
    if name not in NATIVE_ALLOWED:
        for key in ("bin-dependencies", "standard-library", "link-option"):
            val = pkg.get(key)
            if val not in (None, "", [], {}):
                fail.append(f"{path}: 出现平台相关配置 {key}={val!r}，但 {name} 不在 NATIVE_ALLOWED 白名单里")

    if is_publishable(dirname, pkg):
        # 5. 发布成员：制品范围必须显式锚定
        inc = pkg.get("include")
        if not inc:
            fail.append(f"{path}: 发布成员必须显式写 include（不写等于整棵工作树都打包）")
        else:
            for pat in inc:
                if not str(pat).startswith("/"):
                    fail.append(f"{path}: include 项 {pat!r} 未锚定（未锚定=任意层级同名，会把子目录一并打包）")

        # 6. 发布成员：中心仓制品要求的元数据
        for key in ("license", "authors", "repository", "description"):
            if not pkg.get(key):
                fail.append(f"{path}: 发布成员缺 {key}（中心仓制品必需字段）")

        # 7. 发布成员：目录内必须有 README.md（制品要求）
        if not os.path.exists(os.path.join(dirname, "README.md")):
            fail.append(f"{dirname}/README.md: 发布成员目录内缺 README.md")
    else:
        ok.append(f"{name}（不发布，跳过制品检查）")

if len(cjc_versions) > 1:
    detail = "; ".join(f"{v}: {', '.join(p)}" for v, p in cjc_versions.items())
    fail.append(f"各成员 cjc-version 不一致 —— {detail}")

if fail:
    print("❌ 成员配置检查未通过：")
    for f in fail:
        print("  - " + f)
    sys.exit(1)

print(f"✅ 成员配置检查通过（{len(members)} 个成员，workspace 版本 {ws_ver}）")
for line in ok:
    print("  · " + line)
PY
