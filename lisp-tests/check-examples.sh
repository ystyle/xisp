#!/usr/bin/env bash
# 三模式（AST / 字节码 / JIT）examples 一致性校验
#
# 用法: bash lisp-tests/check-examples.sh
# 退出码: 0 = 全部一致；1 = 存在不一致；2 = 用法/二进制缺失
#
# 约定（与 README 一致）:
#  - examples/05-modules/** 需设置 XISP_PATH=examples/05-modules/modules_demo
#  - modules_demo/*/module.lisp 为模块元数据清单（由模块加载器特殊处理，
#    非独立脚本），跳过
#  - legacy/chinese_demo.lisp 为 REPL 中文关键字演示（需先 ,lang zh），跳过

set -u
cd "$(dirname "$0")/.."

BIN=./target/release/bin/ystyle::xisp.cli
MODPATH=$(pwd)/examples/05-modules/modules_demo

if [ ! -x "$BIN" ]; then
    echo "错误: 未找到 $BIN，请先 cjpm build" >&2
    exit 2
fi

SKIP_PATTERNS=(
    "examples/05-modules/modules_demo/module.lisp"
    "examples/05-modules/modules_demo/pkg1/module.lisp"
    "examples/05-modules/modules_demo/pkg2/module.lisp"
    "examples/legacy/chinese_demo.lisp"
)

is_skipped() {
    local f="$1"
    for s in "${SKIP_PATTERNS[@]}"; do
        [ "$f" = "$s" ] && return 0
    done
    return 1
}

run_mode() {
    local mode="$1" f="$2" out="$3"
    if [[ "$f" == examples/05-modules/* ]]; then
        XISP_PATH="$MODPATH" timeout 120 "$BIN" $mode "$f" > "$out" 2>&1
    else
        timeout 120 "$BIN" $mode "$f" > "$out" 2>&1
    fi
}

total=0
diff_count=0
declare -a diffs

while IFS= read -r f; do
    is_skipped "$f" && continue
    total=$((total + 1))
    run_mode "" "$f" /tmp/xisp-3m-ast.$$.out
    run_mode "--with-bytecode-compiler" "$f" /tmp/xisp-3m-bc.$$.out
    run_mode "--with-jit" "$f" /tmp/xisp-3m-jit.$$.out
    if ! cmp -s /tmp/xisp-3m-ast.$$.out /tmp/xisp-3m-bc.$$.out ||
       ! cmp -s /tmp/xisp-3m-ast.$$.out /tmp/xisp-3m-jit.$$.out; then
        diffs+=("$f")
        diff_count=$((diff_count + 1))
        echo "DIFF: $f"
    fi
    rm -f /tmp/xisp-3m-ast.$$.out /tmp/xisp-3m-bc.$$.out /tmp/xisp-3m-jit.$$.out
done < <(find examples -name '*.lisp' | sort)

echo "------------------------------------------"
echo "示例数: $total  一致: $((total - diff_count))  不一致: $diff_count"
if [ "$diff_count" -eq 0 ]; then
    echo "✅ examples 三模式全部一致"
    exit 0
else
    echo "❌ 存在不一致:"
    for d in "${diffs[@]}"; do echo "  - $d"; done
    exit 1
fi
