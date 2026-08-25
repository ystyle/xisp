#!/usr/bin/env bash
# Xisp 字节码性能基准复现脚本
# 用法：bash lisp-tests/perf/run.sh [轮数，默认 5]
# 输出：每场景 AST/BC 中位数（ms）与加速比
set -u
ROUNDS=${1:-5}
BIN=./target/release/bin/ystyle::xisp.cli
DIR=$(dirname "$0")
RES=$(mktemp -d)

for f in "$DIR"/*.lisp; do
  name=$(basename "$f" .lisp)
  for mode in ast bc jit; do
    for ((i=0; i<ROUNDS; i++)); do
      if [ "$mode" = "ast" ]; then
        s=$(date +%s%N); "$BIN" "$f" >/dev/null 2>&1; e=$(date +%s%N)
      elif [ "$mode" = "jit" ]; then
        s=$(date +%s%N); "$BIN" --with-jit "$f" >/dev/null 2>&1; e=$(date +%s%N)
      else
        s=$(date +%s%N); "$BIN" --with-bytecode-compiler "$f" >/dev/null 2>&1; e=$(date +%s%N)
      fi
      echo "$(( (e-s)/1000000 ))" >> "$RES/${name}.${mode}.ms"
    done
  done
done

printf "%-28s %10s %10s %10s %8s %8s\n" "场景" "AST(ms)" "BC(ms)" "JIT(ms)" "BC加速" "JIT/BC"
for f in "$RES"/*.ast.ms; do
  name=$(basename "$f" .ast.ms)
  ast=$(sort -n "$RES/${name}.ast.ms" | sed -n "$(( (ROUNDS+1)/2 ))p")
  bc=$(sort -n "$RES/${name}.bc.ms" | sed -n "$(( (ROUNDS+1)/2 ))p")
  jit=$(sort -n "$RES/${name}.jit.ms" | sed -n "$(( (ROUNDS+1)/2 ))p")
  ratio=$(awk "BEGIN{printf \"%.1fx\", $ast/$bc}")
  jratio=$(awk "BEGIN{printf \"%.1fx\", $bc/$jit}")
  printf "%-28s %10s %10s %10s %8s %8s\n" "$name" "$ast" "$bc" "$jit" "$ratio" "$jratio"
done
rm -rf "$RES"
