#!/bin/sh
# Benchmarks with real libraries (bench/libs.lisp, loaded from ~/quicklisp by
# ASDF on SBCL and on Clysm) and the compile time of the self-hosted compiler
# (the compiler running in Wasm against the same compiler running on SBCL).
#   tools/bench-libs.sh [FILE]
# FILE is bench/libs.lisp by default; with another (bench/apps.lisp: JSON,
# a PEG grammar, dates, tokens of a Lisp file) the self-hosted compiler is
# not timed.
set -e
export LC_ALL=C   # sort and join must agree on the order (a Japanese locale drops items)
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FILE="${1:-$ROOT/bench/libs.lisp}"
SELFHOST=; [ -z "$1" ] && SELFHOST=1
RUNS="${BENCH_RUNS:-3}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
ms() { echo $(( $(date +%s%N) / 1000000 )); }   # (uutils date: %3N is not zero-padded)
# warm the fasl caches (libraries are compiled once)
sbcl --noinform --non-interactive --load "$FILE" >/dev/null 2>&1 || true
"$ROOT/bin/clysm" script "$FILE" >/dev/null 2>&1 || true
if [ -n "$SELFHOST" ]; then
  [ -f "$ROOT/build/clysmc.wasm" ] && [ -z "$(find "$ROOT/src" "$ROOT/runtime" -newer "$ROOT/build/clysmc.wasm" -name '*.lisp' | head -1)" ] \
    || "$ROOT/tools/build-selfhost.sh" >/dev/null 2>&1
  sbcl --script "$ROOT/tools/build.lisp" "$TMP/w.wasm" "$ROOT/bench/suite.lisp" >/dev/null 2>&1   # compiles the compiler's fasls if stale
fi
i=0
while [ $i -lt "$RUNS" ]; do
  sbcl --noinform --non-interactive --load "$FILE" 2>&1 | grep '^[a-z]-' >> "$TMP/a.raw"
  "$ROOT/bin/clysm" script "$FILE" 2>&1 | grep '^[a-z]-' >> "$TMP/b.raw"
  # compiling bench/suite.lisp with the runtime: the same compiler on SBCL and in Wasm
  if [ -n "$SELFHOST" ]; then
  t0=$(ms); sbcl --script "$ROOT/tools/build.lisp" "$TMP/a.wasm" "$ROOT/bench/suite.lisp" >/dev/null 2>&1; t1=$(ms)
  node "$ROOT/host/run.mjs" "$ROOT/build/clysmc.wasm" "$TMP/b.wasm" "$ROOT/bench/suite.lisp" >/dev/null 2>&1; t2=$(ms)
  echo "l-selfhost-compile $((t1 - t0)) ok" >> "$TMP/a.raw"
  echo "l-selfhost-compile $((t2 - t1)) $([ -s "$TMP/b.wasm" ] && echo ok)" >> "$TMP/b.raw"
  fi
  i=$((i+1))
done
best() { awk '{ if (!($1 in t)) { order[n++]=$1 } if (!($1 in t) || $2 < t[$1]) t[$1]=$2; r[$1]=$3 }
              END { for (i=0;i<n;i++) print order[i], t[order[i]], r[order[i]] }' "$@" | sort; }
best "$TMP/a.raw" > "$TMP/a.txt"; best "$TMP/b.raw" > "$TMP/b.txt"
join "$TMP/a.txt" "$TMP/b.txt" | awk '
  { s=$2; c=$4; r=(s>0)? c/s : c; ok=($3==$5)?"":"  RESULT MISMATCH";
    printf "%-24s sbcl %6d ms  clysm %7d ms  x%7.2f%s\n", $1, s, c, r, ok;
    if (s>0) { lg+=log(r); n++; if (r>mx) { mx=r; mn=$1 } } }
  END { printf "geometric mean slowdown: x%.2f   slowest: %s x%.2f   (%d benchmarks)\n", exp(lg/n), mn, mx, n }'
