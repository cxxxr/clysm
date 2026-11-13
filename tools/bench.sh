#!/bin/sh
# Run a benchmark file on SBCL and on Clysm and print the time ratio.
#   tools/bench.sh [FILE]          bench/suite.lisp by default
#   MODE=script tools/bench.sh F   compare `clysm script` (code loaded into the
#                                  REPL image) with `clysm run` instead
# Each side runs BENCH_RUNS times (default 3); the best time of each benchmark
# is used.
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FILE="${1:-$ROOT/bench/suite.lisp}"
RUNS="${BENCH_RUNS:-3}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
best() { awk '{ if (!($1 in t)) { order[n++]=$1 } if (!($1 in t) || $2 < t[$1]) t[$1]=$2; r[$1]=$3 }
              END { for (i=0;i<n;i++) print order[i], t[order[i]], r[order[i]] }' "$@" | sort; }
sbcl --noinform --non-interactive --eval "(compile-file \"$FILE\" :output-file \"$TMP/s.fasl\")" >/dev/null 2>&1
"$ROOT/bin/clysm" compile "$FILE" -o "$TMP/c.wasm" >/dev/null 2>&1
i=0
while [ $i -lt "$RUNS" ]; do
  if [ "$MODE" = script ]; then
    "$ROOT/bin/clysm" script "$FILE" | grep '^[a-z]-' >> "$TMP/b.raw"
  else
    sbcl --noinform --non-interactive --eval "(load \"$TMP/s.fasl\")" 2>&1 | grep '^[a-z]-' >> "$TMP/a.raw"
  fi
  "$ROOT/bin/clysm" exec "$TMP/c.wasm" | grep '^[a-z]-' >> "$TMP/c.raw"
  i=$((i+1))
done
if [ "$MODE" = script ]; then
  best "$TMP/c.raw" > "$TMP/a.txt"; best "$TMP/b.raw" > "$TMP/b.txt"; A=run; B=script
else
  best "$TMP/a.raw" > "$TMP/a.txt"; best "$TMP/c.raw" > "$TMP/b.txt"; A=sbcl; B=clysm
fi
join "$TMP/a.txt" "$TMP/b.txt" | awk -v A="$A" -v B="$B" '
  { s=$2; c=$4; r=(s>0)? c/s : c; ok=($3==$5)?"":"  RESULT MISMATCH";
    printf "%-24s %s %6d ms  %s %7d ms  x%7.2f%s\n", $1, A, s, B, c, r, ok;
    if (s>0) { lg+=log(r); n++; if (r>mx) { mx=r; mn=$1 } } }
  END { printf "geometric mean slowdown: x%.2f   slowest: %s x%.2f   (%d benchmarks)\n", exp(lg/n), mn, mx, n }'
