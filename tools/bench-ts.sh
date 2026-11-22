#!/bin/sh
# Compare Clysm with the same programs written in TypeScript, both on Node.js:
# bench/typescript/bench.ts (ports of benchmarks of bench/suite.lisp and
# bench/declared.lisp) against those files compiled by Clysm.
#   tools/bench-ts.sh
# The two sides run one after the other BENCH_RUNS times (default 3); the best
# time of each benchmark is used. Needs Node.js 24 (it runs the .ts file as it is).
set -e
export LC_ALL=C   # sort and join must agree on the order (a Japanese locale drops items)
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
RUNS="${BENCH_RUNS:-3}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
best() { awk '{ if (!($1 in t) || $2 < t[$1]) { t[$1]=$2; r[$1]=$3 } }
              END { for (k in t) print k, t[k], r[k] }' "$1" | sort; }
"$ROOT/bin/clysm" compile "$ROOT/bench/suite.lisp" -o "$TMP/suite.wasm" >/dev/null 2>&1
"$ROOT/bin/clysm" compile "$ROOT/bench/declared.lisp" -o "$TMP/declared.wasm" >/dev/null 2>&1
touch "$TMP/c.raw"
i=0
while [ $i -lt "$RUNS" ]; do
  node "$ROOT/bench/typescript/bench.ts" | grep '^[a-z]-' >> "$TMP/t.raw"
  "$ROOT/bin/clysm" exec "$TMP/suite.wasm" | grep '^[a-z]-' >> "$TMP/c.raw"
  "$ROOT/bin/clysm" exec "$TMP/declared.wasm" | grep '^[a-z]-' >> "$TMP/c.raw"
  i=$((i+1))
done
best "$TMP/t.raw" > "$TMP/t.txt"; best "$TMP/c.raw" > "$TMP/c.txt"
# only the benchmarks that have a TypeScript port (join drops the others)
join "$TMP/t.txt" "$TMP/c.txt" | awk '
  { t=$2; c=$4; r=(t>0)? c/t : c; ok=($3==$5 || $3"d0"==$5)?"":"  RESULT MISMATCH";
    printf "%-24s ts %6d ms  clysm %7d ms  x%7.2f%s\n", $1, t, c, r, ok;
    if (t>0) { lg+=log(r); n++; if (r>mx) { mx=r; mn=$1 } } }
  END { printf "geometric mean, clysm / typescript: x%.2f   slowest: %s x%.2f   (%d benchmarks)\n", exp(lg/n), mn, mx, n }'
