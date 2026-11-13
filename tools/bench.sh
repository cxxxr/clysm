#!/bin/sh
# Run bench/suite.lisp on SBCL and on Clysm and print the time ratio.
#   tools/bench.sh [FILE]
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FILE="${1:-$ROOT/bench/suite.lisp}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
sbcl --noinform --non-interactive \
  --eval "(load (compile-file \"$FILE\" :output-file \"$TMP/s.fasl\"))" 2>&1 | grep '^[a-z]-' > "$TMP/sbcl.txt"
"$ROOT/bin/clysm" run "$FILE" | grep '^[a-z]-' > "$TMP/clysm.txt"
join "$TMP/sbcl.txt" "$TMP/clysm.txt" | awk '
  { s=$2; c=$4; r=(s>0)? c/s : c; ok=($3==$5)?"":"  RESULT MISMATCH";
    printf "%-18s sbcl %6d ms  clysm %7d ms  x%7.1f%s\n", $1, s, c, r, ok;
    if (s>0) { lg+=log(r); n++ } }
  END { printf "geometric mean slowdown: x%.1f\n", exp(lg/n) }'
