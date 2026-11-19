#!/bin/sh
# Run every benchmark file against SBCL and print one summary line per file.
#   tools/bench-all.sh [OUTDIR]        results in OUTDIR (build/bench-all by default)
#   FILES="suite gabriel" tools/bench-all.sh     only these files of bench/
#   LIBS=0 tools/bench-all.sh          without tools/bench-libs.sh
#   BENCH_SLOT=0 tools/bench-all.sh    always on slot 0 (to compare with a run on it)
#
# Several checkouts (worktrees) may run benchmarks on one machine: each file
# runs on a free slot of tools/bench-slot.sh (its lock held, pinned to its
# CPUs, 0-3 or 4-7), so runs do not disturb each other. Other work should
# keep off those CPUs (taskset -c 8-31 ...).
set -e
export LC_ALL=C
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT="${1:-$ROOT/build/bench-all}"
FILES="${FILES:-suite gabriel holdout programs clos control io library}"
mkdir -p "$OUT"
# build the images first, outside the slots
"$ROOT/bin/clysm" image-native >/dev/null
for f in $FILES; do
  "$ROOT/tools/bench-slot.sh" "$ROOT/tools/bench.sh" "$ROOT/bench/$f.lisp" \
    > "$OUT/$f.txt" 2>&1 || echo "bench $f failed" >> "$OUT/$f.txt"
  printf '%-10s %s\n' "$f" "$(tail -1 "$OUT/$f.txt")"
done
if [ "${LIBS:-1}" != 0 ]; then
  "$ROOT/tools/bench-slot.sh" "$ROOT/tools/bench-libs.sh" > "$OUT/libs.txt" 2>&1 || echo "bench libs failed" >> "$OUT/libs.txt"
  printf '%-10s %s\n' libs "$(tail -1 "$OUT/libs.txt")"
fi
