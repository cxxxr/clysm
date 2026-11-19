#!/bin/sh
# Run every benchmark file against SBCL and print one summary line per file.
#   tools/bench-all.sh [OUTDIR]        results in OUTDIR (build/bench-all by default)
#   FILES="suite gabriel" tools/bench-all.sh     only these files of bench/
#   LIBS=0 tools/bench-all.sh          without tools/bench-libs.sh
#
# Several checkouts (worktrees) may run benchmarks on one machine: each file
# is run holding a lock (BENCH_LOCK, /tmp/clysm-bench.lock by default) and on
# the CPUs of BENCH_CPUS only (0-3 by default; "" for no pinning), so that
# runs do not disturb each other. Other work should keep off those CPUs
# (taskset -c 4-31 ...).
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT="${1:-$ROOT/build/bench-all}"
FILES="${FILES:-suite gabriel holdout programs clos control io library}"
LOCK="${BENCH_LOCK:-/tmp/clysm-bench.lock}"
CPUS="${BENCH_CPUS-0-3}"
mkdir -p "$OUT"
# build the images first, outside the lock (and outside the benchmark CPUs)
"$ROOT/bin/clysm" image-native >/dev/null
for f in $FILES; do
  flock "$LOCK" sh -c "$(command -v taskset >/dev/null && [ -n "$CPUS" ] && echo "taskset -c $CPUS") \"$ROOT/tools/bench.sh\" \"$ROOT/bench/$f.lisp\"" \
    > "$OUT/$f.txt" 2>&1 || echo "bench $f failed" >> "$OUT/$f.txt"
  printf '%-10s %s\n' "$f" "$(tail -1 "$OUT/$f.txt")"
done
if [ "${LIBS:-1}" != 0 ]; then
  flock "$LOCK" sh -c "$(command -v taskset >/dev/null && [ -n "$CPUS" ] && echo "taskset -c $CPUS") \"$ROOT/tools/bench-libs.sh\"" \
    > "$OUT/libs.txt" 2>&1 || echo "bench libs failed" >> "$OUT/libs.txt"
  printf '%-10s %s\n' libs "$(tail -1 "$OUT/libs.txt")"
fi
