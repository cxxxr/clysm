#!/bin/sh
# Run a command on a free benchmark slot: holding that slot's lock and pinned
# to its CPUs, so that benchmarks of parallel checkouts do not disturb each
# other. Slots: CPUs 0-3 (/tmp/clysm-bench.lock) and 4-7
# (/tmp/clysm-bench2.lock); other work should keep to CPUs 8-31.
#   tools/bench-slot.sh tools/bench.sh bench/clos.lisp
#   BENCH_SLOT=0 tools/bench-slot.sh ...     always slot 0 (compare like with like)
export LC_ALL=C
slot_lock() { if [ "$1" = 0 ]; then echo /tmp/clysm-bench.lock; else echo /tmp/clysm-bench2.lock; fi; }
slot_cpus() { if [ "$1" = 0 ]; then echo 0-3; else echo 4-7; fi; }
[ -n "$BENCH_IN_SLOT" ] && exec "$@"   # already on a slot (tools/bench-all.sh inside)
if [ -n "$BENCH_SLOT" ]; then
  export BENCH_IN_SLOT=1
  exec flock "$(slot_lock "$BENCH_SLOT")" taskset -c "$(slot_cpus "$BENCH_SLOT")" "$@"
fi
while :; do
  for s in 0 1; do
    # take the slot if its lock is free right now
    exec 9>>"$(slot_lock $s)"
    if flock -n 9; then
      export BENCH_IN_SLOT=1
      taskset -c "$(slot_cpus $s)" "$@"; status=$?
      exit $status
    fi
    exec 9>&-
  done
  sleep 2
done
