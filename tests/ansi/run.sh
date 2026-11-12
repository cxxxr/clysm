#!/bin/sh
# Run the ANSI test suite (third_party/ansi-test) on Clysm.
#   tests/ansi/run.sh [CATEGORY...]
# Each category runs in its own process; if a test crashes the process (a Wasm
# trap, out of memory, or a timeout), it is recorded as CRASH and the run
# resumes with the next test.  Results go to build/ansi/.
cd "$(dirname "$0")/../.." || exit 1
[ -f third_party/ansi-test/rt.lsp ] || git submodule update --init third_party/ansi-test
IMAGE=$(bin/clysm image-native) || exit 1
CATS=${*:-"symbols eval-and-compile data-and-control-flow iteration objects conditions cons arrays hash-tables packages numbers sequences structures types-and-classes strings characters pathnames files streams printer reader system-construction environment misc"}
TIMEOUT=${ANSI_TIMEOUT:-1800}   # whole process
STALL=${ANSI_STALL:-30}          # seconds without progress before a test is considered hung
mkdir -p build/ansi
: > build/ansi/summary.txt
for cat in $CATS; do
  [ -f "third_party/ansi-test/$cat/load.lsp" ] || { echo "no category $cat"; continue; }
  log="build/ansi/$cat.log"; : > "$log"
  start=0
  while :; do
    base=$(wc -c < "$log")
    timeout "$TIMEOUT" node host/run.mjs "$IMAGE" tests/ansi/driver.lisp "$cat" "$start" >> "$log" 2>&1 &
    pid=$!
    # watchdog: kill the run when the log stops growing (a hung test)
    size=-1; idle=0
    while kill -0 $pid 2>/dev/null; do
      sleep 2
      new=$(wc -c < "$log")
      if [ "$new" = "$size" ]; then idle=$((idle + 2)); else idle=0; size=$new; fi
      if [ $idle -ge $STALL ] && tail -c +$((base + 1)) "$log" | grep -aq "^@@TOTAL"; then
        kill $pid 2>/dev/null; break
      fi
    done
    wait $pid 2>/dev/null
    if grep -aq "^@@DONE" "$log"; then break; fi
    # the process died: the last started test crashed
    last=$(grep -a "^@@START" "$log" | tail -1 | awk '{print $2}')
    if [ -z "$last" ] || [ "$last" -lt "$start" ]; then echo "@@LOADFAIL" >> "$log"; break; fi
    name=$(grep -a "^@@START $last " "$log" | tail -1 | cut -d' ' -f3-)
    echo "@@ $last CRASH $name" >> "$log"
    start=$((last + 1))
  done
  total=$(grep -a "^@@TOTAL" "$log" | head -1 | awk '{print $2}')
  pass=$(grep -ac "^@@ [0-9]* PASS" "$log")
  fail=$(grep -ac "^@@ [0-9]* FAIL" "$log")
  crash=$(grep -ac "^@@ [0-9]* CRASH" "$log")
  if grep -aq "^@@LOADFAIL" "$log"; then status="(load failed)"; else status=""; fi
  printf "%-24s total %5s  pass %5s  fail %5s  crash %4s %s\n" "$cat" "${total:-0}" "$pass" "$fail" "$crash" "$status" | tee -a build/ansi/summary.txt
done
awk '{t+=$3; p+=$5; f+=$7; c+=$9} END {printf "%-24s total %5d  pass %5d  fail %5d  crash %4d  (%.1f%%)\n", "ALL", t, p, f, c, (t ? 100*p/t : 0)}' build/ansi/summary.txt | tee -a build/ansi/summary.txt
