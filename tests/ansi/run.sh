#!/bin/bash
# Run the ANSI test suite (third_party/ansi-test) on Clysm.
#   tests/ansi/run.sh [CATEGORY...]
# Each category runs in its own process; if a test crashes the process (a Wasm
# trap, out of memory, or a timeout), it is recorded as CRASH and the run
# resumes with the next test.  Results go to build/ansi/.
cd "$(dirname "$0")/../.." || exit 1
[ -f third_party/ansi-test/rt.lsp ] || git submodule update --init third_party/ansi-test
IMAGE=$(bin/clysm image-native) || exit 1
NURSERY="--max-semi-space-size=${CLYSM_NURSERY_MB:-256}"   # as bin/clysm starts the image
CATS=${*:-"symbols eval-and-compile data-and-control-flow iteration objects conditions cons arrays hash-tables packages numbers sequences structures types-and-classes strings characters pathnames files streams printer reader system-construction environment misc"}
TIMEOUT=${ANSI_TIMEOUT:-1800}   # whole process
STALL=${ANSI_STALL:-30}          # seconds without progress before a test is considered hung
# start from a pristine sandbox
git -C third_party/ansi-test checkout -q -- sandbox 2>/dev/null
git -C third_party/ansi-test clean -qfdx sandbox 2>/dev/null
JOBS=${ANSI_JOBS:-$(nproc 2>/dev/null || echo 8)}   # processes run in parallel
# A large category runs in parts of CHUNK tests, each in its own process
# (which loads the category again), so that the run is not as long as its
# largest category.  The numbers of tests below only place the parts: the
# last part runs to the end.  The categories whose tests make and delete
# files in the sandbox run in one process.
CHUNK=${ANSI_CHUNK:-400}
approx_total() {
  case $1 in
    sequences) echo 3287;; printer) echo 2446;; cons) echo 1882;; numbers) echo 1436;;
    data-and-control-flow) echo 1428;; structures) echo 1394;; arrays) echo 1356;;
    symbols) echo 1145;; objects) echo 862;; iteration) echo 838;; misc) echo 740;;
    reader) echo 673;; *) echo 0;;
  esac
}
mkdir -p build/ansi
rm -f build/ansi/*.log build/ansi/*.sum
: > build/ansi/summary.txt
# run_part CATEGORY START END LOG: the tests of CATEGORY from START up to END
# (to the end when END is empty)
run_part() {
  cat=$1; start=$2; end=$3; log=$4
  : > "$log"
  while :; do
    base=$(wc -c < "$log")
    timeout "$TIMEOUT" node $NURSERY host/run.mjs "$IMAGE" tests/ansi/driver.lisp "$cat" "$start" $end >> "$log" 2>&1 &
    pid=$!
    # watchdog: kill the run when the log stops growing (a hung test)
    size=-1; since=$(date +%s)
    while kill -0 $pid 2>/dev/null; do
      sleep 0.5
      new=$(wc -c < "$log")
      if [ "$new" != "$size" ]; then size=$new; since=$(date +%s); fi
      if [ $(( $(date +%s) - since )) -ge $STALL ] && tail -c +$((base + 1)) "$log" | grep -aq "^@@TOTAL"; then
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
    [ -n "$end" ] && [ "$start" -ge "$end" ] && { echo "@@DONE" >> "$log"; break; }
  done
}
# finish_category CATEGORY: its log (build/ansi/CATEGORY.log, the parts'
# logs one after another: @@DONE at the end when every part ran to its end)
# and its line of the summary
finish_category() {
  cat=$1; log="build/ansi/$cat.log"
  parts=""; i=0
  while [ -f "build/ansi/$cat.part$i.log" ]; do parts="$parts build/ansi/$cat.part$i.log"; i=$((i + 1)); done
  done=1
  for p in $parts; do grep -aq "^@@DONE" "$p" || done=""; done
  { for p in $parts; do grep -av "^@@DONE" "$p"; done; [ -n "$done" ] && echo "@@DONE"; } > "$log"
  rm -f $parts
  total=$(grep -a "^@@TOTAL" "$log" | head -1 | awk '{print $2}')
  pass=$(grep -ac "^@@ [0-9]* PASS" "$log")
  fail=$(grep -ac "^@@ [0-9]* FAIL" "$log")
  crash=$(grep -ac "^@@ [0-9]* CRASH" "$log")
  if grep -aq "^@@LOADFAIL" "$log"; then status="(load failed)"; else status=""; fi
  printf "%-24s total %5s  pass %5s  fail %5s  crash %4s %s\n" "$cat" "${total:-0}" "$pass" "$fail" "$crash" "$status" > "build/ansi/$cat.sum"
}
# the streams tests use sandbox/scratch/, which the files tests create (the
# suite normally runs every category in one process, files before streams)
case " $CATS " in *" files "*) run_part files 0 "" build/ansi/files.part0.log; finish_category files;; esac
# the parts, the largest categories' first
parts=$(for cat in $CATS; do
  [ -f "third_party/ansi-test/$cat/load.lsp" ] || { echo "no category $cat" >&2; continue; }
  [ "$cat" = files ] && continue
  n=$(approx_total "$cat"); k=$(( (n + CHUNK - 1) / CHUNK )); [ $k -lt 1 ] && k=1
  case $cat in streams|pathnames|system-construction) k=1;; esac
  for i in $(seq 0 $((k - 1))); do
    s=$((i * CHUNK)); e=$(( (i + 1) * CHUNK )); [ $i = $((k - 1)) ] && e=""
    echo "$n $cat $i $s $e"
  done
done | sort -k1,1nr -k3,3n | cut -d' ' -f2-)
running=0
while read -r cat i s e; do
  [ -n "$cat" ] || continue
  run_part "$cat" "$s" "$e" "build/ansi/$cat.part$i.log" &
  running=$((running + 1))
  if [ $running -ge $JOBS ]; then wait -n 2>/dev/null || wait; running=$((running - 1)); fi
done <<< "$parts"
wait
for cat in $CATS; do
  [ "$cat" = files ] || { [ -f "third_party/ansi-test/$cat/load.lsp" ] && finish_category "$cat"; }
done
for cat in $CATS; do [ -f "build/ansi/$cat.sum" ] && cat "build/ansi/$cat.sum"; done | tee build/ansi/summary.txt
awk '{t+=$3; p+=$5; f+=$7; c+=$9} END {printf "%-24s total %5d  pass %5d  fail %5d  crash %4d  (%.1f%%)\n", "ALL", t, p, f, c, (t ? 100*p/t : 0)}' build/ansi/summary.txt | tee -a build/ansi/summary.txt
# the tests create and delete files in the suite's sandbox: restore the submodule
git -C third_party/ansi-test checkout -q -- sandbox 2>/dev/null
git -C third_party/ansi-test clean -qfdx sandbox 2>/dev/null
true
