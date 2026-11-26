#!/bin/bash
# make test-all: the suites that run one program after another (test-link,
# test-fasl-images, test-lib, test-asdf, test-static) and the build of the
# Wasm-hosted compiler (make selfhost) run at the same time as the modes of
# tools/run-tests.sh, which run one after another (they share build/tests/),
# each of them TEST_JOBS files at a time.  Their outputs are printed when they
# end; the status is that of a failure if one failed.
cd "$(dirname "$0")/.." || exit 1
mkdir -p build/test-all
# (the images, before the suites start them at once)
bin/clysm eval --no-userinit nil > /dev/null || { echo "test-all: the REPL image was not built"; exit 1; }
bin/clysm image-interp > /dev/null || { echo "test-all: the interpreter's image was not built"; exit 1; }
status=0
pids=""
for t in test-link test-fasl-images test-lib test-asdf test-static selfhost; do
  make --no-print-directory "$t" > "build/test-all/$t.log" 2>&1 &
  pids="$pids $!:$t"
done
mode() {
  echo "${1:-compiled}: tools/run-tests.sh"
  env $1 tools/run-tests.sh || status=1
}
mode
mode STATIC=1
mode INTERP=1
mode NATIVE=1
for p in $pids; do
  wait "${p%%:*}"; s=$?
  t=${p#*:}
  echo "== make $t"
  if [ "$t" = selfhost ]; then tail -n 1 "build/test-all/$t.log"; else cat "build/test-all/$t.log"; fi
  [ $s = 0 ] || { echo "make $t: FAIL"; status=1; [ "$t" = selfhost ] && selfhost_failed=1; }
done
[ -n "$selfhost_failed" ] || mode SELFHOST=1
exit $status
