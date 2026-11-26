#!/bin/bash
# Compare Clysm against SBCL on every tests/cases/*.lisp file.
# A file NAME.expected overrides the SBCL output.
# With INTERP=1 the files are run by the in-image evaluator (bin/clysm script)
# instead of being compiled to WebAssembly.
# With NATIVE=1 they are loaded by the REPL image that embeds the compiler
# (bin/clysm image-native): every toplevel form is compiled to Wasm at run time.
# With SELFHOST=1 the files are compiled by the compiler running in Wasm
# (build/clysmc.wasm, see tools/build-selfhost.sh).
# With STATIC=1 they are compiled as whole programs by the compiler of the
# REPL image (bin/clysm build, src/static.lisp).
# With SHAKE=1 they are compiled with --shake (the functions nothing refers
# to are left out).
# With THROWN=1 they are compiled with --thrown-exits (every non-local exit a
# Wasm exception); only so, not with NATIVE, INTERP, STATIC or SELFHOST.
# The files run TEST_JOBS at a time (the number of processors by default).
# Compiled by the SBCL-hosted compiler, they are compiled by one process
# (tools/build-many.lisp), which compiles the runtime once.
cd "$(dirname "$0")/.." || exit 1
if [ -n "$THROWN" ] && [ -n "$NATIVE$INTERP$STATIC$SELFHOST" ]; then
  echo "THROWN=1 compiles with tools/build.lisp only, not with NATIVE, INTERP, STATIC or SELFHOST" >&2; exit 2
fi
NURSERY="--max-semi-space-size=${CLYSM_NURSERY_MB:-256}"   # as bin/clysm starts the image
JOBS=${TEST_JOBS:-$(nproc 2>/dev/null || echo 4)}
mkdir -p build/tests
files=(${@:-tests/cases/*.lisp})
# (the images are made, when they are stale, before the files run at once)
if [ -n "$NATIVE" ]; then image=$(bin/clysm image-native) || exit 1
elif [ -n "$INTERP" ]; then image=$(bin/clysm image-interp) || exit 1
elif [ -n "$STATIC" ]; then bin/clysm eval --no-userinit nil > /dev/null || exit 1   # (and its heap image)
fi
batch=""; [ -z "$NATIVE$INTERP$STATIC$SELFHOST" ] && batch=1
for f in "${files[@]}"; do
  name=$(basename "$f" .lisp)
  rm -f "build/tests/$name.out" "build/tests/$name.res" "build/tests/$name.wasm"
done
if [ -n "$batch" ]; then
  : > build/tests/jobs.txt
  for f in "${files[@]}"; do
    name=$(basename "$f" .lisp)
    echo "build/tests/$name.wasm build/tests/$name.build $f" >> build/tests/jobs.txt
  done
  # (a time limit: a compiler that does not finish is a failure, not a wait.
  # tests/cases/deep-forms.lisp has a definition that SBCL does not finish
  # with: it waits for --definition-seconds, which is the time of the whole
  # run when it is the 20 seconds of a build)
  sbcl --script tools/build-many.lisp ${SHAKE:+--shake} ${THROWN:+--thrown-exits} -j "$JOBS" --timeout 600 \
    --definition-seconds "${TEST_DEFINITION_SECONDS:-3}" \
    build/tests/jobs.txt > build/tests/jobs.log 2>&1
fi
run_case() {
  f=$1; name=$(basename "$1" .lisp)
  if [ -f "tests/cases/$name.expected" ]; then
    cp "tests/cases/$name.expected" "build/tests/$name.exp"
  else
    sbcl --script "$f" > "build/tests/$name.exp" 2>/dev/null
  fi
  if [ -n "$NATIVE" ]; then
    timeout 120 node $NURSERY host/run.mjs "$image" "$f" > "build/tests/$name.out" 2> "build/tests/$name.err"
  elif [ -n "$INTERP" ]; then
    timeout 60 node $NURSERY host/run.mjs "$image" "$f" > "build/tests/$name.out" 2> "build/tests/$name.err"
  else
    if [ -n "$batch" ]; then
      grep -qxF "@@ build/tests/$name.wasm 0" build/tests/jobs.log
    else
      if [ -n "$STATIC" ]; then compile="bin/clysm build -o"; else compile="node host/run.mjs build/clysmc.wasm${SHAKE:+ --shake}"; fi
      timeout 600 $compile "build/tests/$name.wasm" "$f" > "build/tests/$name.build" 2>&1
    fi || { echo "FAIL $name (compile)" > "build/tests/$name.res"; return; }
    timeout 20 node host/run.mjs "build/tests/$name.wasm" > "build/tests/$name.out" 2> "build/tests/$name.err"
  fi
  if cmp -s "build/tests/$name.exp" "build/tests/$name.out"; then
    echo PASS > "build/tests/$name.res"
  else
    echo "FAIL $name" > "build/tests/$name.res"
  fi
}
running=0
for f in "${files[@]}"; do
  run_case "$f" &
  running=$((running + 1))
  if [ $running -ge "$JOBS" ]; then wait -n; running=$((running - 1)); fi
done
wait
pass=0; fail=0; failed=""
for f in "${files[@]}"; do
  name=$(basename "$f" .lisp)
  res=$(cat "build/tests/$name.res" 2>/dev/null || echo "FAIL $name")
  if [ "$res" = PASS ]; then pass=$((pass+1)); else echo "$res"; fail=$((fail+1)); failed="$failed $name"; fi
done
echo "passed: $pass  failed: $fail"
[ -n "$failed" ] && echo "failed:$failed"
[ $fail -eq 0 ]
