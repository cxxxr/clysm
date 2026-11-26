#!/bin/sh
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
cd "$(dirname "$0")/.." || exit 1
if [ -n "$THROWN" ] && [ -n "$NATIVE$INTERP$STATIC$SELFHOST" ]; then
  echo "THROWN=1 compiles with tools/build.lisp only, not with NATIVE, INTERP, STATIC or SELFHOST" >&2; exit 2
fi
NURSERY="--max-semi-space-size=${CLYSM_NURSERY_MB:-256}"   # as bin/clysm starts the image
mkdir -p build/tests
pass=0; fail=0; failed=""
for f in ${@:-tests/cases/*.lisp}; do
  name=$(basename "$f" .lisp)
  rm -f "build/tests/$name.out"
  if [ -f "tests/cases/$name.expected" ]; then
    cp "tests/cases/$name.expected" "build/tests/$name.exp"
  else
    sbcl --script "$f" > "build/tests/$name.exp" 2>/dev/null
  fi
  if [ -n "$NATIVE" ]; then
    timeout 120 node $NURSERY host/run.mjs "$(bin/clysm image-native)" "$f" > "build/tests/$name.out" 2> "build/tests/$name.err"
  elif [ -n "$INTERP" ]; then
    timeout 60 node $NURSERY host/run.mjs "$(bin/clysm image-interp)" "$f" > "build/tests/$name.out" 2> "build/tests/$name.err"
  else
    if [ -n "$STATIC" ]; then
      compile="bin/clysm build -o"
    elif [ -n "$SELFHOST" ]; then
      compile="node host/run.mjs build/clysmc.wasm${SHAKE:+ --shake}"
    else
      compile="sbcl --script tools/build.lisp${SHAKE:+ --shake}${THROWN:+ --thrown-exits}"
    fi
    # (a time limit: a compiler that does not finish is a failure, not a wait)
    if ! timeout 600 $compile "build/tests/$name.wasm" "$f" > "build/tests/$name.build" 2>&1; then
      echo "FAIL $name (compile)"; fail=$((fail+1)); failed="$failed $name"; continue
    fi
    timeout 20 node host/run.mjs "build/tests/$name.wasm" > "build/tests/$name.out" 2> "build/tests/$name.err"
  fi
  if cmp -s "build/tests/$name.exp" "build/tests/$name.out"; then
    pass=$((pass+1))
  else
    echo "FAIL $name"; fail=$((fail+1)); failed="$failed $name"
  fi
done
echo "passed: $pass  failed: $fail"
[ -n "$failed" ] && echo "failed:$failed"
[ $fail -eq 0 ]
