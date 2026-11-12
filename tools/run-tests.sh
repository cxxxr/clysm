#!/bin/sh
# Compare Clysm against SBCL on every tests/cases/*.lisp file.
# A file NAME.expected overrides the SBCL output.
# With INTERP=1 the files are run by the in-image evaluator (bin/clysm script)
# instead of being compiled to WebAssembly.
# With SELFHOST=1 the files are compiled by the compiler running in Wasm
# (build/clysmc.wasm, see tools/build-selfhost.sh).
cd "$(dirname "$0")/.." || exit 1
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
  if [ -n "$INTERP" ]; then
    timeout 60 bin/clysm script "$f" > "build/tests/$name.out" 2> "build/tests/$name.err"
  else
    if [ -n "$SELFHOST" ]; then
      compile="node host/run.mjs build/clysmc.wasm"
    else
      compile="sbcl --script tools/build.lisp"
    fi
    if ! $compile "build/tests/$name.wasm" "$f" > "build/tests/$name.build" 2>&1; then
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
