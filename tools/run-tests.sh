#!/bin/sh
# Compare Clysm against SBCL on every tests/cases/*.lisp file.
# A file NAME.expected overrides the SBCL output.
cd "$(dirname "$0")/.." || exit 1
mkdir -p build/tests
pass=0; fail=0; failed=""
for f in ${@:-tests/cases/*.lisp}; do
  name=$(basename "$f" .lisp)
  if [ -f "tests/cases/$name.expected" ]; then
    cp "tests/cases/$name.expected" "build/tests/$name.exp"
  else
    sbcl --script "$f" > "build/tests/$name.exp" 2>/dev/null
  fi
  if ! sbcl --script tools/build.lisp "build/tests/$name.wasm" "$f" > "build/tests/$name.build" 2>&1; then
    echo "FAIL $name (compile)"; fail=$((fail+1)); failed="$failed $name"; continue
  fi
  timeout 20 node host/run.mjs "build/tests/$name.wasm" > "build/tests/$name.out" 2> "build/tests/$name.err"
  if cmp -s "build/tests/$name.exp" "build/tests/$name.out"; then
    pass=$((pass+1))
  else
    echo "FAIL $name"; fail=$((fail+1)); failed="$failed $name"
  fi
done
echo "passed: $pass  failed: $fail"
[ -n "$failed" ] && echo "failed:$failed"
[ $fail -eq 0 ]
