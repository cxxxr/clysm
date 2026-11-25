#!/bin/sh
# The self-hosted compiler (build/clysmc.wasm, make selfhost) shakes a module
# without the heap of src/preinit.lisp: the literal names of JavaScript of a
# function that nothing calls are left out with it (JS-NAME-CELL-GLOBAL in
# src/codegen.lisp), as tests/lib/run.sh checks of bin/clysm library.
cd "$(dirname "$0")/../.." || exit 1
mkdir -p build/lib-test
for f in one one-unused-names; do
  node host/run.mjs build/clysmc.wasm --shake build/lib-test/selfhost-$f.wasm tests/lib/$f.lisp \
    > build/lib-test.log 2>&1 || { cat build/lib-test.log; exit 1; }
done
one=$(wc -c < build/lib-test/selfhost-one.wasm); unused=$(wc -c < build/lib-test/selfhost-one-unused-names.wasm)
if [ "$unused" -gt $((one + 300)) ]; then
  echo "selfhost: 40 names of a function nothing calls kept $((unused - one)) bytes"; exit 1
fi
echo "selfhost: 40 names of a function nothing calls keep $((unused - one)) bytes"
