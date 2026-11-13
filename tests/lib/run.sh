#!/bin/sh
# Compile tests/lib/demo.lisp as a library (bin/clysm library) and call it
# from JavaScript (tests/lib/run.mjs).
cd "$(dirname "$0")/../.." || exit 1
bin/clysm library tests/lib/demo.lisp -o build/lib-test/demo.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/run.mjs build/lib-test/demo.mjs
