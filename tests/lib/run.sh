#!/bin/sh
# Compile tests/lib/demo.lisp as a library (bin/clysm library) and call it
# from JavaScript (tests/lib/run.mjs).
cd "$(dirname "$0")/../.." || exit 1
mkdir -p build
bin/clysm library ${SHAKE:+--shake} tests/lib/demo.lisp -o build/lib-test/demo.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/run.mjs build/lib-test/demo.mjs || exit 1
# a library without the symbols its code does not refer to (--shake-symbols)
bin/clysm library --shake-symbols tests/lib/symbols.lisp -o build/lib-test/symbols.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/symbols.mjs build/lib-test/symbols.mjs || exit 1
# the messages of unhandled errors written without the printer (--shake):
# those of tests/lib/messages.expected, which a module with the printer writes
bin/clysm library --shake tests/lib/messages.lisp -o build/lib-test/messages.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/messages.mjs build/lib-test/messages.mjs > build/lib-test/messages.out 2>&1
if ! cmp -s tests/lib/messages.expected build/lib-test/messages.out; then
  echo "lib: the messages of unhandled errors differ (tests/lib/messages.expected):"
  diff tests/lib/messages.expected build/lib-test/messages.out
  exit 1
fi
echo "lib: messages ok"
# the size of a module of one export (tests/lib/one.lisp)
bin/clysm library --shake tests/lib/one.lisp -o build/lib-test/one.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
size=$(wc -c < build/lib-test/one.wasm); max=$(cat tests/lib/one.max-bytes)
if [ "$size" -gt "$max" ]; then
  echo "lib: the module of one export is $size bytes, more than $max (tests/lib/one.max-bytes);"
  echo "  bin/clysm library --shake-why REPORT tests/lib/one.lisp -o OUT.wasm says what keeps each function"
  exit 1
fi
echo "lib: the module of one export is $size bytes (at most $max)"
