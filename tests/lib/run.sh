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
# classes and a Gray stream in a shaken library (tests/lib/clos.lisp)
bin/clysm library --shake tests/lib/clos.lisp -o build/lib-test/clos.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/clos.mjs build/lib-test/clos.mjs || exit 1
# generic functions without their dispatch in a library that adds no method (tests/lib/nodispatch.lisp)
bin/clysm library --shake tests/lib/nodispatch.lisp -o build/lib-test/nodispatch.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/nodispatch.mjs build/lib-test/nodispatch.mjs || exit 1
# what a package of JavaScript exports, as one module and as a lazy one (tests/lib/package.lisp)
bin/clysm library --shake tests/lib/package.lisp -o build/lib-test/package/package.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/package.mjs build/lib-test/package eager > build/lib-test/eager.out 2> build/lib-test/eager.err || { cat build/lib-test/eager.out build/lib-test/eager.err; exit 1; }
cat build/lib-test/eager.out
if grep -q 'to the standard error' build/lib-test/eager.err || ! grep -qx 'to the standard error' build/lib-test/eager.out; then
  echo "lib: what the library writes to its standard output is not on the standard output"; exit 1
fi
bin/clysm library --shake tests/lib/to-error.lisp tests/lib/package.lisp -o build/lib-test/to-error/package.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/package.mjs build/lib-test/to-error eager > build/lib-test/to-error.out 2> build/lib-test/to-error.err || { cat build/lib-test/to-error.out build/lib-test/to-error.err; exit 1; }
if ! grep -qx 'to the standard error' build/lib-test/to-error.err || grep -q 'to the standard error' build/lib-test/to-error.out; then
  echo "lib: what the library of :output :error writes to its standard output is not on the standard error output"; exit 1
fi
bin/clysm library --shake tests/lib/lazy.lisp tests/lib/package.lisp -o build/lib-test/lazy/package.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
echo '{ "type": "module" }' > build/lib-test/lazy/package.json
node tests/lib/package.mjs build/lib-test/lazy lazy > build/lib-test/lazy.out 2> build/lib-test/lazy.err || { cat build/lib-test/lazy.out build/lib-test/lazy.err; exit 1; }
cat build/lib-test/lazy.out
if ! grep -qx 'to the standard error' build/lib-test/lazy.err || grep -q 'to the standard error' build/lib-test/lazy.out; then
  echo "lib: what the lazy library writes to its standard output is not on the standard error output"; exit 1
fi
bin/clysm library --shake tests/lib/embed.lisp tests/lib/package.lisp -o build/lib-test/embedded/package.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
echo '{ "type": "module" }' > build/lib-test/embedded/package.json
node tests/lib/package.mjs build/lib-test/embedded embedded > build/lib-test/embedded.out 2> build/lib-test/embedded.err || { cat build/lib-test/embedded.out build/lib-test/embedded.err; exit 1; }
cat build/lib-test/embedded.out
if ! grep -qx 'to the standard error' build/lib-test/embedded.err || grep -q 'to the standard error' build/lib-test/embedded.out; then
  echo "lib: what the embedded library of :output :error writes to its standard output is not on the standard error output"; exit 1
fi
# the printer without the reader's macro functions
bin/clysm library --shake tests/lib/print-only.lisp -o build/lib-test/print-only.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/print-only.mjs build/lib-test/print-only.mjs || exit 1
# an explicit fallback message without retaining the Lisp printer
bin/clysm library --shake tests/lib/throw-message.lisp -o build/lib-test/throw-message.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/throw-message.mjs build/lib-test/throw-message.mjs || exit 1
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
