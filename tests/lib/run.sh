#!/bin/sh
# Compile tests/lib/demo.lisp as a library (bin/clysm library) and call it
# from JavaScript (tests/lib/run.mjs).
cd "$(dirname "$0")/../.." || exit 1
mkdir -p build
bin/clysm library ${SHAKE:+--shake} ${THROWN:+--thrown-exits} tests/lib/demo.lisp -o build/lib-test/demo.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/run.mjs build/lib-test/demo.mjs || exit 1
# a library without the symbols its code does not refer to (--shake-symbols)
bin/clysm library --shake-symbols tests/lib/symbols.lisp -o build/lib-test/symbols.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/symbols.mjs build/lib-test/symbols.mjs || exit 1
# a package whose original symbol table becomes empty
bin/clysm library --shake-symbols tests/lib/empty-symbols.lisp -o build/lib-test/empty-symbols.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/empty-symbols.mjs build/lib-test/empty-symbols.mjs || exit 1
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
# gzip-embedded libraries are still synchronous, eager or lazy, with source names
for mode in eager lazy; do
  options=tests/lib/compressed.lisp
  [ "$mode" = lazy ] && options=tests/lib/compressed-lazy.lisp
  bin/clysm library --shake "$options" tests/lib/compressed-body.lisp tests/lib/compression-options-invalid.lisp \
    -o "build/lib-test/compressed-$mode/compressed.wasm" > build/lib-test.log 2>&1 \
    || { cat build/lib-test.log; exit 1; }
  node tests/lib/compressed.mjs "build/lib-test/compressed-$mode" "$mode" || exit 1
done
node tests/lib/compressed.mjs build/lib-test/compressed-lazy lazy bytes || exit 1
node tests/lib/compressed.mjs build/lib-test/compressed-lazy lazy module || exit 1
# XZ uses an embedded Wasm decoder; applications still need no fetch or JSPI.
for mode in eager lazy; do
  options=tests/lib/compressed-xz.lisp
  [ "$mode" = lazy ] && options=tests/lib/compressed-xz-lazy.lisp
  bin/clysm library --shake "$options" tests/lib/compressed-body.lisp tests/lib/compression-options-invalid.lisp \
    -o "build/lib-test/compressed-xz-$mode/compressed.wasm" > build/lib-test.log 2>&1 \
    || { cat build/lib-test.log; exit 1; }
  node tests/lib/compressed.mjs "build/lib-test/compressed-xz-$mode" "$mode" || exit 1
done
node tests/lib/compressed.mjs build/lib-test/compressed-xz-lazy lazy bytes || exit 1
node tests/lib/compressed.mjs build/lib-test/compressed-xz-lazy lazy module || exit 1
node tests/lib/xz.mjs || exit 1
# the printer without the reader's macro functions
bin/clysm library --shake tests/lib/print-only.lisp -o build/lib-test/print-only.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/print-only.mjs build/lib-test/print-only.mjs || exit 1
# an explicit fallback message without retaining the Lisp printer
bin/clysm library --shake tests/lib/throw-message.lisp -o build/lib-test/throw-message.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/throw-message.mjs build/lib-test/throw-message.mjs || exit 1
# conditions from another Wasm instance remain JS errors; local round trips retain identity
bin/clysm library --shake tests/lib/error-owners.lisp -o build/lib-test/error-owners.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/error-owners.mjs build/lib-test/error-owners.wasm || exit 1
# application definitions initialized during compilation, with exports at load time
bin/clysm library --shake --preinit-through tests/lib/preinit-definitions.lisp \
  tests/lib/preinit-definitions.lisp tests/lib/preinit-exports.lisp \
  -o build/lib-test/preinit.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/preinit.mjs build/lib-test/preinit.mjs > build/lib-test/preinit.out 2>&1 || { cat build/lib-test/preinit.out; exit 1; }
if [ "$(cat build/lib-test/preinit.out)" != 'lib: preinitialized definitions ok' ]; then
  echo 'lib: preinitialized forms ran again at load time'; cat build/lib-test/preinit.out; exit 1
fi
cat build/lib-test/preinit.out
if bin/clysm compile --preinit-through tests/lib/preinit-definitions.lisp \
  tests/lib/preinit-definitions.lisp -o build/lib-test/preinit-bad.wasm > build/lib-test.log 2>&1; then
  echo 'lib: preinitialization without --shake was accepted'; exit 1
fi
if bin/clysm compile --shake --preinit-through tests/lib/preinit-exports.lisp \
  tests/lib/preinit-definitions.lisp -o build/lib-test/preinit-bad.wasm > build/lib-test.log 2>&1; then
  echo 'lib: a preinitialization boundary outside the input files was accepted'; exit 1
fi
# Explicit preinitialization must fail, rather than defer it to module load,
# when the subprocess that builds its heap cannot run.
if NODE=/bin/false bin/clysm compile --shake --preinit-through tests/lib/preinit-definitions.lisp \
  tests/lib/preinit-definitions.lisp tests/lib/preinit-exports.lisp \
  -o build/lib-test/preinit-bad.wasm > build/lib-test.log 2>&1; then
  echo 'lib: failed explicit preinitialization silently fell back'; exit 1
fi
# generated loaders find the module in a relocated package or a bundle's assets
bin/clysm library --shake tests/lib/locations.lisp -o build/lib-locations/locations.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
for mode in first second missing override; do
  node tests/lib/locations.mjs build/lib-locations "$mode" || exit 1
done
bin/clysm library --shake tests/lib/locations.lisp tests/lib/locations-eager.lisp -o build/lib-locations-eager/locations.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/locations-eager.mjs build/lib-locations-eager || exit 1
# a public entry composed of generated values, external values, and type-only exports
bin/clysm library --shake tests/lib/reexports.lisp -o build/lib-reexports/reexports.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/reexports.mjs build/lib-reexports || exit 1
for variant in eager embed; do
  bin/clysm library --shake tests/lib/reexports.lisp tests/lib/reexports-$variant.lisp -o build/lib-reexports-$variant/reexports.wasm > build/lib-test.log 2>&1 \
    || { cat build/lib-test.log; exit 1; }
  node tests/lib/reexports.mjs build/lib-reexports-$variant || exit 1
done
bin/clysm compile --shake tests/lib/reexports-invalid.lisp -o build/lib-reexports/invalid.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/reexports-invalid.mjs build/lib-reexports/invalid.wasm || exit 1
# generated initializers preserve lazy imports, entry URLs and bundled inputs
bin/clysm library --shake tests/lib/entrypoints.lisp -o build/lib-entrypoints/entrypoints.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/entrypoints.mjs build/lib-entrypoints || exit 1
bin/clysm library --shake tests/lib/entrypoints.lisp tests/lib/entrypoints-embed.lisp \
  -o build/lib-entrypoints-embed/entrypoints.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/entrypoints.mjs build/lib-entrypoints-embed || exit 1
bin/clysm compile --shake tests/lib/entrypoints-invalid.lisp -o build/lib-entrypoints/invalid.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
node tests/lib/entrypoints-invalid.mjs build/lib-entrypoints/invalid.wasm || exit 1
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
# the same with --shake-symbols, which leaves out more than --shake
bin/clysm library --shake-symbols tests/lib/one.lisp -o build/lib-test/one-symbols.wasm > build/lib-test.log 2>&1 \
  || { cat build/lib-test.log; exit 1; }
size=$(wc -c < build/lib-test/one-symbols.wasm); max=$(cat tests/lib/one-symbols.max-bytes)
if [ "$size" -gt "$max" ]; then
  echo "lib: the module of one export with --shake-symbols is $size bytes, more than $max (tests/lib/one-symbols.max-bytes);"
  echo "  bin/clysm library --shake-symbols --shake-why REPORT tests/lib/one.lisp -o OUT.wasm says what keeps each function"
  exit 1
fi
shaken=$(wc -c < build/lib-test/one.wasm)
if [ "$size" -gt "$shaken" ]; then
  echo "lib: the module of one export is larger with --shake-symbols ($size bytes) than with --shake ($shaken)"; exit 1
fi
echo "lib: the module of one export with --shake-symbols is $size bytes (at most $max)"
