#!/bin/bash
# Compile the programs of tests/lib as libraries (bin/clysm library) and call
# them from JavaScript (tests/lib/*.mjs).  The sections below (a library and
# what is checked of it) run TEST_JOBS at a time (the number of processors by
# default), each in a process of its own with its own log ($log); what they
# print is printed in their order when all ended.
cd "$(dirname "$0")/../.." || exit 1
mkdir -p build/lib-test/sections
JOBS=${TEST_JOBS:-$(nproc 2>/dev/null || echo 4)}
# (the compiler's fasls, before the sections compile them at once: load.lisp
# makes build/fasl/ when it is stale)
sbcl --script load.lisp > build/lib-test/fasl.log 2>&1 || { cat build/lib-test/fasl.log; exit 1; }
sections=""
sections="$sections demo"
section_demo() {
  bin/clysm library ${SHAKE:+--shake} ${THROWN:+--thrown-exits} tests/lib/demo.lisp -o build/lib-test/demo.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/run.mjs build/lib-test/demo.mjs || exit 1
}
sections="$sections symbols"
section_symbols() {
  # a library without the symbols its code does not refer to (--shake-symbols)
  bin/clysm library --shake-symbols tests/lib/symbols.lisp -o build/lib-test/symbols.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/symbols.mjs build/lib-test/symbols.mjs || exit 1
}
sections="$sections symbols-message"
section_symbols_message() {
  # the message of an error that names a symbol whose name is that of one left out
  bin/clysm library --shake-symbols tests/lib/symbols-message.lisp -o build/lib-test/symbols-message.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/symbols-message.mjs build/lib-test/symbols-message.mjs || exit 1
}
sections="$sections empty-symbols"
section_empty_symbols() {
  # a package whose original symbol table becomes empty
  bin/clysm library --shake-symbols tests/lib/empty-symbols.lisp -o build/lib-test/empty-symbols.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/empty-symbols.mjs build/lib-test/empty-symbols.mjs || exit 1
}
sections="$sections messages"
section_messages() {
  # the messages of unhandled errors written without the printer (--shake):
  # those of tests/lib/messages.expected, which a module with the printer writes
  bin/clysm library --shake tests/lib/messages.lisp -o build/lib-test/messages.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/messages.mjs build/lib-test/messages.mjs > build/lib-test/messages.out 2>&1
  if ! cmp -s tests/lib/messages.expected build/lib-test/messages.out; then
    echo "lib: the messages of unhandled errors differ (tests/lib/messages.expected):"
    diff tests/lib/messages.expected build/lib-test/messages.out
    exit 1
  fi
  echo "lib: messages ok"
}
sections="$sections clos"
section_clos() {
  # classes and a Gray stream in a shaken library (tests/lib/clos.lisp)
  bin/clysm library --shake tests/lib/clos.lisp -o build/lib-test/clos.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/clos.mjs build/lib-test/clos.mjs || exit 1
}
sections="$sections nodispatch"
section_nodispatch() {
  # generic functions without their dispatch in a library that adds no method (tests/lib/nodispatch.lisp)
  bin/clysm library --shake tests/lib/nodispatch.lisp -o build/lib-test/nodispatch.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/nodispatch.mjs build/lib-test/nodispatch.mjs || exit 1
}
sections="$sections package"
section_package() {
  # what a package of JavaScript exports, as one module and as a lazy one (tests/lib/package.lisp)
  bin/clysm library --shake tests/lib/package.lisp -o build/lib-test/package/package.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/package.mjs build/lib-test/package eager > build/lib-test/eager.out 2> build/lib-test/eager.err || { cat build/lib-test/eager.out build/lib-test/eager.err; exit 1; }
  cat build/lib-test/eager.out
  if grep -q 'to the standard error' build/lib-test/eager.err || ! grep -qx 'to the standard error' build/lib-test/eager.out; then
    echo "lib: what the library writes to its standard output is not on the standard output"; exit 1
  fi
}
sections="$sections to-error"
section_to_error() {
  bin/clysm library --shake tests/lib/to-error.lisp tests/lib/package.lisp -o build/lib-test/to-error/package.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/package.mjs build/lib-test/to-error eager > build/lib-test/to-error.out 2> build/lib-test/to-error.err || { cat build/lib-test/to-error.out build/lib-test/to-error.err; exit 1; }
  if ! grep -qx 'to the standard error' build/lib-test/to-error.err || grep -q 'to the standard error' build/lib-test/to-error.out; then
    echo "lib: what the library of :output :error writes to its standard output is not on the standard error output"; exit 1
  fi
}
sections="$sections lazy"
section_lazy() {
  bin/clysm library --shake tests/lib/lazy.lisp tests/lib/package.lisp -o build/lib-test/lazy/package.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  echo '{ "type": "module" }' > build/lib-test/lazy/package.json
  node tests/lib/package.mjs build/lib-test/lazy lazy > build/lib-test/lazy.out 2> build/lib-test/lazy.err || { cat build/lib-test/lazy.out build/lib-test/lazy.err; exit 1; }
  cat build/lib-test/lazy.out
  node tests/lib/package.mjs build/lib-test/lazy lazy async > build/lib-test/lazy-async.out 2>&1 || { cat build/lib-test/lazy-async.out; exit 1; }
  if ! grep -qx 'to the standard error' build/lib-test/lazy.err || grep -q 'to the standard error' build/lib-test/lazy.out; then
    echo "lib: what the lazy library writes to its standard output is not on the standard error output"; exit 1
  fi
}
sections="$sections embedded"
section_embedded() {
  bin/clysm library --shake tests/lib/embed.lisp tests/lib/package.lisp -o build/lib-test/embedded/package.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  echo '{ "type": "module" }' > build/lib-test/embedded/package.json
  node tests/lib/package.mjs build/lib-test/embedded embedded > build/lib-test/embedded.out 2> build/lib-test/embedded.err || { cat build/lib-test/embedded.out build/lib-test/embedded.err; exit 1; }
  cat build/lib-test/embedded.out
  node tests/lib/package.mjs build/lib-test/embedded embedded async > build/lib-test/embedded-async.out 2>&1 || { cat build/lib-test/embedded-async.out; exit 1; }
  if ! grep -qx 'to the standard error' build/lib-test/embedded.err || grep -q 'to the standard error' build/lib-test/embedded.out; then
    echo "lib: what the embedded library of :output :error writes to its standard output is not on the standard error output"; exit 1
  fi
}
sections="$sections gzip"
section_gzip() {
  # gzip-embedded libraries are still synchronous, eager or lazy, with source names
  for mode in eager lazy; do
    options=tests/lib/compressed.lisp
    [ "$mode" = lazy ] && options=tests/lib/compressed-lazy.lisp
    bin/clysm library --shake "$options" tests/lib/compressed-body.lisp tests/lib/compression-options-invalid.lisp \
      -o "build/lib-test/compressed-$mode/compressed.wasm" > "$log" 2>&1 \
      || { cat "$log"; exit 1; }
    node tests/lib/compressed.mjs "build/lib-test/compressed-$mode" "$mode" || exit 1
  done
  node tests/lib/compressed.mjs build/lib-test/compressed-lazy lazy bytes || exit 1
  node tests/lib/compressed.mjs build/lib-test/compressed-lazy lazy module || exit 1
  node tests/lib/compressed.mjs build/lib-test/compressed-lazy lazy async || exit 1
  node tests/lib/compressed.mjs build/lib-test/compressed-lazy lazy async-bytes || exit 1
  node tests/lib/compressed.mjs build/lib-test/compressed-lazy lazy async-retry || exit 1
  node tests/lib/compressed.mjs build/lib-test/compressed-lazy lazy async-replace || exit 1
  node tests/lib/compressed.mjs build/lib-test/compressed-lazy lazy async-early || exit 1
}
sections="$sections xz"
section_xz() {
  # XZ uses an embedded Wasm decoder; applications still need no fetch or JSPI.
  for mode in eager lazy; do
    options=tests/lib/compressed-xz.lisp
    [ "$mode" = lazy ] && options=tests/lib/compressed-xz-lazy.lisp
    bin/clysm library --shake "$options" tests/lib/compressed-body.lisp tests/lib/compression-options-invalid.lisp \
      -o "build/lib-test/compressed-xz-$mode/compressed.wasm" > "$log" 2>&1 \
      || { cat "$log"; exit 1; }
    node tests/lib/compressed.mjs "build/lib-test/compressed-xz-$mode" "$mode" || exit 1
  done
  node tests/lib/compressed.mjs build/lib-test/compressed-xz-lazy lazy bytes || exit 1
  node tests/lib/compressed.mjs build/lib-test/compressed-xz-lazy lazy module || exit 1
  node tests/lib/compressed.mjs build/lib-test/compressed-xz-lazy lazy async || exit 1
  node tests/lib/compressed.mjs build/lib-test/compressed-xz-lazy lazy async-bytes || exit 1
  node tests/lib/compressed.mjs build/lib-test/compressed-xz-lazy lazy async-retry || exit 1
  node tests/lib/compressed.mjs build/lib-test/compressed-xz-lazy lazy async-replace || exit 1
  node tests/lib/compressed.mjs build/lib-test/compressed-xz-lazy lazy async-early || exit 1
  node tests/lib/xz.mjs || exit 1
}
sections="$sections wasm-opt"
section_wasm_opt() {
  # --wasm-opt (tools/wasm-opt.mjs): a wasm-opt that is not there is an error, not a module left as it was
  if CLYSM_WASM_OPT_BIN=build/lib-test/no-wasm-opt bin/clysm library --shake --wasm-opt tests/lib/one.lisp \
       -o build/lib-wasm-opt/none/one.wasm > "$log" 2>&1 || ! grep -q 'CLYSM_WASM_OPT_BIN names .*no-wasm-opt, which does not exist' "$log"; then
    echo 'lib: --wasm-opt without a wasm-opt did not fail saying so'; cat "$log"; exit 1
  fi
  if bin/clysm library --shake '--wasm-opt=--no-such-pass' tests/lib/one.lisp -o build/lib-wasm-opt/none/one.wasm > "$log" 2>&1; then
    node tools/wasm-opt.mjs --find > /dev/null 2>&1 && { echo 'lib: --wasm-opt with arguments wasm-opt refuses did not fail'; exit 1; }
  fi
  # (the rest needs Binaryen: the npm package binaryen, or CLYSM_WASM_OPT_BIN)
  if ! node tools/wasm-opt.mjs --find > /dev/null 2>&1; then
    echo 'lib: wasm-opt: no Binaryen here, the library through it is not tested'; return 0
  fi
  env -u CLYSM_WASM_OPT bin/clysm library --shake tests/lib/wasm-opt.lisp -o build/lib-wasm-opt/plain/wasm-opt.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  for again in optimised again; do
    bin/clysm library --shake --wasm-opt tests/lib/wasm-opt.lisp -o build/lib-wasm-opt/$again/wasm-opt.wasm > "$log" 2>&1 \
      || { cat "$log"; exit 1; }
  done
  # the same octets from the same program
  for f in wasm-opt.wasm wasm-opt.wasm.debug wasm-opt.library.mjs wasm-opt.mjs; do
    cmp build/lib-wasm-opt/optimised/$f build/lib-wasm-opt/again/$f || { echo "lib: --wasm-opt wrote another $f the second time"; exit 1; }
  done
  node tests/lib/wasm-opt.mjs build/lib-wasm-opt/plain/wasm-opt.mjs build/lib-wasm-opt/optimised/wasm-opt.mjs || exit 1
}
sections="$sections print-only"
section_print_only() {
  # the printer without the reader's macro functions
  bin/clysm library --shake tests/lib/print-only.lisp -o build/lib-test/print-only.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/print-only.mjs build/lib-test/print-only.mjs || exit 1
}
sections="$sections throw-message"
section_throw_message() {
  # an explicit fallback message without retaining the Lisp printer
  bin/clysm library --shake tests/lib/throw-message.lisp -o build/lib-test/throw-message.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/throw-message.mjs build/lib-test/throw-message.mjs || exit 1
}
sections="$sections error-owners"
section_error_owners() {
  # conditions from another Wasm instance remain JS errors; local round trips retain identity
  bin/clysm library --shake tests/lib/error-owners.lisp -o build/lib-test/error-owners.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/error-owners.mjs build/lib-test/error-owners.wasm || exit 1
}
sections="$sections preinit"
section_preinit() {
  # application definitions initialized during compilation, with exports at load time
  bin/clysm library --shake --preinit-through tests/lib/preinit-definitions.lisp \
    tests/lib/preinit-definitions.lisp tests/lib/preinit-exports.lisp \
    -o build/lib-test/preinit.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/preinit.mjs build/lib-test/preinit.mjs > build/lib-test/preinit.out 2>&1 || { cat build/lib-test/preinit.out; exit 1; }
  if [ "$(cat build/lib-test/preinit.out)" != 'lib: preinitialized definitions ok' ]; then
    echo 'lib: preinitialized forms ran again at load time'; cat build/lib-test/preinit.out; exit 1
  fi
  cat build/lib-test/preinit.out
  if bin/clysm compile --preinit-through tests/lib/preinit-definitions.lisp \
    tests/lib/preinit-definitions.lisp -o build/lib-test/preinit-bad.wasm > "$log" 2>&1; then
    echo 'lib: preinitialization without --shake was accepted'; exit 1
  fi
  if bin/clysm compile --shake --preinit-through tests/lib/preinit-exports.lisp \
    tests/lib/preinit-definitions.lisp -o build/lib-test/preinit-bad.wasm > "$log" 2>&1; then
    echo 'lib: a preinitialization boundary outside the input files was accepted'; exit 1
  fi
  # Explicit preinitialization must fail, rather than defer it to module load,
  # when the subprocess that builds its heap cannot run.
  if NODE=/bin/false bin/clysm compile --shake --preinit-through tests/lib/preinit-definitions.lisp \
    tests/lib/preinit-definitions.lisp tests/lib/preinit-exports.lisp \
    -o build/lib-test/preinit-bad.wasm > "$log" 2>&1; then
    echo 'lib: failed explicit preinitialization silently fell back'; exit 1
  fi
}
sections="$sections locations"
section_locations() {
  # generated loaders find the module in a relocated package or a bundle's assets
  bin/clysm library --shake tests/lib/locations.lisp -o build/lib-locations/locations.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  for mode in first second missing override; do
    node tests/lib/locations.mjs build/lib-locations "$mode" || exit 1
  done
}
sections="$sections locations-eager"
section_locations_eager() {
  bin/clysm library --shake tests/lib/locations.lisp tests/lib/locations-eager.lisp -o build/lib-locations-eager/locations.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/locations-eager.mjs build/lib-locations-eager || exit 1
}
sections="$sections reexports"
section_reexports() {
  # a public entry composed of generated values, external values, and type-only exports
  bin/clysm library --shake tests/lib/reexports.lisp -o build/lib-reexports/reexports.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/reexports.mjs build/lib-reexports || exit 1
  for variant in eager embed; do
    bin/clysm library --shake tests/lib/reexports.lisp tests/lib/reexports-$variant.lisp -o build/lib-reexports-$variant/reexports.wasm > "$log" 2>&1 \
      || { cat "$log"; exit 1; }
    node tests/lib/reexports.mjs build/lib-reexports-$variant || exit 1
  done
  bin/clysm compile --shake tests/lib/reexports-invalid.lisp -o build/lib-reexports/invalid.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/reexports-invalid.mjs build/lib-reexports/invalid.wasm || exit 1
}
sections="$sections entrypoints"
section_entrypoints() {
  # generated initializers preserve lazy imports, entry URLs and bundled inputs
  bin/clysm library --shake tests/lib/entrypoints.lisp -o build/lib-entrypoints/entrypoints.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/entrypoints.mjs build/lib-entrypoints || exit 1
  bin/clysm library --shake tests/lib/entrypoints.lisp tests/lib/entrypoints-embed.lisp \
    -o build/lib-entrypoints-embed/entrypoints.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/entrypoints.mjs build/lib-entrypoints-embed || exit 1
  bin/clysm compile --shake tests/lib/entrypoints-invalid.lisp -o build/lib-entrypoints/invalid.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  node tests/lib/entrypoints-invalid.mjs build/lib-entrypoints/invalid.wasm || exit 1
}
sections="$sections one"
section_one() {
  # the size of a module of one export (tests/lib/one.lisp)
  bin/clysm library --shake tests/lib/one.lisp -o build/lib-test/one.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
  size=$(wc -c < build/lib-test/one.wasm); max=$(cat tests/lib/one.max-bytes)
  if [ "$size" -gt "$max" ]; then
    echo "lib: the module of one export is $size bytes, more than $max (tests/lib/one.max-bytes);"
    echo "  bin/clysm library --shake-why REPORT tests/lib/one.lisp -o OUT.wasm says what keeps each function"
    exit 1
  fi
  echo "lib: the module of one export is $size bytes (at most $max)"
  # the same with --shake-symbols, which leaves out more than --shake
  bin/clysm library --shake-symbols tests/lib/one.lisp -o build/lib-test/one-symbols.wasm > "$log" 2>&1 \
    || { cat "$log"; exit 1; }
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
}
sections="$sections unused-names"
section_unused_names() {
  # the literal names of JavaScript of a function that nothing calls are
  # left out with it, with the heap of src/preinit.lisp and without it
  for preinit in yes no; do
    if [ $preinit = no ]; then np="CLYSM_NO_PREINIT=1"; else np="-u CLYSM_NO_PREINIT"; fi
    env $np bin/clysm library --shake tests/lib/one.lisp -o build/lib-test/one-$preinit.wasm \
      > "$log" 2>&1 || { cat "$log"; exit 1; }
    env $np bin/clysm library --shake tests/lib/one-unused-names.lisp -o build/lib-test/one-unused-$preinit.wasm \
      > "$log" 2>&1 || { cat "$log"; exit 1; }
    one=$(wc -c < build/lib-test/one-$preinit.wasm); unused=$(wc -c < build/lib-test/one-unused-$preinit.wasm)
    if [ "$unused" -gt $((one + 300)) ]; then
      echo "lib: 40 names of a function nothing calls kept $((unused - one)) bytes (preinit: $preinit)"; exit 1
    fi
    echo "lib: 40 names of a function nothing calls keep $((unused - one)) bytes (preinit: $preinit)"
  done
}

rm -f build/lib-test/sections/*
running=0
for s in $sections; do
  { ( log=build/lib-test/sections/$s.log; "section_${s//-/_}" ) > "build/lib-test/sections/$s.out" 2>&1
    echo $? > "build/lib-test/sections/$s.status"; } &
  running=$((running + 1))
  if [ $running -ge "$JOBS" ]; then wait -n; running=$((running - 1)); fi
done
wait
status=0
for s in $sections; do
  cat "build/lib-test/sections/$s.out"
  [ "$(cat "build/lib-test/sections/$s.status" 2>/dev/null)" = 0 ] || { status=1; failed="$failed $s"; }
done
[ $status = 0 ] || echo "lib: FAIL:$failed"
exit $status
