#!/bin/sh
# bin/clysm build: an application of two ASDF systems (tests/static/geom.asd,
# geom-util.asd) and a file built into one module with the runtime, and run.
cd "$(dirname "$0")/../.." || exit 1
out=build/static-test
mkdir -p $out
bin/clysm build --asd tests/static/geom.asd --system geom tests/static/main.lisp --eval '(geom:main)' \
    -o $out/geom.wasm > $out/build.log 2>&1 || { cat $out/build.log; echo "static: the build failed"; exit 1; }
bin/clysm exec $out/geom.wasm > $out/run.out 2> $out/run.err
if ! cmp -s tests/static/run.expected $out/run.out; then
  diff tests/static/run.expected $out/run.out; cat $out/run.err; echo "static: FAIL"; exit 1
fi
echo "static: ok"
