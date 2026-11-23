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
# the same with --shake: the functions nothing refers to are left out
bin/clysm build --shake --asd tests/static/geom.asd --system geom tests/static/main.lisp --eval '(geom:main)' \
    -o $out/geom-shake.wasm > $out/build-shake.log 2>&1 || { cat $out/build-shake.log; echo "static: the build with --shake failed"; exit 1; }
bin/clysm exec $out/geom-shake.wasm > $out/run-shake.out 2> $out/run-shake.err
if ! cmp -s tests/static/run.expected $out/run-shake.out; then
  diff tests/static/run.expected $out/run-shake.out; cat $out/run-shake.err; echo "static: FAIL (--shake)"; exit 1
fi
# (it starts from the heap of the runtime made when it was built, src/preinit.lisp:
# less than two thirds of the module)
if grep -q 'could not be made when compiling' $out/build-shake.log; then
  cat $out/build-shake.log; echo "static: the runtime's heap was not made for --shake"; exit 1
fi
if [ "$(($(wc -c < $out/geom-shake.wasm) * 3))" -ge "$(($(wc -c < $out/geom.wasm) * 2))" ]; then
  echo "static: the module built with --shake is not two thirds of the size"; exit 1
fi
# --main and --image: the program starts from the heap saved after its toplevel forms ran
bin/clysm build --asd tests/static/geom.asd --system geom tests/static/main.lisp --main geom:main --image \
    -o $out/geom-image.wasm > $out/build-image.log 2>&1 || { cat $out/build-image.log; echo "static: the build with --image failed"; exit 1; }
[ -s $out/geom-image.img ] || { echo "static: no heap image"; exit 1; }
bin/clysm exec $out/geom-image.wasm > $out/run-image.out 2> $out/run-image.err
CLYSM_NO_HEAP_IMAGE=1 bin/clysm exec $out/geom-image.wasm > $out/run-noimage.out 2>> $out/run-image.err
if ! cmp -s tests/static/run.expected $out/run-image.out || ! cmp -s tests/static/run.expected $out/run-noimage.out; then
  diff tests/static/run.expected $out/run-image.out; cat $out/run-image.err; echo "static: FAIL (--image)"; exit 1
fi
# a program of libraries (tests/static/libs.lisp), when Quicklisp is installed with them
ql="$HOME/quicklisp/dists/quicklisp/software"
if [ -f "$HOME/quicklisp/setup.lisp" ] && ls -d "$ql"/cl-ppcre-* "$ql"/rt-* >/dev/null 2>&1; then
  bin/clysm build --system cl-ppcre --system rt tests/static/libs.lisp --main static-libs::main \
      -o $out/libs.wasm > $out/build-libs.log 2>&1 || { cat $out/build-libs.log; echo "static: the build of the libraries failed"; exit 1; }
  bin/clysm exec $out/libs.wasm > $out/run-libs.out 2> $out/run-libs.err
  if ! cmp -s tests/static/libs.expected $out/run-libs.out; then
    diff tests/static/libs.expected $out/run-libs.out; cat $out/run-libs.err; echo "static: FAIL (libraries)"; exit 1
  fi
else
  echo "static: no cl-ppcre and rt in ~/quicklisp: the program of libraries is not built"
fi
echo "static: ok"
