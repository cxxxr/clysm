#!/bin/sh
# The compiler in Wasm (build/clysmc.wasm, make selfhost) compiles a program
# to the same octets as the compiler on SBCL: as it is, with --shake (from
# the heap of src/preinit.lisp), and with the options of a library.
cd "$(dirname "$0")/../.." || exit 1
mkdir -p build/selfhost-same
status=0
same() {   # NAME OPTION... -- FILE...
  name=$1; shift; opts=""
  while [ "$1" != "--" ]; do opts="$opts $1"; shift; done; shift
  sbcl --dynamic-space-size 4096 --script tools/build.lisp $opts build/selfhost-same/$name-sbcl.wasm "$@" \
    > build/selfhost-same/$name-sbcl.log 2>&1 || { cat build/selfhost-same/$name-sbcl.log; status=1; return; }
  node host/run.mjs build/clysmc.wasm $opts build/selfhost-same/$name-wasm.wasm "$@" --runtime runtime \
    > build/selfhost-same/$name-wasm.log 2>&1 || { cat build/selfhost-same/$name-wasm.log; status=1; return; }
  if cmp -s build/selfhost-same/$name-sbcl.wasm build/selfhost-same/$name-wasm.wasm; then
    echo "selfhost-same: $name ok"
  else
    echo "selfhost-same: $name differs ($opts)"; status=1
  fi
}
same clos -- tests/cases/clos.lisp
same clos-shake --shake -- tests/cases/clos.lisp
same format-shake --shake -- tests/cases/format2.lisp
same lib-thrown --shake --thrown-exits --preinit-through tests/lib/one.lisp -- tests/lib/one.lisp
exit $status
