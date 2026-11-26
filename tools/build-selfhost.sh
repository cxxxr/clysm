#!/bin/sh
# Compile the Clysm compiler itself to WebAssembly (build/clysmc.wasm).
#   tools/build-selfhost.sh             with SBCL (the cross compiler)
#   tools/build-selfhost.sh --no-sbcl   with a clysmc.wasm (CLYSMC, else
#                                       build/clysmc.wasm): no SBCL; the
#                                       REPL image (build/clysm-native.wasm)
#                                       is built by the new compiler too
# Either compiler compiles the compiler to the same octets
# (docs/INTERNALS.md, "Self-hosting").
set -e
cd "$(dirname "$0")/.."
files="src/package.lisp src/wasm.lisp src/reader.lisp src/layout.lisp src/frontend.lisp
  src/types.lisp src/inline.lisp src/codegen.lisp src/prims.lisp src/typed.lisp src/driver.lisp src/hostenv.lisp
  src/preinit.lisp src/selfhost.lisp"
if [ "$1" != "--no-sbcl" ]; then
  exec sbcl --script tools/build.lisp build/clysmc.wasm $files
fi
seed="${CLYSMC:-build/clysmc.wasm}"
[ -f "$seed" ] || { echo "build-selfhost: no compiler to start from ($seed): set CLYSMC to a clysmc.wasm" >&2; exit 1; }
node="${NODE:-node}"
mkdir -p build
"$node" host/run.mjs "$seed" build/clysmc.wasm.new $files --runtime runtime
mv -f build/clysmc.wasm.new build/clysmc.wasm
"$node" host/run.mjs build/clysmc.wasm --repl-image build/clysm-native.wasm.new --runtime runtime
mv -f build/clysm-native.wasm.new build/clysm-native.wasm
rm -f build/clysm-native.img
