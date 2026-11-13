#!/bin/sh
# Compile the Clysm compiler itself to WebAssembly (build/clysmc.wasm).
set -e
cd "$(dirname "$0")/.."
sbcl --script tools/build.lisp build/clysmc.wasm \
  src/package.lisp src/wasm.lisp src/reader.lisp src/layout.lisp src/frontend.lisp \
  src/types.lisp src/inline.lisp src/codegen.lisp src/prims.lisp src/typed.lisp src/driver.lisp src/hostenv.lisp src/selfhost.lisp
