#!/bin/sh
# Build the browser demo into build/web/ (serve that directory over HTTP).
set -e
cd "$(dirname "$0")/.."
mkdir -p build/web
# the image embeds the compiler: forms typed in the page are compiled to
# WebAssembly in the browser and linked into the running image
sbcl --script tools/build-repl.lisp build/web/clysm.wasm lib/web-main.lisp
cp host/web/index.html host/web/clysm.mjs build/web/
mkdir -p build/host && cp host/common.mjs host/jsffi.mjs build/host/
sed -i "s#'../common.mjs'#'../host/common.mjs'#" build/web/clysm.mjs
echo "built build/web — serve it with e.g.: python3 -m http.server -d build"
