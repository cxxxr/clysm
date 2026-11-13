#!/bin/sh
# Build the TODO example into build/todo/ (serve that directory over HTTP):
#   examples/todo/build.sh && python3 -m http.server -d build/todo
set -e
cd "$(dirname "$0")/../.."
out=build/todo
mkdir -p $out
bin/clysm compile ui/ui.lisp ui/dom.lisp examples/todo/todo.lisp -o $out/todo.wasm 2>/dev/null
cp examples/todo/index.html host/common.mjs host/jsffi.mjs $out/
sed "s#'../common.mjs'#'./common.mjs'#" host/web/clysm.mjs > $out/clysm.mjs
echo "built $out — serve it with e.g.: python3 -m http.server -d $out"
