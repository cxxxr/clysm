#!/bin/sh
# Build the TODO example into build/todo/ (serve that directory over HTTP):
#   examples/todo/build.sh && python3 -m http.server -d build/todo
# With --dev, build the development version: the page has a REPL in which the
# running application can be inspected and redefined.
set -e
cd "$(dirname "$0")/../.."
out=build/todo
mkdir -p $out
rm -f $out/repl.mjs
if [ "$1" = "--dev" ]; then
  tools/build-ui-dev.sh $out/todo.wasm examples/todo/todo.lisp 2>/dev/null
  cp host/web/repl.mjs $out/
else
  bin/clysm compile ui/ui.lisp ui/command.lisp ui/dom.lisp ui/desktop.lisp examples/todo/todo.lisp \
    -o $out/todo.wasm 2>/dev/null
fi
cp examples/todo/index.html host/common.mjs host/jsffi.mjs $out/
sed "s#'../common.mjs'#'./common.mjs'#" host/web/clysm.mjs > $out/clysm.mjs
echo "built $out — serve it with e.g.: python3 -m http.server -d $out"
