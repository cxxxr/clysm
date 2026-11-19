#!/bin/sh
# Build Clysm IDE for a browser into build/ide/ (serve that directory over HTTP):
#   examples/ide/build.sh && python3 -m http.server -d build/ide
# The image contains the compiler (tools/build-repl.lisp).  The IDE
# evaluates in a world: the same image in a worker (world.lisp), where what
# is evaluated is compiled to WebAssembly.
set -e
cd "$(dirname "$0")/../.."
out=build/ide
mkdir -p $out
sbcl --script tools/build-repl.lisp $out/ide.wasm \
  lib/web-main.lisp \
  ui/ui.lisp ui/command.lisp ui/present.lisp ui/pane.lisp ui/dialog.lisp ui/graphics.lisp \
  ui/stream.lisp ui/dom.lisp ui/desktop.lisp \
  ui/editor/buffer.lisp ui/editor/syntax.lisp ui/editor/commands.lisp ui/editor/view.lisp \
  examples/ide/world.lisp examples/ide/client.lisp examples/ide/ide.lisp
cp examples/ide/index.html examples/ide/ide.css ui/ui.css ui/editor/editor.css host/common.mjs host/jsffi.mjs $out/
sed "s#'../common.mjs'#'./common.mjs'#" host/web/clysm.mjs > $out/clysm.mjs
# the worker of a world (world.lisp): it boots the page's image
cp contrib/world/worker.js $out/world-worker.js
echo "built $out — serve it with e.g.: python3 -m http.server -d $out"
