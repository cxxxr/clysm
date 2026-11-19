#!/bin/sh
# Build the editor into build/editor/ (serve that directory over HTTP):
#   examples/editor/build.sh && python3 -m http.server -d build/editor
set -e
cd "$(dirname "$0")/../.."
out=build/editor
mkdir -p $out
bin/clysm compile ui/ui.lisp ui/command.lisp ui/present.lisp ui/pane.lisp ui/dialog.lisp ui/graphics.lisp ui/stream.lisp ui/dom.lisp ui/desktop.lisp \
  ui/editor/buffer.lisp ui/editor/syntax.lisp ui/editor/commands.lisp ui/editor/view.lisp \
  examples/editor/editor.lisp -o $out/editor.wasm
cp examples/editor/index.html ui/ui.css ui/editor/editor.css host/common.mjs host/jsffi.mjs $out/
sed "s#'../common.mjs'#'./common.mjs'#" host/web/clysm.mjs > $out/clysm.mjs
echo "built $out — serve it with e.g.: python3 -m http.server -d $out"
