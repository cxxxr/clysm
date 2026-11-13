#!/bin/sh
# Build the address book example into build/contacts/ (serve that directory over HTTP):
#   examples/contacts/build.sh && python3 -m http.server -d build/contacts
# With --dev, build the development version: the page has a REPL in which the
# running application can be inspected and redefined.
set -e
cd "$(dirname "$0")/../.."
out=build/contacts
mkdir -p $out
rm -f $out/repl.mjs
if [ "$1" = "--dev" ]; then
  tools/build-ui-dev.sh $out/contacts.wasm examples/contacts/contacts.lisp 2>/dev/null
  cp host/web/repl.mjs $out/
else
  bin/clysm compile ui/ui.lisp ui/command.lisp ui/present.lisp ui/pane.lisp ui/dialog.lisp ui/graphics.lisp ui/stream.lisp ui/dom.lisp \
    ui/desktop.lisp examples/contacts/contacts.lisp -o $out/contacts.wasm 2>/dev/null
fi
cp examples/contacts/index.html ui/ui.css host/common.mjs host/jsffi.mjs $out/
sed "s#'../common.mjs'#'./common.mjs'#" host/web/clysm.mjs > $out/clysm.mjs
echo "built $out — serve it with e.g.: python3 -m http.server -d $out"
