#!/bin/sh
# Build the desktop address book into build/contacts-desktop/ and install Electron there:
#   examples/contacts/build-desktop.sh && (cd build/contacts-desktop && npm start)
# Its commands are in the menu bar of the application instead of the page.
# With --dev, the application has a REPL (see build.sh).
set -e
cd "$(dirname "$0")/../.."
examples/contacts/build.sh "$@" >/dev/null
out=build/contacts-desktop
mkdir -p $out
rm -f $out/repl.mjs
cp build/contacts/*.html build/contacts/*.css build/contacts/*.wasm build/contacts/*.mjs \
   ui/electron/main.js ui/electron/preload.js examples/contacts/desktop/package.json $out/
[ -d $out/node_modules/electron ] || (cd $out && npm install --no-audit --no-fund)
echo "built $out — run it with: (cd $out && npm start)"
