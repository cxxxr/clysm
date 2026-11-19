#!/bin/sh
# Build the desktop editor into build/editor-desktop/ and install Electron there:
#   examples/editor/build-desktop.sh && (cd build/editor-desktop && npm start)
set -e
cd "$(dirname "$0")/../.."
examples/editor/build.sh >/dev/null
out=build/editor-desktop
mkdir -p $out
cp build/editor/*.html build/editor/*.wasm build/editor/*.mjs build/editor/*.css \
   ui/electron/main.js ui/electron/preload.js examples/editor/desktop/package.json $out/
[ -d $out/node_modules/electron ] || (cd $out && npm install --no-audit --no-fund)
echo "built $out — run it with: (cd $out && npm start)"
