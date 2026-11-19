#!/bin/sh
# Build the desktop Clysm IDE into build/ide-desktop/ and install Electron there:
#   examples/ide/build-desktop.sh && (cd build/ide-desktop && npm start)
#   (cd build/ide-desktop && npx electron . ~/my-project)     # open a folder
set -e
cd "$(dirname "$0")/../.."
examples/ide/build.sh >/dev/null 2>&1
out=build/ide-desktop
mkdir -p $out
cp build/ide/*.html build/ide/*.css build/ide/*.wasm build/ide/*.mjs build/ide/world-worker.js \
   ui/electron/main.js ui/electron/preload.js examples/ide/desktop/package.json $out/
[ -d $out/node_modules/electron ] || (cd $out && npm install --no-audit --no-fund)
echo "built $out — run it with: (cd $out && npm start)"
