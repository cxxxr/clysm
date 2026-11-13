#!/bin/sh
# Build the desktop TODO app into build/todo-desktop/ and install Electron there:
#   examples/todo/build-desktop.sh && (cd build/todo-desktop && npm start)
# With --dev, the application has a REPL (see build.sh).
set -e
cd "$(dirname "$0")/../.."
examples/todo/build.sh "$@" >/dev/null
out=build/todo-desktop
mkdir -p $out
rm -f $out/repl.mjs
cp build/todo/*.html build/todo/*.wasm build/todo/*.mjs \
   ui/electron/main.js ui/electron/preload.js examples/todo/desktop/package.json $out/
[ -d $out/node_modules/electron ] || (cd $out && npm install --no-audit --no-fund)
echo "built $out — run it with: (cd $out && npm start)"
