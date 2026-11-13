#!/bin/sh
# Build the desktop TODO app into build/todo-desktop/ and install Electron there:
#   examples/todo/build-desktop.sh && (cd build/todo-desktop && npm start)
set -e
cd "$(dirname "$0")/../.."
examples/todo/build.sh >/dev/null
out=build/todo-desktop
mkdir -p $out
cp build/todo/index.html build/todo/todo.wasm build/todo/clysm.mjs \
   build/todo/common.mjs build/todo/jsffi.mjs \
   examples/todo/desktop/main.js examples/todo/desktop/preload.js examples/todo/desktop/package.json $out/
[ -d $out/node_modules/electron ] || (cd $out && npm install --no-audit --no-fund)
echo "built $out — run it with: (cd $out && npm start)"
