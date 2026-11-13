#!/bin/sh
# Build Lem on Clysm as a desktop application in build/lem-desktop/ and
# install Electron there:
#   contrib/lem/build.sh && (cd build/lem-desktop && npm start)
# Lem's source is cloned into build/lem-deps/ (or LEM_DIR names a checkout
# of https://github.com/lem-project/lem); the libraries come from Quicklisp
# (QUICKLISP_HOME, by default ~/quicklisp/).  With LEM_NO_ELECTRON set,
# Electron is not installed (for build-web.sh).
set -e
root="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$root"
out=build/lem-desktop
deps=build/lem-deps
mkdir -p $out $deps
# the version of Lem this frontend was written for
lem_commit=b72d82b4dd40fba07dbde06a13e4a6a0bbec282e
clone() {   # clone REPOSITORY DIRECTORY [COMMIT]
  [ -d "$2" ] && return
  git init -q "$2"
  git -C "$2" fetch -q --depth 1 "$1" "${3:-HEAD}"
  git -C "$2" checkout -q FETCH_HEAD
}
if [ -z "$LEM_DIR" ]; then
  clone https://github.com/lem-project/lem.git $deps/lem $lem_commit
  LEM_DIR=$deps/lem
fi
clone https://github.com/lem-project/lem-mailbox.git $deps/lem-mailbox

# the image: the compiler (ASDF compiles and loads Lem) and the UI framework
echo "building the image"
sbcl --script tools/build-repl.lisp $out/lem.wasm \
  ui/ui.lisp ui/command.lisp ui/present.lisp ui/pane.lisp ui/dialog.lisp ui/graphics.lisp \
  ui/stream.lisp ui/dom.lisp ui/desktop.lisp lib/repl-main.lisp 2>&1 | grep -v '^; ' || true
[ -s $out/lem.wasm ]

# Lem and its libraries, compiled by that image (the first time this takes
# a few minutes)
echo "compiling Lem"
node host/run.mjs $out/lem.wasm contrib/lem/prepare.lisp \
  "$(cd "$LEM_DIR" && pwd)" "$root/$deps/lem-mailbox" "$root/$out" > $out/prepare.log 2>&1 ||
  { tail -20 $out/prepare.log; exit 1; }

# the page and the shell
sed -e "s#@CLYSM_ROOT@#$root#" -e "s#@APP_DIR@#$root/$out#" contrib/lem/desktop/index.html > $out/index.html
sed "s#'../common.mjs'#'./common.mjs'#" host/web/clysm.mjs > $out/clysm.mjs
cp host/common.mjs host/jsffi.mjs contrib/lem/lem.css contrib/lem/desktop/boot.lisp contrib/lem/desktop/package.json \
   ui/electron/main.js ui/electron/preload.js $out/
if [ -n "$LEM_NO_ELECTRON" ]; then echo "built $out (without Electron)"; exit 0; fi
[ -d $out/node_modules/electron ] || (cd $out && npm install --no-audit --no-fund)
# (npm may have left the download of Electron itself to its first start)
[ -d $out/node_modules/electron/dist ] || (cd $out && node node_modules/electron/install.js)
echo "built $out — run it with: (cd $out && npm start)"
