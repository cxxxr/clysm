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
# micros (Lem's SWANK), the commit of Lem's qlfile.lock
clone https://github.com/lem-project/micros.git $deps/micros 08c313476bf436d72bcd5cd8f4e494d9f9323d08

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
  "$(cd "$LEM_DIR" && pwd)" "$root/$deps/lem-mailbox" "$root/$deps/micros" "$root/$out" > $out/prepare.log 2>&1 ||
  { tail -20 $out/prepare.log; exit 1; }

# the page and the shell
sed -e "s#@CLYSM_ROOT@#$root#" -e "s#@APP_DIR@#$root/$out#" contrib/lem/desktop/index.html > $out/index.html
sed "s#'../common.mjs'#'./common.mjs'#" host/web/clysm.mjs > $out/clysm.mjs
cp host/common.mjs host/jsffi.mjs contrib/lem/lem.css contrib/lem/desktop/boot.lisp contrib/lem/load-lem.lisp contrib/lem/desktop/package.json contrib/lem/desktop/lem \
   contrib/lem/desktop/train.js contrib/lem/desktop/hint-image.mjs contrib/lem/desktop/link-image.mjs host/link.mjs \
   ui/electron/main.js ui/electron/preload.js $out/
# worlds (contrib/world/): images in workers that the Lisp mode connects to
cp contrib/world/worker.js $out/world-worker.js
cp contrib/world/boot.lisp $out/world-boot.lisp
# the heap with Lem loaded, from which the page starts (runtime/image.lisp)
echo "saving the heap image"
echo "(defvar cl-user::*lem-save-image* \"$root/$out/lem.image\") (load \"$root/$out/boot.lisp\")" > $out/save-image.lisp
rm -f $out/lem.image
CLYSM_SAVE_IMAGE=1 node host/run.mjs $out/lem.wasm $out/save-image.lisp > $out/save-image.log 2>&1 ||
  { tail -20 $out/save-image.log; exit 1; }
# its modules linked into one (host/link.mjs)
node $out/link-image.mjs $out/lem.image
if [ -n "$LEM_NO_ELECTRON" ]; then echo "built $out (without Electron)"; exit 0; fi
[ -d $out/node_modules/electron ] || (cd $out && npm install --no-audit --no-fund)
# (npm may have left the download of Electron itself to its first start)
[ -d $out/node_modules/electron/dist ] || (cd $out && node node_modules/electron/install.js)
# compilation hints: the functions that run when Lem starts are compiled
# with the modules (host/common.mjs), learnt from a start of the page
echo "learning which functions run when Lem starts"
tmp=$(mktemp -d)
if (cd $out && CLYSM_IMAGE_TRAIN=$tmp/ran.json CLYSM_USER_DATA=$tmp/user-data CLYSM_ELECTRON_TEST=train.js \
      timeout 120 node_modules/electron/dist/electron . --ozone-platform=headless) > $out/train.log 2>&1 &&
   [ -s $tmp/ran.json ]; then
  node $out/hint-image.mjs $out/lem.wasm $out/lem.image $tmp/ran.json
else
  echo "(without compilation hints: see $out/train.log)"
fi
rm -rf $tmp
echo "built $out — run it with: (cd $out && npm start)"
