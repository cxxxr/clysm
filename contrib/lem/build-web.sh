#!/bin/sh
# Build Lem on Clysm for a browser, as one file that needs no server:
#   contrib/lem/build-web.sh             # build/lem-web/lem.html
# Open build/lem-web/lem.html in Chrome or Edge (137 or later), from the
# disk or from any web server.  It contains the image and the files that
# loading Lem reads (the fasls that build.sh compiled); the files written
# in the home directory are kept in the browser's localStorage.
set -e
root="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$root"
desktop=build/lem-desktop
out=build/lem-web
[ -s $desktop/lem.wasm ] && [ -s $desktop/config.lisp ] || contrib/lem/build.sh
mkdir -p $out
cp contrib/lem/web/boot.lisp $desktop/config.lisp $out/
# load Lem on Node.js as the page will, and note the files that it reads
echo "(defvar cl-user::*lem-load-only* t) (load \"$root/$out/boot.lisp\")" > $out/trace.lisp
echo "loading Lem to see which files it reads"
CLYSM_TRACE_FILES=$out/trace.json node host/run.mjs $desktop/lem.wasm $out/trace.lisp > $out/trace.log 2>&1 ||
  { tail -20 $out/trace.log; exit 1; }
node contrib/lem/web/pack.mjs $out/trace.json $out/files.pack.gz
node contrib/lem/web/make-html.mjs "$root" $desktop/lem.wasm $out/files.pack.gz "$root/$out/boot.lisp" "$HOME" \
  > $out/lem.html
echo "built $out/lem.html ($(du -h $out/lem.html | cut -f1)) — open it in Chrome or Edge"
