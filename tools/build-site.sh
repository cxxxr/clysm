#!/bin/sh
# Build the playground, the page of https://cxxxr.github.io/clysm/, into
# build/site/ (serve that directory over HTTP):
#   tools/build-site.sh && python3 -m http.server -d build/site
# The image contains the compiler (tools/build-repl.lisp), so each form run in
# the page is compiled to WebAssembly there; it runs in a worker
# (site/worker.mjs) with site/playground.lisp.
set -e
cd "$(dirname "$0")/.."
out=build/site
rm -rf $out
mkdir -p $out
sbcl --script tools/build-repl.lisp $out/clysm.wasm lib/web-main.lisp site/playground.lisp
cp site/index.html site/playground.mjs site/samples.mjs site/worker.mjs host/common.mjs host/jsffi.mjs $out/
sed "s#'../common.mjs'#'./common.mjs'#" host/web/clysm.mjs > $out/clysm.mjs
touch $out/.nojekyll
echo "built $out — serve it with e.g.: python3 -m http.server -d $out"
