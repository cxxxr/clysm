#!/bin/sh
# Build the npm package of Clysm in build/npm/: the self-hosted compiler
# (clysmc.wasm) and the REPL image (clysm-native.wasm) prebuilt, so that the
# package needs only Node.js (no SBCL):
#   tools/build-npm.sh                 # build/npm/, and build/clysm-VERSION.tgz
#   npm install -g build/clysm-VERSION.tgz
#   clysm library outline.lisp -o dist/outline.wasm
# In a project, `import { load } from 'clysm/lib'` loads a compiled library.
set -e
cd "$(dirname "$0")/.."
root=$(pwd)
tools/build-selfhost.sh > build/selfhost.log 2>&1 || { cat build/selfhost.log; exit 1; }
image=$(bin/clysm image-native)
out=build/npm
rm -rf "$out"
mkdir -p "$out/bin" "$out/tools" "$out/third_party/asdf"
cp bin/clysm.mjs "$out/bin/"
cp build/clysmc.wasm "$image" "$out/"
cp -r host runtime data lib ui "$out/"
mkdir -p "$out/contrib"
for d in contrib/*; do
  case "$d" in contrib/lem) ;; *) cp -r "$d" "$out/contrib/" ;; esac
done
cp third_party/asdf/asdf.lisp "$out/third_party/asdf/"
cp tools/runtime-files.lisp tools/gen-library.mjs "$out/tools/"
cp README.md "$out/"
version="0.1.$(git rev-list --count HEAD)"
commit=$(git rev-parse --short HEAD)
echo "$version ($commit)" > "$out/VERSION"
cat > "$out/package.json" <<EOF
{
  "name": "clysm",
  "version": "$version",
  "description": "Common Lisp compiled to WebAssembly GC, for Node.js, browsers and Electron (built from commit $commit)",
  "type": "module",
  "bin": { "clysm": "bin/clysm.mjs" },
  "exports": {
    "./lib": "./host/lib.mjs",
    "./worker": "./host/worker.mjs",
    "./web": "./host/web/clysm.mjs",
    "./host/*": "./host/*"
  },
  "engines": { "node": ">=24" },
  "files": ["bin", "host", "runtime", "data", "lib", "ui", "contrib", "third_party", "tools",
            "clysmc.wasm", "clysm-native.wasm", "README.md", "VERSION"],
  "clysm": { "commit": "$commit" }
}
EOF
(cd build && npm pack ./npm --silent > /dev/null) && mv "build/clysm-$version.tgz" build/ 2>/dev/null || true
echo "built $out (clysm $version, commit $commit): build/clysm-$version.tgz"
