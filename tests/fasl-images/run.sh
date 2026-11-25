#!/bin/sh
# A fasl compiled in an image is loaded in another image that lacks some of
# the first one's packages (as a world of cxxxr/lem loads micros' contribs
# compiled in Lem's image): what the compiler took from the first image
# (the classes of a generic function's dispatch tree) must not name them.
cd "$(dirname "$0")/../.." || exit 1
dir="$(pwd)/build/fasl-images-test"
rm -rf "$dir" && mkdir -p "$dir"
img="$(bin/clysm image-native)" || exit 1
run() { echo "(defvar cl-user::*fi-dir* \"$dir/\") (load \"tests/fasl-images/$1.lisp\")" > "$dir/$1.lisp"
        node host/run.mjs "$img" "$dir/$1.lisp"; }
run compile > "$dir/compile.log" 2>&1 || { echo "fasl-images: FAIL (compile)"; tail -n 5 "$dir/compile.log"; exit 1; }
run load > "$dir/load.out" 2>&1
if [ "$(cat "$dir/load.out")" = "$(printf '\n((:STANDARD-OBJECT OTHER-CLASS) :NO-METHOD) ')" ]; then
  echo "fasl-images: ok"
else
  echo "fasl-images: FAIL"; head -n 5 "$dir/load.out"; exit 1
fi
