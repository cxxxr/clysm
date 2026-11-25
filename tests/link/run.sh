#!/bin/sh
# host/link.mjs on a heap image: the modules compiled in the image are linked
# into one module (as cxxxr/lem's frontends/clysm/ build does), and the image
# starts from it with the same output as from the image not linked.
cd "$(dirname "$0")/../.." || exit 1
dir=build/link-test
rm -rf "$dir" && mkdir -p "$dir"
img="$(bin/clysm image-native)" || exit 1
echo "(defvar cl-user::*link-test-image* \"$dir/t.image\") (load \"tests/link/save.lisp\")" > "$dir/save.lisp"
CLYSM_SAVE_IMAGE=1 node host/run.mjs "$img" "$dir/save.lisp" > "$dir/save.log" 2>&1 &&
  node tests/link/link.mjs "$dir/t.image" "$dir/linked.image" 2> "$dir/link.log" ||
  { echo "link: FAIL"; tail -n 5 "$dir/save.log" "$dir/link.log"; exit 1; }
CLYSM_IMAGE="$dir/t.image" node host/run.mjs "$img" > "$dir/plain.out" 2>&1
CLYSM_IMAGE="$dir/linked.image" node host/run.mjs "$img" > "$dir/linked.out" 2>&1
if [ "$(cat "$dir/plain.out")" = "$(printf '\n12680 ')" ] && cmp -s "$dir/plain.out" "$dir/linked.out"; then
  echo "link: ok"
else
  echo "link: FAIL"; cat "$dir/plain.out" "$dir/linked.out"; exit 1
fi
