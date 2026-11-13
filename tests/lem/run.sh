#!/bin/sh
# tests/lem/run.sh — Lem on Clysm (contrib/lem): build the application if
# needed, run it in Electron without a display, and type into it
# (tests/lem/drive.js).
cd "$(dirname "$0")/../.." || exit 1
[ -x build/lem-desktop/node_modules/electron/dist/electron ] || contrib/lem/build.sh > /dev/null || exit 1
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
mkdir -p "$dir/home"
cd build/lem-desktop
LEM_HOME="$dir/home/" LEM_TEST_DIR="$dir" CLYSM_ELECTRON_TEST="$OLDPWD/tests/lem/drive.js" \
  timeout 180 node_modules/electron/dist/electron . --ozone-platform=headless 2>&1 \
  | grep -E '^(ok|FAIL|lem:)'
