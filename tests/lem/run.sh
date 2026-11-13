#!/bin/sh
# tests/lem/run.sh — Lem on Clysm (contrib/lem): build the desktop
# application and the page for a browser if needed, run each in Electron
# without a display (the page in a window without Node.js, opened from its
# file), and type into them (tests/lem/drive.js).
cd "$(dirname "$0")/../.." || exit 1
root=$(pwd)
[ -x build/lem-desktop/node_modules/electron/dist/electron ] || contrib/lem/build.sh > /dev/null || exit 1
[ -s build/lem-web/lem.html ] || contrib/lem/build-web.sh > /dev/null || exit 1
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
mkdir -p "$dir/home" "$dir/desktop-data" "$dir/web-data"
electron=$root/build/lem-desktop/node_modules/electron/dist/electron
status=0
(cd build/lem-desktop &&
   LEM_HOME="$dir/home/" LEM_TEST_DIR="$dir" CLYSM_USER_DATA="$dir/desktop-data" \
   CLYSM_ELECTRON_TEST="$root/tests/lem/drive.js" \
   timeout 180 "$electron" . --ozone-platform=headless 2>&1) | grep -E '^(ok|FAIL|lem)' || status=1
LEM_WEB_PAGE="$root/build/lem-web/lem.html" CLYSM_USER_DATA="$dir/web-data" \
  CLYSM_ELECTRON_TEST="$root/tests/lem/drive.js" \
  timeout 180 "$electron" tests/lem/web-shell.js --ozone-platform=headless 2>&1 | grep -E '^(ok|FAIL|lem)' || status=1
exit $status
