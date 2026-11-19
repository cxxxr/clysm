#!/bin/sh
# Build the desktop editor (examples/editor/build-desktop.sh) and drive it in
# Electron with tests/editor/desktop.js.  SCREENSHOT=FILE saves what it shows.
set -e
cd "$(dirname "$0")/../.."
examples/editor/build-desktop.sh >/dev/null
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
printf '(defun square (x)\n  (* x x))\n' > "$work/test.lisp"
cd build/editor-desktop
EDITOR_TEST_FILE="$work/test.lisp" EDITOR_TEST_SCREENSHOT="$SCREENSHOT" \
CLYSM_USER_DATA="$work/data" CLYSM_ELECTRON_TEST="$OLDPWD/tests/editor/desktop.js" \
  npx electron . "$work/test.lisp" 2>&1 | grep -E '^(ok|FAIL|all passed|[0-9]+ failed)'
