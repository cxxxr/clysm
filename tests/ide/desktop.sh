#!/bin/sh
# Build the desktop IDE (examples/ide/build-desktop.sh) and drive it in
# Electron with tests/ide/desktop.js, on a project made in a temporary
# folder.  SCREENSHOT=FILE saves what it shows.
set -e
cd "$(dirname "$0")/../.."
examples/ide/build-desktop.sh >/dev/null
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/project/util" "$work/project/.git"
cat > "$work/project/hello.lisp" <<'LISP'
(defpackage "HELLO"
  (:use "CL")
  (:export "GREET"))

(in-package "HELLO")

(defun greet (name)
  (format t "Hello, ~A!~%" name)
  (math:square (length name)))
LISP
cat > "$work/project/util/math.lisp" <<'LISP'
(defpackage "MATH"
  (:use "CL")
  (:export "SQUARE"))

(in-package "MATH")

(defun square (x)
  (* x x))
LISP
echo "ignored" > "$work/project/.git/config"
cd build/ide-desktop
IDE_TEST_PROJECT="$work/project" IDE_TEST_SCREENSHOT="$SCREENSHOT" \
CLYSM_USER_DATA="$work/data" CLYSM_ELECTRON_TEST="$OLDPWD/tests/ide/desktop.js" \
  npx electron . "$work/project" 2>&1 | grep -E '^(ok|FAIL|all passed|[0-9]+ failed)'
