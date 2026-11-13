#!/bin/sh
# compile each source file and show real errors/warnings
cd "$(dirname "$0")/.." && mkdir -p build
sbcl --noinform --non-interactive --eval '(progn (dolist (f (list "src/package" "src/wasm" "src/reader" "src/layout" "src/frontend" "src/types" "src/codegen" "src/prims" "src/typed" "src/driver")) (let ((fasl (compile-file (concatenate (quote string) f ".lisp") :output-file (concatenate (quote string) "build/check-" (substitute #\- #\/ f) ".fasl")))) (load fasl))))' 2>&1 | grep -B8 -A6 "caught ERROR\|caught WARNING" | grep -v "^--$" | head -${1:-60}
