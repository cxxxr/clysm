#!/bin/sh
# Test the UI framework (ui/) on its S-expression backend: tests/ui/ui.lisp
# (components, state, commands) and tests/ui/pane.lisp (panes, presentations,
# dialogs) are compiled ahead of time, and loaded by the REPL image (which
# compiles every form at run time); the outputs are compared with
# tests/ui/NAME.expected.
cd "$(dirname "$0")/.." || exit 1
mkdir -p build/tests
status=0
check() {
  if cmp -s "tests/ui/$1.expected" "build/tests/$1-$2.out"; then
    echo "$1 ($2): passed"
  else
    echo "FAIL $1 ($2)"; diff "tests/ui/$1.expected" "build/tests/$1-$2.out" | head -20; status=1
  fi
}
run() {
  name="$1"; shift
  bin/clysm run "$@" "tests/ui/$name.lisp" > "build/tests/$name-aot.out" 2> "build/tests/$name-aot.err"
  check "$name" aot
  cat "$@" "tests/ui/$name.lisp" > "build/tests/$name-all.lisp"
  bin/clysm script "build/tests/$name-all.lisp" > "build/tests/$name-native.out" 2> "build/tests/$name-native.err"
  check "$name" native
}
run ui ui/ui.lisp ui/command.lisp ui/sexp.lisp
run pane ui/ui.lisp ui/command.lisp ui/present.lisp ui/pane.lisp ui/dialog.lisp ui/graphics.lisp ui/sexp.lisp
exit $status
