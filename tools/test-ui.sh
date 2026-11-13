#!/bin/sh
# Test the UI framework (ui/) on its S-expression backend: tests/ui/ui.lisp is
# compiled ahead of time, and loaded by the REPL image (which compiles every
# form at run time); both outputs are compared with tests/ui/ui.expected.
cd "$(dirname "$0")/.." || exit 1
mkdir -p build/tests
status=0
check() {
  if cmp -s tests/ui/ui.expected "build/tests/ui-$1.out"; then
    echo "ui ($1): passed"
  else
    echo "FAIL ui ($1)"; diff tests/ui/ui.expected "build/tests/ui-$1.out" | head -20; status=1
  fi
}
bin/clysm run ui/ui.lisp ui/command.lisp ui/sexp.lisp tests/ui/ui.lisp > build/tests/ui-aot.out 2> build/tests/ui-aot.err
check aot
cat ui/ui.lisp ui/command.lisp ui/sexp.lisp tests/ui/ui.lisp > build/tests/ui-all.lisp
bin/clysm script build/tests/ui-all.lisp > build/tests/ui-native.out 2> build/tests/ui-native.err
check native
exit $status
