#!/bin/sh
# Test the UI framework (ui/) on its S-expression backend: tests/ui/ui.lisp
# (components, state, commands), tests/ui/pane.lisp (panes, presentations,
# dialogs...), tests/ui/names.lisp (the names docs/INTERNALS.md gives exist) and
# the examples of the guide (docs/ui/guide) are compiled ahead of time, and loaded by the REPL image (which
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
# run_files NAME FILE...: compile FILE... ahead of time and run it, and load
# it in the REPL image; compare both outputs with tests/ui/NAME.expected
run_files() {
  name="$1"; shift
  bin/clysm run "$@" > "build/tests/$name-aot.out" 2> "build/tests/$name-aot.err"
  check "$name" aot
  cat "$@" > "build/tests/$name-all.lisp"
  bin/clysm script "build/tests/$name-all.lisp" > "build/tests/$name-native.out" 2> "build/tests/$name-native.err"
  check "$name" native
}
# run NAME FILE...: the same with tests/ui/NAME.lisp after FILE...
run() {
  name="$1"; shift
  run_files "$name" "$@" "tests/ui/$name.lisp"
}
# the names that the table of CAPI and CLIM in docs/INTERNALS.md gives for this framework:
# what is in backquotes in the right column — and in the left one where the
# right one says "the same" — and is a plain symbol (not a keyword, not T)
readme_names() {
  sed -n '/^### What of CAPI and CLIM is here/,/^### Commands/p' docs/INTERNALS.md | grep '^|' | grep -v '^| CAPI\|^| CLIM\|^|---' \
    | awk -F'|' '{ if ($3 ~ /the same/) print $2 " " $3; else print $3 }' \
    | grep -o '`[^`]*`' | tr -d '`' | tr ' /' '\n\n' | sed 's/^ui://' \
    | grep -x '[a-z*][a-z0-9*-]*[a-z0-9*]' | grep -v -x 'nil' | sort -u
}
{ echo '(in-package "CL-USER")'; echo '(defparameter *readme-names* (quote ('; readme_names | sed 's/.*/"&"/'; echo ')))'; } > build/tests/readme-names.lisp

run ui ui/ui.lisp ui/command.lisp ui/sexp.lisp
run pane ui/ui.lisp ui/command.lisp ui/present.lisp ui/pane.lisp ui/dialog.lisp ui/graphics.lisp ui/stream.lisp ui/sexp.lisp
# the examples of the guide (docs/ui/guide), run on the S-expression backend
python3 tools/ui-doc.py --examples build/tests/guide-examples.lisp >/dev/null
run_files examples ui/ui.lisp ui/command.lisp ui/present.lisp ui/pane.lisp ui/dialog.lisp ui/graphics.lisp \
  ui/stream.lisp ui/sexp.lisp tests/ui/examples.lisp build/tests/guide-examples.lisp
# every exported symbol is documented, and docs/ui/reference is up to date
if python3 tools/ui-doc.py --check > build/tests/reference.out; then
  echo "reference: passed"
else
  echo "FAIL reference"; cat build/tests/reference.out; status=1
fi
run names ui/ui.lisp ui/command.lisp ui/present.lisp ui/pane.lisp ui/dialog.lisp ui/graphics.lisp ui/stream.lisp ui/sexp.lisp build/tests/readme-names.lisp
exit $status
