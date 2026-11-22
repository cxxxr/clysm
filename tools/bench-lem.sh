#!/bin/sh
# Lem's own code on a large Lisp file, in SBCL and in Clysm, and the time
# ratio, as tools/bench.sh prints it:
#   /tmp/clysm-bench-slot.sh tools/bench-lem.sh [FILE]
# FILE (src/prims.lisp by default, 7000 lines) is opened in a buffer of
# Lem's core with the Lisp mode's syntax table and grammar and the fake
# display of Lem's tests, and the actions that the scenario of a person
# using Lem on Clysm found to take the time are timed in the same process
# after running them once (tests/lem/buffer-bench.lisp): pages down and
# lines down with the redisplay and the highlighting of the new lines,
# typing 200 characters of Lisp in a function (the autodoc parse of each
# space, the indentation of each line, the redisplay), FORM-OFFSET over all
# toplevel forms, the highlighting of the whole file, a search, STRING-WIDTH
# of every line.
#
# Lem: a checkout of cxxxr/lem with micros and lem-mailbox in
# build/clysm/deps/ (Lem itself does not have to be built): LEM_DIR, else
# build/lem of this tree, else $HOME/src/clysm-lem, which tools/lem-setup.sh
# makes once.  The other
# libraries come from Quicklisp (~/quicklisp/, already installed: nothing
# is downloaded).  Clysm runs it in the REPL image (bin/clysm image-native),
# which loads Lem's fasls (compiled by ASDF the first time for each build
# of the image, a minute or two, in ~/.cache/common-lisp/), as `clysm
# script` does; SBCL loads the same systems, with the same stubs, with the
# ASDF of third_party/asdf (SBCL's own is too old for Lem).
# Each side runs BENCH_RUNS times (default 3); the best time is used.
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
NURSERY="--max-semi-space-size=${CLYSM_NURSERY_MB:-256}"   # as bin/clysm starts the image
FILE="$(cd "$(dirname "${1:-$ROOT/src/prims.lisp}")" && pwd)/$(basename "${1:-$ROOT/src/prims.lisp}")"
if [ -z "$LEM_DIR" ]; then
  if [ -d "$ROOT/build/lem/build/clysm/deps" ]; then LEM_DIR=$ROOT/build/lem; else LEM_DIR=$HOME/src/clysm-lem; fi
fi
LEM_DIR="$(cd "$LEM_DIR" && pwd)"
export LEM_DIR
[ -d "$LEM_DIR/build/clysm/deps/micros" ] ||
  { echo "no $LEM_DIR/build/clysm/deps/micros: run tools/lem-setup.sh"; exit 1; }
RUNS="${BENCH_RUNS:-3}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
best() { awk '{ if (!($1 in t)) { order[n++]=$1 } if (!($1 in t) || $2 < t[$1]) t[$1]=$2; r[$1]=$3 }
              END { for (i=0;i<n;i++) print order[i], t[order[i]], r[order[i]] }' "$@" | sort; }
mkdir -p "$ROOT/build"
[ "$ROOT/build/asdf-sbcl.fasl" -nt "$ROOT/third_party/asdf/asdf.lisp" ] ||
  sbcl --noinform --non-interactive --eval '(require :asdf)' \
       --eval "(compile-file \"$ROOT/third_party/asdf/asdf.lisp\" :output-file \"$ROOT/build/asdf-sbcl.fasl\")" >/dev/null 2>&1
cat > "$TMP/clysm.lisp" <<EOF
(require :asdf)
(require :quicklisp)
(load "$ROOT/tests/lem/load-lem.lisp")
(lem-buffer-bench:main "$FILE")
EOF
IMAGE="$("$ROOT/bin/clysm" image-native)"
# (the first run compiles what is missing)
node $NURSERY "$ROOT/host/run.mjs" "$IMAGE" "$TMP/clysm.lisp" > "$TMP/first.txt" 2>&1 ||
  { tail -20 "$TMP/first.txt"; exit 1; }
i=0
while [ $i -lt "$RUNS" ]; do
  sbcl --noinform --non-interactive --eval '(require :asdf)' --eval "(load \"$ROOT/build/asdf-sbcl.fasl\")" \
       --eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' \
       --load "$ROOT/tests/lem/load-lem.lisp" --eval "(lem-buffer-bench:main \"$FILE\")" 2>&1 |
    grep '^l-' >> "$TMP/a.raw"
  node $NURSERY "$ROOT/host/run.mjs" "$IMAGE" "$TMP/clysm.lisp" 2>&1 | grep '^l-' >> "$TMP/c.raw"
  i=$((i+1))
done
best "$TMP/a.raw" > "$TMP/a.txt"; best "$TMP/c.raw" > "$TMP/b.txt"
join "$TMP/a.txt" "$TMP/b.txt" | awk '
  { s=$2; c=$4; r=(s>0)? c/s : c; ok=($3==$5)?"":"  RESULT MISMATCH";
    printf "%-24s sbcl %6d ms  clysm %7d ms  x%7.2f%s\n", $1, s, c, r, ok;
    if (s>0) { lg+=log(r); n++; if (r>mx) { mx=r; mn=$1 } } }
  END { printf "geometric mean slowdown: x%.2f   slowest: %s x%.2f   (%d benchmarks)\n", exp(lg/n), mn, mx, n }'
