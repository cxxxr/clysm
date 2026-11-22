#!/bin/sh
# Time to get going, against SBCL: starting the REPL image, starting it with
# Quicklisp, REQUIRE of ASDF, loading libraries whose fasls are already
# compiled (ql:quickload with a warm cache), and a small script.  Each item is
# a whole process, best of BENCH_RUNS (default 5).  The fasls are compiled
# into a cache of the benchmark's own (BENCH_CACHE, by default
# build/bench-cache/, as $XDG_CACHE_HOME), so other caches are not touched;
# the first run of the libraries compiles them there.
#   tools/bench-startup.sh            (through tools/bench-slot.sh for numbers)
set -e
export LC_ALL=C
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
NURSERY="--max-semi-space-size=${CLYSM_NURSERY_MB:-256}"   # as bin/clysm starts the image
RUNS="${BENCH_RUNS:-5}"
export XDG_CACHE_HOME="${BENCH_CACHE:-$ROOT/build/bench-cache}"
mkdir -p "$XDG_CACHE_HOME"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
QL="${QUICKLISP_HOME:-$HOME/quicklisp/}setup.lisp"
IMG="$("$ROOT/bin/clysm" image-native)"
# (as bin/clysm starts it: from its heap image, which this makes if needed)
"$ROOT/bin/clysm" script /dev/null </dev/null >/dev/null 2>&1 || true
[ -z "$CLYSM_NO_HEAP_IMAGE" ] && [ -f "${IMG%.wasm}.img" ] && export CLYSM_IMAGE="${IMG%.wasm}.img"
cat > "$TMP/script.lisp" <<'EOF'
(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))
(format t "~D~%" (length (princ-to-string (fact 100))))
EOF
# NAME FORM...: the forms evaluated in order, then the process ends
items() {
  echo "s-start|t"
  echo "s-require-asdf|(require :asdf)"
  echo "s-quicklisp|(load \"$QL\")"
  for lib in alexandria cl-ppcre babel; do
    echo "s-ql-$lib|(load \"$QL\")|(funcall (intern \"QUICKLOAD\" \"QL\") :$lib :silent t)"
  done
}
ms() { echo $(( $(date +%s%N) / 1000000 )); }   # (uutils date: %3N is not zero-padded)
# warm the caches (ASDF and the libraries are compiled once)
items | while IFS='|' read -r name a b; do
  if [ -n "$b" ]; then node $NURSERY "$ROOT/host/run.mjs" "$IMG" --no-userinit --eval "$a" "$b" </dev/null >/dev/null 2>&1 || true
                       sbcl --noinform --no-userinit --non-interactive --eval "$a" --eval "$b" </dev/null >/dev/null 2>&1 || true
  else node $NURSERY "$ROOT/host/run.mjs" "$IMG" --no-userinit --eval "$a" </dev/null >/dev/null 2>&1 || true
       sbcl --noinform --no-userinit --non-interactive --eval "$a" </dev/null >/dev/null 2>&1 || true; fi
done
node $NURSERY "$ROOT/host/run.mjs" "$IMG" "$TMP/script.lisp" </dev/null >/dev/null 2>&1 || true
i=0
while [ $i -lt "$RUNS" ]; do
  items | while IFS='|' read -r name a b; do
    if [ -n "$b" ]; then
      t0=$(ms); sbcl --noinform --no-userinit --non-interactive --eval "$a" --eval "$b" </dev/null >/dev/null 2>&1 && s=ok || s=FAIL; t1=$(ms)
      echo "$name $((t1 - t0)) $s" >> "$TMP/a.raw"
      t0=$(ms); node $NURSERY "$ROOT/host/run.mjs" "$IMG" --no-userinit --eval "$a" "$b" </dev/null >/dev/null 2>&1 && s=ok || s=FAIL; t1=$(ms)
      echo "$name $((t1 - t0)) $s" >> "$TMP/b.raw"
    else
      t0=$(ms); sbcl --noinform --no-userinit --non-interactive --eval "$a" </dev/null >/dev/null 2>&1 && s=ok || s=FAIL; t1=$(ms)
      echo "$name $((t1 - t0)) $s" >> "$TMP/a.raw"
      t0=$(ms); node $NURSERY "$ROOT/host/run.mjs" "$IMG" --no-userinit --eval "$a" </dev/null >/dev/null 2>&1 && s=ok || s=FAIL; t1=$(ms)
      echo "$name $((t1 - t0)) $s" >> "$TMP/b.raw"
    fi
  done
  t0=$(ms); sbcl --script "$TMP/script.lisp" </dev/null >/dev/null 2>&1 && s=ok || s=FAIL; t1=$(ms)
  echo "s-script $((t1 - t0)) $s" >> "$TMP/a.raw"
  t0=$(ms); node $NURSERY "$ROOT/host/run.mjs" "$IMG" "$TMP/script.lisp" </dev/null >/dev/null 2>&1 && s=ok || s=FAIL; t1=$(ms)
  echo "s-script $((t1 - t0)) $s" >> "$TMP/b.raw"
  i=$((i+1))
done
best() { awk '{ if (!($1 in t)) { order[n++]=$1 } if (!($1 in t) || $2 < t[$1]) t[$1]=$2; if ($3 != "ok") r[$1]=$3; else if (!($1 in r)) r[$1]="ok" }
              END { for (i=0;i<n;i++) print order[i], t[order[i]], r[order[i]] }' "$@" | sort; }
[ -n "$BENCH_RAW" ] && cat "$TMP/a.raw" "$TMP/b.raw" > "$BENCH_RAW"
best "$TMP/a.raw" > "$TMP/a.txt"; best "$TMP/b.raw" > "$TMP/b.txt"
join "$TMP/a.txt" "$TMP/b.txt" | awk '
  { s=$2; c=$4; d=(s>0)? s : 1; r=c/d; ok=($3==$5)?"":"  RESULT MISMATCH";
    printf "%-20s sbcl %6d ms  clysm %7d ms  x%7.2f%s\n", $1, s, c, r, ok;
    lg+=log(r); n++ }
  END { printf "geometric mean slowdown: x%.2f   (%d items)\n", exp(lg/n), n }'
