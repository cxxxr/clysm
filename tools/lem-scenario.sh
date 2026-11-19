#!/bin/sh
# tools/lem-scenario.sh — run the scenario of a person using Lem on Clysm
# (tests/lem/scenario.js) in the desktop application, without a display,
# and print the latency of each step's keys.
#
#   tools/lem-scenario.sh [LEM-DIR] [OUT-DIR]
#
# LEM-DIR is a checkout of cxxxr/lem whose build/clysm/desktop/ was built
# with this Clysm (CLYSM_DIR=$PWD frontends/clysm/build.sh); by default
# build/lem.  OUT-DIR (by default build/lem-scenario) gets result.json, and
# with LEM_PROFILE=1 a CPU profile per step and trace.json, which
# `node tools/lem-profile.mjs OUT-DIR` summarizes.  LEM_RUNS=N runs it N
# times (OUT-DIR/1, 2, ...).  The editor opens a copy of src/prims.lisp.
# It runs on the CPUs of taskset if given (e.g. taskset -c 4-31).
set -e
clysm="$(cd "$(dirname "$0")/.." && pwd)"
lem="$(cd "${1:-$clysm/build/lem}" && pwd)"
out="${2:-$clysm/build/lem-scenario}"
desktop=$lem/build/clysm/desktop
electron=$desktop/node_modules/electron/dist/electron
[ -x "$electron" ] || { echo "no $desktop: build it with CLYSM_DIR=$clysm $lem/frontends/clysm/build.sh"; exit 1; }
runs=${LEM_RUNS:-1}
mkdir -p "$out"
out="$(cd "$out" && pwd)"
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
i=1
while [ $i -le $runs ]; do
  o=$out; [ "$runs" -gt 1 ] && o=$out/$i && mkdir -p "$o"
  rm -rf "$dir/home" "$dir/data"; mkdir -p "$dir/home" "$dir/data"
  cp "$clysm/src/prims.lisp" "$dir/home/prims.lisp"
  (cd "$desktop" &&
     LEM_HOME="$dir/home/.lem/" CLYSM_USER_DATA="$dir/data" \
     LEM_SCENARIO_OUT="$o" LEM_SCENARIO_FILE="$dir/home/prims.lisp" \
     CLYSM_ELECTRON_TEST="$clysm/tests/lem/scenario.js" CLYSM_STARTUP_T0=$(date +%s%3N) \
     timeout 600 "$electron" . --ozone-platform=headless 2>&1) |
    grep -vE '^\[|^$|^ERR:|Fontconfig|dbus|GPU|gpu' | tee "$o/out.txt"
  grep -q '^lem-scenario: ok' "$o/out.txt"
  i=$((i + 1))
done
