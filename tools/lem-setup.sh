#!/bin/sh
# tools/lem-setup.sh — set up, once, the checkout of Lem (cxxxr/lem) that
# tools/bench-lem.sh and tools/lem-scenario.sh use, in a place of its own
# that outlives any checkout of Clysm:
#   tools/lem-setup.sh [DEST]        DEST: by default $HOME/src/clysm-lem
# The source is LEM_SRC, a checkout of cxxxr/lem (by default build/lem of
# this tree if there is one), cloned with `git clone --local`, else
# https://github.com/cxxxr/lem.  micros and lem-mailbox, which Lem's Clysm
# frontend takes at the commits of qlfile.lock, are copied from
# LEM_SRC/build/clysm/deps/ (else cloned from GitHub at those commits) into
# DEST/build/clysm/deps/; Electron, for the scenario, is copied from
# LEM_SRC/build/clysm/desktop/node_modules/ if it is there.  Quicklisp
# (~/quicklisp/) must have Lem's other libraries already.
#
# Then:
#   LEM_DIR=DEST tools/bench-lem.sh          (bench-lem.sh finds DEST by itself
#                                             when this tree has no build/lem)
#   CLYSM_DIR=$PWD DEST/frontends/clysm/build-offline.sh && tools/lem-scenario.sh DEST
# build-offline.sh is the frontend's build.sh without its downloads; it
# builds the desktop application in DEST/build/clysm/desktop/ with the
# Clysm of CLYSM_DIR (a build for one Clysm replaces that of another).
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DEST="${1:-$HOME/src/clysm-lem}"
SRC="${LEM_SRC:-$ROOT/build/lem}"
if [ -e "$DEST" ]; then echo "$DEST exists already"; exit 1; fi
if [ -d "$SRC/frontends/clysm" ]; then
  git clone -q --local "$SRC" "$DEST"
else
  SRC=""
  git clone -q https://github.com/cxxxr/lem "$DEST"
fi
deps="$DEST/build/clysm/deps"
mkdir -p "$deps" "$DEST/build/clysm/desktop"
locked() {  # the commit of the git source NAME in qlfile.lock
  sed -n "/^(\"$1\" \\./,/:version/s/.*:version \"git-\\([0-9a-f]*\\)\".*/\\1/p" "$DEST/qlfile.lock"
}
for dep in micros lem-mailbox; do
  if [ -n "$SRC" ] && [ -d "$SRC/build/clysm/deps/$dep" ]; then
    cp -r "$SRC/build/clysm/deps/$dep" "$deps/$dep"
  else
    git init -q "$deps/$dep"
    git -C "$deps/$dep" fetch -q --depth 1 "https://github.com/lem-project/$dep.git" "$(locked $dep)"
    git -C "$deps/$dep" checkout -q FETCH_HEAD
  fi
done
chmod -R u+w "$deps"
if [ -n "$SRC" ] && [ -d "$SRC/build/clysm/desktop/node_modules/electron/dist" ]; then
  cp -a "$SRC/build/clysm/desktop/node_modules" "$DEST/build/clysm/desktop/"
fi
# the frontend's build.sh without its downloads (the dependencies are there)
sed -e 's/^clone() {.*/clone() { [ -d "$2" ] \&\& return; echo "missing $2"; exit 1;/' \
    "$DEST/frontends/clysm/build.sh" > "$DEST/frontends/clysm/build-offline.sh"
chmod +x "$DEST/frontends/clysm/build-offline.sh"
echo "Lem for the benchmarks is in $DEST (LEM_DIR=$DEST)"
