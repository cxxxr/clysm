#!/bin/sh
# Run the tests of the programs in tests/downstream/projects.txt with this
# checkout of Clysm:
#   tools/test-downstream.sh [NAME...]
# Each project is cloned into build/downstream/NAME at its commit (a URL of
# "." is this repository) and its command runs there.
cd "$(dirname "$0")/.." || exit 1
root=$(pwd)
mkdir -p build/downstream
pass=0; fail=0; failed=""
while read -r name url commit command; do
  case "$name" in ''|'#'*) continue ;; esac
  if [ $# -gt 0 ]; then
    case " $* " in *" $name "*) ;; *) continue ;; esac
  fi
  dir="build/downstream/$name"
  [ "$url" = "." ] && url="$root"
  if [ ! -d "$dir/.git" ]; then
    git clone -q "$url" "$dir" || { echo "FAIL $name (clone)"; fail=$((fail+1)); failed="$failed $name"; continue; }
  fi
  (cd "$dir" && git fetch -q origin 2>/dev/null; git checkout -q --detach "$commit") ||
    { echo "FAIL $name (checkout $commit)"; fail=$((fail+1)); failed="$failed $name"; continue; }
  if (cd "$dir" && CLYSM="$root/bin/clysm" CLYSM_ROOT="$root" sh -c "$command") > "build/downstream/$name.log" 2>&1; then
    echo "ok   $name"; pass=$((pass+1))
  else
    echo "FAIL $name (build/downstream/$name.log)"; fail=$((fail+1)); failed="$failed $name"
  fi
done < tests/downstream/projects.txt
echo "downstream: $pass passed, $fail failed${failed:+ ($failed )}"
[ $fail -eq 0 ]
