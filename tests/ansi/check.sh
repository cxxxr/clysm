#!/bin/bash
# Judge the last run of tests/ansi/run.sh (build/ansi/): fail unless every
# category ran to its end and the tests that failed or crashed are those of
# tests/ansi/expected-failures.txt.  Used by .github/workflows/ansi.yml.
cd "$(dirname "$0")/../.." || exit 1
[ -s build/ansi/summary.txt ] || { echo "no results in build/ansi: run tests/ansi/run.sh first"; exit 1; }
expected=$(grep -v '^ *\(#\|$\)' tests/ansi/expected-failures.txt | sort -u)
status=0
actual=""
for cat in $(awk '$1 != "ALL" {print $1}' build/ansi/summary.txt); do
  log="build/ansi/$cat.log"
  if ! grep -aq "^@@DONE" "$log"; then
    echo "$cat: did not run to the end (see $log)"; status=1
  fi
  actual="$actual$(grep -a "^@@ [0-9]* \(FAIL\|CRASH\) " "$log" | cut -d' ' -f4-)"$'\n'
done
actual=$(printf '%s' "$actual" | grep -v '^$' | sort -u)
new=$(comm -13 <(printf '%s\n' "$expected") <(printf '%s\n' "$actual") | grep -v '^$')
fixed=$(comm -23 <(printf '%s\n' "$expected") <(printf '%s\n' "$actual") | grep -v '^$')
if [ -n "$new" ]; then
  echo "tests that failed and are not in tests/ansi/expected-failures.txt:"
  printf '%s\n' "$new" | sed 's/^/  /'
  status=1
fi
# only a full run can say that an expected failure is gone
if [ -n "$fixed" ] && [ "$(awk '$1 != "ALL"' build/ansi/summary.txt | wc -l)" -ge 24 ]; then
  echo "tests of tests/ansi/expected-failures.txt that pass now (take them out):"
  printf '%s\n' "$fixed" | sed 's/^/  /'
  status=1
fi
[ $status = 0 ] && echo "ansi: ok ($(tail -1 build/ansi/summary.txt | sed 's/^ALL *//'))"
exit $status
