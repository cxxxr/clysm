#!/bin/sh
# bin/clysm test with an ASDF system (its files, those of its dependencies
# from Quicklisp, and contrib/clysm-test), and a library built from the system.
cd "$(dirname "$0")/../.." || exit 1
out=$(node bin/clysm.mjs test --system wordcount-tests --asd tests/system/wordcount-tests.asd \
        tests/system/test-count.lisp tests/system/test-fails.lisp 2>&1)
echo "$out" | grep -q '^ok   tests/system/test-count.lisp' &&
echo "$out" | grep -q '^FAIL tests/system/test-fails.lisp' &&
echo "$out" | grep -q '1 passed, 1 failed' || { echo "$out"; echo "system: FAIL"; exit 1; }
echo "system: ok"
