#!/bin/sh
# The tests of the editor component's model (ui/editor/), on Clysm and on SBCL.
set -e
cd "$(dirname "$0")/../.."
out=$(bin/clysm run ui/editor/buffer.lisp ui/editor/syntax.lisp ui/editor/commands.lisp \
                    tests/editor/model.lisp 2>/dev/null)
echo "clysm: $out"
echo "$out" | grep -q " 0 failures" || exit 1
sbcl --script tests/editor/sbcl.lisp 2>/dev/null | tail -1 | sed 's/^/sbcl:  /'
