#!/usr/bin/env bash
#
# run-tests-via-interpreter.sh - Run tests using the interpreter
#
# Part of Feature 044: Interpreter Bootstrap Strategy
# Phase 7: T112 - Run tests via interpreter
#
# This script demonstrates running Clysm tests using the interpreter
# instead of compiling to Wasm. Useful for:
# - Rapid test iteration during development
# - Debugging compiler issues
# - Validating interpreter correctness
#
# Usage:
#   ./scripts/run-tests-via-interpreter.sh [options] [test-pattern]
#
# Options:
#   --verbose       Show detailed test output
#   --json          Output results in JSON format
#   --help          Show this help message
#
# Examples:
#   ./scripts/run-tests-via-interpreter.sh               # Run all interpreter tests
#   ./scripts/run-tests-via-interpreter.sh defun         # Run defun tests
#   ./scripts/run-tests-via-interpreter.sh --verbose     # Verbose output
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Default options
VERBOSE=false
JSON_OUTPUT=false
TEST_PATTERN=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        --json)
            JSON_OUTPUT=true
            shift
            ;;
        --help|-h)
            head -25 "$0" | tail -23
            exit 0
            ;;
        -*)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
        *)
            TEST_PATTERN="$1"
            shift
            ;;
    esac
done

# Check SBCL is available (needed to load the interpreter)
if ! command -v sbcl &>/dev/null; then
    echo "Error: SBCL required to run interpreter tests" >&2
    echo "Note: Future versions may run on wasmtime directly" >&2
    exit 1
fi

cd "$PROJECT_ROOT"

# Build test command
if $VERBOSE; then
    echo "=== Running Tests via Interpreter ==="
    echo ""
fi

# Run tests using SBCL + interpreter
LISP_EXPR="
(progn
  (require :asdf)
  (ql:quickload :rove :silent t)
  (asdf:load-system :clysm/tests :verbose nil)

  ;; Run interpreter-specific tests
  (format t \"~%=== Interpreter Tests ===\")

  ;; Core interpreter tests
  (format t \"~%~%Running interpreter unit tests...~%\")
  (rove:run-test 'clysm/tests/interpreter-test::test-interpret-arithmetic)
  (rove:run-test 'clysm/tests/interpreter-test::test-interpret-let)
  (rove:run-test 'clysm/tests/interpreter-test::test-interpret-defun)
  (rove:run-test 'clysm/tests/interpreter-test::test-interpret-lambda)
  (rove:run-test 'clysm/tests/interpreter-test::test-interpret-if)
  (rove:run-test 'clysm/tests/interpreter-test::test-interpret-cond)
  (rove:run-test 'clysm/tests/interpreter-test::test-interpret-progn)

  ;; Bootstrap-specific tests
  (format t \"~%~%Running bootstrap tests...~%\")
  (rove:run-test 'clysm/tests/bootstrap-fixpoint-test::test-interpreter-stage0-generates-valid-wasm)
  (rove:run-test 'clysm/tests/bootstrap-fixpoint-test::test-bootstrap-chain-progress-tracking)
  (rove:run-test 'clysm/tests/bootstrap-fixpoint-test::test-form-compilable-p-for-bootstrap)

  ;; SBCL-free workflow tests
  (format t \"~%~%Running SBCL-free workflow tests...~%\")
  (rove:run-test 'clysm/tests/sbcl-free-test::test-interpreter-generates-stage0)
  (rove:run-test 'clysm/tests/sbcl-free-test::test-interpreter-basic-evaluation)
  (rove:run-test 'clysm/tests/sbcl-free-test::test-interpreter-builtins)

  (format t \"~%~%=== Interpreter Tests Complete ===\")
  (format t \"~%\"))"

if $JSON_OUTPUT; then
    # Run with JSON output (capture results)
    sbcl --noinform --non-interactive --eval "$LISP_EXPR" 2>&1 | \
        grep -E "(✓|✗|PASS|FAIL)" | \
        awk 'BEGIN {print "{"} {gsub(/[✓]/,"pass"); gsub(/[✗]/,"fail"); print "  \"" NR "\": \"" $0 "\","} END {print "  \"complete\": true\n}"}'
else
    # Run normally
    sbcl --noinform --non-interactive --eval "$LISP_EXPR"
fi
