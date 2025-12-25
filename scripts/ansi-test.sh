#!/usr/bin/env bash
# ansi-test.sh - CLI wrapper for ANSI CL compliance test suite
#
# Usage:
#   ./scripts/ansi-test.sh                    # Run all tests
#   ./scripts/ansi-test.sh -c cons            # Run only 'cons' category
#   ./scripts/ansi-test.sh -r report.md       # Generate Markdown report
#   ./scripts/ansi-test.sh --compare          # Compare with baseline
#   ./scripts/ansi-test.sh --update-baseline  # Update baseline
#
# Exit codes:
#   0: All tests passed or no regression detected
#   1: Tests failed or regression detected
#   2: Invalid arguments

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Default values
CATEGORY=""
REPORT_PATH=""
COMPARE_BASELINE=false
UPDATE_BASELINE=false
TIMEOUT=30
PARALLEL=1

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Run ANSI Common Lisp compliance tests for Clysm.

Options:
  -c, --category NAME     Run only tests in specified category
  -r, --report PATH       Generate Markdown report at PATH
  -t, --timeout SECONDS   Test timeout in seconds (default: 30)
  -p, --parallel N        Number of parallel workers (default: 1)
  --compare               Compare results with baseline
  --update-baseline       Update baseline with current results
  -h, --help              Show this help message

Examples:
  $(basename "$0")                        Run all tests
  $(basename "$0") -c cons                Run 'cons' category only
  $(basename "$0") -c numbers -r out.md   Run 'numbers' and save report
  $(basename "$0") --compare              Check for regressions

EOF
}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -c|--category)
            CATEGORY="$2"
            shift 2
            ;;
        -r|--report)
            REPORT_PATH="$2"
            shift 2
            ;;
        -t|--timeout)
            TIMEOUT="$2"
            shift 2
            ;;
        -p|--parallel)
            PARALLEL="$2"
            shift 2
            ;;
        --compare)
            COMPARE_BASELINE=true
            shift
            ;;
        --update-baseline)
            UPDATE_BASELINE=true
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Error: Unknown option: $1" >&2
            usage
            exit 2
            ;;
    esac
done

# Build Lisp command - use funcall with intern to avoid reader issues
# The package doesn't exist until after load-system, so we can't use
# package-qualified symbols directly in the --eval string
build_run_command() {
    local cmd="(let ((run-fn (intern \"RUN-ANSI-TESTS\" :clysm/ansi-test)))"
    cmd+=" (funcall run-fn"

    if [[ -n "$CATEGORY" ]]; then
        cmd+=" :category \"$CATEGORY\""
    fi

    if [[ -n "$REPORT_PATH" ]]; then
        cmd+=" :report-path \"$REPORT_PATH\""
    fi

    cmd+=" :timeout $TIMEOUT))"

    echo "$cmd"
}

build_baseline_command() {
    if [[ "$UPDATE_BASELINE" == "true" ]]; then
        echo "(funcall (intern \"SAVE-BASELINE\" :clysm/ansi-test) *)"
    fi

    if [[ "$COMPARE_BASELINE" == "true" ]]; then
        cat <<'LISP'
(let* ((load-fn (intern "LOAD-BASELINE" :clysm/ansi-test))
       (compare-fn (intern "COMPARE-TO-BASELINE" :clysm/ansi-test))
       (regression-fn (intern "BASELINE-COMPARISON-REGRESSION-P" :clysm/ansi-test))
       (baseline (funcall load-fn))
       (comparison (funcall compare-fn * baseline)))
  (when (funcall regression-fn comparison)
    (sb-ext:exit :code 1)))
LISP
    fi
}

# Run the tests
cd "$PROJECT_ROOT"

echo "Running ANSI CL compliance tests..."
if [[ -n "$CATEGORY" ]]; then
    echo "Category: $CATEGORY"
fi

RUN_CMD=$(build_run_command)
BASELINE_CMD=$(build_baseline_command)

# Use separate --eval calls so package exists before we reference it
exec sbcl --noinform --non-interactive \
    --eval '(require :asdf)' \
    --eval '(asdf:load-system "clysm/ansi-test")' \
    --eval "$RUN_CMD" \
    ${BASELINE_CMD:+--eval "$BASELINE_CMD"}
