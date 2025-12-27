#!/usr/bin/env bash
#
# verify-fixpoint-interp.sh - Fixed-point verification for interpreter bootstrap
#
# Part of Feature 044: Interpreter Bootstrap Strategy
# Phase 6: T106 - Interpreter-based fixpoint verification script
#
# Usage:
#   ./scripts/verify-fixpoint-interp.sh [options]
#
# Options:
#   --json          Output results in JSON format
#   --skip-generate Use existing Stage 0 (don't regenerate)
#   --verbose       Show detailed progress
#   --help          Show this help message
#
# Exit Codes:
#   0 - ACHIEVED: Stage 1 == Stage 2 (fixed-point verified)
#   1 - NOT_ACHIEVED: Binaries differ
#   2 - COMPILATION_ERROR: Stage generation failed
#   3 - MISSING_DEPENDENCY: wasmtime/wasm-tools/Stage 0 missing
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Default options
JSON_OUTPUT=false
SKIP_GENERATE=false
VERBOSE=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --json)
            JSON_OUTPUT=true
            shift
            ;;
        --skip-generate)
            SKIP_GENERATE=true
            shift
            ;;
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        --help|-h)
            head -24 "$0" | tail -22
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 3
            ;;
    esac
done

# Check dependencies
check_dependencies() {
    if ! command -v wasmtime &>/dev/null; then
        if $JSON_OUTPUT; then
            echo '{"status": "MISSING_DEPENDENCY", "error": "wasmtime not found"}'
        else
            echo "Error: wasmtime not found" >&2
            echo "Install: curl https://wasmtime.dev/install.sh -sSf | bash" >&2
        fi
        exit 3
    fi

    if ! command -v wasm-tools &>/dev/null; then
        if $JSON_OUTPUT; then
            echo '{"status": "MISSING_DEPENDENCY", "error": "wasm-tools not found"}'
        else
            echo "Error: wasm-tools not found" >&2
            echo "Install: cargo install wasm-tools" >&2
        fi
        exit 3
    fi

    if ! command -v sbcl &>/dev/null; then
        if $JSON_OUTPUT; then
            echo '{"status": "MISSING_DEPENDENCY", "error": "sbcl not found"}'
        else
            echo "Error: sbcl not found" >&2
        fi
        exit 3
    fi
}

# Main verification
main() {
    check_dependencies

    cd "$PROJECT_ROOT"

    # Build verification command
    LISP_EXPR="(progn
      (require :asdf)
      (asdf:load-system :clysm :verbose nil)
      (let* ((result (clysm/interpreter-bootstrap:verify-fixpoint-interpreter
                      :verbose $(if $VERBOSE; then echo "t"; else echo "nil"; fi)
                      :skip-generate $(if $SKIP_GENERATE; then echo "t"; else echo "nil"; fi)
                      :output-format $(if $JSON_OUTPUT; then echo ":json"; else echo ":text"; fi))))
        (sb-ext:exit :code (clysm/interpreter-bootstrap:fixpoint-exit-code result))))"

    # Run verification
    if $VERBOSE; then
        echo "Starting interpreter bootstrap fixed-point verification..."
    fi

    sbcl --noinform --non-interactive --eval "$LISP_EXPR"
    exit_code=$?

    exit $exit_code
}

main "$@"
