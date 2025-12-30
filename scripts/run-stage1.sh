#!/usr/bin/env bash
# run-stage1.sh - Convenience wrapper for Stage 1 Wasm runner
# Phase 13D-8: Stage 1 Runtime Environment (001-stage1-runtime)
#
# Usage:
#   ./scripts/run-stage1.sh [options] [wasm-path]
#
# Options:
#   --help, -h     Show help message
#   --verbose, -v  Enable verbose logging
#   --wasm <path>  Specify custom Wasm file (alternative to positional arg)
#   --expr <expr>  Compile a Lisp expression
#   --output <path> Output path for compiled Wasm
#
# Exit Codes:
#   0  - Success
#   1  - Partial success
#   2  - Failure
#   3  - Missing dependency
#   77 - Known limitation

set -euo pipefail

# Resolve script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Default paths
RUNNER="$PROJECT_ROOT/host-shim/stage1-runner.js"
DEFAULT_WASM="$PROJECT_ROOT/dist/clysm-stage1.wasm"

# Parse arguments
WASM_PATH=""
RUNNER_ARGS=()

show_help() {
    cat <<EOF
Stage 1 Wasm Runner - Clysm Compiler Bootstrap

Usage:
  $0 [options] [wasm-path]

Arguments:
  wasm-path          Path to Stage 1 Wasm module (default: dist/clysm-stage1.wasm)

Options:
  --help, -h         Show this help message
  --verbose, -v      Enable verbose logging
  --wasm <path>      Specify Wasm file (alternative to positional argument)
  --expr <expr>      Compile a Lisp expression (requires compile_form export)
  --output <path>    Output path for compiled Wasm (with --expr)

Exit Codes:
  0  - Success (_start executed without errors)
  1  - Partial success (some operations failed)
  2  - Failure (Wasm trap or execution error)
  3  - Missing dependency (Wasm file not found)
  77 - Known limitation (required export not available)

Examples:
  $0                             # Run Stage 1 with defaults
  $0 --verbose                   # Run with verbose output
  $0 --wasm ./custom-stage1.wasm # Run custom Wasm module
  $0 --expr "(+ 1 2)"            # Compile expression (may exit 77)

EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --help|-h)
            show_help
            exit 0
            ;;
        --verbose|-v)
            RUNNER_ARGS+=("--verbose")
            shift
            ;;
        --wasm)
            if [[ -z "${2:-}" ]]; then
                echo "ERROR: --wasm requires an argument" >&2
                exit 2
            fi
            WASM_PATH="$2"
            shift 2
            ;;
        --expr)
            if [[ -z "${2:-}" ]]; then
                echo "ERROR: --expr requires an argument" >&2
                exit 2
            fi
            RUNNER_ARGS+=("--expr" "$2")
            shift 2
            ;;
        --output)
            if [[ -z "${2:-}" ]]; then
                echo "ERROR: --output requires an argument" >&2
                exit 2
            fi
            RUNNER_ARGS+=("--output" "$2")
            shift 2
            ;;
        -*)
            echo "ERROR: Unknown option: $1" >&2
            exit 2
            ;;
        *)
            # Positional argument - treat as wasm path
            if [[ -z "$WASM_PATH" ]]; then
                WASM_PATH="$1"
            else
                echo "ERROR: Multiple wasm paths specified" >&2
                exit 2
            fi
            shift
            ;;
    esac
done

# Use default wasm path if not specified
if [[ -z "$WASM_PATH" ]]; then
    WASM_PATH="$DEFAULT_WASM"
fi

# Check runner exists
if [[ ! -f "$RUNNER" ]]; then
    echo "ERROR: Runner script not found: $RUNNER" >&2
    exit 3
fi

# Check wasm exists
if [[ ! -f "$WASM_PATH" ]]; then
    echo "ERROR: Stage 1 binary not found: $WASM_PATH" >&2
    exit 3
fi

# Run Stage 1
exec node "$RUNNER" "${RUNNER_ARGS[@]}" "$WASM_PATH"
