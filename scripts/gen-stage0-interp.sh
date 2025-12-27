#!/usr/bin/env bash
# gen-stage0-interp.sh - Generate Stage 0 via interpreter
# Feature 044: Interpreter Bootstrap Strategy
# Task: T100

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Default options
VERBOSE=false
VALIDATE=true
MODULE_LIMIT=""
OUTPUT=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        --no-validate)
            VALIDATE=false
            shift
            ;;
        -l|--limit)
            MODULE_LIMIT="$2"
            shift 2
            ;;
        -o|--output)
            OUTPUT="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Generate Stage 0 Wasm binary via Tier 1 interpreter."
            echo ""
            echo "Options:"
            echo "  -v, --verbose      Enable verbose output"
            echo "  --no-validate      Skip wasm-tools validation"
            echo "  -l, --limit N      Limit modules to process"
            echo "  -o, --output PATH  Output file path"
            echo "  -h, --help         Show this help message"
            echo ""
            echo "Default output: dist/clysm-stage0-interp.wasm"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

cd "$PROJECT_ROOT"

# Build the Lisp expression for options
LISP_OPTS=":verbose $($VERBOSE && echo 't' || echo 'nil')"
LISP_OPTS="$LISP_OPTS :validate $($VALIDATE && echo 't' || echo 'nil')"

if [[ -n "$MODULE_LIMIT" ]]; then
    LISP_OPTS="$LISP_OPTS :module-limit $MODULE_LIMIT"
fi

if [[ -n "$OUTPUT" ]]; then
    LISP_OPTS="$LISP_OPTS :output \"$OUTPUT\""
fi

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║  Clysm Interpreter-Based Stage 0 Generator                   ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""

# Check for SBCL
if ! command -v sbcl &> /dev/null; then
    echo "Error: sbcl not found in PATH"
    exit 1
fi

# Check for wasm-tools if validation is enabled
if $VALIDATE && ! command -v wasm-tools &> /dev/null; then
    echo "Warning: wasm-tools not found, validation will be skipped"
    VALIDATE=false
    LISP_OPTS="$LISP_OPTS :validate nil"
fi

# Run the generation
echo "; Running SBCL with interpreter bootstrap..."
echo ""

sbcl --noinform --non-interactive \
    --eval "(require :asdf)" \
    --eval "(asdf:load-system :clysm :verbose nil)" \
    --eval "(asdf:load-system :clysm/validation :verbose nil)" \
    --eval "(clysm/interpreter-bootstrap:generate-stage0-via-interpreter $LISP_OPTS)" \
    --quit

EXIT_CODE=$?

if [[ $EXIT_CODE -eq 0 ]]; then
    echo ""
    echo "Stage 0 generation completed."
    if [[ -n "$OUTPUT" ]]; then
        echo "Output: $OUTPUT"
    else
        echo "Output: dist/clysm-stage0-interp.wasm"
    fi
else
    echo ""
    echo "Stage 0 generation failed with exit code $EXIT_CODE"
fi

exit $EXIT_CODE

