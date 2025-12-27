#!/usr/bin/env bash
#
# run-stage2-gen.sh - Generate Stage 2 binary using Stage 1
#
# Part of Feature 040: Fixed-Point Verification
#
# Usage:
#   ./scripts/run-stage2-gen.sh [OPTIONS]
#
# Options:
#   --stage1 PATH    Path to Stage 1 binary (default: dist/clysm-stage1.wasm)
#   --output PATH    Output path for Stage 2 (default: dist/clysm-stage2.wasm)
#   --source-dir P   Source directory (default: src/clysm/)
#   --json           Output progress as JSON
#   --help           Show this help
#
# Exit codes:
#   0 - Stage 2 generated successfully
#   1 - Compilation error (partial failure)
#   2 - Missing dependency

set -euo pipefail

# Defaults
STAGE1_PATH="${STAGE1_PATH:-dist/clysm-stage1.wasm}"
STAGE2_PATH="${STAGE2_PATH:-dist/clysm-stage2.wasm}"
SOURCE_DIR="${SOURCE_DIR:-src/clysm/}"
JSON_MODE=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --stage1)
            STAGE1_PATH="$2"
            shift 2
            ;;
        --output)
            STAGE2_PATH="$2"
            shift 2
            ;;
        --source-dir)
            SOURCE_DIR="$2"
            shift 2
            ;;
        --json)
            JSON_MODE=true
            shift
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Generate Stage 2 binary by compiling Clysm source using Stage 1."
            echo ""
            echo "Options:"
            echo "  --stage1 PATH    Path to Stage 1 binary (default: dist/clysm-stage1.wasm)"
            echo "  --output PATH    Output path for Stage 2 (default: dist/clysm-stage2.wasm)"
            echo "  --source-dir P   Source directory (default: src/clysm/)"
            echo "  --json           Output progress as JSON"
            echo "  --help           Show this help"
            echo ""
            echo "Exit codes:"
            echo "  0 - Stage 2 generated successfully"
            echo "  1 - Compilation error"
            echo "  2 - Missing dependency"
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 2
            ;;
    esac
done

# Change to project root
cd "$(dirname "$0")/.."

# Check dependencies
if ! command -v wasmtime &> /dev/null; then
    if $JSON_MODE; then
        echo '{"status":"MISSING_DEPENDENCY","error":"wasmtime not found"}'
    else
        echo "ERROR: wasmtime not found" >&2
        echo "Install: curl https://wasmtime.dev/install.sh -sSf | bash" >&2
    fi
    exit 2
fi

if ! command -v node &> /dev/null; then
    if $JSON_MODE; then
        echo '{"status":"MISSING_DEPENDENCY","error":"node not found"}'
    else
        echo "ERROR: Node.js not found" >&2
    fi
    exit 2
fi

if [[ ! -f "$STAGE1_PATH" ]]; then
    if $JSON_MODE; then
        echo "{\"status\":\"MISSING_DEPENDENCY\",\"error\":\"Stage 1 not found: $STAGE1_PATH\"}"
    else
        echo "ERROR: Stage 1 not found: $STAGE1_PATH" >&2
        echo "Run: sbcl --load build/stage1-gen.lisp" >&2
    fi
    exit 2
fi

# Run Stage 2 generation via host shim
if $JSON_MODE; then
    node host-shim/stage1-host.js \
        --mode compile \
        --stage1 "$STAGE1_PATH" \
        --output "$STAGE2_PATH" \
        --source-dir "$SOURCE_DIR" \
        2>/dev/null
    exit_code=$?
else
    echo "=== Stage 2 Generation ==="
    echo "Stage 1: $STAGE1_PATH"
    echo "Output:  $STAGE2_PATH"
    echo "Source:  $SOURCE_DIR"
    echo ""

    node host-shim/stage1-host.js \
        --mode compile \
        --stage1 "$STAGE1_PATH" \
        --output "$STAGE2_PATH" \
        --source-dir "$SOURCE_DIR"
    exit_code=$?

    if [[ $exit_code -eq 0 ]]; then
        echo ""
        echo "=== SUCCESS ==="
        if [[ -f "$STAGE2_PATH" ]]; then
            size=$(stat -f%z "$STAGE2_PATH" 2>/dev/null || stat -c%s "$STAGE2_PATH" 2>/dev/null)
            echo "Stage 2 generated: $size bytes"
        fi
    else
        echo ""
        echo "=== PARTIAL/FAILURE ==="
        echo "Exit code: $exit_code"
    fi
fi

exit $exit_code
