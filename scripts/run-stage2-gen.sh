#!/usr/bin/env bash
# run-stage2-gen.sh - Stage 2 generation wrapper script (T039)
# Part of 001-bootstrap-fixpoint Phase 13D-9
#
# Usage: ./scripts/run-stage2-gen.sh [--json]
#
# Exit codes:
#   0 - Success (Stage 2 generated and validates)
#   1 - Partial success (Stage 2 generated with compilation failures)
#   2 - Failure (Stage 2 generation failed completely)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

STAGE2_GEN="$PROJECT_ROOT/host-shim/stage2-gen.js"
STAGE1_PATH="$PROJECT_ROOT/dist/clysm-stage1.wasm"
STAGE2_PATH="$PROJECT_ROOT/dist/clysm-stage2.wasm"
STAGE2_REPORT="$PROJECT_ROOT/dist/stage2-report.json"

# Check prerequisites
if ! command -v node &> /dev/null; then
    echo "ERROR: Node.js not found. Install Node.js 20+."
    exit 2
fi

if [ ! -f "$STAGE1_PATH" ]; then
    echo "ERROR: Stage 1 not found at $STAGE1_PATH"
    echo "Run: sbcl --load build/stage1-complete.lisp"
    exit 2
fi

if [ ! -f "$STAGE2_GEN" ]; then
    echo "ERROR: Stage 2 generator not found at $STAGE2_GEN"
    exit 2
fi

# Run Stage 2 generation
echo "=== Running Stage 2 Generation ===" >&2
cd "$PROJECT_ROOT"

node "$STAGE2_GEN"
EXIT_CODE=$?

# Handle --json flag for CI output
if [ "$1" = "--json" ]; then
    if [ -f "$STAGE2_REPORT" ]; then
        cat "$STAGE2_REPORT"
    else
        echo '{"error": "Report not generated", "exit_code": '"$EXIT_CODE"'}'
    fi
fi

exit $EXIT_CODE
