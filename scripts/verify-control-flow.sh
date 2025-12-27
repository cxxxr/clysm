#!/usr/bin/env bash
#
# verify-control-flow.sh - Verify Stage 0 control flow compilation (US4)
#
# T038: Verification script for V003 test case:
#   (if (> 10 5) 'greater 'less) → GREATER
#
# Usage:
#   ./scripts/verify-control-flow.sh
#   ./scripts/verify-control-flow.sh V003
#
# Exit codes:
#   0  - Test passed
#   1  - Test failed
#   77 - Test skipped (known limitation)
#   2  - Prerequisites missing

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
STAGE0_PATH="$PROJECT_ROOT/dist/clysm-stage0.wasm"
HOST_SHIM="$PROJECT_ROOT/host-shim/verify-stage0.js"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=========================================="
echo "Stage 0 Control Flow Verification (US4)"
echo "=========================================="

# Check prerequisites
check_prerequisites() {
    local missing=0

    if [ ! -f "$STAGE0_PATH" ]; then
        echo -e "${RED}ERROR:${NC} Stage 0 binary not found: $STAGE0_PATH"
        echo "       Run: sbcl --load build/bootstrap.lisp"
        missing=1
    fi

    if ! command -v node &> /dev/null; then
        echo -e "${RED}ERROR:${NC} Node.js not found"
        missing=1
    fi

    if [ $missing -eq 1 ]; then
        exit 2
    fi
}

# Validate Stage 0 binary
validate_stage0() {
    echo ""
    echo "Validating Stage 0 binary..."

    if command -v wasm-tools &> /dev/null; then
        if wasm-tools validate "$STAGE0_PATH" 2>/dev/null; then
            echo -e "${GREEN}✓${NC} Stage 0 validates with wasm-tools"
        else
            echo -e "${RED}✗${NC} Stage 0 validation failed"
            exit 1
        fi
    else
        echo -e "${YELLOW}⚠${NC} Skipping wasm-tools validation (not installed)"
    fi
}

# Run verification test
run_test() {
    local test_id="${1:-V003}"

    echo ""
    echo "Running test: $test_id"
    echo ""

    if [ -f "$HOST_SHIM" ]; then
        node "$HOST_SHIM" "$test_id"
        return $?
    else
        echo -e "${YELLOW}WARNING:${NC} Host shim not found: $HOST_SHIM"
        echo ""
        echo "KNOWN LIMITATION:"
        echo "Stage 0 currently compiles only 14/849 forms (1.6%)."
        echo ""
        echo -e "${YELLOW}Result: SKIP${NC}"
        exit 77
    fi
}

# Main
main() {
    check_prerequisites
    validate_stage0
    run_test "${1:-V003}"
}

main "$@"
