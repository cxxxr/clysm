#!/usr/bin/env bash
#
# verify-arithmetic.sh - Verify Stage 0 arithmetic compilation (US2)
#
# T030: Verification script for V001 test case: (+ 1 2) → 3
#
# Usage:
#   ./scripts/verify-arithmetic.sh
#   ./scripts/verify-arithmetic.sh V001
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
echo "Stage 0 Arithmetic Verification (US2)"
echo "=========================================="

# Check prerequisites
check_prerequisites() {
    local missing=0

    if [ ! -f "$STAGE0_PATH" ]; then
        echo -e "${RED}ERROR:${NC} Stage 0 binary not found: $STAGE0_PATH"
        echo "       Run: sbcl --load build/bootstrap.lisp"
        missing=1
    fi

    if ! command -v wasm-tools &> /dev/null; then
        echo -e "${YELLOW}WARNING:${NC} wasm-tools not found"
        echo "         Install: cargo install wasm-tools"
    fi

    if ! command -v node &> /dev/null; then
        echo -e "${RED}ERROR:${NC} Node.js not found"
        echo "       Required for host shim verification"
        missing=1
    fi

    if ! command -v wasmtime &> /dev/null; then
        echo -e "${YELLOW}WARNING:${NC} wasmtime not found"
        echo "         Install: curl https://wasmtime.dev/install.sh -sSf | bash"
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
    local test_id="${1:-V001}"

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
        echo "The compile export is not functional until Clysm supports"
        echo "additional CL features (defstruct, declare, format, etc.)."
        echo ""
        echo -e "${YELLOW}Result: SKIP${NC}"
        exit 77
    fi
}

# Main
main() {
    check_prerequisites
    validate_stage0
    run_test "${1:-V001}"
}

main "$@"
