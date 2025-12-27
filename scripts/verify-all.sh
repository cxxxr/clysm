#!/usr/bin/env bash
#
# verify-all.sh - Run all Stage 0 verification tests
#
# T040: Combined verification script for V001-V003 tests
#
# Usage:
#   ./scripts/verify-all.sh
#
# Exit codes:
#   0  - All tests passed
#   1  - One or more tests failed
#   77 - All tests skipped (known limitation)
#   2  - Prerequisites missing

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "============================================================"
echo "       Stage 0 Complete Verification Suite"
echo "============================================================"
echo ""
echo "Running all verification tests: V001, V002, V003"
echo ""

# Track results
PASSED=0
FAILED=0
SKIPPED=0

# Run a test and track result
run_test() {
    local script="$1"
    local test_id="$2"
    local name="$3"

    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Test: $name ($test_id)${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

    if [ -x "$SCRIPT_DIR/$script" ]; then
        set +e
        "$SCRIPT_DIR/$script" "$test_id" 2>&1
        local exit_code=$?
        set -e

        case $exit_code in
            0)
                echo -e "${GREEN}✓ PASSED${NC}"
                PASSED=$((PASSED + 1))
                ;;
            77)
                echo -e "${YELLOW}⚠ SKIPPED${NC}"
                SKIPPED=$((SKIPPED + 1))
                ;;
            *)
                echo -e "${RED}✗ FAILED (exit code: $exit_code)${NC}"
                FAILED=$((FAILED + 1))
                ;;
        esac
    else
        echo -e "${RED}✗ Script not found or not executable: $script${NC}"
        FAILED=$((FAILED + 1))
    fi
    echo ""
}

# Check prerequisites first
check_prerequisites() {
    echo "Checking prerequisites..."
    echo ""

    local missing=0

    # Stage 0 binary
    if [ -f "$PROJECT_ROOT/dist/clysm-stage0.wasm" ]; then
        echo -e "${GREEN}✓${NC} Stage 0 binary exists"
    else
        echo -e "${RED}✗${NC} Stage 0 binary not found"
        echo "  Run: sbcl --load build/bootstrap.lisp"
        missing=1
    fi

    # wasm-tools
    if command -v wasm-tools &> /dev/null; then
        echo -e "${GREEN}✓${NC} wasm-tools installed"
    else
        echo -e "${YELLOW}⚠${NC} wasm-tools not found (optional)"
    fi

    # Node.js
    if command -v node &> /dev/null; then
        echo -e "${GREEN}✓${NC} Node.js installed"
    else
        echo -e "${RED}✗${NC} Node.js not found"
        missing=1
    fi

    # wasmtime
    if command -v wasmtime &> /dev/null; then
        echo -e "${GREEN}✓${NC} wasmtime installed"
    else
        echo -e "${YELLOW}⚠${NC} wasmtime not found (optional)"
    fi

    echo ""

    if [ $missing -eq 1 ]; then
        echo -e "${RED}Prerequisites missing. Cannot proceed.${NC}"
        exit 2
    fi
}

# Main
main() {
    check_prerequisites

    # Run all tests
    run_test "verify-arithmetic.sh" "V001" "Arithmetic Compilation"
    run_test "verify-defun.sh" "V002" "Function Definition"
    run_test "verify-control-flow.sh" "V003" "Control Flow"

    # Summary
    echo "============================================================"
    echo "                      Summary"
    echo "============================================================"
    echo ""
    echo -e "  ${GREEN}Passed:${NC}  $PASSED"
    echo -e "  ${YELLOW}Skipped:${NC} $SKIPPED"
    echo -e "  ${RED}Failed:${NC}  $FAILED"
    echo ""

    # Determine exit code
    if [ $FAILED -gt 0 ]; then
        echo -e "${RED}Some tests failed.${NC}"
        exit 1
    elif [ $PASSED -eq 0 ] && [ $SKIPPED -gt 0 ]; then
        echo -e "${YELLOW}All tests skipped due to known limitations.${NC}"
        echo ""
        echo "The Stage 0 binary is valid but cannot function as a compiler"
        echo "until Clysm supports additional CL features:"
        echo "  - defstruct"
        echo "  - declare"
        echo "  - format"
        echo "  - define-condition"
        echo ""
        echo "This is expected behavior. See tasks.md for details."
        exit 77
    else
        echo -e "${GREEN}All tests passed!${NC}"
        exit 0
    fi
}

main "$@"
