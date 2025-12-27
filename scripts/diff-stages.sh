#!/usr/bin/env bash
# diff-stages.sh - Compare Stage 0 and Stage 1 Wasm binaries
#
# Part of Feature 039: Stage 1 Compiler Generation
#
# Usage:
#   ./scripts/diff-stages.sh [stage0.wasm] [stage1.wasm]
#
# Default paths:
#   Stage 0: dist/clysm-stage0.wasm
#   Stage 1: dist/clysm-stage1.wasm
#
# Output:
#   - Summary of size differences
#   - Export comparison
#   - Type section comparison
#   - Function count comparison
#
# Exit codes:
#   0 - Comparison successful
#   1 - Binaries differ (expected during development)
#   2 - Error (missing files, invalid binaries)
#   77 - Skip (prerequisites not met)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

STAGE0_PATH="${1:-${PROJECT_ROOT}/dist/clysm-stage0.wasm}"
STAGE1_PATH="${2:-${PROJECT_ROOT}/dist/clysm-stage1.wasm}"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_section() {
    echo -e "\n${CYAN}=== $1 ===${NC}"
}

# Check prerequisites
check_prerequisites() {
    if ! command -v wasm-tools &> /dev/null; then
        log_error "wasm-tools not found in PATH"
        log_info "Run: nix develop"
        exit 77
    fi

    if [[ ! -f "$STAGE0_PATH" ]]; then
        log_error "Stage 0 binary not found: $STAGE0_PATH"
        exit 2
    fi

    if [[ ! -f "$STAGE1_PATH" ]]; then
        log_error "Stage 1 binary not found: $STAGE1_PATH"
        log_info "Run: ./scripts/run-stage1-gen.sh"
        exit 2
    fi
}

# Compare file sizes
compare_sizes() {
    log_section "Size Comparison"

    STAGE0_SIZE=$(stat -c%s "$STAGE0_PATH" 2>/dev/null || stat -f%z "$STAGE0_PATH")
    STAGE1_SIZE=$(stat -c%s "$STAGE1_PATH" 2>/dev/null || stat -f%z "$STAGE1_PATH")

    echo "Stage 0: $STAGE0_SIZE bytes"
    echo "Stage 1: $STAGE1_SIZE bytes"

    if [[ $STAGE1_SIZE -gt $STAGE0_SIZE ]]; then
        DIFF=$((STAGE1_SIZE - STAGE0_SIZE))
        if [[ $STAGE0_SIZE -gt 0 ]]; then
            PERCENT=$((DIFF * 100 / STAGE0_SIZE))
        else
            PERCENT=0
        fi
        echo "Difference: +$DIFF bytes (+${PERCENT}%)"
    elif [[ $STAGE1_SIZE -lt $STAGE0_SIZE ]]; then
        DIFF=$((STAGE0_SIZE - STAGE1_SIZE))
        if [[ $STAGE0_SIZE -gt 0 ]]; then
            PERCENT=$((DIFF * 100 / STAGE0_SIZE))
        else
            PERCENT=0
        fi
        echo "Difference: -$DIFF bytes (-${PERCENT}%)"
    else
        echo "Difference: 0 bytes (identical size)"
    fi
}

# Compare exports
compare_exports() {
    log_section "Export Comparison"

    STAGE0_EXPORTS=$(wasm-tools print "$STAGE0_PATH" 2>/dev/null | grep -c '(export' || true)
    STAGE0_EXPORTS=${STAGE0_EXPORTS:-0}
    STAGE1_EXPORTS=$(wasm-tools print "$STAGE1_PATH" 2>/dev/null | grep -c '(export' || true)
    STAGE1_EXPORTS=${STAGE1_EXPORTS:-0}

    echo "Stage 0 exports: $STAGE0_EXPORTS"
    echo "Stage 1 exports: $STAGE1_EXPORTS"

    if [[ "$STAGE1_EXPORTS" -lt "$STAGE0_EXPORTS" ]] 2>/dev/null; then
        MISSING=$((STAGE0_EXPORTS - STAGE1_EXPORTS))
        log_warn "Stage 1 has $MISSING fewer exports"

        echo ""
        echo "Stage 0 export names:"
        wasm-tools print "$STAGE0_PATH" 2>/dev/null | grep '(export' | head -20 || true

        echo ""
        echo "Stage 1 export names:"
        wasm-tools print "$STAGE1_PATH" 2>/dev/null | grep '(export' | head -20 || true
    fi
}

# Compare types
compare_types() {
    log_section "Type Section Comparison"

    STAGE0_TYPES=$(wasm-tools print "$STAGE0_PATH" 2>/dev/null | grep -c '(type' 2>/dev/null || true)
    STAGE0_TYPES=${STAGE0_TYPES:-0}
    STAGE1_TYPES=$(wasm-tools print "$STAGE1_PATH" 2>/dev/null | grep -c '(type' 2>/dev/null || true)
    STAGE1_TYPES=${STAGE1_TYPES:-0}

    echo "Stage 0 types: $STAGE0_TYPES"
    echo "Stage 1 types: $STAGE1_TYPES"

    if [[ "$STAGE0_TYPES" -ne "$STAGE1_TYPES" ]] 2>/dev/null; then
        DIFF=$((STAGE1_TYPES - STAGE0_TYPES))
        if [[ $DIFF -gt 0 ]]; then
            echo "Stage 1 has $DIFF more types"
        else
            echo "Stage 1 has ${DIFF#-} fewer types"
        fi
    fi
}

# Compare functions
compare_functions() {
    log_section "Function Count Comparison"

    STAGE0_FUNCS=$(wasm-tools print "$STAGE0_PATH" 2>/dev/null | grep -c '(func' || true)
    STAGE0_FUNCS=${STAGE0_FUNCS:-0}
    STAGE1_FUNCS=$(wasm-tools print "$STAGE1_PATH" 2>/dev/null | grep -c '(func' || true)
    STAGE1_FUNCS=${STAGE1_FUNCS:-0}

    echo "Stage 0 functions: $STAGE0_FUNCS"
    echo "Stage 1 functions: $STAGE1_FUNCS"

    if [[ "$STAGE0_FUNCS" -ne "$STAGE1_FUNCS" ]] 2>/dev/null; then
        DIFF=$((STAGE1_FUNCS - STAGE0_FUNCS))
        if [[ $DIFF -gt 0 ]]; then
            echo "Stage 1 has $DIFF more functions"
        else
            echo "Stage 1 has ${DIFF#-} fewer functions"
        fi
    fi
}

# Compare validation status
compare_validation() {
    log_section "Validation Status"

    if wasm-tools validate "$STAGE0_PATH" 2>/dev/null; then
        echo "Stage 0: VALID"
    else
        echo "Stage 0: INVALID"
    fi

    if wasm-tools validate "$STAGE1_PATH" 2>/dev/null; then
        echo "Stage 1: VALID"
    else
        echo "Stage 1: INVALID"
    fi
}

# Generate summary
generate_summary() {
    log_section "Summary"

    local differences=0

    STAGE0_SIZE=$(stat -c%s "$STAGE0_PATH" 2>/dev/null || stat -f%z "$STAGE0_PATH")
    STAGE1_SIZE=$(stat -c%s "$STAGE1_PATH" 2>/dev/null || stat -f%z "$STAGE1_PATH")

    if [[ $STAGE0_SIZE -ne $STAGE1_SIZE ]]; then
        ((differences++))
    fi

    STAGE0_EXPORTS=$(wasm-tools print "$STAGE0_PATH" 2>/dev/null | grep -c '(export' || true)
    STAGE0_EXPORTS=${STAGE0_EXPORTS:-0}
    STAGE1_EXPORTS=$(wasm-tools print "$STAGE1_PATH" 2>/dev/null | grep -c '(export' || true)
    STAGE1_EXPORTS=${STAGE1_EXPORTS:-0}

    if [[ "$STAGE0_EXPORTS" -ne "$STAGE1_EXPORTS" ]] 2>/dev/null; then
        ((differences++)) || true
    fi

    STAGE0_FUNCS=$(wasm-tools print "$STAGE0_PATH" 2>/dev/null | grep -c '(func' || true)
    STAGE0_FUNCS=${STAGE0_FUNCS:-0}
    STAGE1_FUNCS=$(wasm-tools print "$STAGE1_PATH" 2>/dev/null | grep -c '(func' || true)
    STAGE1_FUNCS=${STAGE1_FUNCS:-0}

    if [[ "$STAGE0_FUNCS" -ne "$STAGE1_FUNCS" ]] 2>/dev/null; then
        ((differences++))
    fi

    if [[ $differences -eq 0 ]]; then
        log_info "Stage 0 and Stage 1 are structurally identical"
        return 0
    else
        log_warn "Found $differences structural differences between stages"
        return 1
    fi
}

# Main
main() {
    log_info "Comparing Stage 0 and Stage 1 binaries"
    echo "Stage 0: $STAGE0_PATH"
    echo "Stage 1: $STAGE1_PATH"

    check_prerequisites
    compare_validation
    compare_sizes
    compare_exports
    compare_types
    compare_functions
    generate_summary
}

main "$@"
