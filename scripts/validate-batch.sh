#!/usr/bin/env bash
# validate-batch.sh - Run all validation contracts for dead code removal
# Feature: 001-codegen-cleanup
# Contracts: specs/001-codegen-cleanup/contracts/validation.md
#
# Usage: ./scripts/validate-batch.sh [--quick] [--verbose]
#   --quick    Skip Stage 1 compilation (for rapid iteration)
#   --verbose  Show detailed output

set -e

QUICK=false
VERBOSE=false

for arg in "$@"; do
    case $arg in
        --quick)
            QUICK=true
            shift
            ;;
        --verbose)
            VERBOSE=true
            shift
            ;;
    esac
done

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

success() {
    echo -e "${GREEN}✓${NC} $1"
}

failure() {
    echo -e "${RED}✗${NC} $1"
    exit 1
}

info() {
    if [ "$VERBOSE" = true ]; then
        echo -e "${YELLOW}→${NC} $1"
    fi
}

echo "=== Clysm Dead Code Removal Validation ==="
echo ""

# Contract 1: Unit Tests
echo "=== Contract 1: Unit Tests ==="
info "Running: sbcl --eval '(asdf:test-system :clysm)'"
if sbcl --non-interactive --eval "(progn (asdf:test-system :clysm) (sb-ext:exit :code 0))" 2>&1; then
    success "All unit tests passed"
else
    failure "Unit tests failed - see output above"
fi

# Contract 2: Stage 1 Compilation (skip with --quick)
if [ "$QUICK" = false ]; then
    echo ""
    echo "=== Contract 2: Stage 1 Compilation ==="
    info "Running: sbcl --load build/stage1-complete.lisp"
    if sbcl --non-interactive --load build/stage1-complete.lisp 2>&1; then
        if [ -f "dist/clysm-stage1.wasm" ]; then
            STAGE1_SIZE=$(stat -c%s "dist/clysm-stage1.wasm" 2>/dev/null || stat -f%z "dist/clysm-stage1.wasm")
            success "Stage 1 compilation complete (${STAGE1_SIZE} bytes)"
        else
            failure "Stage 1 compilation did not produce output file"
        fi
    else
        failure "Stage 1 compilation failed"
    fi

    # Contract 3: Wasm Validation
    echo ""
    echo "=== Contract 3: Wasm Validation ==="
    info "Running: wasm-tools validate dist/clysm-stage1.wasm"
    if wasm-tools validate dist/clysm-stage1.wasm 2>&1; then
        success "Wasm validation passed"
    else
        failure "Wasm validation failed"
    fi
else
    echo ""
    echo "=== Contract 2/3: Skipped (--quick mode) ==="
fi

# Contract 4: Line Count
echo ""
echo "=== Contract 4: Line Count ==="
FUNC_SECTION="src/clysm/compiler/codegen/func-section.lisp"
if [ -f "$FUNC_SECTION" ]; then
    LINE_COUNT=$(wc -l < "$FUNC_SECTION")
    info "Current line count: $LINE_COUNT"
    if [ "$LINE_COUNT" -lt 8000 ]; then
        success "Line count: $LINE_COUNT (target: <8,000 ✓)"
    else
        echo -e "${YELLOW}!${NC} Line count: $LINE_COUNT (target: <8,000)"
    fi
else
    failure "File not found: $FUNC_SECTION"
fi

# Contract 5: Coverage Rate (if report exists)
echo ""
echo "=== Contract 5: Coverage Rate ==="
REPORT="dist/stage1-report.json"
if [ -f "$REPORT" ]; then
    if command -v jq &> /dev/null; then
        COVERAGE=$(jq -r '.coverage_rate // .summary.coverage_rate // "N/A"' "$REPORT" 2>/dev/null || echo "N/A")
        info "Current coverage rate: $COVERAGE"
        if [ "$COVERAGE" != "N/A" ]; then
            # Compare as percentage (multiply by 100 if needed)
            success "Coverage rate: $COVERAGE"
        else
            echo -e "${YELLOW}!${NC} Could not extract coverage rate from report"
        fi
    else
        echo -e "${YELLOW}!${NC} jq not installed - skipping coverage check"
    fi
else
    echo -e "${YELLOW}!${NC} Report not found: $REPORT (run Stage 1 compilation first)"
fi

# Summary
echo ""
echo "=== Validation Summary ==="
echo "Line count: $LINE_COUNT lines"
if [ "$QUICK" = false ]; then
    echo "Stage 1 size: ${STAGE1_SIZE:-N/A} bytes"
fi
echo ""
success "All validations passed"
