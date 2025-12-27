#!/usr/bin/env bash
# run-stage1-gen.sh - Generate Stage 1 binary from Stage 0 self-compilation
#
# Part of Feature 039: Stage 1 Compiler Generation
#
# Prerequisites:
#   - dist/clysm-stage0.wasm exists and validates
#   - wasmtime available in PATH
#   - Node.js 18+ available
#
# Output:
#   - dist/clysm-stage1.wasm: Generated Stage 1 binary
#   - dist/stage1-report.json: Compilation progress report
#
# Exit codes:
#   0 - Success (Stage 1 generated and validated)
#   1 - Partial success (Stage 1 generated but with errors)
#   2 - Failure (Stage 1 could not be generated)
#   77 - Skip (prerequisites not met)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

STAGE0_PATH="${PROJECT_ROOT}/dist/clysm-stage0.wasm"
STAGE1_PATH="${PROJECT_ROOT}/dist/clysm-stage1.wasm"
REPORT_PATH="${PROJECT_ROOT}/dist/stage1-report.json"
HOST_SHIM="${PROJECT_ROOT}/host-shim/stage1-host.js"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
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

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."

    if [[ ! -f "$STAGE0_PATH" ]]; then
        log_error "Stage 0 binary not found: $STAGE0_PATH"
        log_info "Run: sbcl --load build/bootstrap.lisp"
        exit 77
    fi

    if ! command -v wasmtime &> /dev/null; then
        log_error "wasmtime not found in PATH"
        log_info "Run: nix develop"
        exit 77
    fi

    if ! command -v node &> /dev/null; then
        log_error "Node.js not found in PATH"
        exit 77
    fi

    NODE_VERSION=$(node --version | cut -d. -f1 | tr -d 'v')
    if [[ "$NODE_VERSION" -lt 18 ]]; then
        log_error "Node.js 18+ required, found: $(node --version)"
        exit 77
    fi

    if [[ ! -f "$HOST_SHIM" ]]; then
        log_error "Stage 1 host shim not found: $HOST_SHIM"
        exit 77
    fi

    log_info "Prerequisites OK"
}

# Validate Stage 0 binary
validate_stage0() {
    log_info "Validating Stage 0 binary..."

    if ! wasm-tools validate "$STAGE0_PATH" 2>/dev/null; then
        log_error "Stage 0 binary failed validation"
        exit 2
    fi

    STAGE0_SIZE=$(stat -c%s "$STAGE0_PATH" 2>/dev/null || stat -f%z "$STAGE0_PATH")
    log_info "Stage 0 size: $STAGE0_SIZE bytes"
}

# Run Stage 1 generation
run_generation() {
    log_info "Starting Stage 1 generation..."

    # Run Stage 0 via Node.js host shim
    if node "$HOST_SHIM" --stage0 "$STAGE0_PATH" --output "$STAGE1_PATH" --report "$REPORT_PATH"; then
        log_info "Stage 1 generation completed"
        return 0
    else
        local exit_code=$?
        case $exit_code in
            1)
                log_warn "Stage 1 generated with partial success"
                return 1
                ;;
            *)
                log_error "Stage 1 generation failed"
                return 2
                ;;
        esac
    fi
}

# Validate Stage 1 binary
validate_stage1() {
    if [[ ! -f "$STAGE1_PATH" ]]; then
        log_error "Stage 1 binary not generated"
        return 2
    fi

    log_info "Validating Stage 1 binary..."

    if ! wasm-tools validate "$STAGE1_PATH" 2>/dev/null; then
        log_error "Stage 1 binary failed validation"
        return 2
    fi

    STAGE1_SIZE=$(stat -c%s "$STAGE1_PATH" 2>/dev/null || stat -f%z "$STAGE1_PATH")
    log_info "Stage 1 size: $STAGE1_SIZE bytes"
    log_info "Stage 1 validated successfully"
    return 0
}

# Display summary
display_summary() {
    if [[ -f "$REPORT_PATH" ]]; then
        log_info "Progress Report:"
        if command -v jq &> /dev/null; then
            jq '.summary' "$REPORT_PATH"
        else
            cat "$REPORT_PATH"
        fi
    fi
}

# Main
main() {
    check_prerequisites
    validate_stage0

    local gen_result=0
    run_generation || gen_result=$?

    display_summary

    if [[ $gen_result -eq 0 ]]; then
        validate_stage1 || exit $?
        log_info "Stage 1 generation successful!"
        exit 0
    elif [[ $gen_result -eq 1 ]]; then
        validate_stage1 || exit 2
        log_warn "Stage 1 generated with partial coverage"
        exit 1
    else
        log_error "Stage 1 generation failed"
        exit 2
    fi
}

main "$@"
