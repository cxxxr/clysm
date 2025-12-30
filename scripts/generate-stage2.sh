#!/usr/bin/env bash
# generate-stage2.sh - Generate Stage 2 using Stage 1 compiler
# Phase 13D-8: Stage 1 Runtime Environment (001-stage1-runtime)
#
# Usage:
#   ./scripts/generate-stage2.sh [options]
#
# Options:
#   --help, -h        Show help message
#   --verbose, -v     Enable verbose logging
#   --stage1 <path>   Path to Stage 1 Wasm (default: dist/clysm-stage1.wasm)
#   --output <path>   Output path for Stage 2 (default: dist/clysm-stage2.wasm)
#   --report <path>   Path for JSON report (default: dist/stage2-report.json)
#   --source-dir <path>  Source directory (default: src/clysm/)
#   --json            Output progress as JSON (machine-readable)
#
# Exit Codes:
#   0  - Success
#   1  - Partial success (some modules failed)
#   2  - Failure
#   3  - Missing dependency
#   77 - Known limitation (compile_all not exported)

set -euo pipefail

# Resolve script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Default paths
STAGE1="$PROJECT_ROOT/dist/clysm-stage1.wasm"
OUTPUT="$PROJECT_ROOT/dist/clysm-stage2.wasm"
REPORT="$PROJECT_ROOT/dist/stage2-report.json"
SOURCE_DIR="$PROJECT_ROOT/src/clysm"
RUNNER="$PROJECT_ROOT/host-shim/stage1-runner.js"

# Options
VERBOSE=false
JSON_OUTPUT=false

# Logging functions
log_info() {
    if [[ "$JSON_OUTPUT" == "false" ]]; then
        echo "[INFO] $1" >&2
    fi
}

log_verbose() {
    if [[ "$VERBOSE" == "true" && "$JSON_OUTPUT" == "false" ]]; then
        echo "[VERBOSE] $1" >&2
    fi
}

log_error() {
    if [[ "$JSON_OUTPUT" == "false" ]]; then
        echo "[ERROR] $1" >&2
    fi
}

log_limitation() {
    if [[ "$JSON_OUTPUT" == "false" ]]; then
        echo "[KNOWN LIMITATION] $1" >&2
    fi
}

show_help() {
    cat <<EOF
Stage 2 Generation Script - Clysm Compiler Bootstrap

Usage:
  $0 [options]

Options:
  --help, -h         Show this help message
  --verbose, -v      Enable verbose logging
  --stage1 <path>    Path to Stage 1 Wasm (default: dist/clysm-stage1.wasm)
  --output <path>    Output path for Stage 2 (default: dist/clysm-stage2.wasm)
  --report <path>    Path for JSON report (default: dist/stage2-report.json)
  --source-dir <path>  Source directory (default: src/clysm/)
  --json             Output progress as JSON (machine-readable)

Exit Codes:
  0  - Success (Stage 2 generated)
  1  - Partial success (some modules failed)
  2  - Failure (critical error)
  3  - Missing dependency (Stage 1 not found)
  77 - Known limitation (compile_all not exported)

Examples:
  $0                           # Generate with defaults
  $0 --verbose                 # Generate with verbose output
  $0 --json                    # Machine-readable progress
  $0 --output ./my-stage2.wasm # Custom output path

EOF
}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --help|-h)
            show_help
            exit 0
            ;;
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        --json)
            JSON_OUTPUT=true
            shift
            ;;
        --stage1)
            if [[ -z "${2:-}" ]]; then
                log_error "--stage1 requires an argument"
                exit 2
            fi
            STAGE1="$2"
            shift 2
            ;;
        --output)
            if [[ -z "${2:-}" ]]; then
                log_error "--output requires an argument"
                exit 2
            fi
            OUTPUT="$2"
            shift 2
            ;;
        --report)
            if [[ -z "${2:-}" ]]; then
                log_error "--report requires an argument"
                exit 2
            fi
            REPORT="$2"
            shift 2
            ;;
        --source-dir)
            if [[ -z "${2:-}" ]]; then
                log_error "--source-dir requires an argument"
                exit 2
            fi
            SOURCE_DIR="$2"
            shift 2
            ;;
        -*)
            log_error "Unknown option: $1"
            exit 2
            ;;
        *)
            log_error "Unexpected argument: $1"
            exit 2
            ;;
    esac
done

# Check prerequisites
log_verbose "Checking prerequisites..."

if [[ ! -f "$RUNNER" ]]; then
    log_error "Stage 1 runner not found: $RUNNER"
    exit 3
fi

if [[ ! -f "$STAGE1" ]]; then
    log_error "Stage 1 binary not found: $STAGE1"
    exit 3
fi

if [[ ! -d "$SOURCE_DIR" ]]; then
    log_error "Source directory not found: $SOURCE_DIR"
    exit 3
fi

log_info "Stage 2 Generation starting..."
log_verbose "Stage 1: $STAGE1"
log_verbose "Output: $OUTPUT"
log_verbose "Report: $REPORT"
log_verbose "Source: $SOURCE_DIR"

# Check if Stage 1 exports compile_all
log_verbose "Checking Stage 1 exports..."
exports=$(wasm-tools print "$STAGE1" 2>/dev/null | grep "(export" | tr -d '()"' || true)

has_compile_all=false
has_compile_form=false

if echo "$exports" | grep -q "compile_all"; then
    has_compile_all=true
    log_verbose "Stage 1 exports compile_all"
fi

if echo "$exports" | grep -q "compile_form"; then
    has_compile_form=true
    log_verbose "Stage 1 exports compile_form"
fi

# Generate initial report
timestamp=$(date -Iseconds)
generate_report() {
    local status="$1"
    local message="$2"
    local compiled="${3:-0}"
    local failed="${4:-0}"
    local total="${5:-0}"

    cat > "$REPORT" <<EOF
{
  "timestamp": "$timestamp",
  "stage_version": "stage1-v0.1",
  "status": "$status",
  "message": "$message",
  "summary": {
    "total_forms": $total,
    "compiled": $compiled,
    "failed": $failed,
    "skipped": 0,
    "coverage_pct": $(if [[ $total -gt 0 ]]; then echo "scale=2; $compiled * 100 / $total" | bc; else echo "0"; fi)
  },
  "exports_available": {
    "compile_all": $has_compile_all,
    "compile_form": $has_compile_form
  },
  "source_dir": "$SOURCE_DIR",
  "stage1_path": "$STAGE1",
  "output_path": "$OUTPUT"
}
EOF
}

# Handle case where compile_all is not exported
if [[ "$has_compile_all" == "false" && "$has_compile_form" == "false" ]]; then
    log_limitation "Stage 1 does not export compile_all or compile_form."
    log_info "Current Stage 1 has limited compilation capability (14.1% rate)."
    log_info "Stage 2 generation requires compile_all to be exported."
    log_info ""
    log_info "To improve compilation rate:"
    log_info "  1. Implement missing special forms (defstruct, loop extensions)"
    log_info "  2. Add support for missing macros"
    log_info "  3. Re-generate Stage 1 with improved compiler"

    generate_report "skipped" "compile_all not exported" 0 0 0

    if [[ "$JSON_OUTPUT" == "true" ]]; then
        cat "$REPORT"
    fi

    exit 77
fi

# If we have compile_all, attempt actual compilation
if [[ "$has_compile_all" == "true" ]]; then
    log_info "Using compile_all to generate Stage 2..."

    # Count source files
    source_files=$(find "$SOURCE_DIR" -name "*.lisp" -type f | wc -l)
    log_verbose "Found $source_files source files"

    # This would invoke the actual compilation via node
    # For now, we create a placeholder since compile_all is not actually available
    log_error "compile_all invocation not yet implemented"
    generate_report "error" "compile_all invocation not implemented" 0 0 "$source_files"
    exit 2
fi

# If we only have compile_form, try form-by-form compilation
if [[ "$has_compile_form" == "true" ]]; then
    log_info "Using compile_form for form-by-form compilation..."
    log_limitation "This mode is experimental and may produce incomplete output."

    # Count forms (very rough estimate)
    source_files=$(find "$SOURCE_DIR" -name "*.lisp" -type f | wc -l)

    generate_report "partial" "compile_form available but full compilation not implemented" 0 0 "$source_files"

    if [[ "$JSON_OUTPUT" == "true" ]]; then
        cat "$REPORT"
    fi

    exit 1
fi

log_error "Unexpected state: no compilation exports available"
exit 2
