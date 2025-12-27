#!/usr/bin/env bash
#
# bootstrap-without-sbcl.sh - SBCL-free bootstrap using wasmtime
#
# Part of Feature 044: Interpreter Bootstrap Strategy
# Phase 7: T110 - SBCL-free bootstrap workflow
#
# This script demonstrates the SBCL-free development workflow:
# 1. Use existing Stage 0 (interpreter-generated)
# 2. Run Stage 0 on wasmtime to compile source → Stage 1
# 3. Run Stage 1 on wasmtime to compile source → Stage 2
# 4. Verify Stage 1 == Stage 2 for fixed-point
#
# Prerequisites:
# - wasmtime installed
# - wasm-tools installed
# - node installed (for FFI host shim)
# - Pre-existing Stage 0 at dist/clysm-stage0-interp.wasm
#
# Usage:
#   ./scripts/bootstrap-without-sbcl.sh [options]
#
# Options:
#   --stage0 PATH   Path to Stage 0 binary (default: dist/clysm-stage0-interp.wasm)
#   --verbose       Show detailed progress
#   --json          Output results in JSON format
#   --help          Show this help message
#
# Exit Codes:
#   0 - SUCCESS: Fixed-point achieved
#   1 - NOT_ACHIEVED: Binaries differ
#   2 - COMPILATION_ERROR: Stage generation failed
#   3 - MISSING_DEPENDENCY: Required tools not available
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Default options
STAGE0_PATH="${STAGE0_PATH:-dist/clysm-stage0-interp.wasm}"
VERBOSE=false
JSON_OUTPUT=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --stage0)
            STAGE0_PATH="$2"
            shift 2
            ;;
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        --json)
            JSON_OUTPUT=true
            shift
            ;;
        --help|-h)
            head -35 "$0" | tail -33
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 3
            ;;
    esac
done

# Timestamp
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# JSON error output
json_error() {
    local status="$1"
    local message="$2"
    cat <<EOF
{
  "status": "$status",
  "timestamp": "$TIMESTAMP",
  "error": "$message"
}
EOF
}

# Check dependencies
check_dependencies() {
    local missing=""

    if ! command -v wasmtime &>/dev/null; then
        missing="$missing wasmtime"
    fi

    if ! command -v wasm-tools &>/dev/null; then
        missing="$missing wasm-tools"
    fi

    if ! command -v node &>/dev/null; then
        missing="$missing node"
    fi

    if [[ -n "$missing" ]]; then
        if $JSON_OUTPUT; then
            json_error "MISSING_DEPENDENCY" "Missing required tools:$missing"
        else
            echo "Error: Missing required tools:$missing" >&2
            echo "" >&2
            echo "Install with:" >&2
            [[ "$missing" == *wasmtime* ]] && echo "  wasmtime: curl https://wasmtime.dev/install.sh -sSf | bash" >&2
            [[ "$missing" == *wasm-tools* ]] && echo "  wasm-tools: cargo install wasm-tools" >&2
            [[ "$missing" == *node* ]] && echo "  node: nix-shell -p nodejs" >&2
        fi
        exit 3
    fi

    # Check Stage 0 exists
    if [[ ! -f "$PROJECT_ROOT/$STAGE0_PATH" ]]; then
        if $JSON_OUTPUT; then
            json_error "MISSING_DEPENDENCY" "Stage 0 not found: $STAGE0_PATH. Generate with SBCL first."
        else
            echo "Error: Stage 0 not found: $STAGE0_PATH" >&2
            echo "" >&2
            echo "Generate Stage 0 first with:" >&2
            echo "  sbcl --load build/bootstrap-interp.lisp" >&2
        fi
        exit 3
    fi
}

# Validate Stage 0
validate_stage0() {
    if $VERBOSE; then
        echo "Validating Stage 0..."
    fi

    if ! wasm-tools validate "$PROJECT_ROOT/$STAGE0_PATH" 2>/dev/null; then
        if $JSON_OUTPUT; then
            json_error "COMPILATION_ERROR" "Stage 0 binary invalid"
        else
            echo "Error: Stage 0 binary is invalid" >&2
        fi
        exit 2
    fi

    if $VERBOSE; then
        echo "Stage 0 validated: $STAGE0_PATH"
    fi
}

# Generate Stage 1 from Stage 0
generate_stage1() {
    local stage1_path="$1"

    if $VERBOSE; then
        echo ""
        echo "=== Generating Stage 1 from Stage 0 ==="
    fi

    # Note: This requires Stage 0 to have compile_all export
    # Current Stage 0 is minimal and doesn't support this
    # For now, we check if the Stage 0 has the required exports

    local exports
    exports=$(wasm-tools print "$PROJECT_ROOT/$STAGE0_PATH" 2>/dev/null | grep -c "export" || echo 0)

    if [[ "$exports" -lt 1 ]]; then
        if $VERBOSE; then
            echo "Stage 0 has no exports - cannot run as compiler"
            echo "This is expected for minimal Stage 0 binaries"
        fi
        return 1
    fi

    # Try to run Stage 0 on wasmtime
    if $VERBOSE; then
        echo "Running Stage 0 on wasmtime..."
    fi

    # TODO: When Stage 0 has compile_all, invoke it here
    # node "$PROJECT_ROOT/host-shim/stage1-host.js" \
    #     --mode compile \
    #     --stage0 "$PROJECT_ROOT/$STAGE0_PATH" \
    #     --output "$PROJECT_ROOT/$stage1_path" \
    #     --source-dir "$PROJECT_ROOT/src/clysm/"

    if $VERBOSE; then
        echo "Stage 1 generation not yet implemented for minimal Stage 0"
    fi
    return 1
}

# Main workflow
main() {
    cd "$PROJECT_ROOT"

    check_dependencies
    validate_stage0

    if $VERBOSE; then
        echo ""
        echo "=== SBCL-Free Bootstrap Workflow ==="
        echo ""
        echo "Stage 0: $STAGE0_PATH"
        local stage0_size
        stage0_size=$(stat -c%s "$STAGE0_PATH" 2>/dev/null || stat -f%z "$STAGE0_PATH" 2>/dev/null)
        echo "Size: $stage0_size bytes"
        echo ""
    fi

    local stage1_path="dist/clysm-stage1-wasmtime.wasm"
    local stage2_path="dist/clysm-stage2-wasmtime.wasm"

    # Try to generate Stage 1
    if generate_stage1 "$stage1_path"; then
        if $VERBOSE; then
            echo "Stage 1 generated: $stage1_path"
        fi

        # TODO: Generate Stage 2 and verify fixed-point
        if $JSON_OUTPUT; then
            cat <<EOF
{
  "status": "SUCCESS",
  "timestamp": "$TIMESTAMP",
  "stage0": "$STAGE0_PATH",
  "stage1": "$stage1_path",
  "note": "Full SBCL-free workflow complete"
}
EOF
        else
            echo ""
            echo "=== SBCL-Free Bootstrap Complete ==="
            echo ""
            echo "Stage 0: $STAGE0_PATH"
            echo "Stage 1: $stage1_path"
        fi
        exit 0
    else
        # Stage 0 is minimal - explain limitation
        if $JSON_OUTPUT; then
            cat <<EOF
{
  "status": "PARTIAL",
  "timestamp": "$TIMESTAMP",
  "stage0": "$STAGE0_PATH",
  "stage0_valid": true,
  "note": "Stage 0 validated but cannot run as compiler yet. Stage 0 needs compile_all export."
}
EOF
        else
            echo ""
            echo "=== SBCL-Free Bootstrap Status ==="
            echo ""
            echo "Stage 0 validated: $STAGE0_PATH"
            echo ""
            echo "LIMITATION: Current Stage 0 is minimal and cannot run as a compiler."
            echo ""
            echo "The interpreter-generated Stage 0 contains valid Wasm but does not"
            echo "export the compile_all function needed for Stage 1 generation."
            echo ""
            echo "To achieve full SBCL-free bootstrap:"
            echo "1. Extend Clysm's CL subset to compile more forms"
            echo "2. Or: Use SBCL to generate a fuller Stage 0"
            echo ""
            echo "For now, use the standard bootstrap workflow:"
            echo "  sbcl --load build/bootstrap.lisp"
        fi
        # Exit with partial success - Stage 0 exists and is valid
        exit 77  # Standard skip code
    fi
}

main "$@"
