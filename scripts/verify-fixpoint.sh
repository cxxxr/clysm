#!/usr/bin/env bash
#
# verify-fixpoint.sh - Fixed-point verification for Clysm self-hosting
#
# Part of Feature 040: Fixed-Point Verification
#
# Verifies that Stage 1 compiler can reproduce itself (Stage 1 == Stage 2).
#
# Usage:
#   ./scripts/verify-fixpoint.sh [OPTIONS]
#
# Options:
#   --stage1 PATH       Path to Stage 1 binary (default: dist/clysm-stage1.wasm)
#   --stage2 PATH       Path to Stage 2 output (default: dist/clysm-stage2.wasm)
#   --source-dir PATH   Source directory (default: src/clysm/)
#   --json              Output result as JSON
#   --skip-generate     Skip Stage 2 generation, compare existing binaries
#   --history           Append result to history log
#   --help              Show this help
#
# Exit codes (FR-007):
#   0 - ACHIEVED:           Stage 1 == Stage 2 (byte-identical)
#   1 - NOT_ACHIEVED:       Binaries differ
#   2 - COMPILATION_ERROR:  Stage 2 generation failed
#   3 - MISSING_DEPENDENCY: wasmtime/wasm-tools/Stage 1 missing

set -euo pipefail

# Defaults
STAGE1_PATH="${STAGE1_PATH:-dist/clysm-stage1.wasm}"
STAGE2_PATH="${STAGE2_PATH:-dist/clysm-stage2.wasm}"
SOURCE_DIR="${SOURCE_DIR:-src/clysm/}"
JSON_MODE=false
SKIP_GENERATE=false
APPEND_HISTORY=false
USE_INTERPRETER=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --stage1)
            STAGE1_PATH="$2"
            shift 2
            ;;
        --stage2)
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
        --skip-generate)
            SKIP_GENERATE=true
            shift
            ;;
        --history)
            APPEND_HISTORY=true
            shift
            ;;
        --interpreter)
            # Delegate to interpreter-based verification (Feature 044)
            USE_INTERPRETER=true
            shift
            ;;
        --help)
            cat <<EOF
Usage: $0 [OPTIONS]

Fixed-point verification for Clysm self-hosting.
Verifies that Stage 1 compiler can reproduce itself (Stage 1 == Stage 2).

Options:
  --stage1 PATH       Path to Stage 1 binary (default: dist/clysm-stage1.wasm)
  --stage2 PATH       Path to Stage 2 output (default: dist/clysm-stage2.wasm)
  --source-dir PATH   Source directory (default: src/clysm/)
  --json              Output result as JSON
  --skip-generate     Skip Stage 2 generation, compare existing binaries
  --history           Append result to history log
  --interpreter       Use interpreter-based Stage 0 (Feature 044)
  --help              Show this help

Exit codes (FR-007):
  0 - ACHIEVED:           Stage 1 == Stage 2 (byte-identical)
  1 - NOT_ACHIEVED:       Binaries differ
  2 - COMPILATION_ERROR:  Stage 2 generation failed
  3 - MISSING_DEPENDENCY: wasmtime/wasm-tools/Stage 1 missing

Examples:
  # Full verification (generate Stage 2 + compare)
  ./scripts/verify-fixpoint.sh

  # Quick check (compare existing binaries)
  ./scripts/verify-fixpoint.sh --skip-generate

  # CI mode with JSON output
  ./scripts/verify-fixpoint.sh --json

  # Track progress over time
  ./scripts/verify-fixpoint.sh --history
EOF
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            echo "Use --help for usage information" >&2
            exit 3
            ;;
    esac
done

# Change to project root
cd "$(dirname "$0")/.."

# Timestamp for this verification run
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Get section-level diff using wasm-tools objdump (T047)
get_section_diff() {
    local file1="$1"
    local file2="$2"

    # Get sections from both files
    local sections1 sections2
    sections1=$(wasm-tools objdump "$file1" 2>/dev/null | grep -E '^Section' | head -20) || true
    sections2=$(wasm-tools objdump "$file2" 2>/dev/null | grep -E '^Section' | head -20) || true

    # Compare and output differences as JSON array
    local diff_sections=""
    if [ "$sections1" != "$sections2" ]; then
        # Find differing sections
        local only_in_1 only_in_2
        only_in_1=$(comm -23 <(echo "$sections1" | sort) <(echo "$sections2" | sort) 2>/dev/null) || true
        only_in_2=$(comm -13 <(echo "$sections1" | sort) <(echo "$sections2" | sort) 2>/dev/null) || true

        if [ -n "$only_in_1" ] || [ -n "$only_in_2" ]; then
            diff_sections="\"section_diffs\": {"
            if [ -n "$only_in_1" ]; then
                diff_sections="${diff_sections}\"only_in_stage1\": \"$(echo "$only_in_1" | head -1 | tr '\n' ' ')\""
            fi
            if [ -n "$only_in_1" ] && [ -n "$only_in_2" ]; then
                diff_sections="${diff_sections}, "
            fi
            if [ -n "$only_in_2" ]; then
                diff_sections="${diff_sections}\"only_in_stage2\": \"$(echo "$only_in_2" | head -1 | tr '\n' ' ')\""
            fi
            diff_sections="${diff_sections}}"
        fi
    fi

    echo "$diff_sections"
}

# JSON error output helper
json_error() {
    local status="$1"
    local error_type="$2"
    local message="$3"
    cat <<EOF
{
  "status": "$status",
  "timestamp": "$TIMESTAMP",
  "error": {
    "type": "$error_type",
    "message": "$message"
  }
}
EOF
}

# Check dependencies (FR-007: exit 3 for missing dependency)
check_dependencies() {
    if ! command -v wasmtime &> /dev/null; then
        if $JSON_MODE; then
            json_error "MISSING_DEPENDENCY" "wasmtime_not_found" \
                "wasmtime not found in PATH. Install with: curl https://wasmtime.dev/install.sh -sSf | bash"
        else
            echo "ERROR: wasmtime not found" >&2
            echo "Install: curl https://wasmtime.dev/install.sh -sSf | bash" >&2
        fi
        exit 3
    fi

    if ! command -v wasm-tools &> /dev/null; then
        if $JSON_MODE; then
            json_error "MISSING_DEPENDENCY" "wasm_tools_not_found" \
                "wasm-tools not found. Install via cargo or nix."
        else
            echo "ERROR: wasm-tools not found" >&2
            echo "Install: cargo install wasm-tools" >&2
        fi
        exit 3
    fi

    if ! command -v node &> /dev/null; then
        if $JSON_MODE; then
            json_error "MISSING_DEPENDENCY" "node_not_found" \
                "Node.js not found for host shim."
        else
            echo "ERROR: Node.js not found" >&2
        fi
        exit 3
    fi

    if [[ ! -f "$STAGE1_PATH" ]]; then
        if $JSON_MODE; then
            json_error "MISSING_DEPENDENCY" "stage1_not_found" \
                "Stage 1 binary not found: $STAGE1_PATH. Run: sbcl --load build/stage1-gen.lisp"
        else
            echo "ERROR: Stage 1 not found: $STAGE1_PATH" >&2
            echo "Run: sbcl --load build/stage1-gen.lisp" >&2
        fi
        exit 3
    fi
}

# Validate Stage 1 binary (FR-008)
validate_stage1() {
    if ! wasm-tools validate "$STAGE1_PATH" 2>/dev/null; then
        if $JSON_MODE; then
            json_error "MISSING_DEPENDENCY" "stage1_invalid" \
                "Stage 1 binary failed validation: $STAGE1_PATH"
        else
            echo "ERROR: Stage 1 binary invalid: $STAGE1_PATH" >&2
        fi
        exit 3
    fi
}

# Generate Stage 2
generate_stage2() {
    local start_time=$(date +%s%3N)

    # Call host shim in compile mode
    local gen_result
    gen_result=$(node host-shim/stage1-host.js \
        --mode compile \
        --stage1 "$STAGE1_PATH" \
        --output "$STAGE2_PATH" \
        --source-dir "$SOURCE_DIR" 2>&1) || true

    local end_time=$(date +%s%3N)
    local gen_time_ms=$((end_time - start_time))

    # Return time
    echo "$gen_time_ms"
}

# Compare binaries
compare_binaries() {
    local start_time=$(date +%s%3N)

    # Compare using cmp
    if cmp -s "$STAGE1_PATH" "$STAGE2_PATH"; then
        echo "identical"
    else
        # Find first difference
        local diff_info
        diff_info=$(cmp "$STAGE1_PATH" "$STAGE2_PATH" 2>&1) || true
        echo "different:$diff_info"
    fi

    local end_time=$(date +%s%3N)
    local cmp_time_ms=$((end_time - start_time))
    echo "time:$cmp_time_ms"
}

# Main verification
main() {
    # Delegate to interpreter-based verification if requested (Feature 044)
    if $USE_INTERPRETER; then
        local interp_args=""
        if $JSON_MODE; then interp_args="$interp_args --json"; fi
        if $SKIP_GENERATE; then interp_args="$interp_args --skip-generate"; fi
        exec "$(dirname "$0")/verify-fixpoint-interp.sh" $interp_args
    fi

    check_dependencies
    validate_stage1

    local stage1_size
    stage1_size=$(stat -f%z "$STAGE1_PATH" 2>/dev/null || stat -c%s "$STAGE1_PATH" 2>/dev/null)

    local gen_time_ms=0

    # Generate Stage 2 unless skipping
    if ! $SKIP_GENERATE; then
        if ! $JSON_MODE; then
            echo "=== Fixed-Point Verification ==="
            echo ""
            echo "Stage 1: $STAGE1_PATH ($stage1_size bytes, valid)"
            echo "Generating Stage 2..."
        fi
        gen_time_ms=$(generate_stage2)
    fi

    # Check Stage 2 exists
    if [[ ! -f "$STAGE2_PATH" ]]; then
        if $JSON_MODE; then
            cat <<EOF
{
  "status": "COMPILATION_ERROR",
  "timestamp": "$TIMESTAMP",
  "stage1": {
    "path": "$STAGE1_PATH",
    "size_bytes": $stage1_size,
    "valid": true
  },
  "error": {
    "type": "stage2_generation_failed",
    "message": "Stage 2 binary not generated"
  }
}
EOF
        else
            echo ""
            echo "Result: COMPILATION ERROR"
            echo "Stage 2 generation failed - no output file"
        fi
        exit 2
    fi

    local stage2_size
    stage2_size=$(stat -f%z "$STAGE2_PATH" 2>/dev/null || stat -c%s "$STAGE2_PATH" 2>/dev/null)

    # Validate Stage 2 (T050)
    local stage2_valid=true
    if ! wasm-tools validate "$STAGE2_PATH" 2>/dev/null; then
        stage2_valid=false
    fi

    # Compare binaries
    local cmp_start=$(date +%s%3N)
    local identical=false
    local first_diff_offset=""
    local diff_bytes=0

    if cmp -s "$STAGE1_PATH" "$STAGE2_PATH"; then
        identical=true
    else
        # Get first difference offset using cmp (T045-T046)
        local cmp_output
        cmp_output=$(cmp "$STAGE1_PATH" "$STAGE2_PATH" 2>&1) || true

        # Extract byte offset from "file1 file2 differ: byte N, line M" format
        first_diff_offset=$(echo "$cmp_output" | grep -oE 'byte [0-9]+' | grep -oE '[0-9]+' | head -1) || true

        # Count differing bytes (T045)
        # Note: cmp -l stops at shorter file, so also account for size difference
        # Use subshell without pipefail since cmp -l returns non-zero when files differ
        local cmp_diff_count
        cmp_diff_count=$(set +o pipefail; cmp -l "$STAGE1_PATH" "$STAGE2_PATH" 2>/dev/null | wc -l | tr -d '[:space:]')
        cmp_diff_count=${cmp_diff_count:-0}

        # Account for size mismatch (T044)
        local size_diff=0
        if [ "$stage1_size" -gt "$stage2_size" ]; then
            size_diff=$((stage1_size - stage2_size))
        elif [ "$stage2_size" -gt "$stage1_size" ]; then
            size_diff=$((stage2_size - stage1_size))
        fi
        diff_bytes=$((cmp_diff_count + size_diff))

        # Ensure we have at least 1 diff byte if files differ
        if [ "$diff_bytes" -eq 0 ]; then
            diff_bytes=1
        fi

        # Ensure first_diff_offset has a valid value (T050)
        if [ -z "$first_diff_offset" ]; then
            first_diff_offset=1
        fi
    fi

    local cmp_end=$(date +%s%3N)
    local cmp_time_ms=$((cmp_end - cmp_start))

    # Output result
    if $identical; then
        if $JSON_MODE; then
            cat <<EOF
{
  "status": "ACHIEVED",
  "timestamp": "$TIMESTAMP",
  "stage1": {
    "path": "$STAGE1_PATH",
    "size_bytes": $stage1_size,
    "valid": true
  },
  "stage2": {
    "path": "$STAGE2_PATH",
    "size_bytes": $stage2_size,
    "valid": $stage2_valid
  },
  "comparison": {
    "identical": true,
    "first_diff_offset": null,
    "diff_bytes": 0
  },
  "timing": {
    "stage2_generation_ms": $gen_time_ms,
    "comparison_ms": $cmp_time_ms,
    "total_ms": $((gen_time_ms + cmp_time_ms))
  }
}
EOF
        else
            echo "Stage 2: $STAGE2_PATH ($stage2_size bytes, valid)"
            echo ""
            echo "Time: Stage 2 generation ${gen_time_ms}ms, comparison ${cmp_time_ms}ms"
            echo ""
            echo "Result: FIXED-POINT ACHIEVED"
            echo ""
            echo "Binaries are byte-identical. Self-hosting verified."
        fi

        # Append to history if requested
        if $APPEND_HISTORY; then
            append_history "ACHIEVED" 0 "$stage1_size" "$stage2_size"
        fi

        exit 0
    else
        # Get section-level diff (T047)
        local section_diffs
        section_diffs=$(get_section_diff "$STAGE1_PATH" "$STAGE2_PATH")

        local size_delta=$((stage2_size - stage1_size))

        if $JSON_MODE; then
            cat <<EOF
{
  "status": "NOT_ACHIEVED",
  "timestamp": "$TIMESTAMP",
  "stage1": {
    "path": "$STAGE1_PATH",
    "size_bytes": $stage1_size,
    "valid": true
  },
  "stage2": {
    "path": "$STAGE2_PATH",
    "size_bytes": $stage2_size,
    "valid": $stage2_valid
  },
  "comparison": {
    "identical": false,
    "first_diff_offset": $first_diff_offset,
    "diff_bytes": $diff_bytes,
    "size_delta": $size_delta
  },
  "timing": {
    "stage2_generation_ms": $gen_time_ms,
    "comparison_ms": $cmp_time_ms,
    "total_ms": $((gen_time_ms + cmp_time_ms))
  }
}
EOF
        else
            local validity_text="valid"
            if [ "$stage2_valid" = "false" ]; then
                validity_text="INVALID"
            fi
            echo "Stage 2: $STAGE2_PATH ($stage2_size bytes, $validity_text)"
            echo ""
            echo "Time: Stage 2 generation ${gen_time_ms}ms, comparison ${cmp_time_ms}ms"
            echo ""
            echo "Result: FIXED-POINT NOT ACHIEVED"
            echo ""
            echo "First difference at byte offset: $first_diff_offset"
            echo "Total differing bytes: $diff_bytes"
            echo "Size difference: ${size_delta} bytes"
            echo ""
            echo "Run with --json for detailed diff report."
        fi

        # Append to history if requested
        if $APPEND_HISTORY; then
            append_history "NOT_ACHIEVED" "$diff_bytes" "$stage1_size" "$stage2_size"
        fi

        exit 1
    fi
}

# Append to verification history (US5)
append_history() {
    local status="$1"
    local diff_bytes="$2"
    local stage1_size="$3"
    local stage2_size="$4"
    local history_file="dist/verification-history.jsonl"
    local git_commit
    git_commit=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")

    mkdir -p "$(dirname "$history_file")"

    cat >> "$history_file" <<EOF
{"timestamp":"$TIMESTAMP","status":"$status","diff_bytes":$diff_bytes,"stage1_size":$stage1_size,"stage2_size":$stage2_size,"git_commit":"$git_commit"}
EOF
}

main
