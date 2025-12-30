#!/usr/bin/env bash
# test-helpers.sh - Common test assertions for Stage 1 runtime tests
# Source this file in test scripts: source "$(dirname "$0")/test-helpers.sh"

set -euo pipefail

# Colors for output (if terminal supports it)
if [[ -t 1 ]]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[0;33m'
    NC='\033[0m' # No Color
else
    RED=''
    GREEN=''
    YELLOW=''
    NC=''
fi

# Project root directory (use BASH_SOURCE for sourced files)
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Print test header
test_header() {
    echo "========================================"
    echo "TEST: $1"
    echo "========================================"
}

# Print test result
test_pass() {
    ((TESTS_PASSED++)) || true
    echo -e "${GREEN}✓ PASS${NC}: $1"
}

test_fail() {
    ((TESTS_FAILED++)) || true
    echo -e "${RED}✗ FAIL${NC}: $1"
    if [[ -n "${2:-}" ]]; then
        echo "  Expected: $2"
    fi
    if [[ -n "${3:-}" ]]; then
        echo "  Actual: $3"
    fi
}

test_skip() {
    echo -e "${YELLOW}⊘ SKIP${NC}: $1"
    if [[ -n "${2:-}" ]]; then
        echo "  Reason: $2"
    fi
}

# Assert exit code
# Usage: assert_exit_code <expected_code> <command...>
assert_exit_code() {
    local expected="$1"
    shift
    ((TESTS_RUN++)) || true

    local actual=0
    "$@" > /dev/null 2>&1 || actual=$?

    if [[ "$actual" -eq "$expected" ]]; then
        test_pass "Exit code $expected: $*"
        return 0
    else
        test_fail "Exit code mismatch: $*" "$expected" "$actual"
        return 1
    fi
}

# Assert command succeeds (exit code 0)
# Usage: assert_success <command...>
assert_success() {
    assert_exit_code 0 "$@"
}

# Assert command fails (non-zero exit code)
# Usage: assert_failure <command...>
assert_failure() {
    ((TESTS_RUN++)) || true

    local actual=0
    "$@" > /dev/null 2>&1 || actual=$?

    if [[ "$actual" -ne 0 ]]; then
        test_pass "Expected failure (exit $actual): $*"
        return 0
    else
        test_fail "Expected failure but got success: $*" "non-zero" "0"
        return 1
    fi
}

# Assert output contains string
# Usage: assert_output_contains <expected_string> <command...>
assert_output_contains() {
    local expected="$1"
    shift
    ((TESTS_RUN++)) || true

    local output
    output=$("$@" 2>&1) || true

    if echo "$output" | grep -q "$expected"; then
        test_pass "Output contains '$expected'"
        return 0
    else
        test_fail "Output missing '$expected'" "$expected" "(see output above)"
        echo "--- Actual output ---"
        echo "$output"
        echo "--- End output ---"
        return 1
    fi
}

# Assert output does NOT contain string
# Usage: assert_output_not_contains <unexpected_string> <command...>
assert_output_not_contains() {
    local unexpected="$1"
    shift
    ((TESTS_RUN++)) || true

    local output
    output=$("$@" 2>&1) || true

    if ! echo "$output" | grep -q "$unexpected"; then
        test_pass "Output does not contain '$unexpected'"
        return 0
    else
        test_fail "Output unexpectedly contains '$unexpected'"
        echo "--- Actual output ---"
        echo "$output"
        echo "--- End output ---"
        return 1
    fi
}

# Assert file exists
# Usage: assert_file_exists <filepath>
assert_file_exists() {
    local filepath="$1"
    ((TESTS_RUN++)) || true

    if [[ -f "$filepath" ]]; then
        test_pass "File exists: $filepath"
        return 0
    else
        test_fail "File not found: $filepath"
        return 1
    fi
}

# Assert file does not exist
# Usage: assert_file_not_exists <filepath>
assert_file_not_exists() {
    local filepath="$1"
    ((TESTS_RUN++)) || true

    if [[ ! -f "$filepath" ]]; then
        test_pass "File does not exist: $filepath"
        return 0
    else
        test_fail "File unexpectedly exists: $filepath"
        return 1
    fi
}

# Assert Wasm is valid
# Usage: assert_wasm_valid <filepath>
assert_wasm_valid() {
    local filepath="$1"
    ((TESTS_RUN++)) || true

    if wasm-tools validate "$filepath" 2>/dev/null; then
        test_pass "Valid Wasm: $filepath"
        return 0
    else
        test_fail "Invalid Wasm: $filepath"
        return 1
    fi
}

# Print test summary
test_summary() {
    echo ""
    echo "========================================"
    echo "TEST SUMMARY"
    echo "========================================"
    echo "Tests run:    $TESTS_RUN"
    echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"
    echo ""

    if [[ "$TESTS_FAILED" -eq 0 ]]; then
        echo -e "${GREEN}All tests passed!${NC}"
        return 0
    else
        echo -e "${RED}Some tests failed.${NC}"
        return 1
    fi
}

# Cleanup function for temp files
cleanup_temp() {
    if [[ -n "${TEMP_DIR:-}" && -d "$TEMP_DIR" ]]; then
        rm -rf "$TEMP_DIR"
    fi
}

# Create temporary directory
setup_temp() {
    TEMP_DIR=$(mktemp -d)
    trap cleanup_temp EXIT
    echo "$TEMP_DIR"
}
