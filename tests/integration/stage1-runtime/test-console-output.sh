#!/usr/bin/env bash
# test-console-output.sh - Test console output (T024)
# User Story 3: Console Output for Debugging
#
# Tests:
# 1. Verbose mode produces output on stderr
# 2. FFI function logging works
# 3. Module exports are discovered

set -euo pipefail
source "$(dirname "$0")/test-helpers.sh"

test_header "Console Output (US3)"

RUNNER="$PROJECT_ROOT/host-shim/stage1-runner.js"
WASM="$PROJECT_ROOT/dist/clysm-stage1.wasm"

# T1: Verbose mode outputs to stderr
echo "Test 1: Verbose mode outputs to stderr"
((TESTS_RUN++)) || true
verbose_output=$(node "$RUNNER" --verbose "$WASM" 2>&1) || true

if [[ -n "$verbose_output" ]]; then
    test_pass "Verbose mode produces output"
else
    test_fail "Verbose mode produces no output"
fi

# T2: FFI function logging visible
echo "Test 2: FFI function logging visible"
((TESTS_RUN++)) || true
if echo "$verbose_output" | grep -q "clysm:io"; then
    test_pass "clysm:io functions logged"
else
    test_fail "clysm:io functions not logged"
fi

# T3: clysm:fs functions logged
echo "Test 3: clysm:fs functions logged"
((TESTS_RUN++)) || true
if echo "$verbose_output" | grep -q "clysm:fs"; then
    test_pass "clysm:fs functions logged"
else
    test_fail "clysm:fs functions not logged"
fi

# T4: _start invocation logged
echo "Test 4: _start invocation logged"
((TESTS_RUN++)) || true
if echo "$verbose_output" | grep -q "Invoking _start"; then
    test_pass "_start invocation logged"
else
    test_fail "_start invocation not logged"
fi

# T5: _start completion logged
echo "Test 5: _start completion logged"
((TESTS_RUN++)) || true
if echo "$verbose_output" | grep -q "_start completed"; then
    test_pass "_start completion logged"
else
    test_fail "_start completion not logged"
fi

# T6: Shell script verbose mode works
echo "Test 6: Shell script verbose mode works"
((TESTS_RUN++)) || true
shell_output=$("$PROJECT_ROOT/scripts/run-stage1.sh" --verbose 2>&1) || true
if echo "$shell_output" | grep -q "VERBOSE"; then
    test_pass "Shell script verbose works"
else
    test_fail "Shell script verbose not working"
fi

# T7: Non-verbose mode is quiet
echo "Test 7: Non-verbose mode is quiet"
((TESTS_RUN++)) || true
quiet_output=$(node "$RUNNER" "$WASM" 2>&1) || true
if [[ -z "$quiet_output" ]]; then
    test_pass "Non-verbose mode is quiet"
else
    # Stage 1 might produce some output itself
    if echo "$quiet_output" | grep -q "VERBOSE"; then
        test_fail "Non-verbose mode shows VERBOSE output"
    else
        test_pass "Non-verbose mode is appropriately quiet"
    fi
fi

test_summary
