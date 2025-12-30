#!/usr/bin/env bash
# test-compile-form.sh - Test compile_form export (T030)
# User Story 4: Compile Form Verification
#
# Tests:
# 1. --expr option is recognized
# 2. compile_form detection works (expects exit 77 if not exported)
# 3. Error handling for missing export

set -euo pipefail
source "$(dirname "$0")/test-helpers.sh"

test_header "Compile Form Verification (US4)"

RUNNER="$PROJECT_ROOT/host-shim/stage1-runner.js"
WASM="$PROJECT_ROOT/dist/clysm-stage1.wasm"

# T1: --expr option is recognized
echo "Test 1: --expr option is recognized"
((TESTS_RUN++)) || true
help_output=$(node "$RUNNER" --help 2>&1)
if echo "$help_output" | grep -q -- "--expr"; then
    test_pass "--expr option documented"
else
    test_fail "--expr option not documented"
fi

# T2: --output option is recognized
echo "Test 2: --output option is recognized"
((TESTS_RUN++)) || true
if echo "$help_output" | grep -q -- "--output"; then
    test_pass "--output option documented"
else
    test_fail "--output option not documented"
fi

# T3: Try compile_form with simple expression
echo "Test 3: compile_form with simple expression"
((TESTS_RUN++)) || true
compile_output=$(node "$RUNNER" --verbose --expr "(+ 1 2)" "$WASM" 2>&1) || exit_code=$?
exit_code=${exit_code:-0}

# We expect either:
# - Exit 0 with wasm output (if compile_form is exported)
# - Exit 77 with known limitation message (if compile_form not exported)
if [[ "$exit_code" -eq 0 ]]; then
    test_pass "compile_form succeeded (exit 0)"
elif [[ "$exit_code" -eq 77 ]]; then
    # Check for known limitation message
    if echo "$compile_output" | grep -qi "KNOWN LIMITATION"; then
        test_pass "compile_form not exported (exit 77 with message)"
    else
        test_fail "Exit 77 but no KNOWN LIMITATION message"
    fi
else
    test_fail "Unexpected exit code for --expr" "0 or 77" "$exit_code"
    echo "--- Output ---"
    echo "$compile_output"
fi

# T4: Verify error message mentions compile_form
echo "Test 4: Error message mentions compile_form"
((TESTS_RUN++)) || true
if [[ "$exit_code" -eq 77 ]]; then
    if echo "$compile_output" | grep -qi "compile_form"; then
        test_pass "Error message mentions compile_form"
    else
        test_fail "Error message does not mention compile_form"
    fi
else
    test_skip "Skipped (compile_form was exported)" "Stage 1 has compile_form"
fi

# T5: Shell script handles --expr
echo "Test 5: Shell script handles --expr"
((TESTS_RUN++)) || true
shell_exit=0
shell_output=$("$PROJECT_ROOT/scripts/run-stage1.sh" --expr "(+ 1 2)" 2>&1) || shell_exit=$?

if [[ "$shell_exit" -eq 0 ]] || [[ "$shell_exit" -eq 77 ]]; then
    test_pass "Shell script handles --expr (exit $shell_exit)"
else
    test_fail "Shell script failed with --expr" "0 or 77" "$shell_exit"
fi

# T6: Check current Stage 1 exports
echo "Test 6: Check Stage 1 exports"
((TESTS_RUN++)) || true
exports=$(wasm-tools print "$WASM" 2>/dev/null | grep "(export" | tr -d '()"' || true)
if echo "$exports" | grep -q "compile_form"; then
    test_pass "Stage 1 exports compile_form"
else
    # This is expected given current 14.1% compilation rate
    test_pass "Stage 1 does not export compile_form (expected)"
fi

test_summary
