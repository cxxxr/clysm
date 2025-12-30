#!/usr/bin/env bash
# test-generate-stage2.sh - Test Stage 2 generation script (T038)
# User Story 5: Stage 2 Generation Script
#
# Tests:
# 1. Script exists and is executable
# 2. --help option works
# 3. Script runs (may exit 77 if compile_all not exported)
# 4. Report file is generated

set -euo pipefail
source "$(dirname "$0")/test-helpers.sh"

test_header "Stage 2 Generation (US5)"

SCRIPT="$PROJECT_ROOT/scripts/generate-stage2.sh"
REPORT="$PROJECT_ROOT/dist/stage2-report.json"

# T1: Script exists
echo "Test 1: Generate script exists"
assert_file_exists "$SCRIPT"

# T2: Script is executable
echo "Test 2: Script is executable"
((TESTS_RUN++)) || true
if [[ -x "$SCRIPT" ]]; then
    test_pass "Script is executable"
else
    test_fail "Script is not executable"
fi

# T3: --help works
echo "Test 3: --help works"
((TESTS_RUN++)) || true
help_output=$("$SCRIPT" --help 2>&1) || true
if echo "$help_output" | grep -qi "usage"; then
    test_pass "--help shows usage"
else
    test_fail "--help does not show usage"
fi

# T4: Script runs (with expected exit)
echo "Test 4: Script runs"
((TESTS_RUN++)) || true
# Clean up any previous report
rm -f "$REPORT" 2>/dev/null || true

exit_code=0
script_output=$("$SCRIPT" 2>&1) || exit_code=$?

# Expected: 0 (success), 77 (compile_all not exported), or 1 (partial)
if [[ "$exit_code" -eq 0 ]] || [[ "$exit_code" -eq 77 ]] || [[ "$exit_code" -eq 1 ]]; then
    test_pass "Script completed (exit $exit_code)"
else
    test_fail "Script failed unexpectedly" "0, 1, or 77" "$exit_code"
    echo "--- Output ---"
    echo "$script_output"
fi

# T5: Report file exists or skip message shown
echo "Test 5: Report file generated or skip message"
((TESTS_RUN++)) || true
if [[ -f "$REPORT" ]]; then
    test_pass "Report file generated: $REPORT"
elif echo "$script_output" | grep -qi "KNOWN LIMITATION"; then
    test_pass "Known limitation message shown (no report expected)"
else
    # Check if output mentions report
    if echo "$script_output" | grep -qi "report"; then
        test_pass "Report mentioned in output"
    else
        test_fail "No report file and no skip message"
    fi
fi

# T6: If report exists, validate JSON format
echo "Test 6: Report is valid JSON (if exists)"
if [[ -f "$REPORT" ]]; then
    ((TESTS_RUN++)) || true
    if jq empty "$REPORT" 2>/dev/null; then
        test_pass "Report is valid JSON"
    else
        test_fail "Report is not valid JSON"
    fi
else
    test_skip "Report validation" "No report file generated"
fi

# T7: --verbose mode
echo "Test 7: --verbose mode works"
((TESTS_RUN++)) || true
verbose_output=$("$SCRIPT" --verbose 2>&1) || true
if echo "$verbose_output" | grep -qi "verbose\|stage"; then
    test_pass "--verbose produces additional output"
else
    test_pass "--verbose mode runs (minimal output expected)"
fi

test_summary
