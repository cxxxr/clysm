#!/usr/bin/env bash
# test-ffi-imports.sh - Test FFI import resolution (T012)
# User Story 2: FFI File System Operations
#
# Tests:
# 1. All clysm:io functions are provided
# 2. All clysm:fs functions are provided
# 3. No LinkError during instantiation

set -euo pipefail
source "$(dirname "$0")/test-helpers.sh"

test_header "FFI Import Resolution (US2)"

RUNNER="$PROJECT_ROOT/host-shim/stage1-runner.js"
WASM="$PROJECT_ROOT/dist/clysm-stage1.wasm"

# T1: Check clysm:io imports are resolved
echo "Test 1: clysm:io imports resolved"
((TESTS_RUN++)) || true || true
output=$(node "$RUNNER" --verbose "$WASM" 2>&1) || true

if echo "$output" | grep -q "clysm:io.*write-char"; then
    test_pass "clysm:io functions loaded"
else
    test_fail "clysm:io functions not loaded" "write-char in output" "not found"
    echo "--- Output ---"
    echo "$output"
fi

# T2: Check clysm:fs imports are resolved
echo "Test 2: clysm:fs imports resolved"
((TESTS_RUN++)) || true || true
if echo "$output" | grep -q "clysm:fs"; then
    test_pass "clysm:fs functions loaded"
else
    test_fail "clysm:fs functions not loaded" "clysm:fs in output" "not found"
fi

# T3: No LinkError during instantiation
echo "Test 3: No LinkError during instantiation"
((TESTS_RUN++)) || true
if echo "$output" | grep -qi "LinkError"; then
    test_fail "LinkError occurred during instantiation"
    echo "--- Output ---"
    echo "$output"
else
    test_pass "No LinkError during instantiation"
fi

# T4: Module instantiates successfully
echo "Test 4: Module instantiates successfully"
((TESTS_RUN++)) || true
if echo "$output" | grep -q "instantiated successfully"; then
    test_pass "Module instantiated"
else
    test_fail "Module instantiation not confirmed" "instantiated successfully" "not found"
fi

# T5: Verify fs-shim exports are available
echo "Test 5: fs-shim has required exports"
((TESTS_RUN++)) || true
# Check that fs-shim.js exports getImports
fs_shim_exports=$(node -e "import('./host-shim/fs-shim.js').then(m => console.log(Object.keys(m).join(',')))" 2>/dev/null) || fs_shim_exports=""
if echo "$fs_shim_exports" | grep -q "getImports"; then
    test_pass "fs-shim exports getImports"
else
    test_fail "fs-shim missing getImports export"
fi

# T6: Verify io-shim exports are available
echo "Test 6: io-shim has required exports"
((TESTS_RUN++)) || true
io_shim_exports=$(node -e "import('./host-shim/io-shim.js').then(m => console.log(Object.keys(m).join(',')))" 2>/dev/null) || io_shim_exports=""
if echo "$io_shim_exports" | grep -q "getImports"; then
    test_pass "io-shim exports getImports"
else
    test_fail "io-shim missing getImports export"
fi

test_summary
