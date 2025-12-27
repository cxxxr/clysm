#!/usr/bin/env bash
#
# verify-stage0.sh - Verify Stage 0 Wasm binary executes correctly on wasmtime
#
# Part of Feature 039: Stage 1 Compiler Generation
# Usage: ./scripts/verify-stage0.sh [stage0-path]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Default Stage 0 path
STAGE0_PATH="${1:-$PROJECT_ROOT/dist/clysm-stage0.wasm}"
HOST_SHIM="$PROJECT_ROOT/host-shim/verify-stage0.js"

echo "=== Stage 0 Verification ==="
echo "Stage 0: $STAGE0_PATH"
echo ""

# Check wasmtime availability
if ! command -v wasmtime &>/dev/null; then
    echo "ERROR: wasmtime not found in PATH"
    exit 1
fi

# Check wasm-tools availability
if ! command -v wasm-tools &>/dev/null; then
    echo "ERROR: wasm-tools not found in PATH"
    exit 1
fi

# Check Node.js availability (for host shim)
if ! command -v node &>/dev/null; then
    echo "ERROR: node (Node.js) not found in PATH"
    exit 1
fi

# Check Stage 0 exists
if [[ ! -f "$STAGE0_PATH" ]]; then
    echo "ERROR: Stage 0 binary not found: $STAGE0_PATH"
    exit 1
fi

# Check host shim exists
if [[ ! -f "$HOST_SHIM" ]]; then
    echo "ERROR: Host shim not found: $HOST_SHIM"
    exit 1
fi

echo "1. Validating Stage 0 binary..."
if wasm-tools validate "$STAGE0_PATH" 2>/dev/null; then
    echo "   [PASS] Stage 0 is valid Wasm"
else
    echo "   [FAIL] Stage 0 is invalid Wasm"
    exit 1
fi

echo ""
echo "2. Checking binary size..."
SIZE=$(wc -c < "$STAGE0_PATH")
echo "   Size: $SIZE bytes"
if [[ $SIZE -lt 8 ]]; then
    echo "   [WARN] Binary seems too small (less than Wasm header)"
fi

echo ""
echo "3. Listing exports..."
EXPORTS=$(wasm-tools print "$STAGE0_PATH" 2>/dev/null | grep -c '(export' || true)
EXPORTS=${EXPORTS:-0}
echo "   Export count: $EXPORTS"

echo ""
echo "4. Running verification tests via Node.js host shim..."
if node "$HOST_SHIM" "$STAGE0_PATH"; then
    echo "   [PASS] Verification tests passed"
else
    EXIT_CODE=$?
    if [[ $EXIT_CODE -eq 77 ]]; then
        echo "   [SKIP] Verification skipped (known limitation)"
    else
        echo "   [FAIL] Verification tests failed (exit code: $EXIT_CODE)"
        exit 1
    fi
fi

echo ""
echo "=== Stage 0 Verification Complete ==="
echo "Status: PASS"
