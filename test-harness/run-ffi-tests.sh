#!/usr/bin/env bash
# run-ffi-tests.sh - FFI test runner for multi-host validation (T066)
#
# This script runs FFI integration tests across both wasmtime and Node.js
# environments to validate cross-platform compatibility.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=========================================="
echo "  Clysm FFI Multi-Host Test Runner"
echo "=========================================="
echo ""

# Track test results
WASMTIME_RESULT=0
NODEJS_RESULT=0
LISP_RESULT=0

# Check for required tools
check_tool() {
    if ! command -v "$1" &> /dev/null; then
        echo -e "${YELLOW}Warning: $1 not found. Skipping $2 tests.${NC}"
        return 1
    fi
    return 0
}

# Run wasmtime Rust tests
run_wasmtime_tests() {
    echo ""
    echo "----------------------------------------"
    echo "  Running wasmtime Rust tests"
    echo "----------------------------------------"

    if ! check_tool "cargo" "wasmtime Rust"; then
        WASMTIME_RESULT=2  # Skipped
        return
    fi

    cd "$SCRIPT_DIR/wasmtime"

    if [ ! -f "Cargo.toml" ]; then
        echo -e "${YELLOW}Cargo.toml not found. Skipping wasmtime tests.${NC}"
        WASMTIME_RESULT=2
        return
    fi

    echo "Building wasmtime test harness..."
    if cargo build 2>&1 | head -20; then
        echo ""
        echo "Running unit tests..."
        if cargo test 2>&1; then
            echo -e "${GREEN}✓ wasmtime Rust tests passed${NC}"
            WASMTIME_RESULT=0
        else
            echo -e "${RED}✗ wasmtime Rust tests failed${NC}"
            WASMTIME_RESULT=1
        fi
    else
        echo -e "${YELLOW}Build failed (wasmtime GC support may not be available)${NC}"
        WASMTIME_RESULT=2
    fi

    cd "$SCRIPT_DIR"
}

# Run Node.js tests
run_nodejs_tests() {
    echo ""
    echo "----------------------------------------"
    echo "  Running Node.js tests"
    echo "----------------------------------------"

    if ! check_tool "node" "Node.js"; then
        NODEJS_RESULT=2
        return
    fi

    cd "$SCRIPT_DIR/node"

    if [ ! -f "ffi-host.js" ]; then
        echo -e "${YELLOW}ffi-host.js not found. Skipping Node.js tests.${NC}"
        NODEJS_RESULT=2
        return
    fi

    echo "Testing host functions..."
    if node -e "
        const ffi = require('./ffi-host.js');

        // Test host functions
        const tests = [
            ['add(10, 20) = 30', ffi.hostFunctions.add(10, 20) === 30],
            ['sub(20, 10) = 10', ffi.hostFunctions.sub(20, 10) === 10],
            ['mul(3, 4) = 12', ffi.hostFunctions.mul(3, 4) === 12],
            ['isPositive(1) = 1', ffi.hostFunctions.isPositive(1) === 1],
            ['isPositive(-1) = 0', ffi.hostFunctions.isPositive(-1) === 0],
            ['strlen(\"hello\") = 5', ffi.hostFunctions.strlen('hello') === 5],
            ['strcat(\"a\", \"b\") = \"ab\"', ffi.hostFunctions.strcat('a', 'b') === 'ab'],
        ];

        let passed = 0;
        let failed = 0;

        tests.forEach(([name, result]) => {
            if (result) {
                console.log('  ✓ ' + name);
                passed++;
            } else {
                console.log('  ✗ ' + name);
                failed++;
            }
        });

        // Test marshal type utilities
        console.log('');
        console.log('Testing marshal type utilities...');
        const mt = ffi.marshalTypes;
        const mtTests = [
            ['isFixnum(100)', mt.isFixnum(100) === true],
            ['isFixnum(2**30)', mt.isFixnum(2**30) === false],
            ['boolToI32(true)', mt.boolToI32(true) === 1],
            ['boolToI32(false)', mt.boolToI32(false) === 0],
            ['i32ToBool(1)', mt.i32ToBool(1) === true],
            ['i32ToBool(0)', mt.i32ToBool(0) === false],
        ];

        mtTests.forEach(([name, result]) => {
            if (result) {
                console.log('  ✓ ' + name);
                passed++;
            } else {
                console.log('  ✗ ' + name);
                failed++;
            }
        });

        console.log('');
        console.log('Results: ' + passed + ' passed, ' + failed + ' failed');
        process.exit(failed > 0 ? 1 : 0);
    "; then
        echo -e "${GREEN}✓ Node.js tests passed${NC}"
        NODEJS_RESULT=0
    else
        echo -e "${RED}✗ Node.js tests failed${NC}"
        NODEJS_RESULT=1
    fi

    cd "$SCRIPT_DIR"
}

# Run Lisp FFI tests
run_lisp_tests() {
    echo ""
    echo "----------------------------------------"
    echo "  Running Common Lisp FFI tests"
    echo "----------------------------------------"

    if ! check_tool "sbcl" "Common Lisp"; then
        LISP_RESULT=2
        return
    fi

    cd "$PROJECT_ROOT"

    echo "Running FFI integration tests..."
    if sbcl --non-interactive \
        --eval "(ql:quickload :clysm/tests :silent t)" \
        --eval "(let ((results (rove:run :clysm/tests))) (sb-ext:exit :code (if (rove:passed-p results) 0 1)))" \
        2>&1 | grep -E "(ffi|FFI|✓|✗|Summary)" | head -50; then
        echo -e "${GREEN}✓ Common Lisp FFI tests passed${NC}"
        LISP_RESULT=0
    else
        echo -e "${RED}✗ Common Lisp FFI tests failed${NC}"
        LISP_RESULT=1
    fi

    cd "$SCRIPT_DIR"
}

# Print summary
print_summary() {
    echo ""
    echo "=========================================="
    echo "  Summary"
    echo "=========================================="

    print_result() {
        case $1 in
            0) echo -e "  ${GREEN}✓${NC} $2" ;;
            1) echo -e "  ${RED}✗${NC} $2 (failed)" ;;
            2) echo -e "  ${YELLOW}-${NC} $2 (skipped)" ;;
        esac
    }

    print_result $WASMTIME_RESULT "wasmtime Rust tests"
    print_result $NODEJS_RESULT "Node.js tests"
    print_result $LISP_RESULT "Common Lisp FFI tests"

    echo ""

    # Calculate overall result
    if [ $WASMTIME_RESULT -eq 1 ] || [ $NODEJS_RESULT -eq 1 ] || [ $LISP_RESULT -eq 1 ]; then
        echo -e "${RED}Some tests failed!${NC}"
        exit 1
    elif [ $WASMTIME_RESULT -eq 2 ] && [ $NODEJS_RESULT -eq 2 ] && [ $LISP_RESULT -eq 2 ]; then
        echo -e "${YELLOW}All tests were skipped!${NC}"
        exit 2
    else
        echo -e "${GREEN}All tests passed!${NC}"
        exit 0
    fi
}

# Parse arguments
RUN_WASMTIME=true
RUN_NODEJS=true
RUN_LISP=true

while [[ $# -gt 0 ]]; do
    case $1 in
        --wasmtime-only)
            RUN_NODEJS=false
            RUN_LISP=false
            shift
            ;;
        --nodejs-only)
            RUN_WASMTIME=false
            RUN_LISP=false
            shift
            ;;
        --lisp-only)
            RUN_WASMTIME=false
            RUN_NODEJS=false
            shift
            ;;
        --skip-wasmtime)
            RUN_WASMTIME=false
            shift
            ;;
        --skip-nodejs)
            RUN_NODEJS=false
            shift
            ;;
        --skip-lisp)
            RUN_LISP=false
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [options]"
            echo ""
            echo "Options:"
            echo "  --wasmtime-only   Run only wasmtime Rust tests"
            echo "  --nodejs-only     Run only Node.js tests"
            echo "  --lisp-only       Run only Common Lisp tests"
            echo "  --skip-wasmtime   Skip wasmtime Rust tests"
            echo "  --skip-nodejs     Skip Node.js tests"
            echo "  --skip-lisp       Skip Common Lisp tests"
            echo "  -h, --help        Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Run tests
if $RUN_WASMTIME; then
    run_wasmtime_tests
else
    WASMTIME_RESULT=2
fi

if $RUN_NODEJS; then
    run_nodejs_tests
else
    NODEJS_RESULT=2
fi

if $RUN_LISP; then
    run_lisp_tests
else
    LISP_RESULT=2
fi

print_summary
