# Quickstart: Cross-Compile Stage 0

**Phase 1 Output** | **Date**: 2025-12-27

## Prerequisites

```bash
# Enter Nix development environment
nix develop

# Verify tools
sbcl --version          # SBCL 2.4+
wasm-tools --version    # wasm-tools 1.0+
wasmtime --version      # wasmtime 14+
```

## Build Stage 0

```bash
# One-command build
sbcl --load build/bootstrap.lisp

# Expected output:
# [1/41] Reading src/clysm/backend/leb128.lisp...
# [2/41] Reading src/clysm/backend/sections.lisp...
# ...
# [41/41] Reading src/clysm/conditions/standard.lisp...
#
# Expanding macros...
# Collected 849 compilable forms
#
# Phase 2: Compiling to Wasm...
# Testing individual forms...
# Results: 14 compiled, 835 failed
#
# Compiling 14 forms to single module...
#
# === Bootstrap Complete ===
# Output: dist/clysm-stage0.wasm
# Size: 1,584 bytes
# Duration: 0.03 seconds
#
# Compilation Statistics:
#   Successfully compiled: 14 forms
#   Failed to compile: 835 forms
#
# Phase 4: Validating Wasm...
# Validation: PASSED
```

**Note**: The low compilation rate (14/849 = 1.6%) is expected. See Known Limitations below.

## Validate Output

```bash
# Static validation
wasm-tools validate dist/clysm-stage0.wasm

# Expected: exit code 0 (no output = success)
```

## Run Verification Tests

```bash
# Run all verification tests
./scripts/verify-all.sh

# Expected output:
# ==========================================
# Stage 0 Complete Verification Suite
# ==========================================
#
# Checking prerequisites...
# ✓ Stage 0 binary exists
# ✓ wasm-tools installed
# ✓ Node.js installed
# ✓ wasmtime installed
#
# Test: Arithmetic Compilation (V001)
# ...
# ⚠ SKIPPED
#
# Test: Function Definition (V002)
# ...
# ⚠ SKIPPED
#
# Test: Control Flow (V003)
# ...
# ⚠ SKIPPED
#
# Summary:
#   Passed:  0
#   Skipped: 3
#   Failed:  0
#
# All tests skipped due to known limitations.
```

**Note**: Tests are SKIPPED (exit code 77) because Stage 0 doesn't export
a working `compile` function yet. See Known Limitations below.

## Development Workflow

### Incremental Development

```bash
# Run tests during development
sbcl --eval "(asdf:test-system :clysm)"

# Test just bootstrap functionality
sbcl --eval "(asdf:test-system :clysm/bootstrap-tests)"
```

### Debugging Build Failures

```bash
# Verbose mode shows each form being compiled
sbcl --load build/bootstrap.lisp --eval "(setf *bootstrap-verbose* t)"

# Compile single module for testing
sbcl --eval "(clysm-validation:compile-module-by-path \"src/clysm/backend/leb128.lisp\")"
```

### Inspecting Output

```bash
# Convert to WAT for inspection
wasm-tools print dist/clysm-stage0.wasm > dist/clysm-stage0.wat

# Count functions, types, etc.
wasm-tools objdump -h dist/clysm-stage0.wasm
```

## Directory Structure

```text
clysm3/
├── build/
│   └── bootstrap.lisp    # Build script
├── dist/
│   └── clysm-stage0.wasm # Output binary
├── host-shim/
│   └── verify-stage0.js  # Verification host
└── tests/
    └── integration/
        └── stage0-verify-test.lisp
```

## Common Issues

### "Unsupported CL feature: X"

The compiler encountered a CL feature not in the blessed subset. Check `docs/blessed-subset.lisp` for supported features. File an issue if the feature should be supported.

### "wasm-tools validate failed"

The generated Wasm binary has structural errors. Enable verbose mode to see which module caused the issue. Check the WAT output for malformed sections.

### Build timeout (> 5 minutes)

Compilation should complete in under 5 minutes. If slower, check for infinite loops in macro expansion or excessive consing. Profile with `sb-sprof:start-profiling`.

## Known Limitations

### Low Compilation Rate (14/849 forms = 1.6%)

The Stage 0 bootstrap produces a valid Wasm binary but only compiles 14 out of 849 forms successfully. This is expected because Clysm's compiler source code uses CL features that Clysm doesn't yet support:

- `defstruct` - structure definitions
- `declare` - type declarations
- `format` - formatted output
- `define-condition` - condition definitions
- `defconstant` - constant definitions

This is the "chicken-and-egg" problem of self-hosting: the compiler needs features it can't yet compile.

### Verification Tests Skip

All V001-V003 verification tests exit with code 77 (SKIP) because:
1. Stage 0 doesn't export a working `compile` function
2. The 14 successfully compiled forms don't include the full compilation pipeline

### Resolution Path

To achieve full self-hosting, either:
1. **Extend Clysm's CL subset**: Add support for defstruct, declare, format, etc.
2. **Rewrite in blessed subset**: Modify Clysm source to use only supported features

Both approaches require significant work. The current implementation establishes the infrastructure for future self-hosting.
