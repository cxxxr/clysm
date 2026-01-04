# Validation Contracts: Compiler Code Generation Cleanup

**Date**: 2026-01-04
**Feature**: 001-codegen-cleanup

## Overview

This document defines the validation contracts that must be satisfied after each removal batch to ensure compiler correctness is maintained.

## Contract 1: Unit Test Suite Pass

### Description
All existing unit tests must pass after each removal batch.

### Interface
```bash
# Command
sbcl --eval "(asdf:test-system :clysm)"

# Expected Output
; Running tests...
; All tests passed.

# Exit Code
0 (success)
```

### Acceptance Criteria
- Exit code must be 0
- No test failures reported
- No unexpected errors during test execution

### Failure Recovery
1. Identify which test(s) failed
2. Correlate with removed function(s)
3. Restore the function causing the failure
4. Re-run validation

## Contract 2: Stage 1 Compilation

### Description
Stage 1 compiler generation must complete successfully.

### Interface
```bash
# Command
sbcl --load build/stage1-complete.lisp

# Expected Output
Stage 1 compilation complete.
Generated: dist/clysm-stage1.wasm (24,XXX bytes)

# Exit Code
0 (success)
```

### Acceptance Criteria
- Compilation completes without error
- Output file `dist/clysm-stage1.wasm` is created
- File size is non-zero

### Failure Recovery
1. Check compilation error message
2. Identify missing function reference
3. Restore the function causing the failure
4. Re-run validation

## Contract 3: Wasm Binary Validation

### Description
Generated Stage 1 Wasm must pass structural validation.

### Interface
```bash
# Command
wasm-tools validate dist/clysm-stage1.wasm

# Expected Output
(empty on success)

# Exit Code
0 (success)
```

### Acceptance Criteria
- Exit code must be 0
- No validation errors reported
- Wasm binary passes all structural checks

### Failure Recovery
1. Examine wasm-tools error message
2. Use `wasm-tools print` for debugging
3. Identify structural issue caused by removal
4. Restore problematic code
5. Re-run validation

## Contract 4: Line Count Reduction

### Description
Each successful batch removal must reduce func-section.lisp line count.

### Interface
```bash
# Command
wc -l src/clysm/compiler/codegen/func-section.lisp

# Expected Output
XXXXX src/clysm/compiler/codegen/func-section.lisp
```

### Acceptance Criteria
- Line count must be less than previous batch
- Final line count must be under 8,000 (per SC-001)

### Progress Tracking
| Checkpoint | Expected Lines |
|------------|----------------|
| Initial | 15,973 |
| After Batch 1 (String) | ~14,500 |
| After Batch 2 (Numeric) | ~13,700 |
| After Batch 3 (Sequence) | ~11,700 |
| After Batch 4 (List) | ~10,500 |
| After Batch 5 (I/O) | ~9,900 |
| After Batch 6 (Helpers) | <8,000 |

## Contract 5: Compilation Coverage Maintenance

### Description
Stage 1 compilation coverage rate must not regress.

### Interface
```bash
# Command
cat dist/stage1-report.json | jq '.coverage_rate'

# Expected Output
0.22 (or higher)
```

### Acceptance Criteria
- Coverage rate >= 22% (current baseline per SC-005)
- No increase in compilation errors for previously-working forms

### Failure Recovery
1. Compare report before/after removal
2. Identify new compilation failures
3. Restore function if it was actually needed
4. Re-run validation

## Combined Validation Script

```bash
#!/bin/bash
# validate-batch.sh - Run all validation contracts

set -e

echo "=== Contract 1: Unit Tests ==="
sbcl --eval "(asdf:test-system :clysm)" || exit 1

echo "=== Contract 2: Stage 1 Compilation ==="
sbcl --load build/stage1-complete.lisp || exit 1

echo "=== Contract 3: Wasm Validation ==="
wasm-tools validate dist/clysm-stage1.wasm || exit 1

echo "=== Contract 4: Line Count ==="
wc -l src/clysm/compiler/codegen/func-section.lisp

echo "=== Contract 5: Coverage Rate ==="
cat dist/stage1-report.json | jq '.coverage_rate'

echo "=== All Contracts Passed ==="
```

## Batch Completion Checklist

For each batch, all contracts must pass before proceeding:

- [ ] Contract 1: Unit tests pass
- [ ] Contract 2: Stage 1 compiles
- [ ] Contract 3: Wasm validates
- [ ] Contract 4: Line count reduced
- [ ] Contract 5: Coverage maintained

Only when all boxes are checked should the batch be considered complete.
