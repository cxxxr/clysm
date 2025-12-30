# Quickstart: ANSI CL Sequence Operations

**Branch**: `001-ansi-sequence-operations` | **Date**: 2025-12-29

## Prerequisites

1. Phase 13D-1 (001-ansi-array-primitives) must be completed
2. SBCL 2.4+ installed
3. Nix development environment active (`nix develop`)

## Setup

```bash
# Enter development shell
nix develop

# Ensure dependencies are loaded
sbcl --eval "(asdf:load-system :clysm)" --quit
```

## Development Workflow

### 1. Run Existing Tests (Baseline)

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Run specific sequence tests
sbcl --eval "(asdf:test-system :clysm/tests)" \
     --eval "(rove:run :clysm/tests/integration/sequence-test)"
```

### 2. TDD Cycle for Each Function

Following Constitution Principle VII (TDD Non-Negotiable):

```lisp
;; 1. Write failing test (in tests/unit/sequence-codegen-test.lisp)
(deftest compile-subseq-vector-test
  (let ((result (clysm:compile-to-wasm '(subseq #(1 2 3 4 5) 1 3))))
    (ok (wasm-contains-instruction-p result :array.copy)
        "subseq should use array.copy")))

;; 2. Run test - should FAIL
;; 3. Implement in src/clysm/compiler/codegen/func-section.lisp
;; 4. Run test - should PASS
;; 5. Refactor if needed
```

### 3. Add Compile Function

In `src/clysm/compiler/codegen/func-section.lisp`:

```lisp
(defun compile-subseq (ast env)
  "Compile subseq expression to Wasm instructions.
   Dispatches on sequence type: string, vector, or list."
  ;; Implementation here
  )
```

### 4. Wire Up Dispatch

In `src/clysm/compiler/compiler.lisp` (or appropriate dispatch point):

```lisp
;; Add case for 'subseq in the function dispatch
(case (car form)
  ...
  (subseq (compile-subseq form env))
  ...)
```

### 5. Validate Wasm Output

```bash
# Compile a test form
sbcl --eval "(clysm:compile-to-wasm '(subseq \"hello\" 1 3))" \
     --eval "(clysm:write-wasm-to-file \"test-subseq.wasm\")"

# Validate with wasm-tools
wasm-tools validate test-subseq.wasm

# Inspect WAT output
wasm-tools print test-subseq.wasm
```

## Key Files to Modify

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/func-section.lisp` | Add compile-subseq, compile-concatenate, etc. |
| `src/clysm/compiler/compiler.lisp` | Add array.copy instruction emission |
| `tests/unit/sequence-codegen-test.lisp` | NEW: Unit tests for codegen |
| `tests/contract/sequence-wasm-test.lisp` | NEW: Wasm output validation |
| `tests/integration/sequence-test.lisp` | EXTEND: End-to-end tests |

## Quick Reference: Wasm Instructions

```wat
;; Create array with default values
(array.new_default $type)

;; Create array with specific value
(array.new $type)

;; Create array from stack values
(array.new_fixed $type <n>)

;; Copy between arrays
(array.copy $dest-type $src-type)

;; Get array length
(array.len)

;; Get/set elements
(array.get $type)
(array.set $type)
```

## Verification Checklist

Before marking each function complete:

- [ ] Unit tests pass
- [ ] Contract tests pass (Wasm structure validation)
- [ ] Integration tests pass (compile → execute → verify result)
- [ ] `wasm-tools validate` passes on generated output
- [ ] `nix flake check` passes

## Expected Compilation Rate Impact

| Milestone | Expected Rate |
|-----------|---------------|
| Before Phase 13D-2 | ~23% |
| After subseq | ~28% |
| After concatenate | ~32% |
| After make-string | ~35% |
| After make-array ext | ~38% |
| After copy-seq | ~40%+ |

## Troubleshooting

### "Unknown instruction: array.copy"

Add `array.copy` handling to `compiler.lisp` bytecode emission section.

### "Type mismatch in array.copy"

Ensure source and destination array types match. Use correct type indices from `gc-types.lisp`.

### "Bounds check failed at runtime"

Review bounds validation logic. Ensure start ≤ end ≤ length.
