# Quickstart: Self-Hosting Blockers Resolution

**Feature**: 043-self-hosting-blockers
**Date**: 2025-12-28

## Goal

Increase Clysm Stage 0 compilation rate from ~20% to 50%+ by implementing missing CL features.

## Prerequisites

```bash
# Enter Nix development shell
nix develop

# Verify environment
sbcl --version  # SBCL 2.4+
wasm-tools --version
wasmtime --version
```

## Feature Areas

### 1. LOOP Macro Verification

Already implemented. Run tests to verify:

```bash
# Run LOOP macro tests
sbcl --eval "(asdf:test-system :clysm/tests)" --quit
```

### 2. Default Parameter Values

Compile functions with &optional/:key defaults:

```lisp
;; After implementation
(compile-to-wasm
 '(defun greet (&optional (name "World"))
    (format nil "Hello, ~A!" name)))

;; Verify
(funcall (compile nil '(lambda (&optional (x 10)) x)))  ; => 10
```

### 3. Hash Tables

Create and use hash tables:

```lisp
;; After implementation
(compile-to-wasm
 '(let ((ht (make-hash-table)))
    (setf (gethash 'key ht) 'value)
    (gethash 'key ht)))  ; => VALUE, T
```

### 4. List Functions

Use assoc, member with :test/:key:

```lisp
;; After implementation
(compile-to-wasm
 '(assoc "b" '(("a" . 1) ("b" . 2)) :test #'string=))
;; => ("b" . 2)
```

### 5. Sequence Functions

Use find, position with :key:

```lisp
;; After implementation
(compile-to-wasm
 '(find 2 '((a 1) (b 2) (c 3)) :key #'second))
;; => (b 2)
```

## Development Workflow

### Run Bootstrap Compilation

```bash
# Compile Stage 0 and measure rate
sbcl --load build/bootstrap.lisp

# Check compilation rate
cat dist/bootstrap-report.json | jq '.compilation_rate'
```

### Run Tests

```bash
# All tests
sbcl --eval "(asdf:test-system :clysm/tests)" --quit

# Specific test file
sbcl --eval "(asdf:load-system :clysm/tests)" \
     --eval "(rove:run :clysm/tests/unit/hash-table-test)" \
     --quit
```

### Validate Wasm Output

```bash
# Validate generated Wasm
wasm-tools validate dist/clysm-stage0.wasm

# Inspect Wasm
wasm-tools print dist/clysm-stage0.wasm | head -100
```

## Success Criteria

- [ ] Stage 0 compilation rate >= 50%
- [ ] All compiler/codegen/ files have at least one compiled form
- [ ] All unit tests pass
- [ ] Wasm validation passes

## Files to Modify

| File | Changes |
|------|---------|
| `src/clysm/compiler/ast.lisp` | Add ast-param-info for default params |
| `src/clysm/compiler/codegen/func-section.lisp` | Extend compile-defun, add list/seq functions |
| `src/clysm/compiler/codegen/gc-types.lisp` | Add hash-table WasmGC types |
| `src/clysm/lib/macros.lisp` | Verify LOOP expansion |

## Next Steps

1. Run `/speckit.tasks` to generate task list
2. Implement features in priority order (see research.md)
3. Measure compilation rate after each feature
4. Iterate until 50% achieved
