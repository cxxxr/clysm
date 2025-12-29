# Quickstart: ANSI CL Array/Sequence Primitives

**Feature**: 001-ansi-array-primitives
**Date**: 2025-12-29

## Prerequisites

Before starting implementation:

1. Verify development environment:
   ```bash
   sbcl --version  # Should be 2.4+
   nix flake check # Should pass
   ```

2. Understand existing infrastructure:
   - Read `src/clysm/compiler/codegen/func-section.lisp` (line 12487-12560) for `array.get` usage pattern
   - Review `src/clysm/lib/setf-expanders.lisp` for existing `aref` setf expander

## Implementation Order

Follow TDD: write tests first, then implement.

### Phase 1: aref / svref (Priority P1)

**Goal**: Enable defstruct accessor compilation (90 forms)

1. **Write tests** (`tests/unit/array-primitives-test.lisp`):
   ```lisp
   (deftest aref-simple-vector
     (let* ((code '(aref #(1 2 3) 1))
            (result (clysm:eval-to-value code)))
       (ok (= result 2))))
   ```

2. **Add AST recognition** (`src/clysm/compiler/ast.lisp`):
   - No new AST type needed - `aref` is a function call
   - Recognition happens in `compile-call` dispatch

3. **Implement codegen** (`src/clysm/compiler/codegen/func-section.lisp`):
   ```lisp
   (defun compile-aref (args env)
     "Compile (aref array index) to Wasm instructions."
     (let ((array-expr (first args))
           (index-expr (second args)))
       `(,@(compile-to-instructions array-expr env)
         (:ref.cast (:ref 22))
         ,@(compile-to-instructions index-expr env)
         (:ref.cast :i31)
         :i31.get_s
         (:array.get 22))))
   ```

4. **Add call dispatch**:
   ```lisp
   ;; In compile-call or similar dispatch
   (aref (compile-aref args env))
   (svref (compile-aref args env))  ; Same implementation
   ```

5. **Implement %setf-aref**:
   ```lisp
   (defun compile-%setf-aref (args env)
     "Compile (%setf-aref array value index) to Wasm."
     (let ((array-expr (first args))
           (value-expr (second args))
           (index-expr (third args)))
       `(,@(compile-to-instructions array-expr env)
         (:ref.cast (:ref 22))
         ,@(compile-to-instructions index-expr env)
         (:ref.cast :i31)
         :i31.get_s
         ,@(compile-to-instructions value-expr env)
         (:local.tee <value-local>)
         (:array.set 22)
         (:local.get <value-local>))))
   ```

6. **Verify defstruct compilation**:
   ```bash
   sbcl --load build/bootstrap.lisp
   # Check compilation rate improvement
   ```

### Phase 2: schar (Priority P3)

**Goal**: Enable string character access

1. **Write tests**:
   ```lisp
   (deftest schar-ascii
     (let* ((code '(schar "hello" 0))
            (result (clysm:eval-to-value code)))
       (ok (char= result #\h))))
   ```

2. **Implement codegen**:
   ```lisp
   (defun compile-schar (args env)
     "Compile (schar string index) to Wasm instructions."
     (let ((str-expr (first args))
           (index-expr (second args)))
       `(,@(compile-to-instructions str-expr env)
         (:ref.cast (:ref 4))
         ,@(compile-to-instructions index-expr env)
         (:ref.cast :i31)
         :i31.get_s
         (:array.get_u 4)
         :ref.i31)))
   ```

### Phase 3: elt (Priority P4)

**Goal**: Generic sequence access

1. **Implement nth runtime function** (if not exists):
   - Used for list access path

2. **Implement type-dispatching codegen**:
   - See `contracts/array-primitives.md` for full pattern

### Phase 4: coerce (Priority P2)

**Goal**: Sequence type conversion

1. **Implement runtime helpers**:
   ```lisp
   ;; %list-to-vector, %vector-to-list, %chars-to-string
   ```

2. **Add macro/compile-time expansion**:
   - When `result-type` is constant, expand inline
   - Otherwise, call runtime dispatcher

## Validation Checklist

After each phase:

- [ ] Unit tests pass: `sbcl --eval "(asdf:test-system :clysm)"`
- [ ] Wasm validates: `wasm-tools validate dist/output.wasm`
- [ ] Nix check passes: `nix flake check`
- [ ] Compilation rate improved (check with `analyze-blockers`)

## Files to Modify

| File | Changes |
|------|---------|
| `src/clysm/compiler/codegen/func-section.lisp` | Add `compile-aref`, `compile-schar`, etc. |
| `src/clysm/compiler/ast.lisp` | No changes needed (function calls) |
| `src/clysm/lib/setf-expanders.lisp` | Add `svref`, `schar`, `elt` expanders |
| `tests/unit/array-primitives-test.lisp` | NEW: Unit tests |
| `tests/contract/array-wasm-test.lisp` | NEW: Wasm validation tests |
| `tests/integration/array-test.lisp` | NEW: End-to-end tests |

## Common Patterns

### Index Conversion

```lisp
;; Lisp index (i31ref fixnum) → Wasm i32
,@(compile-to-instructions index-expr env)
(:ref.cast :i31)
:i31.get_s
```

### Type Checking

```lisp
;; Test if value is simple-vector (type 22)
(:local.get ,val)
(:ref.test (:ref 22))
(:if ...)
```

### Array Access

```lisp
;; anyref array (type 22): array.get 22
;; i8 array (type 4): array.get_u 4 (for unsigned bytes)
```

## Debugging Tips

1. **Use WAT output**:
   ```lisp
   (clysm:compile-to-wat '(aref #(1 2 3) 1))
   ```

2. **Check type indices**:
   - Type 4 = $string
   - Type 22 = $mv_array (simple-vector)

3. **Validate incrementally**:
   ```bash
   wasm-tools validate output.wasm 2>&1 | head -20
   ```
