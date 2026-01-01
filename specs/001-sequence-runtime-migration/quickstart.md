# Quickstart: Sequence Runtime Migration

**Branch**: `001-sequence-runtime-migration`
**Date**: 2026-01-01

## Prerequisites

```bash
# Enter development environment
nix develop

# Verify SBCL is available
sbcl --version  # Should be 2.4+

# Verify test framework
sbcl --eval "(asdf:load-system :clysm)" --quit
```

## Development Workflow

### 1. Run Existing Tests (Baseline)

```bash
# Run all tests to establish baseline
sbcl --eval "(asdf:test-system :clysm)" --quit

# Run specific sequence tests if they exist
sbcl --eval "(asdf:test-system :clysm/tests)" --quit
```

### 2. Create Test File First (TDD)

```bash
# Create test directory
mkdir -p tests/unit/sequence-runtime

# Create test file for remove family
touch tests/unit/sequence-runtime/remove-test.lisp
```

Example test file structure:

```lisp
;;;; tests/unit/sequence-runtime/remove-test.lisp
(defpackage #:clysm/tests/sequence-runtime/remove
  (:use #:cl #:rove #:clysm))
(in-package #:clysm/tests/sequence-runtime/remove)

;;; Basic remove-rt tests
(deftest remove-rt-basic
  (testing "removes matching elements"
    (ok (equal (remove-rt 3 '(1 2 3 4 3 5) #'eql nil 0 nil nil nil)
               '(1 2 4 5))))
  (testing "returns empty list for empty input"
    (ok (null (remove-rt 3 nil #'eql nil 0 nil nil nil))))
  (testing "returns same list when no matches"
    (ok (equal (remove-rt 9 '(1 2 3) #'eql nil 0 nil nil nil)
               '(1 2 3)))))

;;; Tests with :key argument
(deftest remove-rt-with-key
  (testing ":key extracts comparison value"
    (ok (equal (remove-rt 3 '((1 . a) (2 . b) (3 . c)) #'eql #'car 0 nil nil nil)
               '((1 . a) (2 . b))))))

;;; Tests with :start/:end
(deftest remove-rt-with-bounds
  (testing ":start limits range"
    (ok (equal (remove-rt 3 '(3 1 3 2 3) #'eql nil 1 nil nil nil)
               '(3 1 2))))
  (testing ":end limits range"
    (ok (equal (remove-rt 3 '(3 1 3 2 3) #'eql nil 0 3 nil nil)
               '(1 2 3)))))

;;; Tests with :count
(deftest remove-rt-with-count
  (testing ":count limits removals"
    (ok (equal (remove-rt 3 '(3 1 3 2 3) #'eql nil 0 nil 2 nil)
               '(1 2 3)))))
```

### 3. Run Tests (Should Fail - Red Phase)

```bash
sbcl --eval "(asdf:test-system :clysm)" --quit
# Expected: Tests fail because remove-rt doesn't exist yet
```

### 4. Implement Runtime Function

Create `src/clysm/lib/sequence-runtime.lisp`:

```lisp
;;;; sequence-runtime.lisp - Runtime library sequence functions
;;;; Feature: 001-sequence-runtime-migration
;;;;
;;;; HyperSpec references:
;;;;   [remove](resources/HyperSpec/Body/f_rm_rm.htm)
;;;;   [count](resources/HyperSpec/Body/f_countc.htm)
;;;;   [substitute](resources/HyperSpec/Body/f_sbs_s.htm)

(in-package #:clysm)

(defun remove-rt (item list test key start end count from-end)
  "Runtime implementation of REMOVE for lists.
   Uses only Layer 1 primitives."
  (let ((test-fn (or test #'eql))
        (result nil)
        (index 0)
        (matched 0))
    ;; Handle :from-end by reversing
    (when from-end
      (setf list (nreverse (copy-list list))))
    ;; Process list
    (loop for rest = list then (cdr rest)
          while (consp rest)
          for elem = (car rest)
          do (let* ((in-range (and (>= index (or start 0))
                                   (or (null end) (< index end))))
                    (keyed (if key (funcall key elem) elem))
                    (matches (and in-range
                                  (funcall test-fn item keyed)
                                  (or (null count) (< matched count)))))
               (if matches
                   (incf matched)
                   (push elem result))
               (incf index)))
    ;; Return result (reversed appropriately)
    (if from-end
        result  ; Already in correct order after processing reversed list
        (nreverse result))))
```

### 5. Register Runtime Function

Add to `src/clysm/compiler/codegen/func-section.lisp`:

```lisp
;; After *runtime-function-table* definition (around line 150):
(register-runtime-function 'remove :$remove-rt nil)
```

### 6. Run Tests (Should Pass - Green Phase)

```bash
sbcl --eval "(asdf:test-system :clysm)" --quit
# Expected: Tests pass
```

### 7. Remove Inline Codegen

Delete `compile-remove` function and its dispatch case from `func-section.lisp`:

```lisp
;; Remove from case statement (around line 1206):
;; (remove (compile-remove args env))  <- DELETE THIS LINE

;; Remove function definition (lines 11485-11589):
;; (defun compile-remove (args env) ...) <- DELETE THIS BLOCK
```

### 8. Verify Compilation

```bash
# Compile a test form to verify runtime dispatch works
sbcl --eval "(clysm:compile-to-wasm '(remove 3 '(1 2 3)))" --quit

# Validate generated Wasm
wasm-tools validate dist/test.wasm
```

### 9. Run Full Test Suite

```bash
sbcl --eval "(asdf:test-system :clysm)" --quit
nix flake check  # Must pass before commit
```

## File Locations

| File | Purpose |
|------|---------|
| `src/clysm/lib/sequence-runtime.lisp` | NEW: Runtime implementations |
| `src/clysm/compiler/codegen/func-section.lisp` | MODIFY: Registration + remove inline |
| `clysm.asd` | MODIFY: Add sequence-runtime.lisp component |
| `tests/unit/sequence-runtime/*.lisp` | NEW: Test files |

## Verification Commands

```bash
# Line count check (before migration)
wc -l src/clysm/compiler/codegen/func-section.lisp
# Expected: ~18,327 lines

# After migration (12 functions):
wc -l src/clysm/compiler/codegen/func-section.lisp
# Expected: ~17,700 lines (~3.5% reduction)

# Stage 1 compilation rate
sbcl --load build/stage1-complete.lisp
# Check dist/stage1-report.json for compilation rate

# ANSI compatibility check
sbcl --eval "(equal (remove 3 '(1 2 3 4 3)) '(1 2 4))" --quit
# Expected: T
```

## Common Issues

### Issue: "Unknown runtime function"

**Cause**: Function not registered in `*runtime-function-table*`

**Solution**: Add `(register-runtime-function 'func :$func-rt nil)` before use

### Issue: Tests pass but Wasm validation fails

**Cause**: Runtime function uses non-Layer-1 primitives

**Solution**: Refactor to use only car/cdr/cons/funcall etc.

### Issue: `:from-end` produces wrong order

**Cause**: Incorrect reversal logic

**Solution**: See implementation pattern in quickstart above
