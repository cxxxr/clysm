# Quickstart: Quasiquote Local Variable Compilation

**Feature**: 001-quasiquote-local-vars

## Prerequisites

```bash
# Enter Nix development shell
nix develop

# Verify tools
sbcl --version          # SBCL 2.4+
wasm-tools --version    # For Wasm validation
```

## Development Workflow

### 1. Run Existing Tests (Baseline)

```bash
# Run all Clysm tests
sbcl --eval "(asdf:test-system :clysm)" --quit

# Run specific test file (once created)
sbcl --eval "(asdf:load-system :clysm)" \
     --eval "(rove:run-test-file \"tests/unit/quasiquote-local-test.lisp\")" \
     --quit
```

### 2. TDD Cycle

Following Constitution VII (TDD非交渉):

```bash
# Step 1: Write failing test
# Edit: tests/unit/quasiquote-local-test.lisp

# Step 2: Run test (should fail - RED)
sbcl --eval "(asdf:load-system :clysm)" \
     --eval "(rove:run-test-file \"tests/unit/quasiquote-local-test.lisp\")" \
     --quit

# Step 3: Implement feature
# Edit: src/clysm/compiler/codegen/func-section.lisp
# Edit: src/clysm/compiler/transform/macro.lisp

# Step 4: Run test (should pass - GREEN)
sbcl --eval "(asdf:load-system :clysm)" \
     --eval "(rove:run-test-file \"tests/unit/quasiquote-local-test.lisp\")" \
     --quit

# Step 5: Refactor if needed
# Step 6: Run full test suite
sbcl --eval "(asdf:test-system :clysm)" --quit
```

### 3. Manual Testing (REPL)

```lisp
;; Start SBCL with Clysm loaded
(asdf:load-system :clysm)
(in-package :clysm)

;; Test quasiquote expansion (before codegen)
(clysm/compiler/transform/macro:expand-backquote
  '(quasiquote (a (unquote x) b)))
;; Expected: (LIST 'A X 'B)

;; Test full compilation
(clysm:compile-to-wasm
  '(defun test-qq (x) `(value ,x)))
;; Should return Wasm bytes without error

;; Inspect generated Wasm
(with-open-file (out "/tmp/test.wasm" :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
  (write-sequence (clysm:compile-to-wasm
                   '(defun test-qq (x) `(value ,x)))
                  out))
;; Then in shell: wasm-tools print /tmp/test.wasm
```

### 4. Wasm Validation

```bash
# Validate generated Wasm
wasm-tools validate /tmp/test.wasm

# Print WAT for inspection
wasm-tools print /tmp/test.wasm | head -50

# Check for local.get instructions
wasm-tools print /tmp/test.wasm | grep "local.get"
```

### 5. Stage 1 Impact Check

```bash
# Generate Stage 1 and check for quasiquote errors
sbcl --load build/stage1-complete.lisp 2>&1 | grep -i quasiquote

# Check compilation report
cat dist/stage1-report.json | jq '.blockers | .[] | select(.pattern | contains("quasiquote"))'
```

## Key Files to Modify

| File | Purpose | Lines |
|------|---------|-------|
| `src/clysm/compiler/transform/macro.lisp` | Quasiquote expansion | 264-358 |
| `src/clysm/compiler/codegen/func-section.lisp` | Code generation | 694-729 |

## Key Files to Create (Tests)

| File | Purpose |
|------|---------|
| `tests/unit/quasiquote-local-test.lisp` | Unit tests for expansion and codegen |
| `tests/contract/quasiquote-wasm-test.lisp` | Wasm structure validation |
| `tests/integration/quasiquote-runtime-test.lisp` | End-to-end runtime tests |

## Debugging Tips

### Expansion Issues

```lisp
;; Trace quasiquote expansion
(trace clysm/compiler/transform/macro:expand-backquote)
(trace clysm/compiler/transform/macro:expand-bq)
(trace clysm/compiler/transform/macro:expand-bq-list)

;; Test expansion
(clysm/compiler/transform/macro:expand-backquote '(quasiquote (,x)))
```

### Codegen Issues

```lisp
;; Trace code generation
(trace clysm/compiler/codegen/func-section:compile-quoted-element)
(trace clysm/compiler/codegen/func-section:compile-quoted-list)

;; Compile and inspect instructions
(let ((instrs (clysm/compiler/codegen/func-section:compile-form
               '`(value ,x)
               (clysm/compiler/codegen/func-section:make-cenv))))
  (pprint instrs))
```

### Lexical Environment Issues

```lisp
;; Check variable lookup
(let ((env (clysm/compiler/codegen/func-section:make-cenv)))
  (clysm/compiler/codegen/func-section:env-add-local env 'x)
  (clysm/compiler/codegen/func-section:env-lookup-local env 'x))
;; Should return 0 (first local index)
```

## Success Criteria Verification

| Criterion | Verification Command |
|-----------|---------------------|
| SC-001: No "Cannot compile quoted element" | `sbcl --load build/stage1-complete.lisp 2>&1 \| grep -c "Cannot compile quoted"` (expect 0) |
| SC-002: Wasm validation passes | `wasm-tools validate dist/clysm-stage1.wasm` |
| SC-003: Runtime correctness | Run integration tests |
| SC-004: Stage 1 rate improvement | Compare `dist/stage1-report.json` before/after |
| SC-005: No regression | `sbcl --eval "(asdf:test-system :clysm)"` all pass |
