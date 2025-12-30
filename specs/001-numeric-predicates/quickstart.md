# Quickstart: Phase 14B - Numeric Type Predicates Enhancement

**Feature**: 001-numeric-predicates
**Date**: 2025-12-30

## Prerequisites

```bash
# Enter development environment
nix develop

# Verify SBCL is available
sbcl --version  # Should be 2.4+

# Verify wasm-tools is available
wasm-tools --version
```

## Implementation Order

Follow this sequence (dependencies noted):

### Phase 1: Bit Testing (no dependencies)

1. **logbitp** - Test if bit at index is set
2. **logtest** - Test if integers share any bits

### Phase 2: Byte Specifiers (no dependencies)

3. **byte** - Create byte specifier
4. **byte-size** - Extract size from specifier
5. **byte-position** - Extract position from specifier

### Phase 3: Byte Operations (depends on Phase 2)

6. **ldb** - Load byte (extract and shift)
7. **mask-field** - Extract without shift
8. **dpb** - Deposit byte
9. **deposit-field** - Deposit without shift

## Key Files to Modify

```
src/clysm/compiler/codegen/func-section.lisp
├── Line ~920: Add dispatch cases
├── Line ~5200+: Add compile-* functions
└── Export list: Add function symbols

src/clysm/validation/feature-registry.lisp
└── Add feature entries for each function
```

## Implementation Template

Follow the pattern from `compile-evenp` (line 4576):

```lisp
(defun compile-logbitp (args env)
  "Compile (logbitp index integer).
   Returns T if bit at index is set, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "logbitp requires exactly 2 arguments"))
  (let ((idx-local (env-add-local env (gensym "LOGBITP-IDX")))
        (int-local (env-add-local env (gensym "LOGBITP-INT"))))
    (append
     ;; Compile and store index
     (compile-to-instructions (first args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set idx-local))
     ;; Compile and store integer
     (compile-to-instructions (second args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set int-local))
     ;; Shift right by index, mask bit 0
     (list (list :local.get int-local))
     (list (list :local.get idx-local))
     '(:i32.shr_u)
     '((:i32.const 1) :i32.and)
     ;; Return T or NIL
     '((:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))
```

## Byte Operations Formula Reference

```lisp
;; ldb: Load byte - extract and shift to position 0
;; (ldb bytespec integer) => (logand (ash integer (- pos)) (1- (ash 1 size)))
(defun compile-ldb (args env)
  ;; mask = (1 << size) - 1
  ;; result = (integer >> position) & mask
  ...)

;; dpb: Deposit byte - replace field with new value
;; (dpb newbyte bytespec integer) => ...
(defun compile-dpb (args env)
  ;; mask = ((1 << size) - 1) << position
  ;; result = (integer & ~mask) | ((newbyte << position) & mask)
  ...)

;; mask-field: Extract keeping position
;; (mask-field bytespec integer) => (logand integer mask)
(defun compile-mask-field (args env)
  ;; mask = ((1 << size) - 1) << position
  ;; result = integer & mask
  ...)

;; deposit-field: Deposit keeping position
;; (deposit-field newbyte bytespec integer) => ...
(defun compile-deposit-field (args env)
  ;; mask = ((1 << size) - 1) << position
  ;; result = (integer & ~mask) | (newbyte & mask)
  ...)
```

## Testing

### TDD Cycle (Constitution VII)

```bash
# 1. Write test first
# Edit tests/unit/numeric-predicates-test.lisp

# 2. Run tests (should FAIL)
sbcl --eval "(asdf:test-system :clysm)"

# 3. Implement function
# Edit src/clysm/compiler/codegen/func-section.lisp

# 4. Run tests (should PASS)
sbcl --eval "(asdf:test-system :clysm)"

# 5. Validate Wasm output
wasm-tools validate dist/test-output.wasm
```

### Example Test Case

```lisp
(deftest test-logbitp
  (testing "logbitp returns T for set bits"
    (ok (equal t (eval-wasm '(logbitp 0 5))))   ; bit 0 of 5 (101) is set
    (ok (equal t (eval-wasm '(logbitp 2 5)))))  ; bit 2 of 5 (101) is set

  (testing "logbitp returns NIL for unset bits"
    (ok (null (eval-wasm '(logbitp 1 5))))      ; bit 1 of 5 (101) is not set
    (ok (null (eval-wasm '(logbitp 3 5))))))    ; bit 3 of 5 (101) is not set
```

## Verification Checklist

- [x] All 10 functions compile without error
- [x] `wasm-tools validate` passes on generated Wasm
- [ ] Unit tests pass for all functions
- [ ] Integration tests verify runtime behavior
- [x] Feature registry updated
- [x] HyperSpec links included in code comments
