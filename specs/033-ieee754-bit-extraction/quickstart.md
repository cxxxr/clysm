# Quickstart: IEEE 754 Bit Extraction

**Feature**: 033-ieee754-bit-extraction
**Date**: 2025-12-27

## TL;DR

Replace SBCL-specific float functions with portable implementations:

1. Create `src/clysm/lib/float-bits.lisp` with portable `single-float-bits` and `double-float-bits`
2. Update `compiler/compiler.lisp` to use new functions
3. Update `compiler/ast.lisp` to use `with-safe-float-ops`
4. Update `tests/helpers.lisp` to use portable trap handling

## Implementation Order

### Step 1: Create Portable Float Utilities (TDD)

```lisp
;; tests/unit/float-bits-test.lisp
(deftest single-float-bits-test
  (ok (= (single-float-bits 2.5f0) #x40200000))
  (ok (= (single-float-bits -0.0f0) #x80000000)))

(deftest double-float-bits-test
  (ok (= (double-float-bits 3.14d0) #x40091EB851EB851F)))

(deftest special-values-test
  (ok (= (double-float-bits (/ 1.0d0 0.0d0)) #x7FF0000000000000)) ; +Inf
  (ok (= (double-float-bits (/ -1.0d0 0.0d0)) #xFFF0000000000000))) ; -Inf
```

### Step 2: Implement Core Functions

```lisp
;; src/clysm/lib/float-bits.lisp

(defun float-nan-p (x)
  "Portable NaN detection: NaN is never equal to itself."
  (and (floatp x) (/= x x)))

(defun float-infinity-p (x)
  "Portable infinity detection."
  (and (floatp x)
       (not (zerop x))
       (= x (* x 2))))

(defun double-float-bits (value)
  "Convert double-float to IEEE 754 64-bit representation."
  (cond
    ;; Special: NaN
    ((float-nan-p value)
     #x7FF8000000000000)  ; Canonical quiet NaN
    ;; Special: +Infinity
    ((and (float-infinity-p value) (plusp value))
     #x7FF0000000000000)
    ;; Special: -Infinity
    ((and (float-infinity-p value) (minusp value))
     #xFFF0000000000000)
    ;; Normal/Subnormal/Zero: Use integer-decode-float
    (t
     (multiple-value-bind (significand exponent sign)
         (integer-decode-float value)
       (let* ((sign-bit (if (minusp sign) 1 0))
              (ieee-exp (+ exponent 52 1023))
              (mantissa (logand significand #xFFFFFFFFFFFFF)))
         (cond
           ;; Zero (positive or negative)
           ((zerop significand)
            (ash sign-bit 63))
           ;; Subnormal
           ((< ieee-exp 1)
            (logior (ash sign-bit 63)
                    (ash significand (+ ieee-exp -1))))
           ;; Normal
           (t
            (logior (ash sign-bit 63)
                    (ash ieee-exp 52)
                    mantissa))))))))
```

### Step 3: Update Compiler

```lisp
;; compiler/compiler.lisp - Replace emit-f32/emit-f64

(defun emit-f64 (value buffer)
  "Emit IEEE 754 double-precision float in little-endian."
  (let ((bits (double-float-bits (coerce value 'double-float))))
    (dotimes (i 8)
      (vector-push-extend (logand (ash bits (* i -8)) #xFF) buffer))))
```

### Step 4: Update Constant Folding

```lisp
;; compiler/ast.lisp - Replace fold-arithmetic

(defun fold-arithmetic (op values)
  (with-safe-float-ops
    (case op
      (+ (reduce #'+ values :initial-value 0))
      (* (reduce #'* values :initial-value 1))
      (- (if (= 1 (length values))
             (- (first values))
             (reduce #'- values)))
      (/ (if (= 1 (length values))
             (/ 1 (first values))
             (reduce #'/ values))))))
```

## Verification

```bash
# Run tests
cd /home/user/src/clysm-workbench/clysm3
nix develop --command sbcl --load tests/run.lisp

# Verify no SBCL-specific code remains
grep -r "sb-kernel:.*float-bits" src/
grep -r "sb-int:with-float-traps-masked" src/
# Both should return empty

# Validate Wasm output unchanged
wasm-tools validate output.wasm
```

## Key Files

| File | Action | Description |
|------|--------|-------------|
| `src/clysm/lib/float-bits.lisp` | CREATE | Portable float-to-bits utilities |
| `src/clysm/compiler/compiler.lisp` | MODIFY | Use new emit-f32/emit-f64 |
| `src/clysm/compiler/ast.lisp` | MODIFY | Use with-safe-float-ops |
| `tests/helpers.lisp` | MODIFY | Use portable trap handling |
| `tests/unit/float-bits-test.lisp` | CREATE | Unit tests for new module |
