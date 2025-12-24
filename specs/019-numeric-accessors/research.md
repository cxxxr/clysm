# Research: Numeric Accessors and Float Special Values

**Feature**: 019-numeric-accessors
**Date**: 2025-12-24

## Summary

This research covers implementation details for `numerator`/`denominator` accessor functions and IEEE 754 special value handling in the Clysm compiler.

## 1. Wasm struct.get Instruction for Field Access

### Decision
Use `(:struct.get <type-index> <field-index>)` pattern to extract ratio fields.

### Rationale
The existing codebase uses this pattern extensively for cons cells (`+type-cons+`), symbols (`+type-symbol+`), bignums (`+type-bignum+`), and binding frames. The instruction is already mapped in func-section.lisp.

### Evidence
```lisp
;; From func-section.lisp:175
(:struct.get . (#xFB #x02))  ; Wasm GC struct.get opcode

;; Pattern for cons car extraction (func-section.lisp:1033)
(:struct.get ,clysm/compiler/codegen/gc-types:+type-cons+ 0)  ; car = field 0

;; Pattern for cons cdr extraction (func-section.lisp:1058)
(:struct.get ,clysm/compiler/codegen/gc-types:+type-cons+ 1)  ; cdr = field 1
```

### Implementation for Ratio
```lisp
;; Ratio type has fields: (numerator, denominator) at indices (0, 1)
;; From gc-types.lisp:290-305
(make-wasm-struct-type
  :name '$ratio
  :index +type-ratio+  ; 15
  :fields (list (make-wasm-field :name 'numerator :type :anyref :mutable nil)
                (make-wasm-field :name 'denominator :type :anyref :mutable nil)))

;; To extract numerator:
(:local.get 0)  ; argument
(:ref.cast (:ref ,+type-ratio+))  ; cast to ratio
(:struct.get ,+type-ratio+ 0)     ; get numerator field

;; To extract denominator:
(:local.get 0)  ; argument
(:ref.cast (:ref ,+type-ratio+))  ; cast to ratio
(:struct.get ,+type-ratio+ 1)     ; get denominator field
```

### Alternatives Considered
- Direct anyref field access: Not safe without type cast
- Memory-based access: Violates WasmGC-First constitution principle

---

## 2. IEEE 754 NaN Comparison in Wasm f64

### Decision
Rely on Wasm's native f64 comparison instructions which follow IEEE 754 semantics.

### Rationale
Wasm f64 comparison instructions (f64.eq, f64.lt, f64.gt, f64.le, f64.ge) inherently follow IEEE 754:
- `f64.eq` returns 0 (false) when either operand is NaN
- `f64.lt`, `f64.gt`, etc. return 0 (false) when either operand is NaN
- Infinity comparisons work correctly: +Inf == +Inf, +Inf > any finite

### Evidence
From WebAssembly spec:
- "The comparison is performed according to the IEEE 754 semantics"
- "If either operand is NaN, the operation returns 0 (false) for eq/ne and 0 for ordered comparisons"

### Implementation
Current `compile-comparison-op` in func-section.lisp only handles fixnums:
```lisp
;; Current (fixnum-only):
(append
  (compile-to-instructions (first args) env)
  '((:ref.cast :i31) :i31.get_s)
  (compile-to-instructions (second args) env)
  '((:ref.cast :i31) :i31.get_s)
  (list op)  ; :i32.eq, :i32.lt_s, etc.
  ...)
```

Need to extend with float dispatch:
```lisp
;; For floats:
(:local.get 0)
(:ref.cast (:ref ,+type-float+))
(:struct.get ,+type-float+ 0)  ; extract f64 value
(:local.get 1)
(:ref.cast (:ref ,+type-float+))
(:struct.get ,+type-float+ 0)  ; extract f64 value
:f64.eq  ; or :f64.lt, :f64.gt, etc.
;; Result is i32 (0/1), convert to Lisp boolean
```

The key insight: **No special NaN handling code needed** - Wasm f64 instructions do the right thing automatically.

### Alternatives Considered
- Manual NaN bit-pattern checking: Unnecessary complexity
- Special NaN sentinel value: Violates IEEE 754 compliance

---

## 3. Float Special Value Generation

### Decision
Wasm f64 arithmetic naturally produces IEEE 754 special values.

### Rationale
Wasm's f64 operations follow IEEE 754:
- `f64.div(1.0, 0.0)` → +Infinity
- `f64.div(-1.0, 0.0)` → -Infinity
- `f64.sub(+Inf, +Inf)` → NaN
- `f64.mul(0.0, +Inf)` → NaN

### Evidence
Current float division code generates:
```lisp
(:f64.const 1.0)
(:f64.const 0.0)
:f64.div  ; produces +Infinity automatically
```

### Implementation
Verify existing float arithmetic codegen produces correct f64 instructions:
1. `(/ 1.0 0.0)` must compile to `f64.div` (not error)
2. Result must be boxed in $FLOAT struct
3. Comparison with infinity must work via f64 comparison ops

### Concern: Constant Folding
If the compiler performs constant folding:
- `(/ 1.0 0.0)` might be evaluated at compile time
- Common Lisp `(/ 1.0 0.0)` signals DIVISION-BY-ZERO
- But in Clysm targeting Wasm, we want IEEE 754 infinity

Resolution: Compiler should NOT fold float division by zero at compile time, or should fold to infinity representation.

---

## 4. Double-Float Precision Preservation

### Decision
Already implemented correctly in tokenizer and codegen.

### Rationale
The tokenizer parses all floats as `double-float` (CL's internal representation), and the compiler generates f64 constants.

### Evidence
```lisp
;; From tokenizer.lisp:309
(let ((result (* sign mantissa (expt 10.0d0 exp-value))))
  (list :float result line column))
;; Note: 10.0d0 forces double precision

;; From func-section.lisp:413
(list (list :f64.const (coerce value 'double-float))
```

### Implementation
No changes needed - precision is already preserved.

---

## 5. Type Dispatch for numerator/denominator

### Decision
Implement multi-type dispatch: integer → return self/1, ratio → extract field.

### Rationale
ANSI CL specifies:
- `(numerator 5)` → 5
- `(denominator 5)` → 1
- `(numerator 1/3)` → 1
- `(denominator 1/3)` → 3
- Type error for floats/complex

### Implementation
```lisp
(defun compile-numerator (args env)
  "Compile (numerator x) - ANSI CL numerator accessor."
  ;; Type dispatch:
  ;; - fixnum: return itself
  ;; - bignum: return itself
  ;; - ratio: return numerator field
  ;; - other: type error
  `((:local.get 0)
    (:block (:result :anyref)
      ;; Try fixnum
      (:block
        (:br_on_cast 1 :anyref :i31))
      ;; Try bignum
      (:block
        (:br_on_cast 1 :anyref (:ref ,+type-bignum+)))
      ;; Try ratio
      (:block
        (:local.get 0)
        (:br_on_cast_fail 0 :anyref (:ref ,+type-ratio+))
        (:struct.get ,+type-ratio+ 0)  ; numerator field
        (:br 3))
      ;; Type error
      :unreachable)))

(defun compile-denominator (args env)
  "Compile (denominator x) - ANSI CL denominator accessor."
  ;; Type dispatch:
  ;; - fixnum: return 1
  ;; - bignum: return 1
  ;; - ratio: return denominator field
  ;; - other: type error
  `((:local.get 0)
    (:block (:result :anyref)
      ;; Try fixnum - return 1
      (:block
        (:local.get 0)
        (:br_on_cast_fail 0 :anyref :i31)
        :drop
        (:i32.const 1) :ref.i31
        (:br 1))
      ;; Try bignum - return 1
      (:block
        (:local.get 0)
        (:br_on_cast_fail 0 :anyref (:ref ,+type-bignum+))
        :drop
        (:i32.const 1) :ref.i31
        (:br 1))
      ;; Try ratio
      (:block
        (:local.get 0)
        (:br_on_cast_fail 0 :anyref (:ref ,+type-ratio+))
        (:struct.get ,+type-ratio+ 1)  ; denominator field
        (:br 1))
      ;; Type error
      :unreachable)))
```

---

## Summary of Research Findings

| Topic | Status | Key Finding |
|-------|--------|-------------|
| struct.get for ratio | Resolved | Use existing pattern with `+type-ratio+` index 15 |
| NaN comparison | Resolved | Wasm f64 comparisons are IEEE 754 compliant - no special code needed |
| Special value generation | Resolved | f64 arithmetic produces infinity/NaN automatically |
| Double precision | Resolved | Already preserved via `double-float` and `:f64.const` |
| Type dispatch | Resolved | Use `br_on_cast` pattern for numerator/denominator |
