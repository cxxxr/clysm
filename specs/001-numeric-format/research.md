# Research: Numeric Conversion and Formatting (Phase 14C)

**Date**: 2025-12-30
**Branch**: `001-numeric-format`

## Research Tasks

No NEEDS CLARIFICATION items were identified in the Technical Context. Research focused on:
1. Continued fraction algorithm for `rationalize`
2. Base conversion algorithm for `write-to-string`
3. Existing Clysm patterns to follow

---

## 1. Continued Fraction Algorithm for `rationalize`

### Decision
Use the **mediant-based continued fraction approximation** algorithm, which produces rationals with small denominators that approximate the input float within its precision bounds.

### Rationale
- ANSI CL specifies that [rationalize](../../resources/HyperSpec/Body/f_ration.htm) should return a rational "mathematically equal to or computationally close to" the float
- Unlike `rational` (which returns the exact ratio represented by the float bits), `rationalize` aims for human-readable results
- The mediant algorithm naturally produces reduced fractions with small denominators

### Algorithm (Stern-Brocot Tree / Mediant Method)

```
Input: float x, tolerance epsilon (based on float precision)
Output: ratio p/q where |x - p/q| < epsilon

1. Initialize bounds:
   - lower = (0, 1)  ; fraction 0/1
   - upper = (1, 0)  ; fraction 1/0 (infinity)

2. While not converged:
   - mediant = (lower_num + upper_num, lower_den + upper_den)
   - If mediant approximates x within epsilon: return mediant
   - If mediant > x: upper = mediant
   - Else: lower = mediant

3. Return best approximation found
```

### Alternatives Considered

| Alternative | Rejected Because |
|-------------|------------------|
| Exact bit extraction (`rational`) | Produces large denominators (powers of 2), not user-friendly |
| Simple scaling (multiply by 10^n) | Doesn't minimize denominator size |
| Newton's method | More complex, no denominator minimization guarantee |

### Implementation Notes
- Epsilon derived from float's mantissa precision (~15 decimal digits for f64)
- For negative floats: rationalize absolute value, negate result
- Integer-valued floats (e.g., 3.0) should return integer, not ratio
- Zero returns 0, not 0/1

---

## 2. Base Conversion Algorithm for `write-to-string`

### Decision
Use **repeated division method** with digit extraction, building the string in reverse then reversing at the end.

### Rationale
- Standard algorithm for integer-to-string conversion in any base
- Well-understood, efficient, easy to verify
- Matches [write-to-string](../../resources/HyperSpec/Body/f_wr_to_.htm) ANSI CL semantics

### Algorithm

```
Input: integer n, base b (2-36)
Output: string representation

1. Handle special cases:
   - If n = 0: return "0"
   - If n < 0: prefix "-", convert |n|

2. Build digit string (reverse order):
   - While n > 0:
     - digit = n mod b
     - Push char: '0'-'9' for 0-9, 'A'-'Z' for 10-35
     - n = n / b (integer division)

3. Reverse string, return
```

### Digit Character Mapping

| Digit Value | Character |
|-------------|-----------|
| 0-9 | '0'-'9' (ASCII 48-57) |
| 10-35 | 'A'-'Z' (ASCII 65-90) |

### Type-Specific Handling

| Type | Approach |
|------|----------|
| Fixnum (i31) | Direct division loop on i32 |
| Bignum | Division with limb array (requires bignum div) |
| Ratio | Convert numerator and denominator separately, join with "/" |
| Float | Use decimal representation (existing `prin1` logic) |

### Alternatives Considered

| Alternative | Rejected Because |
|-------------|------------------|
| Lookup table for all digits | Memory overhead for little gain |
| Recursive conversion | Risk of stack overflow for large numbers |
| Printf-style format | Not applicable to WasmGC (no C runtime) |

---

## 3. Existing Clysm Patterns

### Primitive Function Pattern
From `func-section.lisp`:
1. Add function name to primitive list (line 760+)
2. Add dispatch case in `compile-to-instructions`
3. Implement `compile-<function-name>` with signature: `(args env)`

### Type Dispatch Pattern
```lisp
;; From compile-float (line 6138)
(defun compile-xyz (args env)
  (let ((arg-code (compile-form (car args) env)))
    `(,@arg-code
      ;; Type dispatch using block/br_if pattern
      (block $done (result anyref)
        ;; Check fixnum (i31ref)
        (block $not-fixnum
          (br_if $not-fixnum (i32.eqz (ref.test (ref i31) (local.get $temp))))
          ;; fixnum handling...
          (br $done))
        ;; Check ratio (type 15)
        (block $not-ratio
          (br_if $not-ratio (i32.eqz (ref.test (ref 15) (local.get $temp))))
          ;; ratio handling...
          (br $done))
        ;; ... more type checks
        ))))
```

### Keyword Argument Pattern
```lisp
;; From parse-integer (line 6271)
(let* ((kwargs (extract-keyword-args (cdr args)))
       (base (get-keyword-arg :radix kwargs 10)))
  ...)
```

### String Building Pattern
For `write-to-string`, use `$string` struct (type 2) with UTF-8 bytes array.

---

## 4. Test Strategy

### Unit Tests (TDD Red-Green-Refactor)

**rationalize tests**:
```lisp
(ok (eql (rationalize 0.5) 1/2))
(ok (eql (rationalize 3.0) 3))
(ok (eql (rationalize 0.333333) 1/3) "Close approximation")
(ok (eql (rationalize 1/2) 1/2) "Ratio passthrough")
(ok (eql (rationalize 5) 5) "Integer passthrough")
```

**write-to-string tests**:
```lisp
(ok (equal (write-to-string 42 :base 16) "2A"))
(ok (equal (write-to-string 255 :base 16) "FF"))
(ok (equal (write-to-string 42 :base 2) "101010"))
(ok (equal (write-to-string -42 :base 16) "-2A"))
(ok (equal (write-to-string 1/2 :base 10) "1/2"))
```

### Contract Tests (Wasm Validation)
- Verify generated Wasm includes correct type dispatches
- Validate `wasm-tools validate` passes on output

---

## Summary

| Topic | Decision | Confidence |
|-------|----------|------------|
| rationalize algorithm | Mediant/continued fraction | High |
| write-to-string algorithm | Repeated division | High |
| Implementation pattern | Follow existing func-section.lisp | High |
| Type dispatch | block/br_if with ref.test | High |

All research tasks complete. Ready for Phase 1: Design & Contracts.
