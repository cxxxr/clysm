# Research: Phase 14A - Basic Arithmetic Function Extension

**Date**: 2025-12-30
**Branch**: `002-numeric-functions`

## Research Summary

This document consolidates findings from codebase exploration and design decisions for implementing 26 numeric functions in clysm3.

---

## 1. FFI Infrastructure for Math Functions

### Decision: Use Existing Import System

**Rationale**: The `src/clysm/ffi/import-gen.lisp` already defines a comprehensive math function import system under `"clysm:math"` module.

**Existing Functions** (already registered in `*math-function-specs*`):
- Trigonometric: sin, cos, tan, asin, acos, atan, atan2
- Hyperbolic: sinh, cosh, tanh, asinh, acosh, atanh
- Exponential: exp, log, log10, pow

**Alternatives Considered**:
1. Implement transcendental functions in pure Wasm - Rejected: Would require Taylor series approximations, slower than native JS Math
2. Add new WASI imports - Rejected: WASI doesn't define math functions; would require custom extension

**Implementation Path**:
1. Verify all required functions are in `*math-function-specs*`
2. Add missing functions if any (atan2 for two-arg atan)
3. Write compile-time dispatch in `func-section.lisp` to route calls to FFI

---

## 2. Native Wasm Instruction Mapping

### Decision: Use Direct i32 Instructions for Bit Operations

**Rationale**: Wasm provides efficient bit manipulation instructions that map directly to ANSI CL semantics.

**Instruction Mapping**:

| CL Function | Wasm i32 | Wasm i64 | Notes |
|-------------|----------|----------|-------|
| [ash](resources/HyperSpec/Body/f_ash.htm) | i32.shl, i32.shr_s | i64.shl, i64.shr_s | Sign-aware shift |
| [logand](resources/HyperSpec/Body/f_logand.htm) | i32.and | i64.and | Variadic, identity: -1 |
| [logior](resources/HyperSpec/Body/f_logand.htm) | i32.or | i64.or | Variadic, identity: 0 |
| [logxor](resources/HyperSpec/Body/f_logand.htm) | i32.xor | i64.xor | Variadic, identity: 0 |
| [lognot](resources/HyperSpec/Body/f_logand.htm) | i32.xor -1 | i64.xor -1 | Already implemented |
| [logcount](resources/HyperSpec/Body/f_logcou.htm) | i32.popcnt | i64.popcnt | Population count |

**Alternatives Considered**:
1. FFI to JavaScript bitwise ops - Rejected: JS uses 32-bit signed integers, truncates 64-bit
2. Software implementation - Rejected: Wasm native instructions are faster

---

## 3. Domain Error Handling Strategy

### Decision: Return IEEE 754 Special Values (NaN/Infinity)

**Rationale**: JavaScript Math API already returns IEEE 754 special values for domain errors. This approach:
- Allows calculations to continue without signaling
- Matches behavior of most numeric libraries
- Simplifies implementation (no error signaling infrastructure needed)

**Behavior by Function**:

| Operation | Input | Result |
|-----------|-------|--------|
| [sqrt](resources/HyperSpec/Body/f_sqrt_.htm) | -1 | NaN |
| [log](resources/HyperSpec/Body/f_log.htm) | 0 | -Infinity |
| [log](resources/HyperSpec/Body/f_log.htm) | -1 | NaN |
| [asin](resources/HyperSpec/Body/f_asin_.htm) | 2 | NaN |
| [acosh](resources/HyperSpec/Body/f_sinh_.htm) | 0.5 | NaN |
| [atanh](resources/HyperSpec/Body/f_sinh_.htm) | 1 | Infinity |

**Alternatives Considered**:
1. Signal CL conditions - Rejected: Adds complexity; not required for 50% compliance
2. Configurable behavior - Rejected: Over-engineering for Phase 14A scope

---

## 4. Bignum Support for Bit Operations

### Decision: Limit Initial Scope to 64-bit Integers

**Rationale**:
- Most practical use cases fit within 64-bit range
- Bignum bit operations require complex limb-by-limb processing
- 64-bit coverage achieves 50%+ test compliance goal
- Can extend to bignum in future phase

**Implementation Strategy**:
1. Use i32 for values fitting in fixnum range (30-bit signed)
2. Promote to i64 for values exceeding fixnum but within 64-bit
3. Signal error or return truncated result for bignums (document limitation)

**Test Coverage**:
- Focus tests on 32-bit and 64-bit boundary cases
- Include overflow detection tests
- Document bignum limitation in spec

**Alternatives Considered**:
1. Full bignum support - Deferred: Significant complexity; not required for 50% goal
2. Arbitrary precision - Deferred: Would require GMP-style library

---

## 5. Float-to-Rational Conversion Algorithm

### Decision: Use Continued Fraction Algorithm

**Rationale**: Standard algorithm for converting floating-point to rational with controlled precision.

**Algorithm Sketch**:
```
(defun float-to-rational (x &key (max-iterations 100) (tolerance 1e-15))
  "Convert float to rational using continued fraction expansion"
  (let ((sign (if (minusp x) -1 1))
        (x (abs x)))
    (multiple-value-bind (int frac) (truncate x)
      (if (< frac tolerance)
          (* sign int)
          (let ((cf (continued-fraction frac max-iterations tolerance)))
            (* sign (+ int (convergent cf))))))))
```

**Precision Guarantees**:
- Double precision (64-bit) floats have ~15-17 significant digits
- Rational conversion preserves exact representation when possible
- For repeating decimals, returns closest rational within tolerance

**Alternatives Considered**:
1. Simple multiply-and-round - Rejected: Loses precision for non-dyadic rationals
2. Stern-Brocot tree - Rejected: Less efficient for IEEE 754 floats

---

## 6. Parse-Integer Implementation

### Decision: Full ANSI CL Compliance with State Machine

**Rationale**: [parse-integer](resources/HyperSpec/Body/f_parse_.htm) has well-defined behavior that requires careful state handling.

**Implementation Requirements**:

1. **Keyword Arguments**:
   - `:start` - Starting index (default 0)
   - `:end` - Ending index (default string length)
   - `:radix` - Number base 2-36 (default 10)
   - `:junk-allowed` - Whether to allow trailing non-digits (default nil)

2. **Return Values**:
   - Primary: Parsed integer or NIL (if junk-allowed and no valid integer)
   - Secondary: Index where parsing stopped

3. **State Machine**:
   ```
   [START] --whitespace--> [START]
   [START] --sign(+/-)--> [SIGN_SEEN]
   [START] --digit--> [DIGITS]
   [SIGN_SEEN] --digit--> [DIGITS]
   [DIGITS] --digit--> [DIGITS]
   [DIGITS] --whitespace--> [TRAILING_WS]
   [DIGITS] --end--> [DONE]
   [TRAILING_WS] --whitespace--> [TRAILING_WS]
   [TRAILING_WS] --end--> [DONE]
   ```

4. **Radix Digit Mapping**:
   - 0-9 for digits 0-9
   - A-Z (case-insensitive) for digits 10-35

**Test Cases from ANSI CL**:
```lisp
(parse-integer "123")              → 123, 3
(parse-integer "   123   ")        → 123, 9  ; with trailing ws
(parse-integer "FF" :radix 16)     → 255, 2
(parse-integer "1101" :radix 2)    → 13, 4
(parse-integer "+42")              → 42, 3
(parse-integer "-789")             → -789, 4
(parse-integer "abc" :junk-allowed t) → NIL, 0
(parse-integer "123abc" :junk-allowed t) → 123, 3
(parse-integer "123abc" :junk-allowed nil) → ERROR
```

---

## 7. Two-Argument Atan Implementation

### Decision: Use atan2 for Two-Argument Form

**Rationale**: The [atan](resources/HyperSpec/Body/f_asin_.htm) function has two forms:
- `(atan y)` - Returns arctangent of y
- `(atan y x)` - Returns arctangent of y/x, accounting for quadrant

JavaScript's `Math.atan2(y, x)` directly implements the two-argument form.

**Implementation**:
```lisp
(defun compile-atan (args env)
  (case (length args)
    (1 ;; Single argument: use Math.atan
     (compile-ffi-call 'atan args env))
    (2 ;; Two arguments: use Math.atan2
     (compile-ffi-call 'atan2 args env))
    (t (error "atan requires 1 or 2 arguments"))))
```

---

## 8. Two-Argument Log Implementation

### Decision: Compute log_base(x) as log(x)/log(base)

**Rationale**: The [log](resources/HyperSpec/Body/f_log.htm) function has two forms:
- `(log x)` - Natural logarithm
- `(log x base)` - Logarithm base `base`

JavaScript only provides `Math.log` (natural log) and `Math.log10`.

**Implementation**:
```lisp
(defun compile-log (args env)
  (case (length args)
    (1 ;; Natural log
     (compile-ffi-call 'log args env))
    (2 ;; Log with base: log(x)/log(base)
     (let ((x (first args))
           (base (second args)))
       (compile-division
        (compile-ffi-call 'log (list x) env)
        (compile-ffi-call 'log (list base) env))))
    (t (error "log requires 1 or 2 arguments"))))
```

---

## 9. Abs and Signum Type Preservation

### Decision: Preserve Numeric Type in Result

**Rationale**: Per ANSI CL:
- [abs](resources/HyperSpec/Body/f_abs.htm) returns same type as input
- [signum](resources/HyperSpec/Body/f_signum.htm) returns -1, 0, or 1 of same type as input

**Type Dispatch Implementation**:

```lisp
;; abs
(defun compile-abs (arg env)
  (with-type-dispatch arg env
    (fixnum (compile-fixnum-abs arg env))     ; i32.abs via conditional
    (float  (compile-float-abs arg env))       ; f64.abs instruction
    (ratio  (compile-ratio-abs arg env))       ; abs numerator
    (complex (compile-complex-abs arg env))))  ; magnitude

;; signum
(defun compile-signum (arg env)
  (with-type-dispatch arg env
    (fixnum (compile-fixnum-signum arg env))  ; returns fixnum -1, 0, 1
    (float  (compile-float-signum arg env)))) ; returns float -1.0, 0.0, 1.0
```

---

## 10. Existing Infrastructure Findings

### Already Implemented (Partial or Full)

| Function | Status | Location |
|----------|--------|----------|
| lognot | Full | func-section.lisp:2818 |
| logcount | Infrastructure | numeric-runtime.lisp |
| abs | Partial | Needs type dispatch extension |
| +, -, *, / | Full | func-section.lisp:904-912 |

### FFI Imports Already Registered

Location: `src/clysm/ffi/import-gen.lisp:370-423`

All trigonometric and hyperbolic functions are already in `*math-function-specs*`:
- sin, cos, tan, asin, acos, atan, atan2
- sinh, cosh, tanh, asinh, acosh, atanh
- exp, log, log10, pow

### Host Shim Already Provides

Location: `host-shim/math-shim.js`

JavaScript bindings for all Math API functions are exported.

---

## Conclusion

All major design decisions are resolved:

1. **FFI Functions**: Use existing import infrastructure
2. **Bit Operations**: Native Wasm instructions
3. **Domain Errors**: IEEE 754 special values
4. **Integer Range**: 64-bit scope for Phase 14A
5. **Float→Rational**: Continued fraction algorithm
6. **Parse-Integer**: Full ANSI CL state machine
7. **Two-arg atan/log**: Special dispatch to atan2 / division

No NEEDS CLARIFICATION items remain. Ready for Phase 1 design artifacts.
