# Research: ANSI Common Lisp Numeric Functions

**Branch**: `001-ansi-numeric-functions` | **Date**: 2025-12-29

## Research Questions

1. What FFI math infrastructure already exists?
2. How should bitwise operations be implemented?
3. How should complex numbers be represented?
4. What ANSI CL edge cases need special handling?

---

## R1: Existing FFI Math Infrastructure

### Decision: Extend Existing Pattern

**Current State** (already implemented in `src/clysm/ffi/import-gen.lisp`):
- `*math-function-specs*` defines imports from `clysm:math` module
- Already covers: sin, cos, tan, asin, acos, atan, atan2, sinh, cosh, tanh, asinh, acosh, atanh, exp, log, log10, pow
- Host shim (`host-shim/math-shim.js`) exports these as JavaScript Math.* wrappers

**Codegen Pattern** (in `src/clysm/compiler/codegen/func-section.lisp`):
- `compile-unary-math-ffi` helper handles single-argument transcendental functions
- Handles type coercion (i31ref fixnum -> f64, $float -> f64)
- Returns result wrapped in `$float` struct

**What's Missing**:
- `sqrt` needs to be added to compilation (FFI import exists)
- `abs` and `signum` for both integer and float types
- No `expt` implementation yet (distinct from `pow`)

**Alternatives Considered**:
1. Taylor series in Wasm - Rejected (10x slower per existing research comment)
2. WASI math functions - Rejected (not available in browser, Node.js shim preferred)

---

## R2: Bitwise Operations Implementation

### Decision: Native Wasm Instructions

**Rationale**: Bitwise operations are integer-only and map directly to Wasm i32/i64 instructions with no FFI overhead.

**Wasm Instructions Available**:
- `i32.and`, `i32.or`, `i32.xor` for logand/logior/logxor
- `i32.shl`, `i32.shr_s` for ash (shift left, arithmetic shift right)
- `i32.clz`, `i32.ctz`, `i32.popcnt` for bit counting

**Implementation Strategy**:

| Function | Wasm Implementation |
|----------|---------------------|
| `ash` | `i32.shl` (positive count) / `i32.shr_s` (negative count) |
| `logand` | `i32.and` for two args, fold for more |
| `logior` | `i32.or` for two args, fold for more |
| `logxor` | `i32.xor` for two args, fold for more |
| `lognot` | `i32.xor` with -1 (two's complement) |
| `logcount` | `i32.popcnt` (for positive), invert for negative |

**Edge Cases**:
- No-arg `logand` returns -1 (all bits set - identity for AND)
- No-arg `logior` returns 0 (identity for OR)
- No-arg `logxor` returns 0 (identity for XOR)
- Large integers (beyond i32) need bignum support (defer to future)

---

## R3: Complex Number Representation

### Decision: WasmGC Struct Type

**Per Constitution Principle I** (WasmGC-First Type System): All Lisp objects must use WasmGC type hierarchy.

**Proposed Type Definition**:
```wat
(type $complex (struct
  (field $real (ref $float))    ;; Real part
  (field $imag (ref $float))))  ;; Imaginary part
```

**Type Index**: Reserve index 9 for `$complex` (following existing type indices 0-8)

**Rationale**:
- Follows existing pattern for $float (index 4) and $ratio (index 5)
- GC-managed, no linear memory needed
- Real and imaginary parts are $float references for IEEE 754 precision

**Alternative Considered**:
- Inline f64 fields instead of $float refs - Rejected (inconsistent with Lisp type system, would need separate unboxing logic)

**Type Predicates**:
- `complexp` tests for $complex struct type
- `realp` returns true for integer, float, ratio (not complex)
- `numberp` returns true for all numeric types including complex

---

## R4: ANSI CL Edge Cases

### Decision: Mathematical Continuation Approach

**Per Spec Assumption A-005**: Return complex results rather than signaling errors for out-of-domain inputs.

**Edge Case Matrix**:

| Function | Edge Case | ANSI Behavior | Implementation |
|----------|-----------|---------------|----------------|
| `asin` | \|x\| > 1 | Complex result | Return $complex |
| `acos` | \|x\| > 1 | Complex result | Return $complex |
| `acosh` | x < 1 | Complex result | Return $complex |
| `atanh` | \|x\| >= 1 | Complex result | Return $complex |
| `log` | x = 0 | Error | Signal division-by-zero |
| `log` | x < 0 | Complex result | Return $complex |
| `sqrt` | x < 0 | Complex result | Return $complex |
| `expt` | 0^0 | 1 | Return 1 |
| `gcd` | () | 0 | Return 0 |
| `gcd` | n 0 | \|n\| | Return abs(n) |
| `lcm` | () | 1 | Return 1 |
| `lcm` | n 0 | 0 | Return 0 |

**Special Values (IEEE 754)**:

| Value | Handling |
|-------|----------|
| +Inf | Propagate per IEEE 754 semantics |
| -Inf | Propagate per IEEE 754 semantics |
| NaN | Propagate per IEEE 754 semantics |

---

## R5: Function Categories and Dependencies

### Implementation Order (by dependency)

1. **Foundation** (no dependencies):
   - abs, signum for integers and floats
   - max, min (comparison operators exist)

2. **Bitwise** (integer-only):
   - logand, logior, logxor, lognot, ash, logcount

3. **Integer Math**:
   - gcd, lcm (Euclidean algorithm)

4. **Complex Type**:
   - Define $complex WasmGC type
   - complex constructor
   - realpart, imagpart, conjugate

5. **Transcendental with Complex**:
   - Extend sqrt for negative numbers
   - Extend asin, acos for out-of-domain
   - phase (uses atan2)

6. **Power Functions**:
   - expt (uses existing pow FFI)

---

## Summary

| Research Question | Decision | Rationale |
|-------------------|----------|-----------|
| FFI math infrastructure | Extend existing pattern | Proven 10x faster than Wasm math |
| Bitwise operations | Native Wasm i32 ops | Zero FFI overhead |
| Complex representation | WasmGC struct | Follows Constitution Principle I |
| Edge case handling | Mathematical continuation | Returns complex instead of error |

**All NEEDS CLARIFICATION items resolved**. Ready for Phase 1 design.
