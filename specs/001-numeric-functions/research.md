# Research: ANSI Numeric Functions Extension

**Date**: 2025-12-28
**Branch**: `001-numeric-functions`
**Purpose**: Resolve technical decisions before implementation

## Research Questions

### Q1: How to implement transcendental functions (sin, cos, tan, exp, log) in WasmGC?

**Decision**: Import from host via FFI

**Rationale**:
- WebAssembly has NO native transcendental function instructions
- Research shows importing from host is ~10x faster than implementing in Wasm
- clysm3 already has mature FFI infrastructure (`src/clysm/ffi/import-gen.lisp`)
- Host implementations are JIT-optimized by browser/runtime engines

**Alternatives Considered**:

| Alternative | Pros | Cons | Why Rejected |
|-------------|------|------|--------------|
| Taylor series in Wasm | Self-contained, no host dependency | 10x slower, larger code size, precision issues | Performance unacceptable |
| Chebyshev polynomials | Good code size (~137 bytes), moderate accuracy | Still slower than host, 1e-4 max error | Error threshold may not meet ANSI requirements |
| CORDIC algorithm | No multiplications needed | Complex to implement, moderate speed | Over-engineering for this use case |

**Implementation Approach**:
1. Create `math` module imports for: sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, exp, log, sqrt
2. Add host shim (`host-shim/math-shim.js`) providing Math.* functions
3. Use existing FFI import generation from `import-gen.lisp`

**Sources**:
- [Trigonometric Functions in WebAssembly](https://www.nhatcher.com/post/should_i_import_or_should_i_roll/) - Benchmark showing 10x performance difference
- [WebAssembly Math Functions Gist](https://gist.github.com/going-digital/02e46c44d89237c07bc99cd440ebfa43) - Chebyshev implementation reference

---

### Q2: Which Wasm instructions are available for bitwise operations?

**Decision**: Use native i32/i64 instructions where possible

**Available Instructions**:

| Operation | Wasm Instruction | Notes |
|-----------|------------------|-------|
| `logand` | `i32.and` / `i64.and` | Already implemented |
| `logior` | `i32.or` / `i64.or` | Already implemented |
| `logxor` | `i32.xor` / `i64.xor` | Already implemented |
| `lognot` | `i32.xor -1` | Already implemented (no native NOT) |
| `ash` (left) | `i32.shl` / `i64.shl` | Already implemented |
| `ash` (right) | `i32.shr_s` / `i64.shr_s` | Already implemented |
| `integer-length` | `32 - i32.clz` | NEW: count leading zeros |
| `logcount` | `i32.popcnt` / `i64.popcnt` | NEW: population count |

**Implementation Approach**:
- `integer-length`: `(i32.sub (i32.const 32) (i32.clz <value>))`
- `logcount`: Direct `i32.popcnt` for positive numbers; for negative, count 0-bits

---

### Q3: What precision/tolerance should be used for floating-point comparisons?

**Decision**: IEEE 754 double precision with epsilon = 1e-10 for test comparisons

**Rationale**:
- WasmGC f64 is IEEE 754 double precision (~15-17 significant digits)
- Host Math functions return IEEE 754 compliant results
- Test tolerance of 1e-10 allows for accumulated rounding while catching significant errors

**ANSI CL Requirements**:
- ANSI does not mandate specific precision beyond "implementation-defined"
- IEEE 754 double is the de facto standard and sufficient for most use cases

---

### Q4: How should functions behave when results are complex but complex type isn't available?

**Decision**: Implement complex type as part of this feature (P3), required for P1/P2 completeness

**Rationale**:
- `(sqrt -1)` MUST return `#C(0 1)` per ANSI CL
- Complex type struct already defined in `gc-types.lisp` (index 17)
- Without complex support, several P1/P2 tests would fail

**Implementation Order**:
1. Basic functions (abs, signum, max, min, gcd, lcm) - no complex dependency
2. Bitwise operations (logcount, integer-length) - integer only
3. Complex number infrastructure (complex, realpart, imagpart)
4. Trigonometric functions (may return complex for edge cases)
5. Mathematical functions (sqrt, expt may return complex)
6. Hyperbolic functions

---

### Q5: Should bitwise operations support bignums?

**Decision**: 64-bit integers only for Phase 14A; bignum support deferred

**Rationale**:
- WasmGC native i64 provides 64-bit precision
- Bignum bitwise ops require arbitrary-precision algorithms
- 64-bit covers majority of practical use cases
- Bignum support can be added in future phase without API changes

**Behavior for Overflow**:
- If result exceeds 64 bits, signal `arithmetic-error` condition
- Document as implementation restriction

---

### Q6: What is the existing pattern for adding new primitive functions?

**Decision**: Follow established `compile-primitive-call` pattern

**Pattern from `func-section.lisp`**:

```lisp
;; 1. Add case in compile-primitive-call (line ~860)
(case operator
  (my-func (compile-my-func args env))
  ...)

;; 2. Define compilation function
(defun compile-my-func (args env)
  "Compile (my-func arg1 arg2 ...) to Wasm instructions."
  (let ((compiled-args (mapcar (lambda (a) (compile-expression a env)) args)))
    ;; Generate Wasm instructions
    ...))

;; 3. Add test in tests/unit/
(deftest test-my-func-basic
  "my-func basic case"
  (ok (= expected (clysm/tests:compile-and-run-numeric '(my-func args)))
      "assertion"))
```

---

## Resolved Clarifications

| Issue | Resolution |
|-------|------------|
| Transcendental implementation | Import from host via FFI |
| Float precision | IEEE 754 double, 1e-10 test tolerance |
| Complex dependency | Implement as part of feature, not deferred |
| Bignum bitwise | 64-bit only, bignum deferred |
| Integer-length impl | Use i32.clz |
| logcount impl | Use i32.popcnt |

## Key Files for Implementation

| File | Purpose | Changes Needed |
|------|---------|----------------|
| `src/clysm/compiler/codegen/func-section.lisp` | Primary codegen | Add 30+ primitive cases |
| `src/clysm/ffi/import-gen.lisp` | FFI imports | Add math module imports |
| `host-shim/math-shim.js` | NEW | Provide Math.* to Wasm |
| `tests/helpers.lisp` | Test framework | May need tolerance helpers |

## Dependencies

- **Existing**: gc-types.lisp (complex type 17), FFI infrastructure
- **New**: math-shim.js host integration

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Host math import perf variance | Low | Medium | Document runtime requirements |
| Complex number edge cases | Medium | Medium | Comprehensive test suite |
| 64-bit overflow scenarios | Low | Low | Error signaling with clear message |
