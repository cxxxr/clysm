# Research: Type Predicates and Numeric Predicates

**Feature**: 023-type-predicates
**Date**: 2025-12-26

## Research Questions

### RQ1: How does the existing type dispatch system work?

**Decision**: Use `ref.test` for type checking, `ref.cast` for downcasting, and `i31.get_s` for fixnum extraction.

**Rationale**: This is the established pattern in `func-section.lisp` used by existing predicates (consp, null, atom, listp, stringp). The pattern produces efficient Wasm code with single-instruction type tests.

**Alternatives Considered**:
- Type tags in linear memory: REJECTED - violates Constitution I (WasmGC-First)
- Runtime type table lookup: REJECTED - unnecessary indirection, ref.test is O(1)

### RQ2: How are T and NIL represented?

**Decision**:
- T = `(i32.const 1) ref.i31` (fixnum 1 as i31ref)
- NIL = `ref.null :none` (null reference)

**Rationale**: Established in `func-section.lisp:256-262`. NIL as ref.null allows `ref.is_null` for null checks, while T as i31:1 is consistent with other fixnum representations.

**Alternatives Considered**:
- NIL as singleton struct: Considered in Constitution but ref.null simpler for predicates
- T as global constant: REJECTED - inline i31 creation is single instruction

### RQ3: What are the type indices for each predicate?

**Decision**: Use constants from `gc-types.lisp`:

| Predicate | Type Check | Type Index/Instruction |
|-----------|------------|------------------------|
| integerp | fixnum OR bignum | `:i31` or `+type-bignum+` (14) |
| numberp | any numeric | `:i31`, 14, 15, 16, 17 |
| floatp | float struct | `+type-float+` (16) |
| rationalp | integer OR ratio | `:i31`, 14, or 15 |
| complexp | complex struct | `+type-complex+` (17) |
| symbolp | symbol struct | `+type-symbol+` (3) |
| functionp | closure struct | `+type-closure+` (5) |
| characterp | i31 (shared with fixnum) | `:i31` (see note) |

**Note on characterp**: Characters currently share i31 encoding with fixnums. Full implementation requires type tagging (future feature). For now, returns T for all i31ref values.

### RQ4: How do numeric predicates handle multiple types?

**Decision**: Use cascading `if` blocks with `local.tee` for value reuse:

```
1. Save value to local (local.tee)
2. Test fixnum (ref.test :i31)
3. If fixnum: extract value (i31.get_s), apply i32 comparison
4. Else test float (ref.test +type-float+)
5. If float: extract value (struct.get), apply f64 comparison
6. Else: return NIL (or signal error for oddp/evenp)
```

**Rationale**: This cascading pattern is efficient and avoids duplicate evaluation. Used in existing `compile-numerator` and `compile-denominator`.

### RQ5: How should signum handle type preservation?

**Decision**: Return same type as input:
- Integer input → integer result (-1, 0, 1)
- Float input → float result (-1.0, 0.0, 1.0)

**Rationale**: ANSI CL specification requires type contagion. The result type matches the most specific numeric type of the argument.

**Implementation**:
```lisp
;; Fixnum path
(i31.get_s)
(if (i32.lt_s 0)
  (i32.const -1) ref.i31   ; negative
  (if (i32.gt_s 0)
    (i32.const 1) ref.i31  ; positive
    (i32.const 0) ref.i31)) ; zero

;; Float path
(struct.get +type-float+ 0)  ; extract f64
(if (f64.lt 0.0)
  (struct.new +type-float+ (f64.const -1.0))
  (if (f64.gt 0.0)
    (struct.new +type-float+ (f64.const 1.0))
    (struct.new +type-float+ (f64.const 0.0))))
```

### RQ6: What are the ANSI CL edge cases?

**Decision**: Follow ANSI CL specification exactly:

| Edge Case | Expected Behavior |
|-----------|------------------|
| `(zerop 0)` | T |
| `(zerop 0.0)` | T |
| `(zerop -0.0)` | T (IEEE 754 negative zero) |
| `(plusp 0)` | NIL |
| `(minusp 0)` | NIL |
| `(oddp 2.0)` | Error (integer only) |
| `(evenp 3/2)` | Error (integer only) |
| `(symbolp nil)` | T (NIL is a symbol) |
| `(integerp 3/2)` | NIL (ratio, not integer) |
| `(rationalp 3/2)` | T (ratios are rational) |
| `(rationalp 3.14)` | NIL (floats are not rational) |

**Note**: Error signaling for oddp/evenp on non-integers is out of scope for initial implementation. Return NIL for non-integers initially, add proper error signaling in future iteration.

### RQ7: How do existing helper functions support implementation?

**Decision**: Reuse existing helpers from `numeric-runtime.lisp`:

- `emit-is-fixnum` → `(:ref.test :i31)`
- `emit-is-bignum` → `(:ref.test (:ref +type-bignum+))`
- `emit-is-ratio` → `(:ref.test (:ref +type-ratio+))`
- `emit-is-float` → `(:ref.test (:ref +type-float+))`
- `emit-is-complex` → `(:ref.test (:ref +type-complex+))`

These return Wasm instruction lists that can be spliced into predicate compilation.

## Implementation Order

Based on dependencies and test coverage impact:

### Phase 1: Type Predicates (Independent, High Impact)
1. `integerp` - fixnum OR bignum check
2. `floatp` - single struct test
3. `rationalp` - fixnum OR bignum OR ratio check
4. `complexp` - single struct test
5. `numberp` - union of all numeric types
6. `symbolp` - single struct test
7. `functionp` - closure struct test
8. `characterp` - i31 test (temporary, shared with fixnum)

### Phase 2: Numeric Predicates (Depend on type dispatch)
9. `zerop` - multi-type dispatch, == 0 comparison
10. `plusp` - multi-type dispatch, > 0 comparison
11. `minusp` - multi-type dispatch, < 0 comparison
12. `evenp` - integer-only, mod 2 == 0
13. `oddp` - integer-only, mod 2 != 0

### Phase 3: Signum (Type-preserving)
14. `signum` - multi-type dispatch with type preservation

## Test Strategy

### Unit Tests (per predicate)
- Positive case: argument of expected type → T
- Negative case: argument of different type → NIL
- Edge cases: 0, -0.0, large numbers, NIL

### Contract Tests
- Generated Wasm validates with `wasm-tools validate`
- Correct section order and type indices

### Integration Tests
- Full compile → wasmtime execute → verify result
- ANSI test suite pass rate measurement

## Resolved Unknowns

All Technical Context items from plan.md are now resolved:

| Unknown | Resolution |
|---------|------------|
| Type dispatch mechanism | `ref.test` instruction |
| Boolean representation | T=i31:1, NIL=ref.null |
| Type indices | From gc-types.lisp constants |
| Helper functions | From numeric-runtime.lisp |
| Implementation pattern | Cascading if with local.tee |
