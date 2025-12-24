# Implementation Plan: Numeric Accessors and Float Special Values

**Branch**: `019-numeric-accessors` | **Date**: 2025-12-24 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/019-numeric-accessors/spec.md`

## Summary

Implement ANSI Common Lisp numeric accessor functions (`numerator`, `denominator`) for Ratio and Integer types, ensure correct IEEE 754 special value handling (NaN, +Infinity, -Infinity) including proper comparison semantics, and preserve double-float precision. This extends the existing 010-numeric-tower implementation.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - compiler implementation; WasmGC - output target
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing); existing numeric tower (010)
**Storage**: N/A (compile-time code generation)
**Testing**: rove (Common Lisp), wasmtime (Wasm execution verification)
**Target Platform**: WasmGC (wasmtime runtime)
**Project Type**: single (Common Lisp compiler project)
**Performance Goals**: Generated Wasm functions follow existing performance patterns (struct.get for accessors)
**Constraints**: Must maintain ANSI CL compliance; IEEE 754 compliance for float operations
**Scale/Scope**: 4 new/modified functions (numerator, denominator, float comparisons)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. WasmGC-First | PASS | Using existing $RATIO struct with anyref fields; $FLOAT uses f64 |
| II. Lisp Object Representation | PASS | Ratio fields already defined as NUMERATOR/DENOMINATOR; sign on numerator per spec |
| III. Function/Closure Strategy | N/A | Accessors are primitive operations, not closures |
| IV. Wasm Control Flow | N/A | No control flow changes needed |
| V. Shallow Binding | N/A | No dynamic binding involved |
| VI. Tiered Eval/JIT | N/A | Compile-time implementation only |
| VII. TDD | PASS | Existing tests in ratio-test.lisp and float-test.lisp drive implementation |
| VIII. Nix-First | PASS | Using existing flake.nix development environment |

**Gate Status**: PASS - All applicable principles satisfied.

## Project Structure

### Documentation (this feature)

```text
specs/019-numeric-accessors/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (Lisp function signatures)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       ├── gc-types.lisp          # $RATIO, $FLOAT type definitions (existing)
│       ├── numeric-runtime.lisp   # Runtime functions (extend)
│       └── func-section.lisp      # Code generation (extend)
└── reader/
    └── tokenizer.lisp             # Float literal parsing (verify)

tests/
├── integration/
│   ├── ratio-test.lisp            # numerator/denominator tests (existing)
│   └── float-test.lisp            # Special value tests (existing)
└── unit/
    └── numeric-predicates-test.lisp
```

**Structure Decision**: Single project structure. Extending existing compiler infrastructure at `src/clysm/compiler/codegen/`. Tests already exist in `tests/integration/`.

## Complexity Tracking

No violations requiring justification.

## Implementation Approach

### Phase 1: Ratio Accessors (numerator/denominator)

1. **Add `numerator` function** - Extract NUMERATOR field from $RATIO struct
   - For ratio: `(struct.get $ratio 0)` → numerator anyref
   - For integer: return the integer itself
   - Type dispatch: check i31ref (fixnum), bignum, ratio

2. **Add `denominator` function** - Extract DENOMINATOR field from $RATIO struct
   - For ratio: `(struct.get $ratio 1)` → denominator anyref
   - For integer: return 1 (as fixnum)
   - Type dispatch: check i31ref (fixnum), bignum, ratio

### Phase 2: Float Special Values

3. **Verify special value generation** - IEEE 754 f64 semantics
   - `(/ 1.0 0.0)` → +Infinity (f64.div produces inf)
   - `(/ -1.0 0.0)` → -Infinity
   - `(- inf inf)` → NaN

4. **Fix float comparison operators** - NaN handling
   - `=` with NaN must return NIL (use f64.eq which returns false for NaN)
   - `<`, `>`, `<=`, `>=` with NaN must return NIL
   - Key: Wasm f64 comparisons already follow IEEE 754

### Phase 3: Double-Float Precision

5. **Verify double-float literal parsing** - `1.0d0` suffix
   - Confirm reader correctly parses as f64
   - Confirm compiler preserves precision through codegen

## Research Needs

- Wasm struct.get instruction encoding for field access
- IEEE 754 NaN comparison behavior in Wasm f64 operations
- Current constant folding behavior for float division by zero
