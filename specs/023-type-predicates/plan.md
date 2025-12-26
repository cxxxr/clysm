# Implementation Plan: ANSI CL Type Predicates and Numeric Predicates

**Branch**: `023-type-predicates` | **Date**: 2025-12-26 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/023-type-predicates/spec.md`

## Summary

Implement 14 ANSI CL standard predicates (8 type predicates, 5 numeric predicates, 1 signum function) to improve ANSI test pass rates. All predicates will use WasmGC `ref.test` for runtime type dispatch and return T (i31:1) or NIL (ref.null) following established codebase patterns.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - compiler implementation
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (compile-time code generation)
**Testing**: rove (unit), wasmtime (contract/integration)
**Target Platform**: WasmGC (wasmtime with --wasm-features=gc,exceptions,tail-call)
**Project Type**: Single project (compiler)
**Performance Goals**: <1ms per predicate execution (SC-006)
**Constraints**: No linear memory access, WasmGC types only, sandbox preserved
**Scale/Scope**: 14 predicates, ANSI test pass rate ≥10% numbers, ≥5% cons

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. WasmGC-First | **PASS** | Uses `ref.test` for type dispatch, no linear memory |
| II. Lisp Objects | **PASS** | NIL as ref.null, T as i31:1, type structs defined |
| III. Closure Strategy | **N/A** | No new closures, predicates are primitives |
| IV. Wasm Control Flow | **N/A** | No exception handling in predicates |
| V. Shallow Binding | **N/A** | No dynamic variables |
| VI. Tiered Eval/JIT | **N/A** | Predicates compiled at AOT, available to JIT |
| VII. TDD | **PASS** | Tests written before implementation (MUST) |
| VIII. Nix-First | **PASS** | `nix flake check` required for all commits |

**Gate Status**: ALL PASS - Proceed to Phase 0

## Project Structure

### Documentation (this feature)

```text
specs/023-type-predicates/
├── plan.md              # This file
├── research.md          # Phase 0: Type dispatch patterns
├── data-model.md        # Phase 1: Type hierarchy
├── quickstart.md        # Phase 1: Implementation guide
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       ├── func-section.lisp   # ADD: compile-integerp, compile-numberp, etc.
│       ├── gc-types.lisp       # EXISTING: type indices (+type-bignum+, etc.)
│       └── numeric-runtime.lisp # EXISTING: emit-is-fixnum, emit-is-bignum, etc.
└── package.lisp                # EXISTING: exports

tests/
├── unit/
│   ├── type-predicates-test.lisp   # NEW: integerp, numberp, symbolp, etc.
│   └── numeric-predicates-test.lisp # EXISTING: zerop, plusp, minusp, etc.
├── contract/
│   └── predicates-wasm-test.lisp   # NEW: Wasm validation tests
└── integration/
    └── ansi-predicates-test.lisp   # NEW: wasmtime execution tests
```

**Structure Decision**: Single project - compiler with existing test hierarchy. New predicates added to `func-section.lisp` alongside existing primitives (consp, null, atom, listp).

## Implementation Patterns

### Type Predicate Pattern (from existing `compile-consp`)

```lisp
(defun compile-<predicate> (args env)
  (when (/= (length args) 1)
    (error "<predicate> requires exactly 1 argument"))
  (append
   (compile-to-instructions (first args) env)
   `((:ref.test (:ref ,+type-XXX+))
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31   ; T
     :else
     (:ref.null :none)         ; NIL
     :end)))
```

### Numeric Predicate Pattern (multiple type dispatch)

```lisp
(defun compile-zerop (args env)
  (let ((temp-local (env-add-local env (gensym "ZEROP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Check fixnum first
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum path
       (:local.get ,temp-local)
       (:ref.cast :i31)
       (:i31.get_s)
       (:i32.eqz)
       ;; Convert i32 to Lisp boolean
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Float path (or error for other types)
       ;; ...
       :end))))
```

## Type Index Reference

From `gc-types.lisp`:
- `+type-bignum+` = 14 (arbitrary-precision integers)
- `+type-ratio+` = 15 (exact rationals)
- `+type-float+` = 16 (IEEE 754 double)
- `+type-complex+` = 17 (complex numbers)
- `+type-symbol+` = 3 (symbols)
- `+type-closure+` = 5 (functions)
- `:i31` = fixnum (no type index, use ref.test :i31)

## Complexity Tracking

> No violations - complexity within constitution limits.

| Item | Justification |
|------|---------------|
| 14 predicates | All follow same pattern, reuse existing type dispatch infrastructure |
| No new types | Uses existing WasmGC structs from 010-numeric-tower |
| No new dependencies | Uses existing rove, wasmtime toolchain |
