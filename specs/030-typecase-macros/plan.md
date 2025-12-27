# Implementation Plan: Type Dispatch Macros

**Branch**: `030-typecase-macros` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/030-typecase-macros/spec.md`

## Summary

Implement ANSI CL type dispatch macros (typecase, etypecase, ctypecase, check-type) as compile-time macro expansions to typep calls. This is the single largest blocker for self-hosting, with 892 typecase usages in the Clysm compiler. The implementation leverages existing infrastructure: typep from 023-type-predicates, type-error/store-value restart from 014-condition-system, and macro registry from 016-macro-system.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for compiler; WasmGC for output
**Primary Dependencies**: clysm/lib/macros, clysm/conditions, clysm/lib/setf-expanders
**Storage**: N/A (compile-time macro expansion only)
**Testing**: Rove (unit, contract, integration tests)
**Target Platform**: WasmGC (wasmtime for testing)
**Project Type**: Single project (compiler)
**Performance Goals**: Macro expansion at compile-time only; no runtime overhead
**Constraints**: Must expand to typep calls; must not introduce circular dependencies
**Scale/Scope**: 892 existing typecase usages in compiler must work unchanged

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | Macros expand to typep which uses ref.test/ref.cast |
| II. Lisp Object Representation | PASS | Uses existing type predicates, NIL handling intact |
| III. Function/Closure Implementation | N/A | Macro system, no new closures |
| IV. Wasm Control Flow | PASS | Expands to if/typep forms, existing control flow |
| V. Shallow Binding | N/A | No special variable interaction |
| VI. Tiered Eval/JIT | N/A | Compile-time only |
| VII. TDD (Non-negotiable) | REQUIRED | Tests before implementation |
| VIII. Nix-First | REQUIRED | All tests via `nix flake check` |

**Gate Result**: PASS - All applicable principles satisfied.

## Project Structure

### Documentation (this feature)

```text
specs/030-typecase-macros/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
│   └── typecase-api.lisp
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/
├── clysm/
│   ├── lib/
│   │   ├── macros.lisp           # Add typecase expanders here
│   │   └── setf-expanders.lisp   # Existing (used by ctypecase/check-type)
│   ├── conditions/
│   │   ├── types.lisp            # Has type-error (FR-027)
│   │   ├── restarts.lisp         # Has store-value (FR-028)
│   │   └── signaling.lisp        # Has error/signal functions
│   └── compiler/
│       ├── transform/
│       │   └── macro.lisp        # Macro registry (FR-029)
│       └── codegen/
│           └── func-section.lisp # typep compilation (FR-026)

tests/
├── unit/
│   └── typecase/
│       ├── typecase-test.lisp    # typecase macro tests
│       ├── etypecase-test.lisp   # etypecase macro tests
│       ├── ctypecase-test.lisp   # ctypecase macro tests
│       └── check-type-test.lisp  # check-type macro tests
├── contract/
│   └── typecase-wasm-test.lisp   # Wasm validation tests
└── integration/
    └── typecase-ansi-test.lisp   # ANSI compliance tests
```

**Structure Decision**: Single project structure. New macro expanders added to existing `src/clysm/lib/macros.lisp` following the established pattern (see `make-case-expander` as template).

## Complexity Tracking

No violations requiring justification. Feature uses existing patterns:
- Macro expander pattern from `make-case-expander`
- Condition signaling from 014-condition-system
- Place expansion from 028-setf-generalized-refs

## Implementation Phases

### Phase 1: typecase Macro (P1)

**Goal**: Basic type dispatch with typep expansion

**Key Implementation Points**:
1. Create `make-typecase-expander` following `make-case-expander` pattern
2. Expand to nested `(if (typep KEY 'TYPE) ...)` forms
3. Bind keyform to gensym for single evaluation
4. Handle `otherwise` and `t` as catch-all clauses
5. Support multiple type specifiers per clause

**Dependencies**: typep function, macro registry

### Phase 2: etypecase Macro (P1)

**Goal**: Exhaustive type dispatch with type-error signaling

**Key Implementation Points**:
1. Create `make-etypecase-expander`
2. Expand similarly to typecase but with error case
3. Construct `(or type1 type2 ...)` for expected-type
4. Signal `type-error` with :datum and :expected-type

**Dependencies**: typecase implementation, type-error condition

### Phase 3: check-type Macro (P1)

**Goal**: Type assertion with store-value restart

**Key Implementation Points**:
1. Create `make-check-type-expander`
2. Expand to loop with typep check
3. Signal type-error with store-value restart
4. Re-validate after store-value invocation
5. Support optional type-string argument

**Dependencies**: typep, type-error, store-value restart, setf expansion

### Phase 4: ctypecase Macro (P2)

**Goal**: Correctable type dispatch with store-value

**Key Implementation Points**:
1. Create `make-ctypecase-expander`
2. Combine typecase logic with check-type's restart mechanism
3. Loop structure for re-evaluation after store-value
4. Reject otherwise/t clauses

**Dependencies**: typecase, check-type patterns, store-value restart

### Phase 5: Compound Type Specifiers (P2)

**Goal**: Support or/and/not/member/satisfies in type specifiers

**Key Implementation Points**:
1. Verify typep handles compound specifiers
2. If not, extend typep or expand at macro level
3. Test all compound forms in typecase/etypecase

**Dependencies**: typep implementation from 023-type-predicates

### Phase 6: Integration & Validation (P1)

**Goal**: Verify all 892 compiler typecase usages work

**Key Implementation Points**:
1. Run compiler self-compilation test
2. Verify all tests pass with `nix flake check`
3. Document any edge cases found

**Dependencies**: All previous phases
