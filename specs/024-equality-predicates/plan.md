# Implementation Plan: ANSI Common Lisp Equality Predicates and Logical Operators

**Branch**: `024-equality-predicates` | **Date**: 2025-12-26 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/024-equality-predicates/spec.md`

## Summary

Implement ANSI Common Lisp equality predicates (`eq`, `eql`, `equal`, `equalp`) and logical operators (`not`, `and`, `or`) for the clysm WasmGC compiler. The equality predicates use Wasm's `ref.eq` for identity comparison and `ref.test`/`ref.cast` for type-aware comparisons. The `and`/`or` special forms are already parsed to AST nodes that expand to nested `if` forms, requiring only compilation support.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for compiler implementation
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (compile-time only)
**Testing**: Rove test framework with `compile-and-run` helper for execution tests
**Target Platform**: WebAssembly GC (WasmGC) - browser and wasmtime runtimes
**Project Type**: Single (compiler project)
**Performance Goals**: Equality predicates should compile to minimal Wasm instructions; `eq` should compile to single `ref.eq` instruction
**Constraints**: Must use WasmGC type system; no linear memory access; TDD methodology required
**Scale/Scope**: 6 new predicates/operators, ~200-300 lines of codegen, ~50 tests

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Justification |
|-----------|--------|---------------|
| I. WasmGC-First型システム設計 | PASS | Uses `ref.eq` for identity, `ref.test`/`ref.cast` for type checks |
| II. Lispオブジェクト表現規約 | PASS | NIL handled as singleton; `eq` uses `ref.eq` per constitution |
| III. 関数・クロージャ実装戦略 | N/A | No closure modifications needed |
| IV. Wasm制御フロー活用 | PASS | `and`/`or` compile to nested `if`; no new control flow needed |
| V. シャローバインディング | N/A | No dynamic scope changes |
| VI. 段階的動的コンパイル | N/A | Compile-time only; no JIT changes |
| VII. TDD（非交渉） | PASS | All implementation follows Red-Green-Refactor cycle |
| VIII. Nix-Firstワークフロー | PASS | Uses existing `nix flake check` infrastructure |

**Security Constraints**: PASS - No linear memory access; pure GC-managed references
**Performance Constraints**: PASS - `eq` is single instruction; `eql`/`equal` use minimal type dispatch

## Project Structure

### Documentation (this feature)

```text
specs/024-equality-predicates/
├── plan.md              # This file
├── research.md          # Phase 0: Wasm instruction research
├── data-model.md        # Phase 1: Predicate type signatures
├── quickstart.md        # Phase 1: Implementation guide
├── contracts/           # Phase 1: Expected Wasm output contracts
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── codegen/
│   │   ├── func-section.lisp    # ADD: compile-eq, compile-eql, compile-equal, compile-equalp, compile-not
│   │   └── gc-types.lisp        # REF: Type constants for ref.test
│   └── ast.lisp                 # REF: parse-and-form, parse-or-form (already exist)
└── [other modules unchanged]

tests/
├── unit/
│   ├── equality-predicates-test.lisp   # NEW: Unit tests for eq/eql/equal/equalp/not
│   └── logical-operators-test.lisp     # NEW: Unit tests for and/or
├── contract/
│   └── equality-wasm-test.lisp         # NEW: Wasm validation tests
└── integration/
    └── equality-ansi-test.lisp         # NEW: ANSI CL compliance tests
```

**Structure Decision**: Single project (compiler) - modifications to existing `func-section.lisp` with new test files in standard test directories

## Complexity Tracking

> No constitution violations - table not required.

All implementations follow established patterns from feature 023-type-predicates. No new architectural patterns introduced.
