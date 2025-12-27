# Implementation Plan: Destructuring-Bind Macro

**Branch**: `031-destructuring-bind-macro` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/031-destructuring-bind-macro/spec.md`

## Summary

Implement ANSI Common Lisp compliant `destructuring-bind` macro that decomposes a list according to a destructuring lambda-list pattern. The macro will support `&optional`, `&rest`, `&key`, `&body`, `&whole`, and `&allow-other-keys`, plus nested destructuring at any depth. This is required for compiler self-hosting as 9 internal locations use this macro.

The implementation will follow the existing macro expander pattern in `src/clysm/lib/macros.lisp`, creating a `make-destructuring-bind-expander` function that generates code to traverse lists using `car`/`cdr` and bind variables accordingly.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - compiler implementation; WasmGC - output target
**Primary Dependencies**: clysm/compiler, clysm/lib/macros, clysm/conditions
**Storage**: N/A (compile-time macro expansion only)
**Testing**: rove (unit, contract, integration tests)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single project (compiler)
**Performance Goals**: Macro expansion time negligible (<1ms for typical patterns)
**Constraints**: Must be self-hosting (9 compiler locations depend on it)
**Scale/Scope**: Support 5+ levels of nesting depth, 14 functional requirements

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | N/A | Compile-time macro, no runtime type impact |
| II. Lisp Object Representation | N/A | Uses existing list/symbol types |
| III. Function/Closure Strategy | N/A | No closure generation required |
| IV. Wasm Control Flow | N/A | Macro expands to `let`/`if`/`error` forms |
| V. Shallow Binding | N/A | No special variable interaction |
| VI. Tiered Eval/JIT | N/A | Standard macro expansion |
| VII. TDD (Non-Negotiable) | **REQUIRED** | Tests first for each user story |
| VIII. Nix-First | **REQUIRED** | `nix flake check` must pass |

**Gate Status**: PASS - All applicable principles satisfied. TDD workflow applies.

## Project Structure

### Documentation (this feature)

```text
specs/031-destructuring-bind-macro/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── lib/
│   └── macros.lisp              # Add make-destructuring-bind-expander
│   └── destructuring.lisp       # NEW: Parsing utilities for lambda-lists
└── package.lisp                 # Export new symbols

tests/
├── unit/
│   └── destructuring-bind-test.lisp     # NEW: Unit tests
├── contract/
│   └── destructuring-wasm-test.lisp     # NEW: Wasm validation
└── integration/
    └── destructuring-ansi-test.lisp     # NEW: ANSI compliance tests
```

**Structure Decision**: Single project pattern. New source file `destructuring.lisp` for lambda-list parsing utilities (reusable for `defmacro` expansion). Main expander in existing `macros.lisp` following the established `make-*-expander` pattern.

## Complexity Tracking

No constitution violations requiring justification.
