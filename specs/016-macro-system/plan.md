# Implementation Plan: Macro System (Lisp-4)

**Branch**: `016-macro-system` | **Date**: 2025-12-24 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/016-macro-system/spec.md`

## Summary

Extend the existing macro system to provide full Common Lisp-style macro support including:
- Complete `defmacro` with destructuring lambda lists
- Robust backquote (quasiquote) syntax processing
- Standard macros: `when`, `unless`, `cond`, `case`, `dolist`, `dotimes`, `do`, `prog1`, `prog2`
- Macro introspection: `macroexpand-1`, `macroexpand`

**Current State**: Partial implementation exists in `src/clysm/compiler/transform/macro.lisp` (354 lines) with basic macro registry, expansion, and backquote support. Standard macros partially implemented in `src/clysm/lib/macros.lisp` (when, unless, cond, and, or, dolist, dotimes).

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - compiler implementation
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (compile-time only, in-memory macro registry)
**Testing**: rove (unit tests in tests/unit/, integration tests in tests/integration/)
**Target Platform**: WasmGC output, host SBCL for compile-time macro execution
**Project Type**: Single project (compiler)
**Performance Goals**: Macro expansion completes within JIT budget (1000 lines/100ms per Constitution)
**Constraints**: Macros execute on host SBCL, not in Wasm runtime
**Scale/Scope**: ~15 new/extended functions, ~5 new standard macros

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | N/A | Macro expansion is compile-time only, no Wasm output changes |
| II. Lisp Object Representation | N/A | No new object types needed |
| III. Function/Closure Strategy | PASS | Macros produce AST that uses existing closure infrastructure |
| IV. Wasm Control Flow | N/A | No new control flow constructs |
| V. Shallow Binding | N/A | No special variable changes |
| VI. Tiered Eval/JIT | PASS | Macros support Tier 1 and Tier 2 compilation paths |
| VII. TDD (Non-negotiable) | GATE | All changes require tests first |
| VIII. Nix-First | PASS | Existing flake.nix covers dependencies |

**Gate Status**: PASS - No violations requiring justification.

## Project Structure

### Documentation (this feature)

```text
specs/016-macro-system/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── transform/
│       └── macro.lisp          # EXTEND: macro expansion, backquote
├── lib/
│   └── macros.lisp             # EXTEND: standard macro definitions
├── reader/
│   ├── tokenizer.lisp          # VERIFY: backquote token handling
│   └── parser.lisp             # VERIFY: quasiquote form production

tests/
├── unit/
│   ├── macro-test.lisp         # EXTEND: macro unit tests
│   └── backquote-test.lisp     # EXTEND: backquote unit tests
└── integration/
    └── macro-test.lisp         # EXTEND: end-to-end macro tests
```

**Structure Decision**: Single project layout (existing). Macro system extends existing compiler subsystem without new top-level directories.

## Complexity Tracking

> No violations to justify. Feature extends existing infrastructure.

| Aspect | Decision | Rationale |
|--------|----------|-----------|
| Backquote location | Compiler-level (existing) | Already implemented in macro.lisp, reader produces quasiquote forms |
| Macro execution | Host SBCL | Constitution requirement - cross-compilation pattern |
| Expansion depth limit | 1000 iterations | Standard practice for detecting infinite expansion |

## Implementation Phases

### Phase 0: Research & Clarification

Key unknowns to resolve:
1. What destructuring lambda list features are required? (&optional, &rest, &body, &key, &whole, &environment)
2. Current backquote implementation completeness - does it handle all edge cases?
3. Missing standard macros assessment (`case`, `prog1`, `prog2`, `do`)

### Phase 1: Design & Contracts

1. **Data Model**: Lambda list parsing structures, expansion depth tracking
2. **API Contracts**: macroexpand-1, macroexpand, macro-function signatures
3. **Quickstart**: How to define and use macros in clysm

### Phase 2: Task Decomposition

Break into testable increments:
1. P1A: Destructuring lambda list enhancement
2. P1B: Backquote edge case fixes (if needed)
3. P2A: Missing control macros (case)
4. P2B: Missing sequence macros (prog1, prog2)
5. P2C: Missing iteration macro (do)
6. P3: Macro introspection (macroexpand-1, macroexpand as compiled functions)
