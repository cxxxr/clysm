# Implementation Plan: FORMAT Function Foundation

**Branch**: `032-format-function` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Phase 9D - Implement FORMAT function subset for self-hosting (93 compiler call sites)

## Summary

Extend the existing FORMAT implementation in `src/clysm/streams/format.lisp` to support the full directive set required for compiler self-hosting. The current implementation supports ~A, ~S, ~D, ~%, ~~. This feature adds ~& (fresh-line), ~{~} (iteration), ~^ (escape), ~[~] (conditional), ~? (recursive), and proper format-error condition handling.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - compiler implementation; WasmGC - output target
**Primary Dependencies**: clysm/streams, clysm/conditions, alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory format string parsing and execution)
**Testing**: rove (test framework) with unit, contract, and integration tests
**Target Platform**: WasmGC (host SBCL for development)
**Project Type**: Single project (existing Clysm compiler extension)
**Performance Goals**: Format string parsing < 1ms for typical strings; 93 call sites must work correctly
**Constraints**: Must integrate with existing stream infrastructure; no new WasmGC types required
**Scale/Scope**: ~500 lines of new/modified code across format.lisp and tests

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | FORMAT operates on streams (already WasmGC-based) |
| II. Lispオブジェクト表現規約 | ✅ PASS | Uses existing NIL/object representations |
| III. 関数・クロージャ実装戦略 | ✅ PASS | FORMAT is a regular function, no special closure handling |
| IV. Wasm制御フロー活用 | N/A | No control flow changes needed |
| V. シャローバインディング | ✅ PASS | Uses *standard-output* (existing special variable) |
| VI. 段階的動的コンパイル | N/A | FORMAT is compile-time infrastructure |
| VII. TDD | ✅ REQUIRED | Must follow Red-Green-Refactor |
| VIII. Nix-First | ✅ REQUIRED | All tests via `nix flake check` |

**Gate Result**: PASS - No violations

## Project Structure

### Documentation (this feature)

```text
specs/032-format-function/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/streams/
├── format.lisp          # MODIFY: Add ~&, ~{~}, ~[~], ~?, format-error
├── types.lisp           # Existing stream types
├── write.lisp           # Existing write functions
└── package.lisp         # MODIFY: Export new symbols

src/clysm/conditions/
├── types.lisp           # MODIFY: Add format-error condition
└── package.lisp         # MODIFY: Export format-error

tests/
├── unit/
│   └── format/          # NEW: Unit tests for each directive
│       ├── basic-test.lisp
│       ├── iteration-test.lisp
│       ├── conditional-test.lisp
│       └── recursive-test.lisp
├── contract/
│   └── format-wasm-test.lisp  # NEW: Wasm validation
└── integration/
    └── format-ansi-test.lisp  # NEW: ANSI CL compliance
```

**Structure Decision**: Extends existing `src/clysm/streams/` module with new directive handlers and tests.

## Complexity Tracking

No constitution violations requiring justification.
