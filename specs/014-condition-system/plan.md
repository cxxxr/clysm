# Implementation Plan: ANSI Common Lisp Condition System

**Branch**: `014-condition-system` | **Date**: 2025-12-24 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/014-condition-system/spec.md`

## Summary

Implement the ANSI Common Lisp Condition System (Phase 8F) to enable structured error handling with conditions, handlers, and restarts. The implementation builds on the existing `catch`/`throw`/`unwind-protect` infrastructure and WasmGC exception handling, using stack-based binding (like special variables) for handler and restart management.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - compiler implementation language
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory condition/handler/restart stacks)
**Testing**: rove (TDD methodology per constitution)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single project (compiler generating Wasm binaries)
**Performance Goals**: Handler dispatch and restart invocation should not add significant overhead to normal execution paths
**Constraints**: Must integrate with existing `catch`/`throw`/`unwind-protect` and WasmGC `try_table` exception mechanism
**Scale/Scope**: Full ANSI CL condition system subset (core operators and standard condition types)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Evidence/Notes |
|-----------|--------|----------------|
| I. WasmGC-First型システム設計 | PASS | Condition objects will be WasmGC structs; handler/restart stacks use GC arrays |
| II. Lispオブジェクト表現規約 | PASS | Conditions are CLOS instances; NIL handling follows existing patterns |
| III. 関数・クロージャ実装戦略 | PASS | Handler functions are closures; restart functions use same closure representation |
| IV. Wasm制御フロー活用 | PASS | Uses existing `try_table` for unwind-protect; restarts use `throw` for non-local exit |
| V. シャローバインディングによる動的スコープ | PASS | Handler/restart stacks mirror special variable shallow binding pattern |
| VI. 段階的動的コンパイル | N/A | Condition system is runtime behavior, not compilation strategy |
| VII. テスト駆動開発（TDD）（非交渉） | PASS | All implementation follows Red-Green-Refactor; tests before code |
| VIII. Nix-Firstワークフロー | PASS | Uses existing flake.nix; `nix flake check` gates all commits |

**Gate Status**: PASS - No violations requiring justification.

## Project Structure

### Documentation (this feature)

```text
specs/014-condition-system/
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
├── conditions/              # NEW: Condition system implementation
│   ├── types.lisp          # Condition class hierarchy (CLOS)
│   ├── handlers.lisp       # handler-case, handler-bind implementation
│   ├── restarts.lisp       # restart-case, restart-bind, invoke-restart
│   ├── signaling.lisp      # signal, warn, error, cerror
│   └── standard.lisp       # Standard restarts (abort, continue, etc.)
├── compiler/
│   └── ast.lisp            # EXTEND: Add condition system AST nodes
├── runtime/
│   ├── special-vars.lisp   # REFERENCE: Pattern for handler/restart stacks
│   └── condition-runtime.lisp  # NEW: Runtime support (stacks, dispatch)
└── lib/
    └── macros.lisp         # EXTEND: with-simple-restart, etc.

tests/
├── unit/
│   ├── condition-types-test.lisp   # NEW: Condition class hierarchy tests
│   ├── handler-test.lisp           # NEW: Handler establishment tests
│   └── restart-test.lisp           # NEW: Restart mechanism tests
└── integration/
    ├── condition-test.lisp         # NEW: End-to-end condition tests
    └── control-flow-test.lisp      # EXTEND: Add condition/restart tests
```

**Structure Decision**: Single project structure (compiler). New `conditions/` subdirectory for condition system components, following the pattern of existing `clos/`, `runtime/`, `ffi/` directories.

## Complexity Tracking

No constitution violations requiring justification. The condition system is a core ANSI CL feature that integrates naturally with existing infrastructure.
