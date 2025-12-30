# Implementation Plan: ANSI String Trim Functions (Phase 16B)

**Branch**: `001-ansi-string-trim` | **Date**: 2025-12-31 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-ansi-string-trim/spec.md`

## Summary

Implement six ANSI Common Lisp string functions to complete the string manipulation function set:
- **Trim functions**: [string-trim](resources/HyperSpec/Body/f_stg_tr.htm), [string-left-trim](resources/HyperSpec/Body/f_stg_tr.htm), [string-right-trim](resources/HyperSpec/Body/f_stg_tr.htm) - non-destructive removal of characters from string boundaries
- **Destructive case functions**: [nstring-upcase](resources/HyperSpec/Body/f_stg_up.htm), [nstring-downcase](resources/HyperSpec/Body/f_stg_up.htm), [nstring-capitalize](resources/HyperSpec/Body/f_stg_up.htm) - in-place case conversion

All functions support `:start`/`:end` keyword arguments per ANSI CL specification.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, existing Clysm compiler infrastructure
**Storage**: N/A (in-memory compilation, Wasm binary output)
**Testing**: rove (unit tests), wasm-tools validate (contract tests)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single project (compiler)
**Performance Goals**: Correctness-first; no specific latency targets for these standard library functions
**Constraints**: Must pass 70% of strings category test suite; all functions must compile to valid WasmGC
**Scale/Scope**: 6 functions, ~500-800 lines of codegen implementation

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Uses `$string` array type (i8 UTF-8 bytes), no linear memory |
| II. Lispオブジェクト表現規約 | PASS | String type already defined; character bag handling follows existing patterns |
| III. 関数・クロージャ実装戦略 | N/A | No closure-related changes |
| IV. Wasm制御フロー活用 | PASS | Loop-based iteration follows existing string-upcase patterns |
| V. シャローバインディング | N/A | No special variables involved |
| VI. 段階的動的コンパイル | N/A | Standard library functions, not eval/JIT |
| VII. テスト駆動開発（TDD） | PASS | Tests required before implementation |
| VIII. Nix-Firstワークフロー | PASS | Uses existing nix develop environment |
| IX. ANSI CL仕様参照規約 | PASS | HyperSpec links included in this plan |

**Gate Result**: ALL PASS - Proceed to Phase 0

## Project Structure

### Documentation (this feature)

```text
specs/001-ansi-string-trim/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (Wasm function signatures)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       └── func-section.lisp    # Add string-trim, nstring-* implementations
└── lib/
    └── sequences-util.lisp      # Existing bounding index utilities

tests/
├── unit/
│   └── string-trim-test.lisp    # NEW: Unit tests for trim functions
├── contract/
│   └── string-trim-wasm.lisp    # NEW: Wasm validation tests
└── integration/
    └── string-test.lisp         # Extend with trim function tests
```

**Structure Decision**: Single project structure; new functions added to existing `func-section.lisp` following established patterns for string-upcase/string-downcase.

## Complexity Tracking

> No Constitution violations requiring justification.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| (none)    | -          | -                                   |
