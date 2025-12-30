# Implementation Plan: Character Literal Compilation Support

**Branch**: `001-char-literal-compile` | **Date**: 2025-12-31 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-char-literal-compile/spec.md`

## Summary

Add support for compiling character literals in quoted expressions by extending the `compile-quoted-element` function to recognize character objects and encode them as i31ref values (using their Unicode code points). This addresses compilation failures for patterns like `(member char '(#\Space #\Tab))` and targets a compilation rate improvement from 14% to 20%.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Primary Dependencies**: alexandria, babel (UTF-8), existing clysm compiler infrastructure
**Storage**: N/A (in-memory compilation, Wasm binary output)
**Testing**: rove test framework, wasm-tools validation
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (compiler)
**Performance Goals**: No performance regression; character compilation should be O(1)
**Constraints**: Generated Wasm must pass `wasm-tools validate`
**Scale/Scope**: Single function modification (~5 lines of code)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Character codes encoded as i31ref (same as fixnums) |
| II. Lispオブジェクト表現規約 | PASS | Character representation follows established pattern |
| III. 関数・クロージャ実装戦略 | N/A | No function/closure changes |
| IV. Wasm制御フロー活用 | N/A | No control flow changes |
| V. シャローバインディング | N/A | No dynamic scope changes |
| VI. 段階的動的コンパイル | N/A | Compile-time only change |
| VII. TDD（非交渉） | REQUIRED | Must write tests before implementation |
| VIII. Nix-Firstワークフロー | PASS | Use existing nix develop environment |
| IX. ANSI CL仕様参照規約 | PASS | Reference [characterp](resources/HyperSpec/Body/f_chp.htm), [char-code](resources/HyperSpec/Body/f_char_c.htm) |

**Gate Status**: PASS - Proceed to Phase 0

## Project Structure

### Documentation (this feature)

```text
specs/001-char-literal-compile/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output (minimal for this feature)
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
└── compiler/
    └── codegen/
        └── func-section.lisp    # Target file: compile-quoted-element function (lines 509-525)

tests/
├── unit/
│   └── char-literal-test.lisp   # New: Unit tests for character compilation
└── contract/
    └── char-wasm-test.lisp      # New: Wasm validation tests
```

**Structure Decision**: Single project structure (compiler). Modification confined to existing `func-section.lisp` with new test files.

## Complexity Tracking

No violations. This is a minimal, focused change following established patterns.
