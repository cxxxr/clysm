# Implementation Plan: LOOP Macro

**Branch**: `029-loop-macro` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/029-loop-macro/spec.md`

## Summary

Implement ANSI Common Lisp LOOP macro with full extended LOOP support. The macro expands to tagbody/go-based code following the existing do/dolist/dotimes pattern. Key features: FOR/AS iteration clauses (arithmetic, list, vector, hash-table), accumulation clauses (collect/sum/count/maximize/minimize), termination conditions (while/until/always/never/thereis), conditional execution (if/when/unless), and INITIALLY/FINALLY clauses.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for compiler; WasmGC for output
**Primary Dependencies**: clysm/compiler, clysm/lib/macros (existing tagbody/go infrastructure)
**Storage**: N/A (compile-time macro expansion only)
**Testing**: rove (unit/contract/integration tests)
**Target Platform**: WebAssembly GC (WasmGC)
**Project Type**: single (compiler extension)
**Performance Goals**: LOOP expansion O(n) in clause count; generated Wasm matches do/dolist efficiency
**Constraints**: Must use tagbody/go pattern; no runtime interpreter for LOOP
**Scale/Scope**: 25 functional requirements, 8 user stories, 50+ ANSI test cases

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | LOOP expansion uses existing WasmGC types; no new linear memory |
| II. Lispオブジェクト表現規約 | PASS | Uses existing NIL/symbol representations; no new object types |
| III. 関数・クロージャ実装戦略 | PASS | LOOP is macro, not closure; expansion reuses existing closure infrastructure |
| IV. Wasm制御フロー活用 | PASS | Leverages existing tagbody/go compilation with br/br_table |
| V. シャローバインディング | PASS | WITH clause uses existing let/let* binding; special variables unchanged |
| VI. 段階的動的コンパイル | N/A | LOOP is compile-time macro; no JIT impact |
| VII. テスト駆動開発 | REQUIRED | TDD cycle mandatory: tests first, then implementation |
| VIII. Nix-Firstワークフロー | PASS | Existing nix develop/build/check workflow applies |

**Gate Result**: PASS - No violations. Proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/029-loop-macro/
├── plan.md              # This file
├── spec.md              # Feature specification
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (N/A for macro feature)
└── tasks.md             # Phase 2 output
```

### Source Code (repository root)

```text
src/clysm/
├── lib/
│   └── macros.lisp      # Add make-loop-expander function (~500 lines)
└── package.lisp         # Export loop symbol

tests/
├── unit/
│   └── loop-test.lisp   # Unit tests for LOOP clause parsing
├── contract/
│   └── loop-wasm-test.lisp  # Wasm validation for LOOP expansion
└── integration/
    └── loop-ansi-test.lisp  # ANSI CL compliance tests
```

**Structure Decision**: Single project structure. LOOP is a macro in src/clysm/lib/macros.lisp following the existing do/dolist/dotimes pattern. Tests organized by type under tests/.

## Complexity Tracking

No constitution violations requiring justification.
