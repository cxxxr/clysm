# Implementation Plan: Wasm Local Instruction Binding

**Branch**: `001-wasm-local-binding` | **Date**: 2026-01-01 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-wasm-local-binding/spec.md`

## Summary

Fix Wasm local variable instruction binding issues (LOCAL.SET/LOCAL.TEE unbound errors causing 60 compilation failures) and export missing internal functions (ADVANCE-TOKEN, EMIT-MODULE-HEADER) to enable higher Stage 1 compilation coverage. Additionally, handle AST-TAGBODY structure serialization for control flow compilation.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation, Wasm binary output to `dist/`)
**Testing**: rove test framework, `wasm-tools validate` for Wasm validation
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (Common Lisp compiler project)
**Performance Goals**: Stage 1 compilation coverage ≥ 25% (from 19%)
**Constraints**: Backward compatibility with existing compiled forms, Wasm validation must pass
**Scale/Scope**: 101 error occurrences across 5 error patterns to eliminate

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | Using WasmGC instructions (:local.set #x21, :local.tee #x22) |
| II. Lispオブジェクト表現規約 | ✅ PASS | No changes to NIL/UNBOUND representation |
| III. 関数・クロージャ実装戦略 | ✅ PASS | Runtime function table dispatch pattern maintained |
| IV. Wasm制御フロー活用 | ✅ PASS | TAGBODY compilation uses Wasm block/br control flow |
| V. シャローバインディング | N/A | No dynamic scope changes |
| VI. 段階的動的コンパイル | N/A | No eval/JIT changes |
| VII. テスト駆動開発（TDD） | ✅ REQUIRED | Unit tests for each fix before implementation |
| VIII. Nix-Firstワークフロー | ✅ PASS | Uses existing Nix environment |
| IX. ANSI Common Lisp仕様参照 | ✅ PASS | [tagbody](resources/HyperSpec/Body/s_tagbod.htm), [go](resources/HyperSpec/Body/s_go.htm) |

**Gate Result**: PASS - All applicable principles satisfied.

## Project Structure

### Documentation (this feature)

```text
specs/001-wasm-local-binding/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (N/A - no API contracts)
├── checklists/          # Validation checklists
│   └── requirements.md  # Spec quality checklist
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── codegen/
│   │   └── func-section.lisp    # LOCAL.SET/LOCAL.TEE binding, *runtime-function-table*
│   └── ast.lisp                 # AST-TAGBODY structure handling
├── reader/
│   └── parser.lisp              # ADVANCE-TOKEN function (line 34)
├── backend/
│   └── wasm-emit.lisp           # EMIT-MODULE-HEADER function (line 10)
└── package.lisp                 # Export declarations

tests/
├── unit/
│   ├── local-instruction-test.lisp   # NEW: LOCAL.SET/LOCAL.TEE tests
│   ├── advance-token-export-test.lisp # NEW: ADVANCE-TOKEN export tests
│   └── emit-header-export-test.lisp   # NEW: EMIT-MODULE-HEADER export tests
└── contract/
    └── stage1-coverage-test.lisp      # NEW: Coverage verification tests

dist/
├── clysm-stage1.wasm            # Generated Stage 1 binary
└── stage1-report.json           # Compilation statistics
```

**Structure Decision**: Uses existing single-project structure. New test files added under tests/unit/ for TDD compliance.

## Complexity Tracking

No constitution violations requiring justification.

## Implementation Strategy

### Fix Categories

1. **LOCAL.SET/LOCAL.TEE Binding** (FR-001, FR-002)
   - Location: `src/clysm/compiler/codegen/func-section.lisp`
   - Issue: Keywords `:local.set` and `:local.tee` appear unquoted in macro/backquote contexts during self-compilation
   - Solution: Ensure proper quoting in code generation paths

2. **ADVANCE-TOKEN Export** (FR-003, FR-004)
   - Source: `src/clysm/reader/parser.lisp:34`
   - Action: Export from clysm package, register in `*runtime-function-table*`

3. **EMIT-MODULE-HEADER Export** (FR-005, FR-006)
   - Source: `src/clysm/backend/wasm-emit.lisp:10`
   - Note: Already exported from `clysm/backend/wasm-emit` package
   - Action: Re-export from main clysm package, register in `*runtime-function-table*`

4. **AST-TAGBODY Serialization** (FR-007)
   - Location: `src/clysm/compiler/ast.lisp`
   - Issue: #S(CLYSM/COMPILER/AST:AST-TAGBODY ...) appears in error output
   - Solution: Add proper serialization/compilation handling for AST-TAGBODY in codegen

### Error Pattern Mapping

| Pattern ID | Error | Count | Fix |
|------------|-------|-------|-----|
| P221 | Unbound variable: LOCAL.SET | 40 | Proper quoting in codegen |
| P987 | Unbound variable: LOCAL.TEE | 20 | Proper quoting in codegen |
| P027 | Undefined function: ADVANCE-TOKEN | 22 | Export + runtime table |
| P143 | Undefined function: EMIT-MODULE-HEADER | 10 | Export + runtime table |
| P943 | #S(AST-TAGBODY ...) | 9 | Serialization handler |

## References

- [tagbody](resources/HyperSpec/Body/s_tagbod.htm) - ANSI CL TAGBODY special operator
- [go](resources/HyperSpec/Body/s_go.htm) - ANSI CL GO special operator
- Wasm Local Variable Instructions: opcode 0x20 (local.get), 0x21 (local.set), 0x22 (local.tee)
