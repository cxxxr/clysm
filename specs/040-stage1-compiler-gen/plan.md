# Implementation Plan: Stage 1 Compiler Generation (Phase 13D-7)

**Branch**: `040-stage1-compiler-gen` | **Date**: 2025-12-30 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/040-stage1-compiler-gen/spec.md`

## Summary

Generate a functional Stage 1 Wasm binary by compiling the entire Clysm compiler codebase (~45,000 lines) using SBCL's Clysm host compiler. Leverage Phase 13D-1~6 features ([aref](resources/HyperSpec/Body/f_aref.htm), [svref](resources/HyperSpec/Body/f_svref.htm), [coerce](resources/HyperSpec/Body/f_coerce.htm), [subseq](resources/HyperSpec/Body/f_subseq.htm), [concatenate](resources/HyperSpec/Body/f_concat.htm), [handler-case](resources/HyperSpec/Body/m_hand_1.htm), [values](resources/HyperSpec/Body/f_values.htm), [the](resources/HyperSpec/Body/s_the.htm), [labels](resources/HyperSpec/Body/s_flet_.htm), [loop](resources/HyperSpec/Body/m_loop.htm)) to achieve 80%+ compilation rate. Output `dist/clysm-stage1.wasm` (100KB+) passing `wasm-tools validate`.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation, Wasm binary output to `dist/`)
**Testing**: rove test framework + `wasm-tools validate`
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (compiler)
**Performance Goals**: 80%+ compilation rate, complete compilation within reasonable time
**Constraints**: Output >= 100KB, must pass `wasm-tools validate`
**Scale/Scope**: ~45,000 lines of source code, ~90 source files

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. WasmGC-First型システム | PASS | Output targets WasmGC with struct/array types |
| II. Lispオブジェクト表現規約 | PASS | NIL/UNBOUND conventions maintained in codegen |
| III. 関数・クロージャ実装 | PASS | Closure conversion via existing transform |
| IV. Wasm制御フロー活用 | PASS | `handler-case` uses `try_table`, tail calls available |
| V. シャローバインディング | PASS | Special vars use shallow binding via existing runtime |
| VI. Tiered Eval/JIT | N/A | Not applicable - batch compilation only |
| VII. TDD（非交渉） | PASS | Contract tests verify Wasm structure, integration tests verify output |
| VIII. Nix-First | PASS | Build uses `nix develop` shell with wasm-tools |
| IX. ANSI CL仕様参照 | PASS | HyperSpec references in spec/plan documents |

**Gate Result**: PASS - No violations requiring justification.

## Project Structure

### Documentation (this feature)

```text
specs/040-stage1-compiler-gen/
├── plan.md              # This file
├── spec.md              # Feature specification
├── research.md          # Phase 0 research findings
├── data-model.md        # Compilation entities and states
├── quickstart.md        # Developer guide for running Stage 1 build
├── contracts/           # Wasm structure contracts
│   └── stage1-wasm.md   # Stage 1 binary structure contract
└── tasks.md             # Implementation tasks (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── stage1/              # Stage 1 generation (existing)
│   ├── package.lisp     # Package definition
│   ├── types.lisp       # Data types (source-form, compilation-result)
│   ├── reader.lisp      # Source file reader
│   ├── generator.lisp   # Main generation logic [MODIFY]
│   ├── progress.lisp    # Progress reporting
│   ├── logging.lisp     # Compilation logging
│   ├── blocker.lisp     # Compilation blocker analysis [MODIFY]
│   └── fixpoint.lisp    # Fixpoint verification (future)
├── compiler/            # Core compiler (Phase 13D-1~6 features)
│   ├── codegen/         # Wasm code generation [VERIFY]
│   └── transform/       # AST transformations [VERIFY]
└── lib/                 # Standard library macros

build/
├── stage1-gen.lisp      # Entry script (existing)
└── stage1-complete.lisp # Full build script [CREATE]

tests/
├── contract/
│   └── stage1/          # Stage 1 binary structure tests [CREATE]
└── integration/
    └── stage1/          # End-to-end Stage 1 tests [CREATE]

dist/
├── clysm-stage1.wasm    # Stage 1 binary (output)
└── stage1-report.json   # Compilation report
```

**Structure Decision**: Use existing single-project structure. Stage 1 generation builds on existing `src/clysm/stage1/` infrastructure. New `build/stage1-complete.lisp` provides the FR-010 entry point.

## Complexity Tracking

No violations requiring justification.

## Implementation Phases

### Phase 1: Verify Phase 13D Features Integration

Ensure all Phase 13D-1~6 features are correctly integrated and functional:

1. **Verify codegen support** for each feature:
   - [aref](resources/HyperSpec/Body/f_aref.htm) / [svref](resources/HyperSpec/Body/f_svref.htm) → array access Wasm instructions
   - [coerce](resources/HyperSpec/Body/f_coerce.htm) → type conversion
   - [subseq](resources/HyperSpec/Body/f_subseq.htm) / [concatenate](resources/HyperSpec/Body/f_concat.htm) → sequence operations
   - [handler-case](resources/HyperSpec/Body/m_hand_1.htm) → `try_table` exception handling
   - [values](resources/HyperSpec/Body/f_values.htm) / [the](resources/HyperSpec/Body/s_the.htm) → multiple values and type declarations
   - [labels](resources/HyperSpec/Body/s_flet_.htm) → local function definitions
   - [loop](resources/HyperSpec/Body/m_loop.htm) → iteration macros

2. **Run existing unit tests** to confirm feature stability

### Phase 2: Enhance Stage 1 Generator

Extend `src/clysm/stage1/generator.lisp` for full-codebase compilation:

1. **Add module ordering** based on ASDF system dependencies
2. **Improve error recovery** to continue after failures
3. **Add per-file statistics** tracking
4. **Create aggregated Wasm module** combining all compiled forms

### Phase 3: Create Build Entry Script

Create `build/stage1-complete.lisp` (FR-010):

1. Load Clysm system via ASDF
2. Invoke `clysm/stage1:generate-stage1`
3. Report statistics to stdout
4. Exit with code 0 on success, 1 on fatal error

### Phase 4: Add Contract Tests

Create tests verifying Stage 1 binary structure:

1. **Size test**: Verify output >= 100KB
2. **Validation test**: Verify `wasm-tools validate` passes
3. **Structure test**: Verify module has expected sections (type, func, code)

### Phase 5: Integration Testing

End-to-end tests for the full build workflow:

1. Run `sbcl --load build/stage1-complete.lisp`
2. Verify `dist/clysm-stage1.wasm` exists
3. Verify compilation rate in report >= 80%
4. Verify `wasm-tools validate` passes

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Compilation rate < 80% | Medium | High | Identify and fix top blockers via blocker.lisp analysis |
| Generated Wasm invalid | Low | High | wasm-tools validate catches structural issues |
| Memory exhaustion during compilation | Low | Medium | Process files incrementally, not all at once |
| Phase 13D features not fully integrated | Low | Medium | Unit tests verify each feature before integration |

## Success Verification

1. `sbcl --load build/stage1-complete.lisp` completes without fatal error
2. `ls -la dist/clysm-stage1.wasm` shows file >= 100KB
3. `wasm-tools validate dist/clysm-stage1.wasm` exits with code 0
4. `dist/stage1-report.json` shows compilation rate >= 80%
