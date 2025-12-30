# Implementation Plan: Bootstrap Fixpoint Achievement

**Branch**: `001-bootstrap-fixpoint` | **Date**: 2025-12-30 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-bootstrap-fixpoint/spec.md`

## Summary

Achieve bootstrap fixpoint by exporting `compile_form` from Stage 1 and using it to compile Clysm into Stage 2. Success is either byte-identical Stage 1 == Stage 2 or a detailed blocker report identifying what prevents fixpoint.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, JavaScript (Node.js 20+) for runtime
**Primary Dependencies**: alexandria, babel (UTF-8), wasm-tools, wasmtime
**Storage**: N/A (in-memory compilation, Wasm binary output to dist/)
**Testing**: rove (Lisp), Jest (JavaScript), contract tests via wasm-tools validate
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single project (compiler bootstrap)
**Performance Goals**: Compilation completes within 5 minutes for full compiler
**Constraints**: Deterministic output (same input → identical bytes), current 14.2% compilation rate
**Scale/Scope**: ~1157 forms in compiler, 24.5KB Stage 1 output

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | All Stage 1 output uses GC types |
| II. Lispオブジェクト表現規約 | PASS | NIL/UNBOUND as singletons |
| III. 関数・クロージャ実装戦略 | PASS | Closure struct with arity dispatch |
| IV. Wasm制御フロー活用 | PASS | tail-call, exception handling |
| V. シャローバインディング | PASS | Special vars via $value field |
| VI. 段階的動的コンパイル | N/A | Not JIT, static bootstrap |
| VII. TDD（非交渉） | REQUIRES | Tests before implementation |
| VIII. Nix-Firstワークフロー | REQUIRES | nix flake check must pass |
| IX. ANSI CL仕様参照 | REQUIRES | HyperSpec links for CL functions |

## Project Structure

### Documentation (this feature)

```text
specs/001-bootstrap-fixpoint/
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
├── stage0/              # Stage 0 infrastructure (exports.lisp)
├── stage1/              # Stage 1 generation (runner.lisp, blocker.lisp)
└── stage2/              # Stage 2 generation (generator.lisp)

build/
├── stage1-complete.lisp # Stage 1 generation entry point
├── stage2-gen.lisp      # Stage 2 generation entry point
└── fixpoint-check.lisp  # Fixpoint verification script

host-shim/
├── stage1-runner.js     # Stage 1 runtime (already has compile_form handling)
├── stage2-gen.js        # Stage 2 generation script (NEW)
├── io-shim.js           # I/O FFI shim
└── fs-shim.js           # Filesystem FFI shim

scripts/
├── verify-fixpoint.sh   # Fixpoint verification
├── run-stage2-gen.sh    # Stage 2 generation wrapper
└── generate-stage2.sh   # Alternative Stage 2 script

dist/
├── clysm-stage1.wasm    # Stage 1 binary (24.5KB)
├── clysm-stage2.wasm    # Stage 2 binary (NEW)
├── stage1-report.json   # Stage 1 compilation report
└── stage2-report.json   # Stage 2 compilation report (NEW)

tests/
├── contract/            # Wasm validation tests
├── integration/         # End-to-end tests
└── unit/                # Unit tests
```

**Structure Decision**: Single project structure. Stage 2 generation extends existing Stage 1 infrastructure with new host-shim/stage2-gen.js script.

## Complexity Tracking

No constitution violations requiring justification.

## Implementation Approach

### Phase 1: compile_form Export

1. Modify `src/clysm/stage1/` to mark compile_form for export in generated Wasm
2. Verify export appears in Stage 1 module exports via stage1-runner.js
3. Test basic invocation with simple form `(+ 1 2)`

### Phase 2: Stage 2 Generation Script

1. Create `host-shim/stage2-gen.js` that:
   - Loads Stage 1 Wasm
   - Iterates through compiler source files
   - Calls compile_form for each form
   - Aggregates output into Stage 2 Wasm binary
2. Track successful/failed compilations for blocker report

### Phase 3: Fixpoint Verification

1. Extend `scripts/verify-fixpoint.sh` to compare Stage 1 and Stage 2
2. If mismatch: generate detailed diff (offset, size, section differences)
3. If identical: output success confirmation

### Phase 4: Blocker Reporting

1. Aggregate failed forms by error category
2. Generate JSON report with top blockers by frequency
3. Include remediation suggestions based on error type

## Key Files to Modify

| File | Changes |
|------|---------|
| `src/clysm/stage1/runner.lisp` | Add export directive for compile_form |
| `host-shim/stage2-gen.js` | NEW: Stage 2 generation script |
| `scripts/verify-fixpoint.sh` | Enhance diff reporting |
| `build/stage2-gen.lisp` | Entry point for SBCL-based Stage 2 fallback |
| `tests/contract/fixpoint.lisp` | Fixpoint verification tests |

## Success Metrics

- [ ] Stage 1 exports compile_form (verifiable via wasm-tools print)
- [ ] Stage 2 generation script completes (even with failures)
- [ ] wasm-tools validate Stage 2 exits 0
- [ ] Fixpoint verification produces comparison result
- [ ] Blocker report generated with categorized failures
