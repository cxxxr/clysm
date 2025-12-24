# Implementation Plan: Eval/JIT Compile System

**Branch**: `017-eval-jit-compile` | **Date**: 2025-12-24 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/017-eval-jit-compile/spec.md`

## Summary

Complete the Eval/JIT infrastructure by implementing a fully functional `compile` function that returns callable compiled functions, with automatic tier promotion from Tier 1 (interpreter) to Tier 2 (JIT-compiled Wasm) based on hot spot detection. The existing `eval/interpreter.lisp` provides Tier 1 execution, `eval/jit.lisp` provides the Tier 2 JIT compilation foundation, and `compile-to-wasm` handles Wasm binary generation.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - compiler implementation
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory data structures for function slots, invocation counters)
**Testing**: Rove (via `nix flake check` or `(asdf:test-system :clysm)`)
**Target Platform**: WebAssembly GC (browser/wasmtime)
**Project Type**: Single project - Common Lisp compiler producing Wasm binaries
**Performance Goals**: JIT compilation ≤100ms for 1000 lines (per constitution)
**Constraints**: Single-threaded runtime, Wasm sandbox preservation
**Scale/Scope**: Support all lambda expressions with existing special forms

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | JIT uses existing GC type infrastructure |
| II. Lispオブジェクト表現規約 | ✅ PASS | No changes to NIL/UNBOUND representation |
| III. 関数・クロージャ実装戦略 | ✅ PASS | Uses existing closure struct with arity dispatch |
| IV. Wasm制御フロー活用 | ✅ PASS | Tier 2 uses compile-to-wasm with tail-call |
| V. シャローバインディング | ✅ PASS | Interpreter uses interpreter-env, not shallow binding |
| VI. 段階的動的コンパイル | ✅ PASS | **Core principle** - this feature implements it |
| VII. TDD（非交渉） | ✅ PASS | Tests first, then implementation |
| VIII. Nix-Firstワークフロー | ✅ PASS | All builds via `nix flake check` |

**Gate Result**: PASS - All principles satisfied

## Project Structure

### Documentation (this feature)

```text
specs/017-eval-jit-compile/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (internal API contracts)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── eval/
│   ├── interpreter.lisp  # Tier 1 - EXISTS, fully functional
│   ├── jit.lisp           # Tier 2 foundation - EXISTS, needs integration
│   ├── eval.lisp          # eval* function - EXISTS
│   └── compile.lisp       # compile* function - EXISTS, needs completion
├── compiler/
│   └── compiler.lisp      # compile-to-wasm - EXISTS
└── package.lisp           # Package definitions - EXISTS

tests/
├── unit/
│   └── interpreter-test.lisp  # EXISTS
├── integration/
│   ├── eval-test.lisp         # EXISTS
│   └── jit-test.lisp          # EXISTS, needs expansion
└── contract/
    └── (new tier-switching contract tests)
```

**Structure Decision**: Single project structure maintained. All changes within existing `src/clysm/eval/` module with new tests in `tests/`.

## Complexity Tracking

No constitution violations requiring justification.

## Implementation Approach

### Key Changes to Existing Code

1. **compile.lisp**: Complete the `compile*` function to:
   - Return wrapped functions with invocation tracking
   - Integrate with function slot registration for named functions
   - Support tier promotion triggers

2. **jit.lisp**: Connect JIT compilation to actual Wasm execution:
   - Improve `instantiate-wasm` to properly link runtime imports
   - Implement actual function extraction from Wasm instances

3. **New: tier-switching infrastructure**:
   - Wrapper functions that count invocations
   - Promotion logic triggered at threshold
   - Graceful degradation when Tier 2 compilation fails

### Tier Promotion Flow

```
[compile* called]
       ↓
[Create Tier 1 function via interpreter]
       ↓
[Wrap with invocation counter]
       ↓
[Register in function slot if named]
       ↓
[On each call: increment counter]
       ↓
[If count > threshold]
       ↓
[Attempt Tier 2 JIT compilation]
       ↓
[Success?] → Yes → [Hot-patch function slot]
       ↓ No
[Remain in Tier 1 (graceful degradation)]
```
