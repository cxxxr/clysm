# Implementation Plan: Sequence Runtime Migration

**Branch**: `001-sequence-runtime-migration` | **Date**: 2026-01-01 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-sequence-runtime-migration/spec.md`

## Summary

Migrate sequence operations ([remove](resources/HyperSpec/Body/f_rm_rm.htm), [count](resources/HyperSpec/Body/f_countc.htm), [substitute](resources/HyperSpec/Body/f_sbs_s.htm), [delete](resources/HyperSpec/Body/f_rm_rm.htm)) from inline Wasm codegen in `func-section.lisp` to Lisp-implemented runtime functions using only Layer 1 primitives. The existing `*runtime-function-table*` dispatch mechanism will route calls to new runtime implementations in `src/clysm/lib/sequence-runtime.lisp`.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation, Wasm binary output)
**Testing**: rove (unit tests), wasm-tools (validation), SBCL for ANSI conformance reference
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (compiler)
**Performance Goals**: No regression >10% for sequence operations on typical workloads
**Constraints**: Runtime functions must use only Layer 1 primitives (car, cdr, cons, null, consp, funcall, eq, eql)
**Scale/Scope**: 12 functions to migrate across 4 families; ~600 lines of inline Wasm codegen to remove

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | ✅ PASS | Runtime functions compile to WasmGC via existing compiler |
| II. Lisp Object Representation | ✅ PASS | Uses existing NIL/cons representation |
| III. Function/Closure Strategy | ✅ PASS | Runtime functions are standard closures |
| IV. Wasm Control Flow | ✅ PASS | No special control flow requirements |
| V. Shallow Binding | ✅ PASS | No dynamic variable usage |
| VI. Tiered Eval/JIT | ✅ PASS | Not applicable |
| VII. TDD (Non-negotiable) | ✅ GATE | Tests must be written BEFORE runtime implementations |
| VIII. Nix-First | ✅ PASS | `nix flake check` must pass |
| IX. ANSI CL Spec References | ✅ GATE | All functions must link to HyperSpec |

**Gate Violations**: None

## Project Structure

### Documentation (this feature)

```text
specs/001-sequence-runtime-migration/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       └── func-section.lisp    # MODIFY: Remove inline codegen, add runtime registration
└── lib/
    ├── list-runtime.lisp        # EXISTING: Pattern reference
    ├── io-runtime.lisp          # EXISTING: Pattern reference
    └── sequence-runtime.lisp    # NEW: 12 runtime functions

tests/
└── unit/
    └── sequence-runtime/        # NEW: Test directory
        ├── remove-test.lisp
        ├── count-test.lisp
        ├── substitute-test.lisp
        └── delete-test.lisp
```

**Structure Decision**: Single project structure following existing `lib/*-runtime.lisp` pattern. New file `sequence-runtime.lisp` mirrors `list-runtime.lisp` structure with `-rt` suffix naming convention.

## Complexity Tracking

No violations to justify.

---

## Phase 0: Research (Complete)

See [research.md](./research.md) for details.

### Key Decisions

1. **Naming Convention**: Functions use `-rt` suffix (e.g., `remove-rt`) per existing pattern in `list-runtime.lisp`
2. **Sequence Type Scope**: Lists only for initial implementation (strings/vectors deferred)
3. **Keyword Argument Handling**: Parse at compile time, pass as regular arguments to runtime
4. **Delete Family Strategy**: Share implementation with remove family, using destructive cons mutation

---

## Phase 1: Design

See [data-model.md](./data-model.md) and [quickstart.md](./quickstart.md) for details.

### Runtime Function Signatures

All functions follow ANSI CL semantics with these keyword arguments:
- `:test` - Comparison function (default: `#'eql`)
- `:key` - Key extraction function (default: `nil`)
- `:start` - Start index (default: `0`)
- `:end` - End index (default: `nil` = end of list)
- `:count` - Maximum operations (remove/substitute only)
- `:from-end` - Process from end (default: `nil`)

### Integration Points

1. **Registration**: Add entries to `*runtime-function-table*` in `func-section.lisp`
2. **ASDF**: Add `sequence-runtime.lisp` to `clysm.asd` component list
3. **Compiler Dispatch**: Existing `compile-runtime-call` handles argument compilation and call emission

### Lines to Remove from func-section.lisp

| Function | Start Line | Approx Lines |
|----------|------------|--------------|
| compile-remove | 11485 | ~105 |
| compile-remove-if | 11591 | ~86 |
| compile-remove-if-not | 11678 | ~86 |
| compile-substitute | 11764 | ~85 |
| compile-substitute-if | 11849 | ~90 |
| compile-count | 11939 | ~80 |
| compile-count-if | 12019 | ~80 |
| **Total** | | **~612** |

Plus case clauses in dispatch (~30 lines) = **~642 lines removed** (3.5% reduction)

**Note**: Achieving 3.5% reduction with 12 functions; additional migrations can be pursued in follow-up features per research.md recommendations.
