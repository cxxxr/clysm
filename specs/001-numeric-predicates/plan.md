# Implementation Plan: Phase 14B - Numeric Type Predicates Enhancement

**Branch**: `001-numeric-predicates` | **Date**: 2025-12-30 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-numeric-predicates/spec.md`

## Summary

Implement ANSI CL bit testing functions ([logbitp](resources/HyperSpec/Body/f_logbtp.htm), [logtest](resources/HyperSpec/Body/f_logtes.htm)), byte specifier functions ([byte](resources/HyperSpec/Body/f_by_by.htm), [byte-size](resources/HyperSpec/Body/f_by_by.htm), [byte-position](resources/HyperSpec/Body/f_by_by.htm)), and byte manipulation functions ([ldb](resources/HyperSpec/Body/f_ldb.htm), [dpb](resources/HyperSpec/Body/f_dpb.htm), [mask-field](resources/HyperSpec/Body/f_mask_f.htm), [deposit-field](resources/HyperSpec/Body/f_deposi.htm)) as Wasm-compiled primitives.

**Note**: Sign predicates (plusp, minusp, zerop) and parity predicates (oddp, evenp) are **already implemented** in `func-section.lisp:4414-4603`. This plan focuses on the remaining 10 functions.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) host compiler, WasmGC target
**Primary Dependencies**: alexandria, babel (UTF-8), existing bit operations (ash, logand, logior, logxor)
**Storage**: N/A (in-memory compilation, Wasm binary output)
**Testing**: rove (unit tests), wasm-tools validate (contract tests), wasmtime (integration)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single (compiler feature addition)
**Performance Goals**: Fixnum operations comparable to native i32 ops (< 2x overhead)
**Constraints**: All operations must work within i31ref (31-bit signed) fixnum range; bignum support deferred
**Scale/Scope**: 10 new functions in func-section.lisp

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Using i31ref for byte specifiers, i32 ops for bit manipulation |
| II. Lispオブジェクト表現規約 | PASS | Returns T (i31ref 1) or NIL (ref.null :none) for predicates |
| III. 関数・クロージャ実装戦略 | N/A | These are primitive built-in functions, not closures |
| IV. Wasm制御フロー活用 | PASS | Simple conditional paths, no exception handling needed |
| V. シャローバインディング | N/A | No dynamic bindings used |
| VI. 段階的動的コンパイル | PASS | Functions compile to direct Wasm instructions |
| VII. テスト駆動開発（TDD） | PENDING | Tests must be written before implementation |
| VIII. Nix-Firstワークフロー | PASS | Using existing nix develop environment |
| IX. ANSI CL仕様参照規約 | PASS | HyperSpec links included in this document |

## Project Structure

### Documentation (this feature)

```text
specs/001-numeric-predicates/
├── plan.md              # This file
├── research.md          # Phase 0 output - byte specifier encoding decision
├── data-model.md        # Phase 1 output - byte specifier structure
├── quickstart.md        # Phase 1 output - implementation guide
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/compiler/codegen/
├── func-section.lisp    # Main implementation file (add compile-* functions)
└── gc-types.lisp        # No changes needed (using existing i31ref)

tests/
├── unit/
│   └── numeric-predicates-test.lisp  # NEW: Unit tests for all 10 functions
├── contract/
│   └── byte-ops-wasm-test.lisp       # NEW: Wasm output validation
└── integration/
    └── byte-ops-run-test.lisp        # NEW: Runtime verification
```

**Structure Decision**: Single project structure. All implementation goes into existing `func-section.lisp` following established patterns for `compile-logand`, `compile-ash`, etc.

## Implementation Strategy

### Byte Specifier Encoding

Based on research, use **fixnum encoding** (`(logior (ash size 6) position)`):
- Maximum size: 31 bits (fits in 5 bits, but use 6 for safety)
- Maximum position: 63 bits (fits in 6 bits)
- Encoding: `(size << 6) | position` stored as i31ref
- Decoding: `byte-size = (spec >> 6)`, `byte-position = (spec & 63)`

This matches SBCL/CCL patterns and enables compile-time constant folding.

### Function Dependencies

```text
Level 0 (no deps):     logbitp, logtest
Level 1 (byte):        byte, byte-size, byte-position
Level 2 (byte + ash):  ldb, mask-field
Level 3 (ldb):         dpb, deposit-field
```

## Complexity Tracking

No constitution violations requiring justification. Implementation follows existing patterns in `func-section.lisp`.
