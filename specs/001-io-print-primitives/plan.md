# Implementation Plan: I/O Print Primitives

**Branch**: `001-io-print-primitives` | **Date**: 2025-12-31 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-io-print-primitives/spec.md`

## Summary

Add compilation support for Common Lisp I/O print primitives ([print](resources/HyperSpec/Body/f_wr_pr.htm), [prin1](resources/HyperSpec/Body/f_wr_pr.htm), [princ](resources/HyperSpec/Body/f_wr_pr.htm), [terpri](resources/HyperSpec/Body/f_terpri.htm), [write](resources/HyperSpec/Body/f_wr_pr.htm)) and [format](resources/HyperSpec/Body/f_format.htm) function to WasmGC. This enables DEFUN bodies containing print operations to compile successfully, unblocking progress toward the 55% compilation rate target for self-hosting.

The implementation leverages existing infrastructure:
- FFI I/O functions (`%host-write-char`, `%host-write-string`) already defined in `src/clysm/streams/ffi-io.lisp`
- Format function already implemented in interpreter mode in `src/clysm/streams/format.lisp`
- Host shim I/O support already exists in `host-shim/io-shim.js`

Primary work: Add compiler primitive handlers for print functions that generate appropriate Wasm FFI calls.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) host compiler, WasmGC target
**Primary Dependencies**: alexandria, babel (UTF-8), existing FFI infrastructure (feature 027), existing format implementation
**Storage**: N/A (in-memory compilation)
**Testing**: rove (unit tests), wasm-tools (validation), wasmtime (execution)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (compiler feature)
**Performance Goals**: Compilation rate increase toward 55% (from current ~25%)
**Constraints**: Generated Wasm must pass `wasm-tools validate`
**Scale/Scope**: 4 print functions + 6 format directives + write function

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Compliance | Notes |
|-----------|------------|-------|
| I. WasmGC-First型システム設計 | ✓ PASS | Uses existing WasmGC type infrastructure; strings are `(ref $string)` |
| II. Lispオブジェクト表現規約 | ✓ PASS | NIL handling uses existing singleton pattern |
| III. 関数・クロージャ実装戦略 | ✓ PASS | Print functions compiled as closures via existing infrastructure |
| IV. Wasm制御フロー活用 | N/A | No special control flow requirements |
| V. シャローバインディング | N/A | No dynamic variable changes |
| VI. Tiered Eval/JIT | ✓ PASS | Format already works in interpreter; this adds compilation support |
| VII. TDD | ✓ REQUIRED | Tests must be written before implementation |
| VIII. Nix-First | ✓ REQUIRED | Must use `nix develop` environment |
| IX. ANSI CL仕様参照規約 | ✓ REQUIRED | HyperSpec links included for all functions |

**Gate Status**: PASS - All applicable principles satisfied or marked as required actions.

## Project Structure

### Documentation (this feature)

```text
specs/001-io-print-primitives/
├── spec.md              # Feature specification
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── checklists/
    └── requirements.md  # Quality checklist
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       └── func-section.lisp   # ADD: print/prin1/princ/terpri/write handlers
├── streams/
│   ├── ffi-io.lisp             # EXISTING: FFI declarations (reuse)
│   ├── format.lisp             # EXISTING: format implementation (extend)
│   ├── write.lisp              # EXISTING: write-char/write-string
│   └── types.lisp              # EXISTING: stream types
└── ffi/
    ├── marshalling.lisp        # EXISTING: type marshalling (reuse)
    └── import-gen.lisp         # EXISTING: import section generation (reuse)

tests/
├── unit/
│   └── io/
│       └── print-primitives-test.lisp  # NEW: unit tests
├── contract/
│   └── io/
│       └── print-wasm-test.lisp        # NEW: Wasm contract tests
└── integration/
    └── io/
        └── print-self-host-test.lisp   # NEW: end-to-end tests

host-shim/
└── io-shim.js              # EXISTING: writeChar/writeString (no changes needed)
```

**Structure Decision**: Single project structure; compiler source in `src/clysm/`, tests in `tests/`.

## Complexity Tracking

No violations requiring justification. Implementation leverages existing infrastructure without adding new patterns or abstractions.
