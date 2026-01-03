# Implementation Plan: String Runtime Migration

**Branch**: `001-string-runtime-migration` | **Date**: 2026-01-03 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `specs/001-string-runtime-migration/spec.md`

## Summary

Migrate 5 string manipulation functions from inline Wasm codegen to a Lisp runtime library. Create `lib/string-runtime.lisp` following the pattern established in `sequence-runtime.lisp`, register functions in `*runtime-function-table*` for dispatch, and remove ~710 lines of compile-* functions from func-section.lisp.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target
**Primary Dependencies**: alexandria, babel (UTF-8), existing runtime dispatch infrastructure
**Storage**: N/A (in-memory compilation)
**Testing**: rove (unit tests), wasm-tools validate (contract tests)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single project (compiler)
**Performance Goals**: Runtime dispatch overhead acceptable (validated in 001-sequence-runtime-migration)
**Constraints**: Layer 1 primitives only (char, schar, length, make-string) for self-hosting compatibility
**Scale/Scope**: 5 functions, ~710 lines removal, ~300-400 lines runtime library

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | Runtime functions compile to WasmGC structs/arrays |
| II. Lisp Object Representation | PASS | Uses $string type (index 2), NIL singleton |
| III. Function/Closure Strategy | PASS | Runtime functions follow closure convention |
| IV. Wasm Control Flow | N/A | No tail calls or exceptions in string functions |
| V. Shallow Binding | N/A | No dynamic variables used |
| VI. Tiered Eval/JIT | N/A | Not eval-related |
| VII. TDD (Non-negotiable) | REQUIRED | Tests must be written before implementation |
| VIII. Nix-First | PASS | Uses existing flake.nix environment |
| IX. ANSI CL Spec References | REQUIRED | HyperSpec links required for all functions |

## Project Structure

### Documentation (this feature)

```text
specs/001-string-runtime-migration/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── lib/
│   ├── sequence-runtime.lisp    # Existing pattern to follow
│   ├── list-runtime.lisp        # Existing runtime library
│   ├── io-runtime.lisp          # Existing runtime library
│   └── string-runtime.lisp      # NEW: String runtime functions
├── compiler/
│   └── codegen/
│       └── func-section.lisp    # MODIFY: Remove compile-* functions

tests/
├── unit/
│   └── string-runtime-test.lisp # NEW: Unit tests for runtime functions
└── contract/
    └── string-runtime/          # NEW: Contract tests for Wasm output
```

**Structure Decision**: Single project structure following existing compiler layout. New runtime library file follows established pattern in lib/ directory.

## Complexity Tracking

No constitution violations requiring justification. All work follows established patterns.

## Target Functions Analysis

| Function | Lines | ANSI Spec | Key Features |
|----------|-------|-----------|--------------|
| compile-string-char | 195 | [char](resources/HyperSpec/Body/f_char_.htm), [schar](resources/HyperSpec/Body/f_char_.htm) | UTF-8 byte iteration, codepoint decode |
| compile-string-capitalize | 120 | [string-capitalize](resources/HyperSpec/Body/f_stg_up.htm) | Word boundary detection, case conversion |
| compile-string-trim | 135 | [string-trim](resources/HyperSpec/Body/f_stg_tr.htm) | Character bag matching, :start/:end |
| compile-string-compare-ci | 220 | [string-equal](resources/HyperSpec/Body/f_stgeq_.htm) | Case-insensitive, mismatch index return |
| compile-nstring-capitalize | 120 | [nstring-capitalize](resources/HyperSpec/Body/f_stg_up.htm) | Destructive, modifies in-place |
| **Total** | ~710 | | |

## Implementation Approach

### Pattern from sequence-runtime.lisp

1. Runtime functions use `-rt` suffix (e.g., `string-trim-rt`)
2. Layer 1 primitives only for self-hosting
3. HyperSpec references in docstrings
4. Registration via `register-runtime-function`

### Runtime Dispatch Mechanism

```lisp
;; In func-section.lisp (existing)
(register-runtime-function 'string-trim :$string-trim-rt 2)

;; Compiler generates call to :$string-trim-rt instead of inline Wasm
```

### UTF-8 Handling Strategy

String-char requires UTF-8 codepoint iteration. Runtime implementation can use:
- char-code, code-char for codepoint handling
- aref on string for byte-level access
- schar for simple character access (ASCII fast path)
