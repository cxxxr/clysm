# Implementation Plan: Numeric Conversion and Formatting (Phase 14C)

**Branch**: `001-numeric-format` | **Date**: 2025-12-30 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-numeric-format/spec.md`

## Summary

Implement ANSI CL numeric conversion functions [rationalize](../../resources/HyperSpec/Body/f_ration.htm) (float→ratio via continued fraction approximation) and [write-to-string](../../resources/HyperSpec/Body/f_wr_to_.htm) with `:base` keyword support for radix conversion output. Target: numbers test category 50%+ pass rate.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Primary Dependencies**: alexandria, babel (UTF-8)
**Storage**: N/A (in-memory compilation, Wasm binary output to `dist/`)
**Testing**: rove (existing test framework), contract tests
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (compiler)
**Performance Goals**: Standard function call overhead (not performance-critical)
**Constraints**: WasmGC bytecode must pass `wasm-tools validate`
**Scale/Scope**: 2 functions, ~300-500 lines of codegen

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. WasmGC-First | PASS | Uses existing `$ratio` (type 15) and `$float` (type 16) structs |
| II. Lisp Object Representation | PASS | Ratio already defined with numerator/denominator fields |
| III. Function/Closure Strategy | PASS | Standard function compilation pattern |
| IV. Wasm Control Flow | PASS | No special control flow needed |
| V. Shallow Binding | N/A | No dynamic variables |
| VI. Tiered Eval/JIT | PASS | Interpreter registration exists for `rationalize` |
| VII. TDD (Non-negotiable) | PENDING | Tests will be written before implementation |
| VIII. Nix-First Workflow | PASS | Uses existing `nix develop` environment |
| IX. ANSI CL Spec Reference | PASS | HyperSpec links included above |

**Gate Result**: PASS - All applicable principles satisfied.

## Project Structure

### Documentation (this feature)

```text
specs/001-numeric-format/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       └── func-section.lisp    # Add compile-rationalize, compile-write-to-string
├── lib/
│   └── macros.lisp              # Continued fraction helper (if needed)
├── eval/
│   └── interpreter-builtins.lisp # Register write-to-string
└── streams/
    └── format.lisp              # Extend with write-to-string

tests/
├── contract/
│   └── numeric-format-test.lisp # Wasm output validation
├── unit/
│   └── numeric/
│       ├── rationalize-test.lisp
│       └── write-to-string-test.lisp
└── integration/
    └── numeric-format-test.lisp # End-to-end compilation tests
```

**Structure Decision**: Single project structure following existing Clysm layout. New functions added to existing `func-section.lisp` (codegen), `interpreter-builtins.lisp` (eval), and `format.lisp` (streams).

## Complexity Tracking

No constitution violations requiring justification.

## Implementation Approach

### `rationalize` Implementation

**Location**: `src/clysm/compiler/codegen/func-section.lisp`

**Algorithm**: Continued fraction approximation (Stern-Brocot)
- Input: IEEE 754 double-precision float
- Output: Ratio with "reasonably small" denominator
- Edge cases: integers/ratios pass through, NaN/infinity signal error

**Existing Infrastructure**:
- `compile-rational` at line 6199 (reference pattern)
- Ratio type constructor available (type index 15)
- `numerator`/`denominator` accessors at lines 1336-1384

### `write-to-string` Implementation

**Location**: `src/clysm/compiler/codegen/func-section.lisp` + `src/clysm/streams/format.lisp`

**Features**:
- `:base` keyword (2-36), default 10
- Uppercase A-Z for digits 10-35
- Handle: fixnum, bignum, ratio, float

**Existing Infrastructure**:
- `extract-keyword-args` at line 117 (keyword parsing)
- `parse-integer` at line 6271 (reference for :radix handling)
- `prin1-to-string` at format.lisp:340 (extend pattern)
