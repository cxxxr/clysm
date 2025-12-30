# Implementation Plan: Arithmetic Primitives 1- and 1+

**Branch**: `001-arithmetic-primitives` | **Date**: 2025-12-31 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-arithmetic-primitives/spec.md`

## Summary

Add ANSI Common Lisp arithmetic primitives [1-](resources/HyperSpec/Body/f_1pl_1_.htm) and [1+](resources/HyperSpec/Body/f_1pl_1_.htm) to the Clysm compiler. These are unary operators equivalent to `(- x 1)` and `(+ x 1)` respectively. Implementation follows the established primitive compilation pattern in `func-section.lisp`.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams
**Storage**: N/A (in-memory compilation)
**Testing**: rove (testing framework)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (compiler project)
**Performance Goals**: N/A (trivial arithmetic operations)
**Constraints**: Generated Wasm must pass `wasm-tools validate`
**Scale/Scope**: 2 new primitives, ~50 lines of code

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | Uses existing i31ref arithmetic infrastructure |
| II. Lisp Object Representation | PASS | No new object types required |
| III. Function/Closure Strategy | PASS | Not affected |
| IV. Wasm Control Flow | PASS | Not affected |
| V. Shallow Binding | PASS | Not affected |
| VI. Tiered Eval/JIT | PASS | Not affected |
| VII. TDD | MUST FOLLOW | Tests first, then implementation |
| VIII. Nix-First | PASS | Use `nix develop` environment |
| IX. ANSI CL HyperSpec | MUST FOLLOW | HyperSpec links included above |

**Gate Status**: PASS - No violations

## Project Structure

### Documentation (this feature)

```text
specs/001-arithmetic-primitives/
├── plan.md              # This file
├── research.md          # Phase 0 output (minimal - pattern already known)
├── spec.md              # Feature specification
└── checklists/
    └── requirements.md  # Quality checklist
```

### Source Code (repository root)

```text
src/clysm/compiler/codegen/
└── func-section.lisp    # Main file to modify (primitive list + compile functions)

tests/
├── unit/
│   └── 001-arithmetic-primitives/
│       └── arithmetic-test.lisp   # Unit tests for 1- and 1+
└── contract/
    └── 001-arithmetic-primitives/
        └── wasm-validation-test.lisp  # Wasm validation tests
```

**Structure Decision**: Single project structure. All changes confined to `func-section.lisp` following the existing primitive compilation pattern.

## Implementation Approach

### Pattern Analysis

Based on `compile-unary-minus` (func-section.lisp:3420-3427):

```lisp
(defun compile-unary-minus (arg env)
  "Compile unary minus: (- x) => (- 0 x)."
  (append
   '((:i32.const 0))
   (compile-to-instructions arg env)
   '((:ref.cast :i31) :i31.get_s
     :i32.sub
     :ref.i31)))
```

### Implementation for 1-

```lisp
(defun compile-1- (args env)
  "Compile (1- x) => (- x 1).
   Stack: [] -> [anyref (i31ref)]"
  (when (/= (length args) 1)
    (error "1- requires exactly 1 argument"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s
     (:i32.const 1)
     :i32.sub
     :ref.i31)))
```

### Implementation for 1+

```lisp
(defun compile-1+ (args env)
  "Compile (1+ x) => (+ x 1).
   Stack: [] -> [anyref (i31ref)]"
  (when (/= (length args) 1)
    (error "1+ requires exactly 1 argument"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s
     (:i32.const 1)
     :i32.add
     :ref.i31)))
```

### Changes Required

1. **Primitive List** (func-section.lisp ~line 725): Add `1-` and `1+` after basic arithmetic operators
2. **Case Statement** (func-section.lisp ~line 931): Add dispatch cases for `1-` and `1+`
3. **Compile Functions** (func-section.lisp ~line 3428): Add `compile-1-` and `compile-1+` after `compile-unary-minus`

## Complexity Tracking

> No complexity tracking needed - no Constitution violations.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| None | N/A | N/A |
