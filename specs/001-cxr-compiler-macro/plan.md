# Implementation Plan: CXR Compiler Macro Consolidation

**Branch**: `001-cxr-compiler-macro` | **Date**: 2026-01-03 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-cxr-compiler-macro/spec.md`

## Summary

Refactor 12 identical compile-cXXr functions into a single `define-cxr-compiler` macro that generates these functions automatically. Each function follows the pattern `(compile-cxr-chain "ops" args env)` where "ops" is a string like "aa", "da", "dda". The macro will reduce ~60 lines of boilerplate to ~20 lines while maintaining identical runtime behavior.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria (hash-table utilities), existing clysm compiler infrastructure
**Storage**: N/A (in-memory compilation)
**Testing**: rove (unit tests), wasm-tools (validation)
**Target Platform**: WasmGC (compiler code generation module)
**Project Type**: single (compiler codebase)
**Performance Goals**: No runtime impact (compile-time macro only)
**Constraints**: Generated functions must produce identical Wasm output
**Scale/Scope**: 12 functions consolidated, ~40 line reduction

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | No changes to Wasm type system |
| II. Lisp Object Representation | PASS | No changes to object representation |
| III. Function/Closure Strategy | PASS | No changes to closure model |
| IV. Wasm Control Flow | PASS | No changes to control flow |
| V. Shallow Binding | PASS | No changes to dynamic scope |
| VI. Tiered Eval/JIT | PASS | No changes to JIT |
| VII. TDD (Non-negotiable) | **REQUIRED** | Unit tests for macro expansion and generated functions |
| VIII. Nix-First | PASS | No new dependencies |
| IX. ANSI CL References | PASS | [caar](resources/HyperSpec/Body/f_car_c.htm), [cadr](resources/HyperSpec/Body/f_car_c.htm), etc. are standard CL accessors |

**Gate Status**: PASS - Proceed to implementation

## Project Structure

### Documentation (this feature)

```text
specs/001-cxr-compiler-macro/
├── plan.md              # This file
├── spec.md              # Feature specification
├── research.md          # Phase 0 output (minimal - straightforward design)
├── data-model.md        # Phase 1 output (macro interface)
├── quickstart.md        # Phase 1 output
└── checklists/
    └── requirements.md  # Quality checklist
```

### Source Code (repository root)

```text
src/clysm/compiler/codegen/
└── func-section.lisp    # Lines 4130-4221: cXr accessors section

tests/unit/
└── cxr-macro-test.lisp  # New: Unit tests for define-cxr-compiler
```

**Structure Decision**: Minimal change - modify single file (func-section.lisp), add unit test file.

## Complexity Tracking

No violations requiring justification. This is a simple code deduplication refactoring.

## Design Overview

### Macro Interface

```lisp
;; Define a compile-cXXr function
(define-cxr-compiler name ops-string)

;; Examples:
(define-cxr-compiler caar "aa")   ; → compile-caar
(define-cxr-compiler cadr "da")   ; → compile-cadr
(define-cxr-compiler caddr "dda") ; → compile-caddr
```

### Macro Expansion

```lisp
(define-cxr-compiler caddr "dda")
;; expands to:
(defun compile-caddr (args env)
  "Compile (caddr x) = (car (cdr (cdr x))).
   Feature: 001-cxr-compiler-macro"
  (compile-cxr-chain "dda" args env))
```

### Validation Logic

The macro performs compile-time validation:
1. `ops-string` must be non-empty
2. `ops-string` must contain only 'a' and 'd' characters
3. Signal `error` at macroexpansion time if invalid

### Before/After Comparison

**Before** (lines 4163-4221, 59 lines):
```lisp
(defun compile-caar (args env)
  "Compile (caar x) = (car (car x)).
   Feature: 043-self-hosting-blockers"
  (compile-cxr-chain "aa" args env))

(defun compile-cadr (args env)
  "Compile (cadr x) = (car (cdr x)).
   Feature: 043-self-hosting-blockers"
  (compile-cxr-chain "da" args env))
;; ... 10 more identical patterns
```

**After** (~20 lines):
```lisp
(defmacro define-cxr-compiler (name ops)
  "Generate a compile-cXXr function for the given operation sequence.
   NAME: Symbol like CAAR, CADR, CADDR
   OPS: String like \"aa\", \"da\", \"dda\" (a=car, d=cdr)"
  (check-type ops string)
  (assert (plusp (length ops)) (ops) "Operation string must be non-empty")
  (assert (every (lambda (c) (member c '(#\a #\d))) ops)
          (ops) "Operation string must contain only 'a' and 'd'")
  (let ((func-name (intern (format nil "COMPILE-~A" name)))
        (docstring (format nil "Compile (~(~A~) x) = ~A.~%   Feature: 001-cxr-compiler-macro"
                           name (ops-to-expansion name ops))))
    `(defun ,func-name (args env)
       ,docstring
       (compile-cxr-chain ,ops args env))))

;; All 12 accessors in one block
(define-cxr-compiler caar "aa")
(define-cxr-compiler cadr "da")
(define-cxr-compiler cdar "ad")
(define-cxr-compiler cddr "dd")
(define-cxr-compiler caaar "aaa")
(define-cxr-compiler caadr "daa")
(define-cxr-compiler cadar "ada")
(define-cxr-compiler caddr "dda")
(define-cxr-compiler cdaar "aad")
(define-cxr-compiler cdadr "dad")
(define-cxr-compiler cddar "add")
(define-cxr-compiler cdddr "ddd")
```

## Implementation Phases

### Phase 1: Create Unit Tests (TDD)
1. Create `tests/unit/cxr-macro-test.lisp`
2. Test macro expansion produces correct defun form
3. Test validation rejects empty strings
4. Test validation rejects invalid characters
5. Run tests - expect failures (Red)

### Phase 2: Implement Macro
1. Add `define-cxr-compiler` macro to func-section.lisp (before existing functions)
2. Add helper `ops-to-expansion` for docstring generation
3. Run tests - expect pass (Green)

### Phase 3: Replace Functions
1. Replace 12 defun forms with 12 macro invocations
2. Verify line count reduction (target: 40+ lines)
3. Run all existing tests

### Phase 4: Verification
1. Run full test suite: `sbcl --eval "(asdf:test-system :clysm)"`
2. Run Stage 1 compilation: `sbcl --load build/stage1-complete.lisp`
3. Validate Wasm: `wasm-tools validate dist/clysm-stage1.wasm`
4. Confirm no new warnings

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Macro expansion differs from original | Low | High | Exact character-for-character comparison of expanded forms |
| Package/export issues | Low | Medium | Macro is internal; no export changes needed |
| Test coverage gap | Low | Medium | Existing cXr tests + new macro-specific tests |

## Success Metrics

- [ ] Unit tests for macro pass
- [ ] All existing tests pass unchanged
- [ ] Stage 1 compilation succeeds
- [ ] wasm-tools validate passes
- [ ] Line count in cXr section reduced by 40+
- [ ] No new compiler warnings
