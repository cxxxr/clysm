# Research: Advanced Defmacro and Compile-Time Macro Expansion

**Feature**: 042-advanced-defmacro | **Date**: 2025-12-28

## Executive Summary

Research confirms the existing macro system (Feature 016) provides a solid foundation that can be extended for `&whole` and `&environment` support. The key architectural decisions involve integrating existing destructuring support and designing a WasmGC-compatible environment type.

---

## Research Topic 1: Existing Macro System Architecture

### Decision
Extend the existing `src/clysm/compiler/transform/macro.lisp` rather than rewriting.

### Rationale
The current implementation provides:
- Working macro registry with hash-table storage
- Functional `macroexpand-1*` and `macroexpand*` with 1000-step limit
- Backquote expansion support
- Integration with compiler pipeline

### Alternatives Considered
1. **Complete rewrite**: Rejected - too much working code to replace
2. **Separate macro2.lisp**: Rejected - would fragment the system
3. **Extend in place**: Selected - minimal disruption, proven patterns

### Key Files
- `src/clysm/compiler/transform/macro.lisp` (448 lines)
- `src/clysm/compiler/codegen/func-section.lisp` (lines 4923-4969)

---

## Research Topic 2: &whole Parameter Implementation

### Decision
Extract `&whole` before all other lambda-list processing; bind to complete macro call form.

### Rationale
ANSI CL requires `&whole` to appear before all other parameters and receive the entire form including the macro name. The existing `parse-lambda-list` in macro.lisp does not detect `&whole`, but `destructuring.lisp` (lines 192-206) already has working `&whole` support that can be adapted.

### Implementation Pattern
```lisp
;; Parse phase: detect &whole as first element
(defun parse-macro-lambda-list (lambda-list)
  (let ((whole-var nil)
        (env-var nil)
        (remaining lambda-list))
    ;; &whole must be first if present
    (when (and remaining (eq (first remaining) '&whole))
      (setf whole-var (second remaining))
      (setf remaining (cddr remaining)))
    ;; &environment can appear anywhere - scan and remove
    (multiple-value-setq (env-var remaining)
      (extract-environment-param remaining))
    ;; Parse rest with existing logic
    (parse-lambda-list remaining)))

;; Expansion phase: bind whole-var to form before destructuring
(let ((,whole-var ,form))
  ,@(destructure remaining (cdr form))
  ,body)
```

### Alternatives Considered
1. **Modify parse-lambda-list directly**: Rejected - would affect non-macro uses
2. **Create parse-macro-lambda-list wrapper**: Selected - clean separation
3. **Inline in compile-defmacro**: Rejected - harder to test

---

## Research Topic 3: &environment Parameter Implementation

### Decision
Create a lightweight environment struct containing only macro bindings (per spec assumptions).

### Rationale
ANSI CL `&environment` provides access to the lexical environment for macro-function queries. The spec assumes "Environment introspection is limited to macro definitions (not variable bindings)." This simplifies implementation while maintaining ANSI CL compliance for the common use cases.

### Environment Structure
```lisp
(defstruct macro-environment
  "Compile-time environment for macro expansion."
  (macros nil :type (or null macro-registry))  ; local macros from macrolet
  (parent nil :type (or null macro-environment))) ; outer environment
```

### WasmGC Type Definition
```wat
(type $macro-environment (struct
  (field $macros (ref null $macro-registry))  ; anyref to macro hash
  (field $parent (ref null $macro-environment))))
```

### Alternatives Considered
1. **Full environment (vars, functions, etc.)**: Rejected - overkill for macro expansion
2. **Pass registry directly**: Rejected - doesn't support macrolet nesting
3. **Lightweight macro-only env**: Selected - sufficient for ANSI compliance

---

## Research Topic 4: Runtime Macro Expansion

### Decision
Implement runtime macroexpand via Tier 1 interpreter with fallback to global macro registry.

### Rationale
Current implementation (func-section.lisp:4944) notes "No runtime expansion available - would require runtime macro registry." For self-hosting, the compiler needs macroexpand at runtime. The Tier 1 interpreter (Feature 017) provides the execution context.

### Implementation Strategy
1. **Global Macro Registry Export**: Export `*global-macro-registry*` as Wasm global
2. **macro-function Runtime**: Lookup in registry, return closure or nil
3. **macroexpand-1 Runtime**:
   - Get macro-function for car of form
   - If nil: return (values form nil)
   - If found: call with form, return (values result t)
4. **macroexpand Runtime**: Loop macroexpand-1 until no expansion

### Two-Value Return Pattern
```lisp
;; Uses Feature 025 multiple values
(defun macroexpand-1 (form &optional env)
  (let ((fn (macro-function (car form) env)))
    (if fn
        (values (funcall fn form env) t)
        (values form nil))))
```

### Alternatives Considered
1. **JIT-compile macro bodies**: Rejected - overkill for most use cases
2. **Interpreter-based expansion**: Selected - simpler, sufficient performance
3. **Compile-time only**: Rejected - blocks self-hosting goal

---

## Research Topic 5: WasmGC Type Allocation

### Decision
Use type index 24 for `$macro-environment` struct.

### Rationale
Current type indices 0-23 are allocated (per gc-types.lisp exploration). The next available index is 24.

### Type Definition
```wat
;; Type 24: $macro-environment
(type $macro-environment (struct
  (field $registry anyref)     ; macro name → expander hash table
  (field $parent anyref)))     ; parent environment or null
```

### Alternatives Considered
1. **Reuse existing struct types**: Rejected - semantically distinct
2. **Use raw anyref fields**: Partially selected - keeps simple while typed
3. **Complex nested types**: Rejected - unnecessary complexity

---

## Research Topic 6: Self-Compilation Baseline

### Decision
Target 27 defmacro definitions (updated from spec's 36 estimate).

### Rationale
Actual grep shows 27 defmacro forms in Clysm source. None currently use `&whole` or `&environment`, so basic compilation should work immediately. The feature enables future macros to use these parameters.

### Defmacro Distribution
| File | Count | Notes |
|------|-------|-------|
| lib/macros.lisp | 7 | when*, unless*, cond*, etc. |
| lib/setf-expanders.lisp | 2 | define-setf-expander*, defsetf* |
| lib/package-macros.lisp | 2 | in-package*, defpackage* |
| ffi/macros.lisp | 2 | FFI integration |
| filesystem/macros.lisp | 1 | with-open-file* |
| conditions/*.lisp | 7 | handler-case, restart-case, etc. |
| stage1/logging.lisp | 4 | log-debug, log-info, etc. |
| eval/compile.lisp | 1 | tiered-function-wrapper |
| **Total** | **27** | |

### Success Metric
- All 27 defmacro forms compile without error
- Macro expansion produces identical results to host SBCL

---

## Research Topic 7: Circular Expansion Detection

### Decision
Maintain 1000-step limit with explicit depth counter.

### Rationale
Current implementation already has this limit (macro.lisp line 60+). For runtime, pass depth as hidden parameter to prevent stack overflow.

### Implementation
```lisp
(defvar *macro-expansion-limit* 1000)

(defun macroexpand (form &optional env)
  (let ((depth 0))
    (loop
      (when (>= depth *macro-expansion-limit*)
        (error 'macro-expansion-depth-exceeded :form form))
      (multiple-value-bind (new-form expanded-p)
          (macroexpand-1 form env)
        (unless expanded-p
          (return (values form nil)))
        (setf form new-form)
        (incf depth)))))
```

---

## Research Topic 8: ANSI CL Compliance Verification

### Decision
Test against ANSI CL specification sections 3.4.4 (Macro Lambda Lists) and function definitions.

### Key Compliance Points
1. **&whole position**: Must be first if present (3.4.4)
2. **&environment position**: May appear anywhere (3.4.4)
3. **macroexpand return values**: Two values (form, expanded-p)
4. **macro-function**: Returns expander or nil; setf-able
5. **Environment argument**: Optional for macroexpand functions

### Test Categories
- Lambda-list parsing correctness
- Expansion result equivalence with SBCL
- Error conditions for malformed input
- Multiple-value return behavior

---

## Summary: Implementation Phases

### Phase 1: Lambda-List Enhancement
1. Add `parse-macro-lambda-list` with &whole/&environment extraction
2. Modify `compile-defmacro` to use new parser
3. Unit tests for lambda-list parsing

### Phase 2: Environment Support
1. Define `macro-environment` struct
2. Add WasmGC type definition (index 24)
3. Implement environment creation and lookup
4. Unit tests for environment operations

### Phase 3: Runtime Macro Expansion
1. Export macro registry as Wasm global
2. Implement runtime `macro-function`
3. Implement runtime `macroexpand-1` and `macroexpand`
4. Integration tests with Tier 1 interpreter

### Phase 4: Setf Macro-Function
1. Implement `(setf macro-function)` for runtime macro definition
2. Update registry modification to work at runtime
3. Unit tests for dynamic macro registration

### Phase 5: Self-Compilation Verification
1. Compile all 27 Clysm defmacro forms
2. Verify expansion equivalence with host
3. Integration tests for compiler bootstrap

---

## Dependencies Confirmed

| Dependency | Status | Notes |
|------------|--------|-------|
| Feature 016 (Macro System) | Available | Base implementation in macro.lisp |
| Feature 017 (Eval/JIT) | Available | Tier 1 interpreter for runtime |
| Feature 025 (Multiple Values) | Available | For macroexpand return values |
| Feature 031 (Destructuring) | Available | &whole parsing reference |
