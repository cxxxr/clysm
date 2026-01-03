# Research: Primitive Dispatch Table

**Feature**: 002-primitive-dispatch-table
**Date**: 2026-01-03
**Status**: Complete

## Executive Summary

Analysis of the Clysm compiler reveals an existing infrastructure for hash-table dispatch in `primitive-dispatch.lisp` with an empty registry. The main migration task involves populating this registry with 240+ primitive compilers currently embedded in a 538-line case statement in `func-section.lisp`.

## Findings

### 1. Current Implementation Location

**File**: `src/clysm/compiler/codegen/func-section.lisp`
**Function**: `compile-primitive-call` at line 1148
**Case Statement**: Lines 1184-1722 (538 lines)

**Decision**: Leverage existing infrastructure in `primitive-dispatch.lisp`
**Rationale**: Infrastructure already exists; only registration and integration needed
**Alternatives Considered**:
- Build new dispatch from scratch (rejected: duplication)
- Use CLOS generic functions (rejected: slower than hash-table for symbol dispatch)

### 2. Primitive Count and Categories

| Category | Count | Examples |
|----------|-------|----------|
| Arithmetic | 28 | [+](resources/HyperSpec/Body/f_pl.htm), [-](resources/HyperSpec/Body/f__.htm), [*](resources/HyperSpec/Body/f_st.htm), [/](resources/HyperSpec/Body/f_sl.htm), [1+](resources/HyperSpec/Body/f_1pl_1_.htm), [1-](resources/HyperSpec/Body/f_1pl_1_.htm), [abs](resources/HyperSpec/Body/f_abs.htm), [signum](resources/HyperSpec/Body/f_signum.htm) |
| Comparison | 12 | [=](resources/HyperSpec/Body/f_eq_sle.htm), [<](resources/HyperSpec/Body/f_eq_sle.htm), [>](resources/HyperSpec/Body/f_eq_sle.htm), [<=](resources/HyperSpec/Body/f_eq_sle.htm), [>=](resources/HyperSpec/Body/f_eq_sle.htm), [/=](resources/HyperSpec/Body/f_eq_sle.htm), [zerop](resources/HyperSpec/Body/f_zerop.htm) |
| Trigonometric | 12 | [sin](resources/HyperSpec/Body/f_sin_c.htm), [cos](resources/HyperSpec/Body/f_sin_c.htm), [tan](resources/HyperSpec/Body/f_sin_c.htm), [asin](resources/HyperSpec/Body/f_asin_.htm), [acos](resources/HyperSpec/Body/f_asin_.htm), [atan](resources/HyperSpec/Body/f_asin_.htm) |
| Hyperbolic | 6 | [sinh](resources/HyperSpec/Body/f_sinh_.htm), [cosh](resources/HyperSpec/Body/f_sinh_.htm), [tanh](resources/HyperSpec/Body/f_sinh_.htm), [asinh](resources/HyperSpec/Body/f_sinh_.htm) |
| Bit Operations | 10 | [ash](resources/HyperSpec/Body/f_ash.htm), [logand](resources/HyperSpec/Body/f_logand.htm), [logior](resources/HyperSpec/Body/f_logior.htm), [logxor](resources/HyperSpec/Body/f_logxor.htm), [lognot](resources/HyperSpec/Body/f_lognot.htm) |
| List Operations | 35 | [car](resources/HyperSpec/Body/f_car_c.htm), [cdr](resources/HyperSpec/Body/f_car_c.htm), [cons](resources/HyperSpec/Body/f_cons.htm), [list](resources/HyperSpec/Body/f_list_.htm), [append](resources/HyperSpec/Body/f_append.htm), [nconc](resources/HyperSpec/Body/f_nconc.htm) |
| Sequence | 25 | [length](resources/HyperSpec/Body/f_length.htm), [elt](resources/HyperSpec/Body/f_elt.htm), [subseq](resources/HyperSpec/Body/f_subseq.htm), [reverse](resources/HyperSpec/Body/f_revers.htm), [nreverse](resources/HyperSpec/Body/f_revers.htm) |
| Array | 20 | [aref](resources/HyperSpec/Body/f_aref.htm), [array-rank](resources/HyperSpec/Body/f_ar_ran.htm), [array-dimension](resources/HyperSpec/Body/f_ar_dim.htm), [make-array](resources/HyperSpec/Body/f_mk_ar.htm) |
| String | 22 | [char](resources/HyperSpec/Body/f_char_.htm), [string=](resources/HyperSpec/Body/f_stgeq_.htm), [string-upcase](resources/HyperSpec/Body/f_stg_up.htm), [string-trim](resources/HyperSpec/Body/f_stg_tr.htm) |
| Character | 15 | [char-code](resources/HyperSpec/Body/f_char_c.htm), [code-char](resources/HyperSpec/Body/f_code_c.htm), [char-upcase](resources/HyperSpec/Body/f_char_u.htm), [alpha-char-p](resources/HyperSpec/Body/f_alpha_.htm) |
| Type Predicates | 25 | [consp](resources/HyperSpec/Body/f_consp.htm), [symbolp](resources/HyperSpec/Body/f_symbol.htm), [numberp](resources/HyperSpec/Body/f_nump.htm), [stringp](resources/HyperSpec/Body/f_stgp.htm), [functionp](resources/HyperSpec/Body/f_fnp.htm) |
| Equality | 4 | [eq](resources/HyperSpec/Body/f_eq.htm), [eql](resources/HyperSpec/Body/f_eql.htm), [equal](resources/HyperSpec/Body/f_equal.htm), [equalp](resources/HyperSpec/Body/f_equalp.htm) |
| Symbol | 8 | [symbol-name](resources/HyperSpec/Body/f_symb_2.htm), [symbol-value](resources/HyperSpec/Body/f_symb_5.htm), [symbol-function](resources/HyperSpec/Body/f_symb_1.htm), [get](resources/HyperSpec/Body/f_get.htm) |
| Hash Table | 6 | [gethash](resources/HyperSpec/Body/f_gethas.htm), [remhash](resources/HyperSpec/Body/f_remhas.htm), [maphash](resources/HyperSpec/Body/f_maphas.htm) |
| Control | 8 | [apply](resources/HyperSpec/Body/f_apply.htm), [funcall](resources/HyperSpec/Body/f_funcal.htm), [values](resources/HyperSpec/Body/f_values.htm), [error](resources/HyperSpec/Body/f_error.htm) |
| Misc | 14 | [identity](resources/HyperSpec/Body/f_identi.htm), [constantly](resources/HyperSpec/Body/f_cons_1.htm), [not](resources/HyperSpec/Body/f_not.htm), [null](resources/HyperSpec/Body/f_not.htm) |
| **TOTAL** | **~240** | |

**Decision**: Organize registrations by category in separate blocks within `primitive-registry.lisp`
**Rationale**: Maintainability and code navigation
**Alternatives Considered**: Single alphabetical list (rejected: harder to find related primitives)

### 3. Code Generator Function Signature

**Current Pattern**:
```lisp
(defun compile-<primitive> (args env)
  "Documentation with stack effect: [] -> [result-type]"
  (when (/= (length args) N)
    (error "<primitive> requires N arguments"))
  (with-instruction-collector
    (emit* (compile-to-instructions (first args) env))
    (emit :wasm-instruction type-constant)))
```

**Key Components**:
- `args`: List of AST nodes (arguments to primitive)
- `env`: Lexical compilation environment
- Returns: List of Wasm instructions
- Uses `with-instruction-collector`, `emit`, `emit*` macros

**Decision**: Keep existing function signatures; no wrapper needed
**Rationale**: Existing dispatch infrastructure matches this signature
**Alternatives Considered**: Wrap in lambda with op parameter (rejected: unnecessary overhead)

### 4. Existing Dispatch Infrastructure

**File**: `src/clysm/compiler/codegen/primitive-dispatch.lisp` (134 lines)

**Structures**:
```lisp
(defstruct primitive-entry
  (compiler-fn nil :type (or function null))
  (arity nil :type (or fixnum null))
  (flags nil :type list))

(defvar *primitive-symbol-table* (make-hash-table :test 'eq))
(defvar *primitive-string-table* (make-hash-table :test 'equal))
```

**Registration API**:
```lisp
(register-primitive-compiler symbol compiler-fn &key arity flags string-name)
(dispatch-primitive op args env) -> instruction-list or NIL
```

**Decision**: Use existing infrastructure as-is
**Rationale**: Already implements FR-001 through FR-010 from spec
**Alternatives Considered**: Build separate registry (rejected: violates DRY)

### 5. Parallel Dispatch Pattern

**Existing**: `*runtime-function-table*` in func-section.lisp (lines 69-240)

This pattern already migrated runtime functions (I/O, list search, sequence ops) out of `compile-primitive-call` to a hash-table dispatch.

**Decision**: Follow same migration pattern for inline primitives
**Rationale**: Proven pattern in same codebase; maintains consistency
**Alternatives Considered**: Unified single table (rejected: different compilation strategies)

### 6. Migration Strategy

**Phase 1**: Register all primitives with existing `register-primitive-compiler`
```lisp
;; In primitive-registry.lisp
(register-primitive-compiler 'car #'compile-car :arity 1)
(register-primitive-compiler 'cdr #'compile-cdr :arity 1)
(register-primitive-compiler 'cons #'compile-cons :arity 2)
;; ... 237 more
```

**Phase 2**: Replace case statement with dispatch call
```lisp
;; In compile-primitive-call
(defun compile-primitive-call (op args env)
  (or (dispatch-primitive op args env)
      (error "Unknown primitive: ~A" op)))
```

**Phase 3**: Verify byte-identical output
- Generate baseline Wasm with case-statement version
- Generate test Wasm with dispatch-table version
- Binary diff to confirm identical output

**Decision**: Incremental migration with per-category verification
**Rationale**: Reduces risk; enables early detection of issues
**Alternatives Considered**: Big-bang migration (rejected: too risky for 240+ primitives)

### 7. Test Strategy

**Existing Tests**:
- `tests/unit/primitive-dispatch-test.lisp`: API unit tests
- `tests/contract/primitive-dispatch-wasm-test.lisp`: Wasm output validation

**Additional Tests Needed**:
- Byte-identical baseline comparison for each primitive category
- Coverage test for all 240+ primitives registered
- Performance regression (dispatch lookup time)

**Decision**: TDD with category-by-category baseline verification
**Rationale**: Constitution Principle VII mandates TDD; baselines ensure FR-004
**Alternatives Considered**: Post-migration testing only (rejected: violates TDD)

## Unresolved Items

None. All technical decisions made with clear rationale.

## Dependencies

| Dependency | Version | Purpose |
|------------|---------|---------|
| alexandria | Latest | Hash-table utilities |
| SBCL | 2.4+ | Host Lisp with hash-table performance |
| wasm-tools | Latest | Binary validation |
