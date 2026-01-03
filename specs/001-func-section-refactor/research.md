# Research: func-section.lisp Refactoring

**Date**: 2026-01-03
**Feature**: 001-func-section-refactor

## Research Topics

### 1. Hash Table Dispatch for Primitive Compilers

**Question**: What is the best approach for replacing 200+ case branches with table-driven dispatch?

**Decision**: Use dual hash tables - one for symbol lookup (`:test 'eq`) and one for name-based lookup (`:test 'equal`)

**Rationale**:
- Symbol lookup is O(1) with `eq` test (pointer comparison)
- Name-based lookup needed for `%SETF-*` prefixed operations (string comparison)
- Separating tables avoids type dispatch overhead on every lookup
- Follows Common Lisp idiom of `gethash` for extensible dispatch

**Alternatives Considered**:
- Single hash table with mixed keys: Rejected - requires runtime type checking
- Property list dispatch: Rejected - O(n) lookup time
- Keep case statement: Rejected - 363 lines, O(n) worst case, hard to maintain

**Implementation Pattern**:
```lisp
(defparameter *primitive-compilers* (make-hash-table :test 'eq))
(defparameter *primitive-compilers-by-name* (make-hash-table :test 'equal))

(defun register-primitive-compiler (op compiler &key by-name)
  (if by-name
      (setf (gethash (string op) *primitive-compilers-by-name*) compiler)
      (setf (gethash op *primitive-compilers*) compiler)))
```

### 2. Instruction Collection Efficiency

**Question**: How to replace O(n²) append patterns with O(n) alternatives?

**Decision**: Use `push` + `nreverse` pattern with a `with-instruction-collector` macro

**Rationale**:
- `push` is O(1) cons operation
- Single `nreverse` at end is O(n)
- Total: O(n) vs O(n²) for repeated append
- Macro encapsulates the pattern, reducing boilerplate
- Generated Wasm output is identical (order preserved via nreverse)

**Alternatives Considered**:
- Vector with fill-pointer: More complex, marginally faster for large n
- Difference lists: Not idiomatic in Common Lisp
- Keep append: Rejected - measured O(n²) impact on large modules

**Implementation Pattern**:
```lisp
(defmacro with-instruction-collector ((var) &body body)
  `(let ((,var '()))
     (macrolet ((emit (instr) `(push ,instr ,',var))
                (emit* (&rest instrs)
                  `(progn ,@(mapcar (lambda (i) `(push ,i ,',var)) instrs))))
       ,@body
       (nreverse ,var))))
```

### 3. cXXr Function Consolidation

**Question**: How to eliminate 12 nearly-identical compile-cXXr functions?

**Decision**: Use macro to generate all cXXr compilers from a specification

**Rationale**:
- All cXXr functions follow identical pattern: chain of car/cdr operations
- Macro generation eliminates repetition (12 functions → 12 one-liners)
- `compile-cxr-chain` already exists but isn't properly exported
- Error pattern P626 indicates export/accessibility issue

**Alternatives Considered**:
- Keep individual functions: Rejected - 100+ lines of redundant code
- Single function with runtime dispatch: Rejected - extra runtime overhead

**Implementation Pattern**:
```lisp
(defmacro define-cxr-compiler (name ops)
  `(defun ,(intern (format nil "COMPILE-~A" name)) (args env)
     ,(format nil "Compile (~A x)" (string-downcase name))
     (compile-cxr-chain ',ops args env)))

(define-cxr-compiler caar (:car :car))
(define-cxr-compiler cadr (:cdr :car))
(define-cxr-compiler cdar (:car :cdr))
(define-cxr-compiler cddr (:cdr :cdr))
;; ... etc for all 12 cXXr variants
```

**Fix for P626**: Export `compile-cxr-chain` from `clysm/compiler/codegen` package.

### 4. Equality Function Unification

**Question**: How to unify 4 separate equality predicates (eq, eql, equal, equalp) totaling 800+ lines?

**Decision**: Create unified `compile-equality-predicate` with level parameter, backed by shared type dispatch infrastructure

**Rationale**:
- [eq](resources/HyperSpec/Body/f_eq.htm): Pointer equality (`ref.eq`)
- [eql](resources/HyperSpec/Body/f_eql.htm): eq + numeric/character value equality
- [equal](resources/HyperSpec/Body/f_equal.htm): eql + structural equality for lists/strings
- [equalp](resources/HyperSpec/Body/f_equalp.htm): equal + case-insensitive strings + numeric type coercion
- Each level extends the previous - natural for unified implementation
- Type dispatch code is 80% identical across all four

**Alternatives Considered**:
- Keep separate functions: Rejected - 800+ lines, inconsistent type handling
- Single runtime function: Rejected - loses inline optimization for simple cases

**Implementation Pattern**:
```lisp
(defun compile-equality-predicate (args env &key (level :eq))
  (let ((type-dispatch (build-equality-type-dispatch level)))
    (compile-binary-predicate args env type-dispatch)))

(defun compile-eq (args env)
  (compile-equality-predicate args env :level :eq))

(defun compile-eql (args env)
  (compile-equality-predicate args env :level :eql))
;; ... etc
```

### 5. Runtime Function Migration Strategy

**Question**: Which functions should be migrated from inline Wasm generation to Lisp runtime?

**Decision**: Migrate functions that:
1. Have complex control flow (multiple branches, loops)
2. Use keyword arguments
3. Are >100 lines of codegen
4. Have pure Lisp semantics (no Wasm-specific optimizations needed)

**Migration Candidates by Category**:

| Category | Functions | Est. Lines Saved |
|----------|-----------|------------------|
| String | [string-trim](resources/HyperSpec/Body/f_stg_tr.htm), [string-upcase](resources/HyperSpec/Body/f_stg_up.htm), [string-downcase](resources/HyperSpec/Body/f_stg_up.htm), [string-capitalize](resources/HyperSpec/Body/f_stg_up.htm), nstring-* variants | 600 |
| Numeric | [parse-integer](resources/HyperSpec/Body/f_parse_.htm), [write-to-string](resources/HyperSpec/Body/f_wr_to_.htm), [rationalize](resources/HyperSpec/Body/f_ration.htm), [signum](resources/HyperSpec/Body/f_signum.htm), [phase](resources/HyperSpec/Body/f_phase.htm) | 800 |
| Sequence | [subseq](resources/HyperSpec/Body/f_subseq.htm), [adjust-array](resources/HyperSpec/Body/f_adjust.htm) | 350 |

**NOT Migrating** (keep as inline Wasm):
- [car](resources/HyperSpec/Body/f_car_c.htm)/[cdr](resources/HyperSpec/Body/f_car_c.htm): Single struct.get instruction
- Arithmetic (+, -, *, /): Direct i32/f64 operations
- Type predicates: Direct ref.test instructions
- [cons](resources/HyperSpec/Body/f_cons.htm): Single struct.new instruction

### 6. Duplicate Registration Policy

**Question**: What should happen when a primitive is registered twice?

**Decision**: Later registration silently replaces earlier one (override semantics)

**Rationale**:
- Follows Common Lisp's `setf` semantics for hash tables
- Enables iterative development (redefine without restart)
- Matches CLOS method redefinition behavior
- Alternative (error on duplicate) would break REPL workflow

### 7. Phased Migration Strategy

**Question**: Should dispatch table coexist with old case-based dispatch during migration?

**Decision**: Yes - implement hybrid approach with gradual migration

**Rationale**:
- Allows incremental testing of each migrated primitive
- Reduces risk of breaking changes
- Each phase independently deployable
- Fallback to case statement for unmigrated primitives

**Implementation**:
```lisp
(defun compile-primitive-call (op args env)
  ;; 1. Try new dispatch table first
  (let ((compiler (or (gethash op *primitive-compilers*)
                      (gethash (symbol-name op) *primitive-compilers-by-name*))))
    (when compiler
      (return-from compile-primitive-call (funcall compiler args env))))
  ;; 2. Fall back to legacy case statement (shrinks each phase)
  (legacy-compile-primitive-call op args env))
```

## Resolved Clarifications

| Question | Resolution |
|----------|------------|
| Duplicate registration | Override silently (standard CL hash table semantics) |
| Migration coexistence | Hybrid approach with gradual fallback removal |
| Performance verification | Existing tests + wasm-tools validate |
| Runtime dependency order | Existing `*runtime-function-table*` handles this |

## Next Steps

1. Proceed to data-model.md for entity definitions
2. No contracts/ needed (internal compiler refactoring)
3. Generate quickstart.md with development workflow
