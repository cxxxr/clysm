# Research: Type Dispatch Macros

**Feature**: 030-typecase-macros
**Date**: 2025-12-27
**Status**: Complete

## Research Questions

### RQ1: What is the current state of typep implementation?

**Finding**: `typep` is NOT yet implemented as a runtime function in Clysm. The compiler uses SBCL's built-in `typep` at compile-time for AST analysis, but there's no compiled `typep` for runtime dispatch.

**Evidence**:
- `src/clysm/compiler/codegen/func-section.lisp` has no `compile-typep` function
- Individual predicates (integerp, symbolp, etc.) are compiled separately
- Spec assumes 023-type-predicates provides typep, but only primitive predicates exist

**Decision**: Implement `typep` expansion within the typecase macros themselves, using the existing primitive predicates (integerp, symbolp, consp, etc.) rather than a general typep function.

**Rationale**:
- Macro-level expansion to primitive predicates is more efficient (no runtime dispatch)
- Matches how SBCL optimizes typecase (compiler macro expansion)
- Avoids adding new runtime function for self-hosting

**Alternatives Considered**:
- Implement runtime typep: Rejected - adds runtime overhead, more complex
- Defer to 023-type-predicates: Not possible - typep not actually implemented there

### RQ2: What type specifiers are used in the 892 compiler typecase forms?

**Finding**: The compiler predominantly uses atomic type specifiers that map directly to existing predicates.

**Common patterns found**:
```lisp
;; From restarts.lisp:17
(etypecase identifier
  (symbol ...)
  (clysm/runtime/condition:%restart ...))

;; Common typecase patterns in compiler
(typecase form
  (ast-literal ...)
  (ast-call ...)
  (cons ...)
  (null ...))
```

**Decision**: Support atomic type specifiers first (P1), then compound specifiers (P2).

**Type Specifier to Predicate Mapping**:
| Type Specifier | Compile to |
|----------------|------------|
| integer | (integerp x) |
| symbol | (symbolp x) |
| cons | (consp x) |
| null | (null x) |
| list | (listp x) |
| number | (numberp x) |
| float | (floatp x) |
| ratio | (rationalp x) with additional check |
| character | (characterp x) |
| function | (functionp x) |
| string | (stringp x) |
| t | (progn true) |
| otherwise | (progn default) |

### RQ3: How does the condition system's store-value restart work?

**Finding**: Full store-value restart infrastructure exists in 014-condition-system.

**Evidence** (from `src/clysm/conditions/standard.lisp:63`):
```lisp
(defun store-value (value &optional condition)
  "Store VALUE as the new value for a place.
   FR-028: store-value MUST invoke the store-value restart with value."
  (let ((restart (find-restart 'store-value condition)))
    (when restart
      (invoke-restart restart value))
    ;; No store-value restart - signal control-error
    ...))
```

**Decision**: Use existing `restart-case` and `store-value` infrastructure for ctypecase and check-type.

**Rationale**: Infrastructure already tested and working.

### RQ4: How does the macro registry work?

**Finding**: Macro registry uses `register-macro` function from `clysm/compiler/transform/macro`.

**Evidence** (from `src/clysm/lib/macros.lisp:1507`):
```lisp
(defun install-standard-macros (registry)
  (clysm/compiler/transform/macro:register-macro
   registry 'when (make-when-expander))
  ...)
```

**Decision**: Follow same pattern for typecase macros - create `make-typecase-expander`, `make-etypecase-expander`, `make-ctypecase-expander`, `make-check-type-expander` and register in `install-standard-macros`.

### RQ5: What is the correct expansion pattern?

**Finding**: The existing `make-case-expander` provides the template.

**Pattern** (from `src/clysm/lib/macros.lisp:166-207`):
1. Bind keyform to gensym for single evaluation
2. Recursively expand clauses to nested `if` forms
3. Handle `otherwise`/`t` as catch-all

**typecase Expansion Strategy**:
```lisp
;; Input:
(typecase x
  (integer (+ x 1))
  (symbol (symbol-name x))
  (otherwise "other"))

;; Expands to:
(let ((#:KEY x))
  (if (integerp #:KEY)
      (+ #:KEY 1)
      (if (symbolp #:KEY)
          (symbol-name #:KEY)
          "other")))
```

**etypecase Expansion Strategy**:
```lisp
;; Input:
(etypecase x
  (integer (+ x 1))
  (symbol (symbol-name x)))

;; Expands to:
(let ((#:KEY x))
  (if (integerp #:KEY)
      (+ #:KEY 1)
      (if (symbolp #:KEY)
          (symbol-name #:KEY)
          (error 'type-error
                 :datum #:KEY
                 :expected-type '(or integer symbol)))))
```

**check-type Expansion Strategy**:
```lisp
;; Input:
(check-type x integer)

;; Expands to:
(loop
  (when (integerp x)
    (return nil))
  (restart-case
      (error 'type-error :datum x :expected-type 'integer)
    (store-value (new-value)
      :interactive (lambda () (list (read)))
      :report (lambda (s) (format s "Supply a new value for X"))
      (setf x new-value))))
```

### RQ6: Compound type specifier handling?

**Finding**: Compound specifiers can be expanded at macro level.

**Expansion Strategy**:
| Compound | Expansion |
|----------|-----------|
| (or t1 t2) | (or (typep x 't1) (typep x 't2)) |
| (and t1 t2) | (and (typep x 't1) (typep x 't2)) |
| (not t1) | (not (typep x 't1)) |
| (member a b c) | (member x '(a b c)) |
| (satisfies pred) | (funcall pred x) |

**Decision**: Recursively expand compound specifiers at macro expansion time.

## Summary of Decisions

| Question | Decision | Rationale |
|----------|----------|-----------|
| typep implementation | Expand to primitive predicates | Efficiency, no runtime overhead |
| Type specifiers | Atomic first, compound second | Match priority levels |
| store-value | Use existing infrastructure | Already implemented |
| Macro pattern | Follow case expander pattern | Proven pattern in codebase |
| Compound types | Macro-level expansion | No runtime dispatch needed |

## Next Steps

1. Create `make-typecase-expander` following `make-case-expander` pattern
2. Add type-specifier-to-predicate mapping function
3. Implement compound type specifier recursion
4. Integrate with `install-standard-macros`
5. Create tests following TDD
