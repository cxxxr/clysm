# Research: DEFSTRUCT Wasm Compilation

**Feature**: 001-defstruct-wasm-compile
**Date**: 2025-12-30
**Status**: Complete

## Executive Summary

[defstruct](resources/HyperSpec/Body/m_defstr.htm) compilation is feasible by leveraging the existing CLOS infrastructure. The compiler already has 150+ defstruct definitions using `:conc-name` and `:include` options, making this a high-impact feature for self-hosting.

## Research Areas

### 1. CLOS Infrastructure Reusability

**Decision**: Reuse existing `defclass` compilation path via macro expansion

**Rationale**:
- `defclass.lisp` already parses slots with initargs, initforms, accessors
- `$instance` type (index 6) in WasmGC provides class + slots-vector representation
- `$standard-class` type (index 7) stores class metadata (name, slots, superclass)
- Structures are a simplified subset of CLOS classes

**Alternatives Considered**:
| Alternative | Rejected Because |
|-------------|------------------|
| Direct WasmGC struct codegen | Duplicates CLOS machinery; maintenance burden |
| Runtime structure definition | Would require interpreter support; compilation-only approach preferred |

**Key Files**:
- `src/clysm/clos/defclass.lisp` (lines 1-122): defclass parsing
- `src/clysm/clos/mop.lisp` (lines 40-62): standard-class, standard-instance structs
- `src/clysm/compiler/codegen/gc-types.lisp` (lines 432-470): $instance, $standard-class types

### 2. Structure Type Registration

**Decision**: Register structure types in class registry using `structure-class` metaclass

**Rationale**:
- Existing `*class-registry*` hash table supports lookup by symbol name
- [type-of](resources/HyperSpec/Body/f_tp_of.htm) and [typep](resources/HyperSpec/Body/f_typep.htm) already use class registry
- Structure predicate can delegate to `typep` call

**Implementation**:
```lisp
;; Structure class marker
(defstruct (structure-class (:include standard-class)
                            (:conc-name structure-class-))
  (copier nil :type (or null symbol))
  (predicate nil :type (or null symbol)))
```

**Key Files**:
- `src/clysm/clos/mop.lisp` (lines 9-20): `*class-registry*` and `find-class*`

### 3. Setf Expander Generation

**Decision**: Generate setf expanders using existing `register-setf-expander` API

**Rationale**:
- `get-setf-expansion*` returns 5 values per ANSI CL specification
- Infrastructure supports compound places (consp check → lookup → funcall)
- Standard expanders for AREF, CAR, etc. provide implementation patterns

**Implementation Pattern** (from `setf-expanders.lisp` lines 294-310):
```lisp
;; Generated expander for (setf (point-x p) value)
(lambda (place env)
  (declare (ignore env))
  (let ((obj (gensym "OBJ"))
        (val (gensym "VAL")))
    (values (list obj)                     ; temps
            (list (second place))           ; vals
            (list val)                      ; stores
            `(slot-value ,obj 'x ,val)      ; store-form (returns val)
            `(slot-value ,obj 'x))))        ; access-form
```

**Key Files**:
- `src/clysm/lib/setf-expanders.lisp` (lines 83-112): `get-setf-expansion*`
- `src/clysm/lib/setf-expanders.lisp` (lines 15-27): Registry API

### 4. Current defstruct Usage Patterns

**Decision**: Support `:conc-name`, `:include`, `:constructor`, `:predicate`, `:copier`

**Rationale**: These options cover 100% of existing compiler defstructs

**Usage Statistics** (150+ definitions):
| Option | Count | Files |
|--------|-------|-------|
| `:conc-name` | ~80 | ast.lisp, mop.lisp, generic.lisp |
| `:include` | ~35 | ast.lisp, macros.lisp, gc-types.lisp |
| `:constructor` | ~20 | gc-types.lisp, streams.lisp |
| `:print-function` | 2 | mop.lisp (OUT OF SCOPE) |

**Critical Pattern** (AST nodes):
```lisp
(defstruct (ast-node (:conc-name ast-node-))
  (source-location nil :type (or null source-location)))

(defstruct (ast-literal (:include ast-node) (:conc-name ast-literal-))
  (value nil))
```

**Key Files**:
- `src/clysm/compiler/ast.lisp`: 30+ AST node structs with `:include`
- `src/clysm/lib/macros.lisp` (lines 574-669): LOOP infrastructure structs

### 5. Macro Expansion Approach

**Decision**: Implement defstruct as macro expanding to progn of defclass + defun forms

**Rationale**:
- Macro registry already supports registration via `register-macro`
- `install-standard-macros` pattern for initialization
- &whole and &environment support available (Feature 042)

**Expansion Template**:
```lisp
(defstruct (point (:conc-name p-) (:predicate pointp))
  x y)

;; Expands to:
(progn
  (defclass* point ()
    ((x :initarg :x :accessor p-x)
     (y :initarg :y :accessor p-y))
    (:metaclass structure-class))

  (defun make-point (&key x y)
    (make-instance 'point :x x :y y))

  (defun pointp (obj)
    (typep obj 'point))

  (define-setf-expander p-x (place)
    ...)
  (define-setf-expander p-y (place)
    ...)

  'point)
```

**Key Files**:
- `src/clysm/compiler/transform/macro.lisp` (lines 596-597): `*global-macro-registry*`
- `src/clysm/lib/macros.lisp` (lines 2021-2060): `install-standard-macros`

### 6. Inheritance via :include

**Decision**: Map `:include` to defclass superclass with slot copying

**Rationale**:
- CLOS CPL (Class Precedence List) handles method dispatch
- Parent accessors work on child instances via slot indexing
- `compute-slot-indices` already merges inherited slots

**Implementation**:
```lisp
(defstruct (child (:include parent) (:conc-name child-))
  new-slot)

;; Expands to:
(defclass* child (parent)  ; parent as superclass
  ((new-slot :initarg :new-slot :accessor child-new-slot))
  (:metaclass structure-class))
```

**Note**: Child predicate must also pass parent predicate (enforced by class hierarchy)

**Key Files**:
- `src/clysm/clos/mop.lisp` (lines 101-121): `compute-slot-indices`
- `src/clysm/clos/mop.lisp` (lines 78-84): `compute-class-precedence-list`

## Dependencies

| Dependency | Status | Notes |
|------------|--------|-------|
| defclass compilation | ✅ Ready | Full slot parsing, WasmGC codegen |
| make-instance | ✅ Ready | Keyword argument construction |
| setf-expander registry | ✅ Ready | Five-value protocol |
| macro registry | ✅ Ready | &whole, &environment support |
| typep | ✅ Ready | Class-based type checking |

## Risks and Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| :include with slot override | Medium | Low | Document as unsupported initially |
| :constructor nil (suppress) | Low | Medium | Support by skipping defun generation |
| Circular struct definitions | Low | High | Detect and error at macro expansion |

## Unresolved Questions

None - all clarifications resolved through codebase research.
