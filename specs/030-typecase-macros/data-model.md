# Data Model: Type Dispatch Macros

**Feature**: 030-typecase-macros
**Date**: 2025-12-27

## Entity Overview

This feature is primarily a macro system with no persistent data structures. The key entities are compile-time structures used during macro expansion.

## Entities

### Type Specifier

A symbol or list describing a type for matching.

**Forms**:
- **Atomic**: `integer`, `symbol`, `cons`, `null`, `list`, `number`, `float`, `ratio`, `character`, `function`, `string`, `t`
- **Compound**:
  - `(or type1 type2 ...)` - matches if any type matches
  - `(and type1 type2 ...)` - matches if all types match
  - `(not type)` - matches if type does NOT match
  - `(member item1 item2 ...)` - matches if value is eql to any item
  - `(satisfies pred)` - matches if predicate returns true

**Mapping to Predicates**:
```
integer    -> integerp
symbol     -> symbolp
cons       -> consp
null       -> null
list       -> listp
number     -> numberp
float      -> floatp
ratio      -> rationalp (then check not integerp)
character  -> characterp
function   -> functionp
string     -> stringp
t          -> (constantly t)
```

### Typecase Clause

A single clause in a typecase form.

**Structure**:
```
(type-specifier-or-list . body-forms)
```

**Examples**:
```lisp
(integer (+ x 1))           ; single type, single body
((integer symbol) x)        ; multiple types, single body
(string (length s) (print s)) ; single type, multiple body forms
(otherwise nil)             ; catch-all
(t default-value)           ; catch-all (alternative syntax)
```

**Validation Rules**:
- For etypecase/ctypecase: `otherwise` and `t` are NOT allowed
- Type specifiers must be valid (atomic or compound)
- Body forms can be any Lisp expression

### Expected Type

Compound type created from all clause types for error reporting.

**Construction**:
```lisp
;; From clauses
((integer ...) (symbol ...) (cons ...))

;; Produces
(or integer symbol cons)
```

### Place

A setf-able location for ctypecase and check-type.

**Valid Place Forms**:
- Variable: `x`
- Accessor: `(car list)`, `(aref array i)`, `(gethash key table)`
- CLOS slot: `(slot-value obj 'slot)`

**Used By**:
- ctypecase: `(ctypecase place ...clauses...)`
- check-type: `(check-type place typespec)`

## Expansion Results

### typecase Expansion

**Input**:
```lisp
(typecase keyform
  (type1 body1...)
  (type2 body2...)
  (otherwise default...))
```

**Output**:
```lisp
(let ((#:KEY keyform))
  (if (type1-p #:KEY)
      (progn body1...)
      (if (type2-p #:KEY)
          (progn body2...)
          (progn default...))))
```

### etypecase Expansion

**Input**:
```lisp
(etypecase keyform
  (type1 body1...)
  (type2 body2...))
```

**Output**:
```lisp
(let ((#:KEY keyform))
  (if (type1-p #:KEY)
      (progn body1...)
      (if (type2-p #:KEY)
          (progn body2...)
          (error 'type-error
                 :datum #:KEY
                 :expected-type '(or type1 type2)))))
```

### ctypecase Expansion

**Input**:
```lisp
(ctypecase place
  (type1 body1...)
  (type2 body2...))
```

**Output**:
```lisp
(loop
  (let ((#:KEY place))
    (if (type1-p #:KEY)
        (return (progn body1...))
        (if (type2-p #:KEY)
            (return (progn body2...))
            (restart-case
                (error 'type-error
                       :datum #:KEY
                       :expected-type '(or type1 type2))
              (store-value (new-value)
                :report "Supply a new value"
                :interactive read-new-value
                (setf place new-value)))))))
```

### check-type Expansion

**Input**:
```lisp
(check-type place typespec "a positive integer")
```

**Output**:
```lisp
(loop
  (when (typespec-p place)
    (return nil))
  (restart-case
      (error 'type-error
             :datum place
             :expected-type 'typespec)
    (store-value (new-value)
      :report "Supply a new value (a positive integer)"
      :interactive read-new-value
      (setf place new-value))))
```

## Type Specifier Expansion

### Compound Type Expansion

**or**:
```lisp
(typep x '(or t1 t2 t3))
;; expands to
(or (typep x 't1) (typep x 't2) (typep x 't3))
```

**and**:
```lisp
(typep x '(and t1 t2))
;; expands to
(and (typep x 't1) (typep x 't2))
```

**not**:
```lisp
(typep x '(not t1))
;; expands to
(not (typep x 't1))
```

**member**:
```lisp
(typep x '(member :a :b :c))
;; expands to
(member x '(:a :b :c) :test #'eql)
;; or more efficiently
(or (eql x :a) (eql x :b) (eql x :c))
```

**satisfies**:
```lisp
(typep x '(satisfies evenp))
;; expands to
(funcall #'evenp x)
```

## State Transitions

No runtime state - all transformations occur at compile time (macro expansion time).

## Relationships

```
typecase       ->  uses: type-specifier, clause
etypecase      ->  uses: type-specifier, clause, type-error
ctypecase      ->  uses: type-specifier, clause, type-error, store-value, place
check-type     ->  uses: type-specifier, type-error, store-value, place

type-specifier ->  maps to: predicate function
clause         ->  contains: type-specifier(s), body forms
place          ->  requires: setf expansion (from 028)
type-error     ->  from: 014-condition-system
store-value    ->  from: 014-condition-system
```
