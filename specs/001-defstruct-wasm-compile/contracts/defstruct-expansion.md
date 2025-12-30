# Contract: DEFSTRUCT Macro Expansion

**Feature**: 001-defstruct-wasm-compile
**Date**: 2025-12-30

## Overview

This contract defines the expected output of [defstruct](resources/HyperSpec/Body/m_defstr.htm) macro expansion.

## Input Format

```lisp
(defstruct name-and-options &rest slot-descriptions)
```

Where `name-and-options` is either:
- A symbol: `name`
- A list: `(name option*)`

And `slot-descriptions` are either:
- A symbol: `slot-name` (default initform is nil)
- A list: `(slot-name initform &key type read-only)`

## Output Format

### Basic Structure (no options)

```lisp
;; Input
(defstruct point x y)

;; Output
(progn
  (defclass* point ()
    ((x :initarg :x :initform nil :accessor point-x)
     (y :initarg :y :initform nil :accessor point-y))
    (:metaclass structure-class))

  (defun make-point (&key x y)
    (make-instance 'point :x x :y y))

  (defun point-p (obj)
    (typep obj 'point))

  (defun copy-point (structure)
    (make-instance 'point
                   :x (point-x structure)
                   :y (point-y structure)))

  (define-setf-expander* point-x (place &environment env)
    (declare (ignore env))
    (let ((obj (gensym "OBJ"))
          (val (gensym "VAL")))
      (values (list obj) (list (second place)) (list val)
              `(setf (slot-value ,obj 'x) ,val)
              `(slot-value ,obj 'x))))

  (define-setf-expander* point-y (place &environment env)
    ...)

  'point)
```

### With :conc-name Option

```lisp
;; Input
(defstruct (point (:conc-name p-)) x y)

;; Output: accessors named p-x, p-y instead of point-x, point-y
```

### With :conc-name nil

```lisp
;; Input
(defstruct (point (:conc-name nil)) x y)

;; Output: accessors named x, y (no prefix)
```

### With :include Option

```lisp
;; Input
(defstruct (child (:include parent)) new-slot)

;; Output
(progn
  (defclass* child (parent)  ; parent as superclass
    ((new-slot :initarg :new-slot :initform nil :accessor child-new-slot))
    (:metaclass structure-class))

  (defun make-child (&key a b new-slot)  ; includes parent slots
    (make-instance 'child :a a :b b :new-slot new-slot))

  (defun child-p (obj)
    (typep obj 'child))

  (defun copy-child (structure)
    (make-instance 'child
                   :a (parent-a structure)
                   :b (parent-b structure)
                   :new-slot (child-new-slot structure)))

  ;; Only new-slot gets accessor/setf-expander defined
  ;; Parent accessors (parent-a, parent-b) already exist

  'child)
```

### With :predicate Option

```lisp
;; Input
(defstruct (point (:predicate is-point)) x y)

;; Output: predicate named is-point instead of point-p
```

```lisp
;; Input: suppress predicate
(defstruct (point (:predicate nil)) x y)

;; Output: no predicate function generated
```

### With :copier Option

```lisp
;; Input
(defstruct (point (:copier clone-point)) x y)

;; Output: copier named clone-point instead of copy-point
```

```lisp
;; Input: suppress copier
(defstruct (point (:copier nil)) x y)

;; Output: no copier function generated
```

### With :constructor Option

```lisp
;; Input: custom name
(defstruct (point (:constructor create-point)) x y)

;; Output: constructor named create-point instead of make-point
```

```lisp
;; Input: suppress constructor
(defstruct (point (:constructor nil)) x y)

;; Output: no constructor function generated
```

### With Slot Options

```lisp
;; Input
(defstruct point
  (x 0 :type fixnum)
  (y 0 :type fixnum :read-only t))

;; Output
(progn
  (defclass* point ()
    ((x :initarg :x :initform 0 :accessor point-x)
     (y :initarg :y :initform 0 :reader point-y))  ; reader, not accessor
    (:metaclass structure-class))

  ;; No setf-expander for y (read-only)
  ...)
```

## Verification

### Required Outputs

| Component | Generated When |
|-----------|---------------|
| defclass* form | Always |
| make-NAME function | Unless `:constructor nil` |
| NAME-p predicate | Unless `:predicate nil` |
| copy-NAME copier | Unless `:copier nil` |
| NAME-slot accessors | For each slot |
| setf expanders | For each non-read-only slot |

### Return Value

The expansion MUST return the structure name as the last form in the progn:

```lisp
(progn
  ...
  'point)  ; <-- structure name returned
```

## Error Conditions

| Condition | When |
|-----------|------|
| `undefined-structure-type` | :include references unknown structure |
| `duplicate-slot-name` | Same slot name in parent and child |
| `invalid-defstruct-option` | Unrecognized option keyword |
| `invalid-slot-specification` | Malformed slot definition |
